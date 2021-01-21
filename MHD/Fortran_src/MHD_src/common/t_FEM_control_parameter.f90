!>@file   t_FEM_control_parameter.f90
!!@brief  module t_FEM_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!!@brief  module t_FEM_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for FEM MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_area_group_name(ngrp, area_group)
!!      subroutine dealloc_area_group_name(area_group)
!!
!!      subroutine set_residual_4_crank                                 &
!!     &         (dt, fl_prop, cd_prop, ht_prop, cp_prop, FEM_prm)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!@endverbatim
!
      module t_FEM_control_parameter
!
      use m_precision
      use m_constants
      use t_physical_property
      use t_iccg_parameter
      use t_MGCG_parameter
      use t_ctl_param_volume_repart
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                      :: cflag_rot_form = 'Rotation_form'
!
!>      ID for using SUPG by mangeitc field
      integer (kind=kint), parameter :: id_magnetic_SUPG =  2
!
!
      type area_group_name_list
        integer(kind = kint) :: num_group = 0
        character(len=kchara), allocatable :: group_name(:)
      end type area_group_name_list
!
!
      type FEM_MHD_paremeters
!>        Number of quadrature points for time evolution
        integer(kind = kint) :: npoint_t_evo_int =   2
!>        Number of quadrature points for Poisson equation
        integer(kind = kint) :: npoint_poisson_int = 2
!
!>        Number of iteration for Multi-pass scheme
        integer(kind = kint) :: num_multi_pass =     0
!
!>          Using rotation form for inertia and Lorentz force
        integer(kind = kint) :: iflag_rotate_form =  id_turn_OFF
!>         Coriolist terms adjustment in implicit scheme
        integer(kind = kint) :: iflag_imp_correct = id_turn_OFF
!
!>        SUPG flag for velocity
        integer (kind=kint) :: iflag_velo_supg =  id_turn_OFF
!>        SUPG flag for magnetic field
        integer (kind=kint) :: iflag_magne_supg = id_turn_OFF
!>        SUPG flag for temperature
        integer (kind=kint) :: iflag_temp_supg = id_turn_OFF
!>        SUPG flag for light element
        integer (kind=kint) :: iflag_comp_supg = id_turn_OFF
!
!>        Maximum CG iteration count for mass conservation
        integer (kind=kint) :: maxiter_stokes
!>        Maximum CG iteration count for Coulomb Gauge
        integer (kind=kint) :: maxiter_coulomb
!>        Error torrance for Poisson equation
        real (kind=kreal) :: eps_4_stokes
!>        Error torrance for iteration of Coulomb gauge
        real (kind=kreal) :: eps_4_coulomb
!
!>        Reference error torrance for diffusion matrices
        real(kind=kreal) :: eps_crank
!
!>        Error torrance for viscous diffusion matrix
        real(kind=kreal)   :: eps_4_velo_crank =  zero
!>        Error torrance for magnetic diffusion matrix
        real(kind=kreal)   :: eps_4_magne_crank = zero
!>        Error torrance for thermal diffusion matrix
        real(kind=kreal)   :: eps_4_temp_crank =  zero
!>        Error torrance for composition diffusion matrix
        real(kind=kreal)   :: eps_4_comp_crank =  zero
!
!>        Preconditionang for 3x3 solver
        character (len=kchara) :: precond_33
!>        Method for 3x3 solver
        character (len=kchara) :: method_33
!
!>        Poisson solver parameters
        type(CG_poarameter) :: CG11_param
!>        DJDS ordering parameters
        type(DJDS_poarameter)  :: DJDS_param
!>        Multigrid parameters
        type(MGCG_parameter) :: MG_param
!>        File list for Multigrid solver
        type(MGCG_file_list) :: MG_file
!
!>        Structure for volume repartitiong paramteres
        type(volume_repart_params) :: repart_p
!
        type(area_group_name_list) :: fluid_group
        type(area_group_name_list) :: condutive_group
        type(area_group_name_list) :: insulator_group
        type(area_group_name_list) :: inner_core_group
      end type FEM_MHD_paremeters
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_area_group_name(ngrp, area_group)
!
      integer(kind = kint), intent(in) :: ngrp
      type(area_group_name_list), intent(inout) :: area_group
!
!
      area_group%num_group = ngrp
      allocate(area_group%group_name(area_group%num_group))
!
      end subroutine alloc_area_group_name
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_area_group_name(area_group)
!
      type(area_group_name_list), intent(inout) :: area_group
!
      deallocate(area_group%group_name)
!
      end subroutine dealloc_area_group_name
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_residual_4_crank                                   &
     &         (dt, fl_prop, cd_prop, ht_prop, cp_prop, FEM_prm)
!
      use m_machine_parameter
!
      real(kind = kreal), intent(in) :: dt
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!
!
      if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        FEM_prm%eps_4_velo_crank                                        &
     &      = FEM_PRM%eps_crank * fl_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_velo', FEM_prm%eps_4_velo_crank
      end if
!
      if (ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
        FEM_prm%eps_4_temp_crank                                        &
     &      = FEM_PRM%eps_crank * ht_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_temp_crank', FEM_prm%eps_4_temp_crank
      end if
!
      if (   cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson           &
     &  .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
!
        if(FEM_prm%eps_4_magne_crank .le. 0.0d0) then
          FEM_prm%eps_4_magne_crank                                     &
     &        = FEM_PRM%eps_crank * cd_prop%coef_diffuse * dt**2
        end if
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_magne_crank', FEM_prm%eps_4_magne_crank
      end if
!
      if (cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
        FEM_prm%eps_4_comp_crank                                        &
     &      = FEM_PRM%eps_crank * cp_prop%coef_diffuse * dt**2
        if(iflag_debug.eq.1)                                            &
     &     write(12,*) 'eps_4_comp_crank', FEM_prm%eps_4_comp_crank
      end if
!
      end subroutine set_residual_4_crank
!
! ----------------------------------------------------------------------
!
      end module t_FEM_control_parameter
