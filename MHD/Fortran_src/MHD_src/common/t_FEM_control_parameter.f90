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
!!@endverbatim
!
      module t_FEM_control_parameter
!
      use m_precision
      use m_constants
      use t_iccg_parameter
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
!>        Error torrance for viscous diffusion matrix
        real(kind=kreal)   :: eps_4_velo_crank =  zero
!>        Error torrance for magnetic diffusion matrix
        real(kind=kreal)   :: eps_4_magne_crank = zero
!>        Error torrance for thermal diffusion matrix
        real(kind=kreal)   :: eps_4_temp_crank =  zero
!>        Error torrance for composition diffusion matrix
        real(kind=kreal)   :: eps_4_comp_crank =  zero
!
        type(CG_poarameter) :: CG11_param
!
        type(area_group_name_list) :: fluid_group
        type(area_group_name_list) :: condutive_group
        type(area_group_name_list) :: insulator_group
        type(area_group_name_list) :: inner_core_group
      end type FEM_MHD_paremeters
!
!FEM_PRM%CG11_param%METHOD
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
!
      end module t_FEM_control_parameter
