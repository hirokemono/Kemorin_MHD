!>@file   m_control_parameter.f90
!!@brief  module m_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!@verbatim
!!      subroutine allocate_force_list
!!      subroutine allocate_fluid_ele_grp_name
!!      subroutine allocate_conduct_ele_grp_name
!!      subroutine allocate_icore_ele_grp_name
!!      subroutine allocate_whole_filter_groups
!!      subroutine allocate_fluid_filter_groups
!!
!!      subroutine deallocate_force_list
!!      subroutine deallocate_fluid_ele_grp_name
!!      subroutine deallocate_conduct_ele_grp_name
!!      subroutine deallocate_icore_ele_grp_name
!!      subroutine deallocate_whole_filter_groups
!!      subroutine deallocate_fluid_filter_groups
!!@endverbatim
!
      module   m_control_parameter
!
      use m_precision
      use t_time_stepping_parameter
!
      implicit  none
!
!
!>      Turn OFF flag
      integer (kind=kint), parameter :: id_turn_OFF = 0
!>      Turn ON flag
      integer (kind=kint), parameter :: id_turn_ON =  1
!
!>      Number of fields for time evolution
      integer (kind=kint)  :: num_field_to_evolve
!>      Field name for time evolution
      character (len=kchara), allocatable :: t_evo_name(:)
!>      Time evolution scheme for each field
      character (len=kchara), allocatable :: time_evo_method(:)
!
!
      integer (kind=kint) :: iflag_scheme = id_Crank_nicolson
!
!>      TIme evolution parameters for magnetic field
      type(time_evolution_params), save :: evo_velo
!>      TIme evolution parameters for magnetic field
      type(time_evolution_params), save :: evo_magne
!>      TIme evolution parameters for vector potential
      type(time_evolution_params), save :: evo_vect_p
!>      TIme evolution parameters for temperature
      type(time_evolution_params), save :: evo_temp
!>      TIme evolution parameters for composition variation
      type(time_evolution_params), save :: evo_comp
!
!
!>      Number of forces
      integer (kind=kint) :: num_force
!>      Name of forces
      character (len=kchara), allocatable :: name_force(:)
!
!>      Turn ON and evaluate over elements flag
      integer (kind=kint), parameter :: id_FORCE_ele_int =  1
!>      Turn ON and evaluate at node flag
      integer (kind=kint), parameter :: id_FORCE_at_node =  2
!>      Force flag for thermal buoyancy
      integer (kind=kint) :: iflag_4_gravity =        id_turn_OFF
!>      Force flag for compositional buoyancy
      integer (kind=kint) :: iflag_4_composit_buo =   id_turn_OFF
!>      Force flag for filtered thermal buoyancy
      integer (kind=kint) :: iflag_4_filter_gravity = id_turn_OFF
!
!>      Turn ON and evaluate implicitly over elements flag
      integer (kind=kint), parameter :: id_Coriolis_ele_imp = 11
!>      Turn ON and evaluate implicitly at node flag
      integer (kind=kint), parameter :: id_Coriolis_nod_imp = 12
!
!>      Turn ON and including magnetic pressure
      integer (kind=kint), parameter :: id_Lorentz_w_Emag = 2
!
!
      integer (kind=kint) :: num_fl_ele_grp
      integer (kind=kint) :: num_cd_ele_grp
      integer (kind=kint) :: num_ins_ele_grp
      integer (kind=kint) :: num_in_core_ele_grp
!
      character (len=kchara), allocatable :: fl_ele_grp_name(:)
      character (len=kchara), allocatable :: cd_ele_grp_name(:)
      character (len=kchara), allocatable :: ins_ele_grp_name(:)
      character (len=kchara), allocatable :: in_core_ele_grp_name(:)
!
!
!>      ID not to read external boundary condition file
      integer (kind=kint), parameter :: id_no_boundary_file =   0
!>      ID to read external boundary condition file
      integer (kind=kint), parameter :: id_read_boundary_file = 1
!>      Flag to check external boundary condition file
      integer (kind=kint) :: iflag_boundary_file = id_no_boundary_file
!>      Flag to check external file for radial field
      integer (kind=kint) :: iflag_radial_param_file                    &
     &       = id_no_boundary_file
!
!
!  Parameters for FEM dynamo
!
!>      Using rotation form for inertia and Lorentz force
      integer (kind=kint) :: iflag_4_rotate =        id_turn_OFF
!
!
!>      Number of quadrature points for time evolution
      integer (kind=kint)  :: intg_point_t_evo =   2
!>      Number of quadrature points for Poisson equation
      integer (kind=kint)  :: intg_point_poisson = 2
!>      Number of iteration for Multi-pass scheme
      integer (kind=kint)  :: num_multi_pass =     0
!
!>      ID for using SUPG by mangeitc field
      integer (kind=kint), parameter :: id_magnetic_SUPG =  2
!
!>      SUPG flag for velocity
      integer (kind=kint) :: iflag_velo_supg = id_turn_OFF
!>      SUPG flag for temperature
      integer (kind=kint) :: iflag_temp_supg = id_turn_OFF
!>      SUPG flag for magnetic field
      integer (kind=kint) :: iflag_mag_supg = id_turn_OFF
!>      SUPG flag for light element
      integer (kind=kint) :: iflag_comp_supg = id_turn_OFF
!
!
!>      Maximum CG iteration count for Poisson equation
      integer (kind=kint) :: maxiter
!>      Maximum CG iteration count for time integration
      integer (kind=kint) :: maxiter_vecp
! 
!>      Error torrance for Poisson equation
      real (kind=kreal) :: eps_4_velo
!>      Error torrance for time integration
      real (kind=kreal) :: eps_4_magne
!
!
      integer (kind=kint) :: iflag_implicit_correct = 0
!
      integer (kind=kint), parameter :: id_SGS_none =       0
      integer (kind=kint), parameter :: id_SGS_NL_grad =    1
      integer (kind=kint), parameter :: id_SGS_similarity = 2
      integer (kind=kint), parameter :: id_SGS_diffusion =  3
      integer (kind=kint) :: iflag_SGS_model = id_SGS_none
!
      integer (kind=kint), parameter :: id_SGS_DYNAMIC_OFF =   0
      integer (kind=kint), parameter :: id_SGS_DYNAMIC_ON =    1
      integer (kind=kint) :: iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
      integer (kind=kint), parameter :: id_SGS_NO_FILTERING =         0
      integer (kind=kint), parameter :: id_SGS_3D_FILTERING =         1
      integer (kind=kint), parameter :: id_SGS_3D_EZ_FILTERING =     11
      integer (kind=kint), parameter :: id_SGS_3D_SMP_FILTERING =    21
      integer (kind=kint), parameter :: id_SGS_3D_EZ_SMP_FILTERING = 31
!
      integer (kind=kint), parameter :: id_SGS_LINE_FILTERING =       2
      integer (kind=kint), parameter :: id_SGS_PLANE_FILTERING =      3
      integer (kind=kint), parameter :: id_SGS_IDEAL_SPH_LOWPASS =    4
!
      integer (kind=kint) :: iflag_SGS_filter = id_SGS_3D_FILTERING
      integer (kind=kint) :: iset_DIFF_model_coefs =  0
      integer (kind=kint) :: iset_SGS_nagetive_clip = 0
      integer (kind=kint) :: iset_SGS_coef_marging =  0
      real (kind = kreal) :: SGS_clipping_limit = 0.0d0
!
      real (kind = kreal) :: SGS_hf_factor =      1.0d0
      real (kind = kreal) :: SGS_mf_factor =      1.0d0
      real (kind = kreal) :: SGS_mawell_factor =  1.0d0
      real (kind = kreal) :: SGS_uxb_factor =     1.0d0
!
      integer (kind=kint) :: min_step_dynamic =  1
      integer (kind=kint) :: max_step_dynamic =  1
      real (kind = kreal) :: delta_to_shrink_dynamic = 1.0d5
      real (kind = kreal) :: delta_to_extend_dynamic = 1.0d-5
!
      integer (kind=kint) :: iflag_SGS_heat =      id_SGS_none
      integer (kind=kint) :: iflag_SGS_inertia =   id_SGS_none
      integer (kind=kint) :: iflag_SGS_lorentz =   id_SGS_none
      integer (kind=kint) :: iflag_SGS_induction = id_SGS_none
      integer (kind=kint) :: iflag_SGS_comp_flux = id_SGS_none
      integer (kind=kint) :: iflag_SGS_gravity =   id_SGS_none
!
      integer (kind=kint) :: iflag_SGS_parterbuation = 0
!
      integer (kind=kint) :: itype_SGS_model_coef =  0
      integer (kind=kint) :: icoord_SGS_model_coef = 0
!
      integer (kind=kint) :: itype_SGS_h_flux_coef =   0
      integer (kind=kint) :: itype_SGS_m_flux_coef =   0
      integer (kind=kint) :: itype_SGS_maxwell_coef =  0
      integer (kind=kint) :: itype_SGS_uxb_coef =      0
!
!>      ID not to apply commutation error correction
      integer (kind=kint), parameter :: id_SGS_commute_OFF = 0
!>      ID to apply commutation error correction
      integer (kind=kint), parameter :: id_SGS_commute_ON =  1
!
!>      commutation error correction flag for system
      integer (kind=kint) :: iflag_commute_correction                   &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for linear terms
      integer (kind=kint) :: iflag_commute_linear                       &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for nonlinear terms
      integer (kind=kint) :: iflag_commute_nonlinar                     &
     &                      = id_SGS_commute_OFF
!
!>      commutation error correction flag for temperature
      integer (kind=kint) :: iflag_commute_temp                         &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for velocity
      integer (kind=kint) :: iflag_commute_velo                         &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for magnetic field
      integer (kind=kint) :: iflag_commute_magne                        &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for composition variation
      integer (kind=kint) :: iflag_commute_composit                     &
     &                      = id_SGS_commute_OFF
!
!>      commutation error correction flag for heat flux
      integer (kind=kint) :: iflag_commute_heat                         &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for momentum flux
      integer (kind=kint) :: iflag_commute_inertia                      &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for heat flux
      integer (kind=kint) :: iflag_commute_lorentz                      &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for magnetic induction
      integer (kind=kint) :: iflag_commute_induction                    &
     &                      = id_SGS_commute_OFF
!>      commutation error correction flag for composition flux
      integer (kind=kint) :: iflag_commute_c_flux                       &
     &                      = id_SGS_commute_OFF
!
      integer (kind=kint) :: num_whole_filter_grp = 0
      integer (kind=kint) :: num_fluid_filter_grp = 0
      integer (kind=kint), allocatable :: id_whole_filter_grp(:)
      integer (kind=kint), allocatable :: id_fluid_filter_grp(:)
      character (len=kchara), allocatable :: whole_filter_grp(:)
      character (len=kchara), allocatable :: fluid_filter_grp(:)
!
      integer (kind=kint) :: num_whole_w_filter_grp = 0
      integer (kind=kint) :: num_fluid_w_filter_grp = 0
      integer (kind=kint), allocatable :: id_whole_w_filter_grp(:)
      integer (kind=kint), allocatable :: id_fluid_w_filter_grp(:)
      character (len=kchara), allocatable :: whole_w_filter_grp(:)
      character (len=kchara), allocatable :: fluid_w_filter_grp(:)
!
      integer (kind=kint) :: iflag_heat_filtering = 0
      integer (kind=kint) :: iflag_momentum_filtering = 0
      integer (kind=kint) :: iflag_induction_filtering = 0
!
!
!>      filter ID for @f$ s\Delta @f$  filter
      integer (kind=kint), parameter :: ifilter_2delta = 1
!>      filter ID for @f$ 4\Delta @f$  filter
      integer (kind=kint), parameter :: ifilter_4delta = 2
!>      filter ID to obtain SGS terms
      integer (kind=kint) :: ifilter_final = ifilter_2delta
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_force_list
!
      allocate(name_force(num_force))
!
      end subroutine allocate_force_list
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_fluid_ele_grp_name
!
      allocate(fl_ele_grp_name(num_fl_ele_grp))
!
      end subroutine allocate_fluid_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_conduct_ele_grp_name
!
      allocate(cd_ele_grp_name(num_cd_ele_grp))
!
      end subroutine allocate_conduct_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_icore_ele_grp_name
!
      allocate(in_core_ele_grp_name(num_in_core_ele_grp))
!
      end subroutine allocate_icore_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_whole_filter_groups
!
!
      allocate(whole_filter_grp(num_whole_filter_grp))
      allocate(id_whole_filter_grp(num_whole_filter_grp))
      allocate(whole_w_filter_grp(num_whole_w_filter_grp))
      allocate(id_whole_w_filter_grp(num_whole_w_filter_grp))
      id_whole_filter_grp =   0
      id_whole_w_filter_grp = 0
!
      end subroutine allocate_whole_filter_groups
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_fluid_filter_groups
!
!
      allocate(fluid_filter_grp(num_fluid_filter_grp))
      allocate(id_fluid_filter_grp(num_fluid_filter_grp))
      allocate(fluid_w_filter_grp(num_fluid_w_filter_grp))
      allocate(id_fluid_w_filter_grp(num_fluid_w_filter_grp))
      id_fluid_filter_grp =   0
      id_fluid_w_filter_grp = 0
!
      end subroutine allocate_fluid_filter_groups
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_force_list
!
      deallocate(name_force)
!
      end subroutine deallocate_force_list
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_fluid_ele_grp_name
!
      deallocate(fl_ele_grp_name)
!
      end subroutine deallocate_fluid_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_conduct_ele_grp_name
!
      deallocate(cd_ele_grp_name)
!
      end subroutine deallocate_conduct_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_icore_ele_grp_name
!
      deallocate(in_core_ele_grp_name)
!
      end subroutine deallocate_icore_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_whole_filter_groups
!
!
      deallocate(whole_filter_grp,   id_whole_filter_grp)
      deallocate(whole_w_filter_grp, id_whole_w_filter_grp)
!
      end subroutine deallocate_whole_filter_groups
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_fluid_filter_groups
!
!
      deallocate(fluid_filter_grp,   id_fluid_filter_grp)
      deallocate(fluid_w_filter_grp, id_fluid_w_filter_grp)
!
      end subroutine deallocate_fluid_filter_groups
!
!  ---------------------------------------------------------------------
!
      end module m_control_parameter
