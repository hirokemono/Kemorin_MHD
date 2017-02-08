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
!!
!!      subroutine deallocate_force_list
!!      subroutine deallocate_fluid_ele_grp_name
!!      subroutine deallocate_conduct_ele_grp_name
!!      subroutine deallocate_icore_ele_grp_name
!!@endverbatim
!
      module   m_control_parameter
!
      use m_precision
      use t_time_stepping_parameter
      use t_SGS_control_parameter
!
      implicit  none
!
!
!>      Turn OFF flag
!      integer (kind=kint), parameter :: id_turn_OFF = 0
!>      Turn ON flag
!      integer (kind=kint), parameter :: id_turn_ON =  1
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
!!@N      (FEM only)
      integer (kind=kint) :: iflag_4_rotate =        id_turn_OFF
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
      end module m_control_parameter
