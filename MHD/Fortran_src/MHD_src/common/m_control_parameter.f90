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
!!      subroutine deallocate_force_list
!!@endverbatim
!
      module   m_control_parameter
!
      use m_precision
      use m_constants
      use t_time_stepping_parameter
      use t_FEM_control_parameter
!
      implicit  none
!
!
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
!>      ID not to read external boundary condition file
      integer (kind=kint), parameter :: id_no_boundary_file =   0
!>      ID to read external boundary condition file
      integer (kind=kint), parameter :: id_read_boundary_file = 1
!>      Flag to check external boundary condition file
      integer (kind=kint) :: iflag_boundary_file = id_no_boundary_file
!
!  Parameters for FEM dynamo
!
      type(FEM_MHD_paremeters), save :: FEM_prm1
!
      end module m_control_parameter
