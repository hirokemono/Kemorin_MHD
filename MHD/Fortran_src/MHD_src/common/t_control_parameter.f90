!>@file   t_control_parameter.f90
!!@brief  module t_control_parameter
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
      module t_control_parameter
!
      use m_precision
      use t_time_stepping_parameter
      use t_physical_property
      use t_reference_scalar_param
!
      implicit  none
!
!
!>      Structure for evolution parameter for valocity
      type velocity_evolution_param
!>        Structure for fluid property
        type(fluid_property) :: fl_prop
      end type velocity_evolution_param
!
!>      Structure for evolution parameter for magnetic field
      type magnetic_evolution_param
!>        TIme evolution parameters for magnetic field
        type(time_evolution_params) :: evo_B
!>        TIme evolution parameters for vector potential
        type(time_evolution_params) :: evo_A
!
!>      Structure for manetic property
      type(conductive_property) :: cd_prop
      end type magnetic_evolution_param
!
!>      Structure for evolution parameter for scalar
      type scalar_evolution_param
!>        TIme evolution parameters for temperature
        type(time_evolution_params) :: evo_S
!
!>        Structure for thermal property
        type(scalar_property) :: property
!
!>        reference paramter for temperature
        type(reference_scalar_param) :: ref_param
!>        Takepiro stratified temperature
        type(takepiro_model_param) :: takepito
      end type scalar_evolution_param
!
      type time_evolution_list
!>        TIme evolution schme for all fields
        integer (kind=kint) :: iflag_scheme = id_Crank_nicolson
      end type time_evolution_list
!
!
      end module t_control_parameter
