!>@file   t_time_stepping_parameter.f90
!!@brief  module t_time_stepping_parameter
!!
!!@author H. Matsui
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!!@n    Modified by H. Matsui in 2016
!
!> @brief Structure for time steppings
!
      module t_time_stepping_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>      Scheme ID for no evolution
      integer (kind=kint), parameter :: id_no_evolution =     0
!>      Scheme ID for explicit Euler scheme
      integer (kind=kint), parameter :: id_explicit_euler =   1
!>      Scheme ID for 2nd order Adams-Bashforth Scheme
      integer (kind=kint), parameter :: id_explicit_adams2 =  2
!>      Scheme ID for Crank-Nicolson Scheme
      integer (kind=kint), parameter :: id_Crank_nicolson =   3
!>      Scheme ID for Crank-Nicolson Scheme with consistent mass matrix
      integer (kind=kint), parameter :: id_Crank_nicolson_cmass = 4
!>      TIme evolution schme flag
!
!
      type time_evolution_params
!>        Time evolution flag for velocity
        integer (kind=kint) :: iflag_scheme = id_no_evolution
      end type time_evolution_params
!
!fl_prop%iflag_scheme
!
      end module  t_time_stepping_parameter
