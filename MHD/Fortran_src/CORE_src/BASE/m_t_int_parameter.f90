!> @file  m_t_int_parameter.f90
!!      module m_t_int_parameter
!!
!! @author H. Matsui and H. Okuda
!! @date Written on Oct., 2000
!!
!> @brief Parameters for time integration
!
      module m_t_int_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>      length of each time step @f$ \Delta t @f$
      real(kind=kreal) :: dt
!
!>      Coefficient of terms at current step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_0 =  three / two
!>      Coefficient of terms at previous step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_1 = -one / two
!>      1 / adam_0
      real(kind=kreal), parameter :: adam_r =  two / three
!
      end module m_t_int_parameter
