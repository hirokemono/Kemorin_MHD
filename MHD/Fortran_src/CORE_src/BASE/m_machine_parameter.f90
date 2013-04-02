!> @file m_machine_parameter.f90
!!      module m_machine_parameter
!!
!!
!! @author H. Matsui
!! @date Written on May, 2003
!!
!!> @brief  Parameters for computer
!
      module m_machine_parameter
!
      use m_precision
!
      implicit none
!
!
!>    number of SMP threads
      integer(kind=kint) :: np_smp = 1
!
!>    debug flag for all processes (1: 0n, 0: Off)
      integer(kind = kint) :: i_debug =     0
!>    debug flag for master process
      integer(kind = kint) :: iflag_debug = 0
!
!>    character array for error message
      character(len=256) :: e_message
!
      end module m_machine_parameter
