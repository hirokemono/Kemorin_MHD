!>@file   m_t_step_parameter.f90
!!@brief  module m_t_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module  m_t_step_parameter
!
!
      use m_precision
      use t_time_data
!
      implicit  none
!
!
!>      Coefficient of terms at current step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_0 =  three / two
!>      Coefficient of terms at previous step for Adams-Bashforth
      real(kind=kreal), parameter :: adam_1 = -one / two
!>      1 / adam_0
      real(kind=kreal), parameter :: adam_r =  two / three
!
!>      Elapsed time to terminate simulation
!      real(kind=kreal)   :: elapsed_time
! 
!>      Flag for initial step to use Euler scheme
!!      insted of Adams-BAshforth
      integer(kind=kint) :: iflag_initial_step = 0
!
!>      Structure for time data
      type(time_data), save :: time_d1
!
!>      Structure for initial time data
      type(time_data), save :: init_d1
!
!>      Structure for end time data
      type(finish_data), save :: finish_d1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_step_4_restart(rst_step, i_step, istep_rst)
!
      use t_IO_step_parameter
!
      type(IO_step_param), intent(in) :: rst_step
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: istep_rst
!
!
      if(rst_step%increment .gt. 0) then
        istep_rst = int(i_step / rst_step%increment)
      else
        istep_rst = i_step
      end if
!
      if(i_step .eq. -1)   istep_rst = -1
!
      end subroutine set_step_4_restart
!
! -----------------------------------------------------------------------
!
      end module  m_t_step_parameter
