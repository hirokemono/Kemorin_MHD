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
      use t_IO_step_parameter
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
!>      length of each time step @f$ \Delta t @f$
      real(kind=kreal) :: dt
!
!>      Time
      real(kind=kreal) :: time
!>      Time for the initail step
      real(kind=kreal) :: time_init
! 
!>      Time step
      integer(kind=kint) :: i_step_MHD
! 
!>      Elapsed time to terminate simulation
      real(kind=kreal)   :: elapsed_time
! 
!>      Flag for initial step to use Euler scheme
!!      insted of Adams-BAshforth
      integer(kind=kint) :: iflag_initial_step = 0
!
!>      Start time step
      integer(kind=kint) :: i_step_init
!>      End time steo
      integer(kind=kint) :: i_step_number
! 
!>      Start step for restarting file
      integer(kind=kint) :: istep_rst_start
!>      End step for restarting file
      integer(kind=kint) :: istep_rst_end
!
!
      type(IO_step_param), save :: rms_step1
!
      type(IO_step_param), save :: point_step1
!
      type(IO_step_param), save :: boundary_step1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_start_stop_4_restart(rst_step)
!
      type(IO_step_param), intent(in) :: rst_step
!
!
      if(rst_step%increment .gt. 0) then
        istep_rst_start = int(i_step_init /   rst_step%increment)
        istep_rst_end =   int(i_step_number / rst_step%increment)
      else
        istep_rst_start = i_step_init 
        istep_rst_end =   i_step_number
      end if
!
      end subroutine set_start_stop_4_restart
!
! -----------------------------------------------------------------------
!
      subroutine set_start_stop_by_restart(rst_step)
!
      type(IO_step_param), intent(in) :: rst_step
!
!
      i_step_init =   istep_rst_start * rst_step%increment
      i_step_number = istep_rst_end *   rst_step%increment
!
      end subroutine set_start_stop_by_restart
!
! -----------------------------------------------------------------------
!
      end module  m_t_step_parameter
