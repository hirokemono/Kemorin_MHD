!>@file   copy_time_steps_4_restart.f90
!!@brief  module copy_time_steps_4_restart
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Routines to copy time step information from IO data
!!
!!@verbatim
!!      subroutine copy_time_from_restart(t_IO)
!!      subroutine copy_init_time_from_restart(t_IO)
!!      subroutine copy_time_steps_from_restart(t_IO)
!!        type(time_params_IO), intent(in) :: t_IO
!!
!!      subroutine copy_time_steps_to_restart(t_IO)
!!        type(time_params_IO), intent(inout) :: t_IO
!!@endverbatim
!
      module copy_time_steps_4_restart
!
      use m_precision
!
      use m_constants
      use m_t_step_parameter
      use m_t_int_parameter
      use t_time_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_from_restart(t_IO)
!
      type(time_params_IO), intent(in) :: t_IO
!
      time_init =   t_IO%time_IO
!
      end subroutine copy_time_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_init_time_from_restart(t_IO)
!
      type(time_params_IO), intent(in) :: t_IO
!
!
      time_init =   t_IO%time_IO
      i_step_init = t_IO%i_time_step_IO
!
      end subroutine copy_init_time_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_from_restart(t_IO)
!
      use cal_num_digits
!
      type(time_params_IO), intent(in) :: t_IO
!
!
      time_init =   t_IO%time_IO
      i_step_init = t_IO%i_time_step_IO
!
      if(t_IO%delta_t_IO .gt. zero) then
        dt = t_IO%delta_t_IO
        ddt= one / dt
        call cal_num_digit_real(dt, dt_fact, idt_digit)
      end if
!
      end subroutine copy_time_steps_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_to_restart(t_IO)
!
      type(time_params_IO), intent(inout) :: t_IO
!
!
      t_IO%i_time_step_IO = i_step_MHD
      t_IO%time_IO =        time
      t_IO%delta_t_IO =     dt
!
      end subroutine copy_time_steps_to_restart
!
!  ---------------------------------------------------------------------
!
      end module copy_time_steps_4_restart
