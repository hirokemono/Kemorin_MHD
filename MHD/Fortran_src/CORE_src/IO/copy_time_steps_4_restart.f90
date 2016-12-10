!>@file   copy_time_steps_4_restart.f90
!!@brief  module copy_time_steps_4_restart
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Routines to copy time step information from IO data
!!
!!@verbatim
!!      subroutine copy_time_from_restart
!!      subroutine copy_init_time_from_restart
!!      subroutine copy_time_steps_from_restart
!!      subroutine copy_time_steps_to_restart
!!@endverbatim
!
      module copy_time_steps_4_restart
!
      use m_precision
!
      use m_constants
      use m_t_step_parameter
      use m_t_int_parameter
      use m_time_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_from_restart
!
!
      time_init =   t1_IO%time_IO
!
      end subroutine copy_time_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_init_time_from_restart
!
!
      time_init =   t1_IO%time_IO
      i_step_init = t1_IO%i_time_step_IO
      if(dt .le. zero) dt = t1_IO%delta_t_IO
!
      end subroutine copy_init_time_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_from_restart
!
      use cal_num_digits
!
!
      time_init =   t1_IO%time_IO
      i_step_init = t1_IO%i_time_step_IO
!
      if(t1_IO%delta_t_IO .gt. zero) then
        dt = t1_IO%delta_t_IO
        ddt= one / dt
        call cal_num_digit_real(dt, dt_fact, idt_digit)
      end if
!
      end subroutine copy_time_steps_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_to_restart
!
!
      t1_IO%i_time_step_IO = i_step_MHD
      t1_IO%time_IO =        time
      t1_IO%delta_t_IO =     dt
!
      end subroutine copy_time_steps_to_restart
!
!  ---------------------------------------------------------------------
!
      end module copy_time_steps_4_restart
