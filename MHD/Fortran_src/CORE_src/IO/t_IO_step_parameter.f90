!>@file   t_IO_step_parameter.f90
!!@brief  module t_IO_step_parameter
!!
!!@author H. Matsui
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2017
!
!> @brief Parameteres for time steppings
!!
!!@verbatim
!!      subroutine output_step_4_fixed_step_ctl(istep_def, dt,          &
!!     &          istep_ctl, delta_t_ctl, IO_step)
!!      subroutine output_step_4_flex_step_ctl                          &
!!     &         (istep_def, dt_max, step_ctl, delta_t_ctl, IO_step)
!!        type(read_integer_item), intent(inout) :: step_ctl
!!        type(read_real_item), intent(inout) :: delta_t_ctl
!!        type(IO_step_param), intent(inout) :: IO_step
!!      logical function output_IO_flag(i_step, IO_step)
!!        type(IO_step_param), intent(in) :: IO_step
!!      integer(kind = kint) function set_IO_step(i_step, IO_step)
!!        type(IO_step_param), intent(inout) :: IO_step
!!
!!      subroutine set_monitor_param_4_fixed_step(istep_def, istep_ctl, &
!!     &          delta_t_ctl, istep_out, dt_out)
!!      subroutine set_monitor_param_4_flex_step(istep_def, dt_max,     &
!!     &          istep_ctl, delta_t_ctl, istep_out, dt_out)
!!      logical function output_flag(i_step, increment)
!!
!!      logical function iflag_viz_flex_step(time_d, IO_step)
!!        type(time_data), intent(in) :: time_d
!!        type(IO_step_param), intent(in) :: IO_step
!!      integer(kind = kint) function istep_file_w_fix_dt               &
!!     &                            (i_step, IO_step)
!!      integer(kind = kint) function                                   &
!!     &                    istep_file_w_flex_dt(time_d, IO_step)
!!        type(time_data), intent(in) :: time_d
!!        type(IO_step_param), intent(in) :: IO_step
!!@endverbatim
!!
      module  t_IO_step_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      type IO_step_param
!>        Increment of time step
        integer(kind=kint) :: increment
!>        Time interval for output
        real(kind=kreal)   :: delta_t
      end type IO_step_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      logical function output_IO_flag(i_step, IO_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: IO_step
!
!
      output_IO_flag = output_flag(i_step, IO_step%increment)
!
      end function output_IO_flag
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function set_IO_step(i_step, IO_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: IO_step
!
!
      if(IO_step%increment .eq. 0) then
         set_IO_step = 0
       else if(i_step .eq. -1) then
         set_IO_step = -1
       else
         set_IO_step = i_step / IO_step%increment
       end if
!
      end function set_IO_step
!

!-----------------------------------------------------------------------
!
      integer(kind = kint) function IO_step_exc_zero_inc                &
     &                            (i_step, IO_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: IO_step
!
!
      if(IO_step%increment .le. 0) then
        IO_step_exc_zero_inc = -1
      else
        IO_step_exc_zero_inc = i_step / IO_step%increment
      end if
!
      end function IO_step_exc_zero_inc
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine output_step_4_fixed_step_ctl(istep_def, dt,            &
     &          istep_ctl, delta_t_ctl, IO_step)
!
      use t_control_array_integer
      use t_control_array_real
!
      integer(kind = kint), intent(in) :: istep_def
      real(kind = kreal), intent(in) :: dt
      type(read_real_item), intent(in) :: delta_t_ctl
      type(read_integer_item), intent(in) :: istep_ctl
!
      type(IO_step_param), intent(inout) :: IO_step
!
!
      if ( (istep_ctl%iflag + delta_t_ctl%iflag) .eq. 0) then
        IO_step%increment =   istep_def
        IO_step%delta_t = dble(istep_def) * dt
      else if(istep_ctl%iflag .eq. 0) then
        IO_step%delta_t =    delta_t_ctl%realvalue
        IO_step%increment = nint(IO_step%delta_t / dt)
      else
        IO_step%increment =   istep_ctl%intvalue
        IO_step%delta_t = dble(IO_step%increment) * dt
      end if
!
      end subroutine output_step_4_fixed_step_ctl
!
! -----------------------------------------------------------------------
!
      subroutine output_step_4_flex_step_ctl(istep_def, dt_max,         &
     &          istep_ctl, delta_t_ctl, IO_step)
!
      use t_control_array_integer
      use t_control_array_real
!
      integer(kind = kint), intent(in) :: istep_def
      real(kind = kreal), intent(in) :: dt_max
      type(read_real_item), intent(in) :: delta_t_ctl
      type(read_integer_item), intent(in) :: istep_ctl
!
      type(IO_step_param), intent(inout) :: IO_step
!
!
      if ( (istep_ctl%iflag+delta_t_ctl%iflag) .eq. 0) then
        IO_step%increment =   istep_def
        IO_step%delta_t = dble(istep_def) * dt_max
      else if(delta_t_ctl%iflag .eq. 0) then
        IO_step%increment =   istep_ctl%intvalue
        IO_step%delta_t = dble(IO_step%increment) * dt_max
      else
        IO_step%delta_t =    delta_t_ctl%realvalue
        IO_step%increment = nint(IO_step%delta_t / dt_max)
      end if
!
      end subroutine output_step_4_flex_step_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function output_flag(i_step, increment)
!
      integer (kind = kint), intent(in) :: i_step, increment
!
!
      output_flag = .FALSE.
      if(increment .eq. 0) return
!
      output_flag = (mod(i_step,increment) .eq. 0)
!
      end function output_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function iflag_viz_flex_step(time_d, IO_step)
!
      use t_time_data
!
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: IO_step
!
      real(kind= kreal) :: t_next
      integer(kind = kint) :: i_now, i_next
!
!
      iflag_viz_flex_step = .FALSE.
      if (IO_step%delta_t .eq. zero) return
!
      t_next = time_d%time + time_d%dt
      i_next = int(t_next / IO_step%delta_t,KIND(i_next))
      i_now =  int(time_d%time / IO_step%delta_t,KIND(i_now))
      iflag_viz_flex_step = (i_next .ne. i_now)
!
      end function iflag_viz_flex_step
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function istep_file_w_fix_dt                 &
     &                            (i_step, IO_step)
!
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: IO_step
!
!
      istep_file_w_fix_dt = -ione
      if(IO_step%increment .eq. izero) return
      if (mod(i_step,IO_step%increment) .eq. izero) then
        istep_file_w_fix_dt = i_step / IO_step%increment
      end if
!
      end function istep_file_w_fix_dt
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    istep_file_w_flex_dt(time_d, IO_step)
!
      use t_time_data
!
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: IO_step
!
      real(kind= kreal) :: t_next
      integer(kind = kint) :: i_now, i_next
!
!
      istep_file_w_flex_dt = -ione
      if(IO_step%delta_t .eq. zero) return
!
      t_next = time_d%time + time_d%dt
      i_next = int(t_next / IO_step%delta_t, KIND(i_next))
      i_now =  int(time_d%time / IO_step%delta_t, KIND(i_now))
      if(i_next .ne. i_now) istep_file_w_flex_dt = i_next
!
      end function istep_file_w_flex_dt
!
! -----------------------------------------------------------------------
!
      end module t_IO_step_parameter
