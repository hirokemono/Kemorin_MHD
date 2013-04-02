!check_flexible_time_step.f90
!     module check_flexible_time_step
!
      module check_flexible_time_step
!
!      Written by H. Matsui on Nov., 2009
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
      use m_t_step_parameter
      use m_t_int_parameter
      use m_flex_delta_t_data
!
      implicit  none
!
      private :: shrink_delta_t, extend_delta_t
!
!      subroutine set_new_time_and_step
!      subroutine s_check_flexible_time_step
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_new_time_and_step
!
      use copy_field_data_4_dt_check
!
!
      time = time + dt
      i_step_MHD = i_step_MHD + 1
!
      if (iflag_flexible_step .eq. iflag_fixed_step) then
        istep_max_dt = i_step_MHD
      else
        istep_flex_to_max = istep_flex_to_max + 1
        istep_flex_to_max = mod(istep_flex_to_max,i_interval_flex_2_max)
        if(istep_flex_to_max .eq. 0) istep_max_dt = istep_max_dt + 1
      end if
!
      if (iflag_debug.eq.1) write(*,*) 's_copy_field_data_for_dt_check'
      call s_copy_field_data_for_dt_check
!
      end subroutine set_new_time_and_step
!
! -----------------------------------------------------------------------
!
      subroutine s_check_flexible_time_step
!
      use m_file_control_parameter
      use check_deltat_by_prev_rms
!
!
      if( mod(istep_flex_to_max,2) .eq. 0) then
!        call s_check_deltat_by_previous
        call s_check_deltat_by_prev_rms
!
        if(my_rank .eq. izero) then
          call write_max_delta_t_check(dt_check_max_code,               &
     &        i_step_MHD, time)
        end if
!
        if(d_ratio_allmax .gt. min_eps_to_expand_dt) then
          call shrink_delta_t
          iflag_flex_step_changed = 1
          return
        else
          iflag_flex_step_changed = 0
        end if
!
        if(istep_flex_to_max .eq. 0) then
          if(d_ratio_allmax .lt. max_eps_to_shrink_dt) then
            call extend_delta_t
            iflag_flex_step_changed = 1
          end if
!
          if(my_rank .eq. izero) then
            call write_rms_delta_t_check(dt_check_max_code,             &
     &        i_step_MHD, time)
          end if
        end if
      end if
!
      end subroutine s_check_flexible_time_step
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine shrink_delta_t
!
!
      if(my_rank .eq. izero) then
        write(*,*) 'Shrink Delta t from ', dt, ' at ', i_step_MHD,      &
     &            'd_ratio_max', d_ratio_allmax
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'Old temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      if     (dt_fact .eq. one) then
        dt_fact =   five
        idt_digit = idt_digit - 1
        i_interval_flex_2_max = i_interval_flex_2_max * 2
        istep_flex_to_max = istep_flex_to_max * 2
      else if(dt_fact .eq. two) then
        dt_fact = one
        i_interval_flex_2_max = (i_interval_flex_2_max * 5) / 2
        istep_flex_to_max = (istep_flex_to_max * 5) / 2
      else if(dt_fact .eq. five) then
        dt_fact = two
        i_interval_flex_2_max = i_interval_flex_2_max * 2
        istep_flex_to_max = istep_flex_to_max * 2
      end if
!
      dt = dt_fact * ten**(idt_digit)
      ddt = one / dt
!
      if(my_rank .eq. izero) then
        write(*,*) 'New Delta t is ', dt
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'New temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      end subroutine shrink_delta_t
!
! -----------------------------------------------------------------------
!
      subroutine extend_delta_t
!
!
      if(my_rank .eq. izero) then
        write(*,*) 'Extend Delta t from ', dt, ' at ', i_step_MHD,      &
     &            'd_ratio_max', d_ratio_allmax
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'Old temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      if     (dt_fact .eq. one) then
        dt_fact =   two
        i_interval_flex_2_max = i_interval_flex_2_max / 2
        istep_flex_to_max =     istep_flex_to_max / 2
      else if(dt_fact .eq. two) then
        dt_fact = five
        i_interval_flex_2_max = (i_interval_flex_2_max * 2) / 5
        istep_flex_to_max =     (istep_flex_to_max * 2) / 5
      else if(dt_fact .eq. five) then
        dt_fact = one
        idt_digit = idt_digit + 1
        i_interval_flex_2_max = i_interval_flex_2_max / 2
        istep_flex_to_max =     istep_flex_to_max / 2
      end if
!
      dt = dt_fact * ten**(idt_digit)
      ddt = one / dt
!
      if(my_rank .eq. izero) then
        write(*,*) 'New Delta t is ', dt
      end if
      if(iflag_debug .gt. izero) then
        write(*,*) 'New temporal step is ', istep_flex_to_max,          &
     &             'to ', istep_flex_to_max
      end if
!
      end subroutine extend_delta_t
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_flex_step_monitor
!
      use m_file_control_parameter
      use m_flex_delta_t_data
!
!
      if(my_rank .eq. izero) then
        open(dt_check_max_code, file = dt_check_max_name)
        call write_delta_t_check_head(dt_check_max_code)
      end if
!
      end subroutine open_flex_step_monitor
!
! -----------------------------------------------------------------------
!
      subroutine close_flex_step_monitor
!
      use m_file_control_parameter
!
!
      if(my_rank .eq. izero) then
        close(dt_check_max_code)
      end if
!
      end subroutine close_flex_step_monitor
!
! -----------------------------------------------------------------------
!
      end module check_flexible_time_step
