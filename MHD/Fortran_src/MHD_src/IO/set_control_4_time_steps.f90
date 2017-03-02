!>@file   set_control_4_time_steps.f90
!!@brief  module set_control_4_time_steps
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    modified by H. Matsui in 2001
!!@n    modified by H. Matsui in Sep., 2006
!
!> @brief set parameters for time stepping
!!
!!@verbatim
!!      subroutine s_set_control_4_time_steps(SGS_param, mr_ctl, tctl)
!!        type(mhd_restart_control), intent(in) :: mr_ctl
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(time_data_control), intent(inout) :: tctl
!!@endverbatim
!
      module set_control_4_time_steps
!
      use m_precision
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_t_step_parameter
      use m_t_int_parameter
      use t_SGS_control_parameter
      use t_ctl_data_4_time_steps
      use t_VIZ_step_parameter
!
      implicit  none
!
      private :: set_flex_time_step_controls
      private :: set_monitor_param_4_flex_step
      private :: set_fixed_time_step_controls
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_time_steps(SGS_param, mr_ctl, tctl)
!
      use t_ctl_data_mhd_evo_scheme
      use m_initial_field_control
      use cal_num_digits
      use skip_comment_f
!
      type(mhd_restart_control), intent(in) :: mr_ctl
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(time_data_control), intent(inout) :: tctl
!
!
!  control for restert
!
      call set_initial_field_id(mr_ctl%restart_flag_ctl, tctl)
!
        iflag_flexible_step = iflag_fixed_step
        if(tctl%flexible_step_ctl%iflag .gt. 0                          &
     &     .and. yes_flag(tctl%flexible_step_ctl%charavalue)) then
          iflag_flexible_step = iflag_flex_step
        end if
!
        if (tctl%dt_ctl%iflag .eq. 0) then
          e_message = 'Set delta t'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          dt = tctl%dt_ctl%realvalue
          ddt = 1.0d0 / dt
          call cal_num_digit_real(dt, dt_fact, idt_digit)
        end if
!
        if(iflag_flexible_step .eq. iflag_flex_step) then
          if (tctl%min_delta_t_ctl%iflag .eq. 0) then
            e_message = 'Set maximum delta t'
            call calypso_MPI_abort(ierr_evo, e_message)
          else
            dt_min = tctl%min_delta_t_ctl%realvalue
          end if
!
          if (tctl%max_delta_t_ctl%iflag .eq. 0) then
            e_message = 'Set maximum delta t'
            call calypso_MPI_abort(ierr_evo, e_message)
          else
            dt_max = tctl%max_delta_t_ctl%realvalue
          end if
!
          if (tctl%max_eps_to_shrink_ctl%iflag .eq. 0) then
            e_message = 'Set maximum error to shrink delta t'
            call calypso_MPI_abort(ierr_evo, e_message)
          else
            max_eps_to_shrink_dt = tctl%max_eps_to_shrink_ctl%realvalue
          end if
!
          if (tctl%min_eps_to_expand_ctl%iflag .eq. 0) then
            e_message = 'Set minimum error to expand delta t'
            call calypso_MPI_abort(ierr_evo, e_message)
          else
            min_eps_to_expand_dt = tctl%min_eps_to_expand_ctl%realvalue
          end if
!
          istep_flex_to_max = izero
!
          if(dt .gt. zero) i_interval_flex_2_max = nint(dt_max / dt)
        else
          dt_max = dt
          dt_min = dt
          i_interval_flex_2_max = ione
          istep_flex_to_max = izero
        end if
!
!   parameters for time evolution
!
      if(iflag_flexible_step .eq. iflag_flex_step) then
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_flex_time_step_controls'
        call set_flex_time_step_controls(SGS_param, tctl, viz_step1)
      else
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_fixed_time_step_controls'
        call set_fixed_time_step_controls(SGS_param, tctl, viz_step1)
      end if
!
      if (i_step_number.eq.-1) then
        if (tctl%elapsed_time_ctl%iflag .eq. 0) then
          e_message                                                     &
     &      = 'Set elapsed time to finish (second)'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          elapsed_time  = tctl%elapsed_time_ctl%realvalue
        end if
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'dt', dt, dt_fact, idt_digit
        write(*,*) 'i_step_init ',i_step_init
        write(*,*) 'i_step_number ',i_step_number
        write(*,*) 'istep_rst_start ', istep_rst_start
        write(*,*) 'istep_rst_end ',  istep_rst_end
        write(*,*) 'elapsed_time ',elapsed_time
        write(*,*) 'i_step_check ',i_step_check
        write(*,*) 'i_step_output_rst ',i_step_output_rst
        write(*,*) 'i_step_output_ucd ',i_step_output_ucd
      end if
!
      end subroutine s_set_control_4_time_steps
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_time_step_controls(SGS_param, tctl, viz_step)
!
      use set_fixed_time_step_params
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(time_data_control), intent(inout) :: tctl
      type(VIZ_step_params), intent(inout) :: viz_step
!
      integer(kind = kint) :: ierr
!
!
      call s_set_fixed_time_step_params                                 &
     &   (tctl, viz_step, ierr, e_message)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
      i_step_sgs_coefs = 1
      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call monitor_param_4_fixed_step(ione,                       &
     &      tctl%i_step_sgs_coefs_ctl, tctl%delta_t_sgs_coefs_ctl,      &
     &      i_step_sgs_output, delta_t_sgs_output)
      end if
!
      call monitor_param_4_fixed_step                               &
     &   (izero, tctl%i_step_monitor_ctl, tctl%delta_t_monitor_ctl,     &
     &    i_step_output_monitor, delta_t_output_monitor)
!
      call monitor_param_4_fixed_step(izero,                        &
     &    tctl%i_step_boundary_ctl, tctl%delta_t_boundary_ctl,          &
     &    i_step_output_boundary, delta_t_output_boundary)
!
      end subroutine set_fixed_time_step_controls
!
! -----------------------------------------------------------------------
!
      subroutine set_flex_time_step_controls(SGS_param, tctl, viz_step)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(time_data_control), intent(inout) :: tctl
      type(VIZ_step_params), intent(inout) :: viz_step
!
!
      istep_rst_start   = 0
      if (tctl%start_rst_step_ctl%iflag .gt. 0) then
        istep_rst_start   = tctl%start_rst_step_ctl%intvalue
      end if
!
      if (tctl%end_rst_step_ctl%iflag .eq. 0) then
        e_message = 'Set time to finish'
          call calypso_MPI_abort(ierr_evo, e_message)
      else
        istep_rst_end = tctl%end_rst_step_ctl%intvalue
      end if
!
      call set_monitor_param_4_flex_step(ione, tctl%i_step_check_ctl,   &
     &    tctl%delta_t_check_ctl, i_step_check, delta_t_step_check)
!
!
      call set_monitor_param_4_flex_step(ione, tctl%i_step_rst_ctl,     &
     &    tctl%delta_t_rst_ctl, i_step_output_rst, delta_t_output_rst)
!
      call set_monitor_param_4_flex_step                                &
     &   (ione, tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl,            &
     &    i_step_output_ucd, delta_t_output_ucd)
!
      i_step_init =   istep_rst_start * i_step_output_rst
      i_step_number = istep_rst_end *   i_step_output_rst
!
      call viz_flex_time_step_controls(tctl, viz_step)
!
!
      i_step_sgs_coefs = 1
      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call set_monitor_param_4_flex_step(ione,                        &
     &      tctl%i_step_sgs_coefs_ctl, tctl%delta_t_sgs_coefs_ctl,      &
     &      i_step_sgs_output, delta_t_sgs_output)
      end if
!
      call set_monitor_param_4_flex_step                                &
     &   (izero, tctl%i_step_monitor_ctl, tctl%delta_t_monitor_ctl,     &
     &    i_step_output_monitor, delta_t_output_monitor)
!
      call set_monitor_param_4_flex_step(izero,                         &
     &    tctl%i_step_boundary_ctl, tctl%delta_t_boundary_ctl,          &
     &    i_step_output_boundary, delta_t_output_boundary)
!
      if (istep_rst_end .eq. -1) then
        if (tctl%elapsed_time_ctl%iflag .eq. 0) then
          e_message                                                     &
     &      = 'Set elapsed time to finish (second)'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          elapsed_time  = tctl%elapsed_time_ctl%realvalue
        end if
      end if
!
      end subroutine set_flex_time_step_controls
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_monitor_param_4_flex_step(istep_def,               &
     &          istep_ctl, delta_t_ctl, istep_out, dt_out)
!
      use t_control_elements
!
      integer(kind = kint), intent(in) :: istep_def
      type(read_real_item), intent(in) :: delta_t_ctl
      type(read_integer_item), intent(in) :: istep_ctl
!
      integer(kind = kint), intent(inout) :: istep_out
      real(kind = kreal), intent(inout) :: dt_out
!
!
      if ( (istep_ctl%iflag+delta_t_ctl%iflag) .eq. 0) then
        istep_out =   istep_def
        dt_out = dble(istep_def) * dt_max
      else if(delta_t_ctl%iflag .eq. 0) then
        istep_out =   istep_ctl%intvalue
        dt_out = dble(istep_out) * dt_max
      else
        dt_out =    delta_t_ctl%realvalue
        istep_out = nint(dt_out / dt_max)
      end if
!
      end subroutine set_monitor_param_4_flex_step
!
! -----------------------------------------------------------------------
!
      end module set_control_4_time_steps
