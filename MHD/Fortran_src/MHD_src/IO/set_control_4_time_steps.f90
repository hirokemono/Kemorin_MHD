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
        write(*,*) 'elapsed_time ', elapsed_time
        write(*,*) 'i_step_check ', rms_step1%increment
        write(*,*) 'i_step_output_rst ', rst_step1%increment
        write(*,*) 'i_step_output_ucd ', ucd_step1%increment
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
        call set_output_step_4_fixed_step(ione,                         &
     &      tctl%i_step_sgs_coefs_ctl, tctl%delta_t_sgs_coefs_ctl,      &
     &      sgs_step1)
      end if
!
      call set_output_step_4_fixed_step                                 &
     &   (izero, tctl%i_step_monitor_ctl, tctl%delta_t_monitor_ctl,     &
     &    point_step1)
!
      call set_output_step_4_fixed_step(izero,                          &
     &    tctl%i_step_boundary_ctl, tctl%delta_t_boundary_ctl,          &
     &    boundary_step1)
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
      call set_output_step_4_flex_step(ione, dt_max,                    &
     &    tctl%i_step_check_ctl, tctl%delta_t_check_ctl, rms_step1)
!
!
      call set_output_step_4_flex_step(ione, dt_max,                    &
     &    tctl%i_step_rst_ctl, tctl%delta_t_rst_ctl, rst_step1)
!
      call set_output_step_4_flex_step(ione, dt_max,                    &
     &   tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl, ucd_step1)
!
      i_step_init =   istep_rst_start * rst_step1%increment
      i_step_number = istep_rst_end *   rst_step1%increment
!
      call viz_flex_time_step_controls(tctl, viz_step)
!
!
      i_step_sgs_coefs = 1
      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call set_output_step_4_flex_step(ione, dt_max,                  &
     &      tctl%i_step_sgs_coefs_ctl, tctl%delta_t_sgs_coefs_ctl,      &
     &      sgs_step1)
      end if
!
      call set_output_step_4_flex_step(izero, dt_max,                   &
     &    tctl%i_step_monitor_ctl, tctl%delta_t_monitor_ctl,            &
     &    point_step1)
!
      call set_output_step_4_flex_step(izero, dt_max,                   &
     &    tctl%i_step_boundary_ctl, tctl%delta_t_boundary_ctl,          &
     &    boundary_step1)
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
!
      end module set_control_4_time_steps
