!>@file   set_fixed_time_step_params.f90
!!@brief  module set_fixed_time_step_params
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Set time step parameters
!!
!!@verbatim
!!      subroutine s_set_fixed_time_step_params                           &
!!     &         (tctl, viz_step, ierr, errmsg)
!!        type(time_data_control), intent(in) :: tctl
!!        type(VIZ_step_params), intent(inout) :: viz_step
!!@endverbatim
!
      module set_fixed_time_step_params
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_ctl_data_4_time_steps
      use t_VIZ_step_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_fixed_time_step_params                           &
     &         (tctl, viz_step, ierr, errmsg)
!
      use m_error_IDs
      use m_t_step_parameter
      use m_t_int_parameter
!
      type(time_data_control), intent(in) :: tctl
!
      type(VIZ_step_params), intent(inout) :: viz_step
      integer(kind = kint), intent(inout) :: ierr
      character(len=kchara), intent(inout) :: errmsg
!
!
      i_step_init   = 0
      if (tctl%i_step_init_ctl%iflag .gt. 0) then
        i_step_init   = tctl%i_step_init_ctl%intvalue
      end if
!
      if (tctl%i_step_number_ctl%iflag .eq. 0) then
        ierr = ierr_evo
        errmsg = 'Set step number to finish'
        return
      else
        i_step_number = tctl%i_step_number_ctl%intvalue
      end if
!
      call set_output_step_4_fixed_step(ione, tctl%i_step_check_ctl,    &
     &    tctl%delta_t_check_ctl, rms_step1)
!
!
      call set_output_step_4_fixed_step(ione, tctl%i_step_rst_ctl,      &
     &    tctl%delta_t_rst_ctl, rst_step1)
!
      call set_output_step_4_fixed_step(ione, tctl%i_step_ucd_ctl,      &
     &    tctl%delta_t_field_ctl, ucd_step1)
!
      if(rst_step1%increment .gt. 0) then
        istep_rst_start = int(i_step_init /   rst_step1%increment)
        istep_rst_end =   int(i_step_number / rst_step1%increment)
      else
        istep_rst_start = i_step_init 
        istep_rst_end =   i_step_number
      end if
!
      if(i_step_init .eq. -1)   istep_rst_start = -1
      if(i_step_number .eq. -1) istep_rst_end =   -1
!
      call viz_fixed_time_step_params(tctl, viz_step)
!
      if (i_step_number.eq.-1) then
        if (tctl%elapsed_time_ctl%iflag .eq. 0) then
          ierr = ierr_evo
          errmsg = 'Set elapsed time to finish (second)'
          return
        else
          elapsed_time  = tctl%elapsed_time_ctl%realvalue
        end if
      end if
!
      ierr = 0
!
      end subroutine s_set_fixed_time_step_params
!
! -----------------------------------------------------------------------
!
      end module set_fixed_time_step_params
