!>@file   set_fixed_time_step_params.f90
!!@brief  module set_fixed_time_step_params
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Set time step parameters
!!
!!@verbatim
!!      subroutine s_set_fixed_time_step_params(tctl, init_d, finish_d, &
!!     &          rst_step, ucd_step, ierr, errmsg)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_data), intent(inout) :: init_d
!!        type(finish_data), intent(inout) :: finish_d
!!        type(IO_step_param), intent(inout) :: rst_step, ucd_step
!!@endverbatim
!
      module set_fixed_time_step_params
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_time_data
      use t_ctl_data_4_time_steps
      use t_IO_step_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_fixed_time_step_params(tctl, init_d, finish_d,   &
     &          rst_step, ucd_step, ierr, errmsg)
!
      use m_error_IDs
!
      type(time_data_control), intent(in) :: tctl
!
      type(time_data), intent(inout) :: init_d
      type(finish_data), intent(inout) :: finish_d
      type(IO_step_param), intent(inout) :: rst_step, ucd_step
      integer(kind = kint), intent(inout) :: ierr
      character(len=kchara), intent(inout) :: errmsg
!
!
      init_d%i_time_step   = 0
      if (tctl%i_step_init_ctl%iflag .gt. 0) then
        init_d%i_time_step = tctl%i_step_init_ctl%intvalue
      end if
!
      if (tctl%i_step_number_ctl%iflag .eq. 0) then
        ierr = ierr_evo
        errmsg = 'Set step number to finish'
        return
      else
        finish_d%i_end_step = tctl%i_step_number_ctl%intvalue
      end if
!
!
      call set_output_step_4_fixed_step(ione, init_d%dt,                &
     &    tctl%i_step_rst_ctl, tctl%delta_t_rst_ctl, rst_step)
!
      call set_output_step_4_fixed_step(ione, init_d%dt,                &
     &    tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl, ucd_step)
!
      if (finish_d%i_end_step .eq. -1) then
        if (tctl%elapsed_time_ctl%iflag .eq. 0) then
          ierr = ierr_evo
          errmsg = 'Set elapsed time to finish (second)'
          return
        else
          finish_d%elapsed_time  = tctl%elapsed_time_ctl%realvalue
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
