!>@file   viz3_step_ctls_to_time_ctl.f90
!!@brief  module viz3_step_ctls_to_time_ctl
!!
!!@author H. Matsui
!!@date Programmed in July, 2020
!
!> @brief Copy time stepin visualization control to time step control 
!!
!!@verbatim
!!      subroutine s_viz3_step_ctls_to_time_ctl(viz3_ctls, tctl)
!!        type(vis3_controls), intent(in) :: viz3_ctls
!!        type(time_data_control), intent(inout) :: tctl
!!@endverbatim
!
      module viz3_step_ctls_to_time_ctl
!
      use m_precision
      use m_constants
!
      use t_control_data_viz3
      use t_ctl_data_4_time_steps
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_viz3_step_ctls_to_time_ctl(viz3_ctls, tctl)
!
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
!
      type(vis3_controls), intent(in) :: viz3_ctls
      type(time_data_control), intent(inout) :: tctl
!
!
      if(viz3_ctls%i_step_psf_v_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (viz3_ctls%i_step_psf_v_ctl, tctl%i_step_psf_ctl)
      end if
      if(viz3_ctls%i_step_iso_v_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (viz3_ctls%i_step_iso_v_ctl, tctl%i_step_iso_ctl)
      end if
      if(viz3_ctls%i_step_pvr_v_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (viz3_ctls%i_step_pvr_v_ctl, tctl%i_step_pvr_ctl)
      end if
      if(viz3_ctls%i_step_ucd_v_ctl%iflag .gt. 0) then
        call copy_integer_ctl                                           &
     &     (viz3_ctls%i_step_ucd_v_ctl, tctl%i_step_ucd_ctl)
      end if
!
      if(viz3_ctls%delta_t_psf_v_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (viz3_ctls%delta_t_psf_v_ctl, tctl%delta_t_psf_ctl)
      end if
      if(viz3_ctls%delta_t_iso_v_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (viz3_ctls%delta_t_iso_v_ctl, tctl%delta_t_iso_ctl)
      end if
      if(viz3_ctls%delta_t_pvr_v_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (viz3_ctls%delta_t_pvr_v_ctl, tctl%delta_t_pvr_ctl)
      end if
      if(viz3_ctls%delta_t_ucd_v_ctl%iflag .gt. 0) then
        call copy_real_ctl                                              &
     &     (viz3_ctls%delta_t_ucd_v_ctl, tctl%delta_t_field_ctl)
      end if
!
      end subroutine s_viz3_step_ctls_to_time_ctl
!
!  ---------------------------------------------------------------------
!
      end module viz3_step_ctls_to_time_ctl
