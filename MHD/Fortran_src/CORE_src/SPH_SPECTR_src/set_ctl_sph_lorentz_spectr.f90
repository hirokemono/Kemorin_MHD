!>@file   set_ctl_sph_lorentz_spectr.f90
!!        module set_ctl_sph_lorentz_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Set control parameter for monitoring spectrum
!!
!!@verbatim
!!      subroutine set_ctl_params_layer_lor_spec(lp_ctl, lor_spectr)
!!      subroutine set_ctl_params_vol_lor_spectr(smonitor_ctl,          &
!!     &                                         lor_spectr)
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(sph_mean_squares), intent(inout) :: lor_spectr
!!@endverbatim
!!
      module set_ctl_sph_lorentz_spectr
!
      use m_precision
      use t_rms_4_sph_spectr
!
      implicit  none
!
      private :: set_ctl_base_vol_lor_spec_file
      private :: set_ctl_vol_lor_spec_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_layer_lor_spec(lp_ctl, lor_spectr)
!
      use t_ctl_data_sph_layer_spectr
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use set_control_sph_spectr
      use skip_comment_f
!
      type(layerd_spectr_control), intent(in) :: lp_ctl
      type(sph_mean_squares), intent(inout) :: lor_spectr
!
!
      lor_spectr%iflag_layer_rms_spec                                   &
     &      = lp_ctl%layered_work_spectr_prefix%iflag
      if(lor_spectr%iflag_layer_rms_spec .gt. 0) then
        lor_spectr%fhead_rms_layer                                      &
     &        = lp_ctl%layered_work_spectr_prefix%charavalue
      end if
!
      call set_ctl_prm_layered_spectr(lp_ctl, lor_spectr)
!
      end subroutine set_ctl_params_layer_lor_spec
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_vol_lor_spectr(smonitor_ctl,            &
     &                                         lor_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use set_control_sph_spectr
      use skip_comment_f
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_mean_squares), intent(inout) :: lor_spectr
!
      integer(kind = kint) :: inum, icou
!
!
      icou = 0
      if(smonitor_ctl%volume_work_spectr_prefix%iflag .gt. 0)           &
     &  icou = icou + 1
      do inum = 1, smonitor_ctl%num_vspec_ctl
        if(smonitor_ctl%v_pwr(inum)%volume_lor_spec_file_ctl%iflag      &
     &        .gt. 0) icou = icou + 1
      end do
!
      call alloc_volume_spectr_data(icou, lor_spectr)
!
      icou = 0
      if(smonitor_ctl%volume_work_spectr_prefix%iflag .gt. 0) then
        icou = icou + 1
        call set_ctl_base_vol_lor_spec_file(smonitor_ctl,               &
     &                                      lor_spectr%v_spectr(icou))
        call set_ctl_prm_base_vol_spectr(smonitor_ctl,                  &
     &                                   lor_spectr%v_spectr(icou))
      end if
!
      do inum = 1, smonitor_ctl%num_vspec_ctl
        if(smonitor_ctl%v_pwr(inum)%volume_lor_spec_file_ctl%iflag      &
     &        .eq. 0) cycle
!
        icou = icou + 1
        call set_ctl_vol_lor_spec_file(smonitor_ctl%v_pwr(inum),        &
     &                                   lor_spectr%v_spectr(icou))
      end do
!
      end subroutine set_ctl_params_vol_lor_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ctl_base_vol_lor_spec_file(smonitor_ctl, v_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
!
      use m_file_format_labels
      use skip_comment_f
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_vol_mean_squares), intent(inout) :: v_spectr
!
!
      v_spectr%iflag_volume_ave_sph = 0
!
      v_spectr%iflag_volume_rms_spec                                    &
     &        = smonitor_ctl%volume_work_spectr_prefix%iflag
      if(v_spectr%iflag_volume_rms_spec .gt. 0) then
        v_spectr%fhead_rms_v                                            &
     &        = smonitor_ctl%volume_work_spectr_prefix%charavalue
      end if
!
      end subroutine set_ctl_base_vol_lor_spec_file
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_vol_lor_spec_file(v_pwr_ctl, v_lor_spectr)
!
      use t_ctl_data_4_sph_monitor
      use t_pickup_sph_spectr_data
      use t_multi_flag_labels
      use m_file_format_labels
!
      use set_control_sph_spectr
      use skip_comment_f
!
      type(volume_spectr_control), intent(in) :: v_pwr_ctl
      type(sph_vol_mean_squares), intent(inout) :: v_lor_spectr
!
      character(len = kchara) :: input_flag
!
!
      v_lor_spectr%iflag_volume_ave_sph = 0
      v_lor_spectr%iflag_volume_rms_spec                                &
     &      = v_pwr_ctl%volume_lor_spec_file_ctl%iflag
      if(v_lor_spectr%iflag_volume_rms_spec .gt. 0) then
        v_lor_spectr%fhead_rms_v                                        &
     &     = v_pwr_ctl%volume_lor_spec_file_ctl%charavalue
      end if
!
      v_lor_spectr%gzip_flag_vol_spec = .FALSE.
      if(v_pwr_ctl%volume_spec_format_ctl%iflag .gt. 0) then
        input_flag = v_pwr_ctl%volume_spec_format_ctl%charavalue
        if(check_mul_flags(input_flag, gzip_flags))                     &
     &                   v_lor_spectr%gzip_flag_vol_spec = .TRUE.
      end if
!
      call set_ctl_prm_vol_sph_spectr(v_pwr_ctl, v_lor_spectr)
      call set_ctl_vol_sph_spectr_range(v_pwr_ctl, v_lor_spectr)
!
      end subroutine set_ctl_vol_lor_spec_file
!
! -----------------------------------------------------------------------
!
      end module set_ctl_sph_lorentz_spectr
