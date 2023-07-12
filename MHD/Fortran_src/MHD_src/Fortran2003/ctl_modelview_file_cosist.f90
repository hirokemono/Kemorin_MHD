!>@file   ctl_modelview_file_cosist.f90
!!@brief  module ctl_modelview_file_cosist
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for SGS model controls
!!
!!@verbatim
!!      subroutine sweep_consistent_VIZs_modelview                      &
!!     &         (pvr_ctls, map_ctls, lic_ctls)
!!        type(volume_rendering_controls), intent(inout) :: pvr_ctls
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!@endverbatim
!
      module ctl_modelview_file_cosist
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_ctl_data_SGS_filter
      use t_ctl_data_SGS_model
!
      implicit  none
!
      private :: sweep_consistent_pvr_modelview
      private :: sweep_consistent_mul_modelview
      private :: check_consistent_viz_modelview
      private :: check_consistent_pvr_modelview
      private :: check_consistent_mul_viewmat
      private :: check_consistent_viewmat_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sweep_consistent_VIZ_modelview                         &
     &         (pvr_ctls, map_ctls, lic_ctls)
!
      use t_control_data_maps
      use t_control_data_pvrs
      use t_control_data_LIC_pvrs
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      integer(kind = kint) :: i
!
!
      do i = 1, map_ctls%num_map_ctl
        call check_consistent_viz_modelview                             &
    &      (map_ctls%map_ctl_struct(i)%fname_mat_ctl,                   &
    &       map_ctls%map_ctl_struct(i)%mat,                             &
    &       pvr_ctls, map_ctls, lic_ctls)
        map_ctls%map_ctl_struct(i)%mat%flag_checked = .TRUE.
      end do
!
      do i = 1, pvr_ctls%num_pvr_ctl
        call sweep_consistent_pvr_modelview                             &
    &      (pvr_ctls%pvr_ctl_type(i), pvr_ctls, map_ctls, lic_ctls)
      end do
!
      do i = 1, lic_ctls%num_lic_ctl
        call sweep_consistent_pvr_modelview                             &
    &      (lic_ctls%pvr_ctl_type(i), pvr_ctls, map_ctls, lic_ctls)
      end do
!
      end subroutine sweep_consistent_VIZ_modelview
!
!   --------------------------------------------------------------------
!
      subroutine sweep_consistent_pvr_modelview                         &
     &         (pvr_ctl, pvr_ctls, map_ctls, lic_ctls)
!
      use t_control_data_4_pvr
      use t_control_data_maps
      use t_control_data_pvrs
      use t_control_data_LIC_pvrs
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
!
        call check_consistent_viz_modelview                             &
    &      (pvr_ctl%fname_mat_ctl, pvr_ctl%mat,                         &
    &       pvr_ctls, map_ctls, lic_ctls)
!
        call check_consistent_viz_modelview                             &
    &      (pvr_ctl%movie%fname_view_start_ctl,                         &
    &       pvr_ctl%movie%view_start_ctl, pvr_ctls, map_ctls, lic_ctls)
        call check_consistent_viz_modelview                             &
    &      (pvr_ctl%movie%fname_view_end_ctl,                           &
    &       pvr_ctl%movie%view_end_ctl, pvr_ctls, map_ctls, lic_ctls)
!
        call sweep_consistent_mul_modelview                             &
    &      (pvr_ctl%movie%mul_mmats_c, pvr_ctls, map_ctls, lic_ctls)
        call sweep_consistent_mul_modelview                             &
    &      (pvr_ctl%quilt_c%mul_qmats_c, pvr_ctls, map_ctls, lic_ctls)
!
      end subroutine sweep_consistent_pvr_modelview
!
!   --------------------------------------------------------------------
!
      subroutine sweep_consistent_mul_modelview                         &
     &         (ref_mul_mats, pvr_ctls, map_ctls, lic_ctls)
!
      use t_ctl_data_view_transfers
      use t_control_data_maps
      use t_control_data_pvrs
      use t_control_data_LIC_pvrs
!
      type(multi_modeview_ctl), intent(inout) :: ref_mul_mats
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      integer(kind = kint) :: j
!
!
      do j = 1, ref_mul_mats%num_modelviews_c
        call check_consistent_viz_modelview                             &
    &      (ref_mul_mats%fname_mat_ctl(j), ref_mul_mats%matrices(j),    &
    &       pvr_ctls, map_ctls, lic_ctls)
      end do
!
      end subroutine sweep_consistent_mul_modelview
!
!   --------------------------------------------------------------------
!
      subroutine check_consistent_viz_modelview                        &
     &         (ref_fname, ref_mat, pvr_ctls, map_ctls, lic_ctls)
!
      use t_ctl_data_4_view_transfer
      use t_control_data_maps
      use t_control_data_pvrs
      use t_control_data_LIC_pvrs
!
      character(len=kchara), intent(in) :: ref_fname
      type(modeview_ctl), intent(inout) :: ref_mat
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      integer(kind = kint) :: i
!
!
      if(ref_mat%flag_checked .eqv. .TRUE.) return
      do i = 1, map_ctls%num_map_ctl
        call check_consistent_viewmat_ctl(ref_fname, ref_mat,           &
     &      map_ctls%map_ctl_struct(i)%fname_mat_ctl,                   &
     &      map_ctls%map_ctl_struct(i)%mat)
      end do
      do i = 1, pvr_ctls%num_pvr_ctl
        call check_consistent_pvr_modelview(ref_fname, ref_mat,         &
     &                                      pvr_ctls%pvr_ctl_type(i))
      end do
      do i = 1, lic_ctls%num_lic_ctl
        call check_consistent_pvr_modelview(ref_fname, ref_mat,         &
     &                                      lic_ctls%pvr_ctl_type(i))
      end do
      ref_mat%flag_checked = .TRUE.
!
      end subroutine check_consistent_viz_modelview
!
!   --------------------------------------------------------------------
!
      subroutine check_consistent_pvr_modelview                         &
     &         (ref_fname, ref_mat, pvr_ctl)
!
      use t_ctl_data_4_view_transfer
      use t_control_data_4_pvr
!
      character(len=kchara), intent(in) :: ref_fname
      type(modeview_ctl), intent(inout) :: ref_mat
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
!
      if(ref_mat%flag_checked .eqv. .TRUE.) return
      call check_consistent_viewmat_ctl(ref_fname, ref_mat,             &
     &                              pvr_ctl%fname_mat_ctl, pvr_ctl%mat)
!
      call check_consistent_viewmat_ctl(ref_fname, ref_mat,             &
     &                              pvr_ctl%movie%fname_view_start_ctl, &
     &                              pvr_ctl%movie%view_start_ctl)
      call check_consistent_viewmat_ctl(ref_fname, ref_mat,             &
     &                              pvr_ctl%movie%fname_view_end_ctl,   &
     &                              pvr_ctl%movie%view_end_ctl)
!
      call check_consistent_mul_viewmat(ref_fname, ref_mat,             &
     &                                  pvr_ctl%movie%mul_mmats_c)
      call check_consistent_mul_viewmat(ref_fname, ref_mat,             &
     &                                  pvr_ctl%quilt_c%mul_qmats_c)
!
      end subroutine check_consistent_pvr_modelview
!
!   --------------------------------------------------------------------
!
      subroutine check_consistent_mul_viewmat(ref_fname, ref_mat,       &
     &                                        mul_mats_c)
!
      use t_ctl_data_4_view_transfer
      use t_ctl_data_view_transfers
!
      character(len=kchara), intent(in) :: ref_fname
      type(modeview_ctl), intent(inout) :: ref_mat
      type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!
      integer(kind = kint) :: i
!
      if(ref_mat%flag_checked .eqv. .TRUE.) return
      do i = 1, mul_mats_c%num_modelviews_c
        call check_consistent_viewmat_ctl(ref_fname, ref_mat,           &
     &      mul_mats_c%fname_mat_ctl(i), mul_mats_c%matrices(i))
      end do
!
      end subroutine check_consistent_mul_viewmat
!
!   --------------------------------------------------------------------
!
      subroutine check_consistent_viewmat_ctl(ref_fname, ref_mat,       &
     &                                        fname_mat_ctl, mat)
!
      use t_ctl_data_4_view_transfer
!
      character(len=kchara), intent(in) :: ref_fname, fname_mat_ctl
      type(modeview_ctl), intent(inout) :: ref_mat, mat
!
      if(ref_mat%flag_checked) return
      if(mat%flag_checked) return
!
      if(ref_fname .eq. 'NO_FILE') ref_mat%flag_checked = .TRUE.
      if(fname_mat_ctl .eq. 'NO_FILE') mat%flag_checked = .TRUE.
!
      if(fname_mat_ctl .ne. ref_fname) return
      mat%flag_checked = cmp_modeview_ctl(mat, ref_mat)
!
      if(.not. mat%flag_checked) then
        write(*,*) trim(mat%block_name), ' in file ',                   &
     &             trim(fname_mat_ctl),  ' has incosistencty.'
        stop
      end if
!
      end subroutine check_consistent_viewmat_ctl
!
!   --------------------------------------------------------------------
!
      end module ctl_modelview_file_cosist
