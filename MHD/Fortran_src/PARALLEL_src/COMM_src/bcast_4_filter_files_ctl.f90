!>@file   bcast_4_filter_files_ctl.f90
!!@brief  module bcast_4_filter_files_ctl
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief  Structure for reading parameters for filtering files
!!
!!@verbatim
!!      subroutine bcast_ctl_data_4_filter_files(ffile_ctl)
!!        type(filter_file_control), intent(inout) :: ffile_ctl
!!@endverbatim
!
      module bcast_4_filter_files_ctl
!
      use m_precision
      use t_ctl_data_filter_files
!
      implicit  none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_filter_files(ffile_ctl)
!
      use bcast_control_arrays
!
      type(filter_file_control), intent(inout) :: ffile_ctl
!
!
      call bcast_ctl_type_c1(ffile_ctl%filter_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_coef_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_elen_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_moms_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%filter_wide_head_ctl)
      call bcast_ctl_type_c1(ffile_ctl%model_coef_ini_head_ctl)
!
      call bcast_ctl_type_c1(ffile_ctl%filter_elen_format)
      call bcast_ctl_type_c1(ffile_ctl%filter_3d_format)
      call bcast_ctl_type_c1(ffile_ctl%filter_wide_format)
!
      end subroutine bcast_ctl_data_4_filter_files
!
!  ---------------------------------------------------------------------
!
      end module bcast_4_filter_files_ctl
