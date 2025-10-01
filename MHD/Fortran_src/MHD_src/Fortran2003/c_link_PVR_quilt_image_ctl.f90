!>@file   c_link_PVR_quilt_image_ctl.f90
!!@brief  module c_link_PVR_quilt_image_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_PVR_quilt_img_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_PVR_quilt_img_ctl_block_name')
!!      type(c_ptr) function c_PVR_quilt_img_ctl_iflag(c_ctl)           &
!!     &          bind(C, NAME = 'c_PVR_quilt_img_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_PVR_quilt_num_column_row_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_PVR_quilt_num_column_row_ctl')
!!      type(c_ptr) function c_PVR_quilt_num_row_column_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_PVR_quilt_num_row_column_ctl')
!!      type(c_ptr) function c_PVR_quilt_mul_qmats_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_PVR_quilt_mul_qmats_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_VIZ_mul_mdlvw_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_ctl_block_name')
!!      integer(c_int) function c_VIZ_mul_mdlvw_num_mat_c(c_ctl)        &
!!     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_num_mat_c')
!!      type(c_ptr) function c_VIZ_mul_mdlvw_fname_ctl(idx_in, c_ctl)   &
!!     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_fname_ctl')
!!      type(c_ptr) function c_VIZ_mul_mdlvw_matrices(idx_in, c_ctl)    &
!!     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_matrices')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!      type(c_ptr) function c_append_mul_mdlvw_mat_ctl                 &
!!     &                  (idx, c_name, c_ctl)                          &
!!     &                  bind(C, NAME = 'c_append_mul_mdlvw_mat_ctl')
!!      type(c_ptr) function c_delete_mul_mdlvw_mat_ctl(idx, c_ctl)     &
!!     &                  bind(C, NAME = 'c_delete_mul_mdlvw_mat_ctl')
!!        integer(c_int), value, intent(in) :: idx
!!        character(C_char), intent(in) :: c_name(*)
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_PVR_quilt_image_ctl
!
      use iso_c_binding
      use t_ctl_data_quilt_image
      use t_ctl_data_view_transfers
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_quilt_img_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_quilt_img_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(quilt_image_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_quilt_img_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_quilt_img_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_quilt_img_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_PVR_quilt_img_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(quilt_image_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_quilt_img_ctl_iflag = C_loc(f_ctl%i_quilt_image)
      end function c_PVR_quilt_img_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_quilt_num_column_row_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_quilt_num_column_row_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(quilt_image_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_quilt_num_column_row_ctl = C_loc(f_ctl%num_column_row_ctl)
      end function c_PVR_quilt_num_column_row_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_quilt_num_row_column_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_quilt_num_row_column_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(quilt_image_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_quilt_num_row_column_ctl = C_loc(f_ctl%num_row_column_ctl)
      end function c_PVR_quilt_num_row_column_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_quilt_mul_qmats_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_PVR_quilt_mul_qmats_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(quilt_image_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_quilt_mul_qmats_ctl = C_loc(f_ctl%mul_qmats_c)
      end function c_PVR_quilt_mul_qmats_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_mul_mdlvw_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(multi_modelview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_mul_mdlvw_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_mul_mdlvw_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_VIZ_mul_mdlvw_num_mat_c(c_ctl)          &
     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_num_mat_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(multi_modelview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_mul_mdlvw_num_mat_c = f_ctl%num_modelviews_c
      end function c_VIZ_mul_mdlvw_num_mat_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_mul_mdlvw_fname_ctl(idx_in, c_ctl)     &
     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_fname_ctl')
      integer(c_int), value :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(multi_modelview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_mul_mdlvw_fname_ctl = C_loc(f_ctl%fname_mat_ctl(idx_in+1))
      end function c_VIZ_mul_mdlvw_fname_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_mul_mdlvw_matrices(idx_in, c_ctl)      &
     &          bind(C, NAME = 'c_VIZ_mul_mdlvw_matrices')
      integer(c_int), value :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(multi_modelview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_mul_mdlvw_matrices = C_loc(f_ctl%matrices(idx_in+1))
      end function c_VIZ_mul_mdlvw_matrices
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_append_mul_mdlvw_mat_ctl                   &
     &                  (idx, c_name, c_ctl)                            &
     &                  bind(C, NAME = 'c_append_mul_mdlvw_mat_ctl')
      use ctl_array_chara_to_c
!
      integer(c_int), value, intent(in) :: idx
      character(C_char), intent(in) :: c_name(*)
      type(c_ptr), value, intent(in) :: c_ctl
      type(multi_modelview_ctl), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call append_mul_view_trans_ctl(idx, copy_char_from_c(c_name),     &
     &                               f_ctl)
      c_append_mul_mdlvw_mat_ctl = C_loc(f_ctl%matrices)
      f_ctl%matrices(idx+1)%i_view_transform = 1
!
      end function c_append_mul_mdlvw_mat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_delete_mul_mdlvw_mat_ctl(idx, c_ctl)       &
     &                  bind(C, NAME = 'c_delete_mul_mdlvw_mat_ctl')
      integer(c_int), value, intent(in) :: idx
      type(c_ptr), value, intent(in) :: c_ctl
      type(multi_modelview_ctl), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call delete_mul_view_trans_ctl((idx+1), f_ctl)
      c_delete_mul_mdlvw_mat_ctl = C_loc(f_ctl%matrices)
!
      end function c_delete_mul_mdlvw_mat_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_PVR_quilt_image_ctl
