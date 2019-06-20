!>@file   bcast_dup_view_transfer_ctl.f90
!!@brief  module bcast_dup_view_transfer_ctl
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine bcast_view_transfer_ctl(mat)
!!        type(modeview_ctl), intent(inout) :: mat
!!      subroutine dup_view_transfer_ctl(org_mat, new_mat)
!!        type(modeview_ctl), intent(in) :: org_mat
!!        type(modeview_ctl), intent(inout) :: new_mat
!!@endverbatim
!!
!
      module bcast_dup_view_transfer_ctl
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use t_ctl_data_4_view_transfer
!
      implicit  none
!
      private :: bcast_view_matrix_ctl, dup_view_matrix_ctl
      private :: bcast_projection_mat_ctl, copy_projection_mat_ctl
      private :: bcast_image_size_ctl, copy_image_size_ctl 
      private :: bcast_stereo_view_ctl, copy_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_view_transfer_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call bcast_view_matrix_ctl(mat)
      call bcast_projection_mat_ctl(mat)
      call bcast_image_size_ctl(mat)
      call bcast_stereo_view_ctl(mat)
!
      end subroutine bcast_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dup_view_transfer_ctl(org_mat, new_mat)
!
      type(modeview_ctl), intent(in) :: org_mat
      type(modeview_ctl), intent(inout) :: new_mat
!
!
      call dup_view_matrix_ctl(org_mat, new_mat)
      call copy_projection_mat_ctl(org_mat, new_mat)
      call copy_image_size_ctl(org_mat, new_mat)
      call copy_stereo_view_ctl(org_mat, new_mat)
!
      end subroutine dup_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_view_matrix_ctl(mat)
!
      use bcast_control_arrays
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_view_transform,  1,                          &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_array_cr(mat%lookpoint_ctl)
      call bcast_ctl_array_cr(mat%viewpoint_ctl)
      call bcast_ctl_array_cr(mat%up_dir_ctl)
!
      call bcast_ctl_array_cr(mat%view_rot_vec_ctl)
      call bcast_ctl_array_cr(mat%scale_vector_ctl)
      call bcast_ctl_array_cr(mat%viewpt_in_viewer_ctl)
!
      call bcast_ctl_array_c2r(mat%modelview_mat_ctl)
!
      call bcast_ctl_type_r1(mat%view_rotation_deg_ctl)
      call bcast_ctl_type_r1(mat%scale_factor_ctl)
!
      end subroutine bcast_view_matrix_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_projection_mat_ctl(mat)
!
      use bcast_control_arrays
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_project_mat,  1,                             &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_r1(mat%perspective_angle_ctl)
      call bcast_ctl_type_r1(mat%perspective_xy_ratio_ctl)
      call bcast_ctl_type_r1(mat%perspective_near_ctl)
      call bcast_ctl_type_r1(mat%perspective_far_ctl)
!
      end subroutine bcast_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_image_size_ctl(mat)
!
      use bcast_control_arrays
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_image_size,  1,                              &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_i1(mat%num_xpixel_ctl)
      call bcast_ctl_type_i1(mat%num_ypixel_ctl)
!
      end subroutine bcast_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_stereo_view_ctl(mat)
!
      use bcast_control_arrays
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_stereo_view,  1,                             &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_r1(mat%focalpoint_ctl)
      call bcast_ctl_type_r1(mat%eye_separation_ctl)
!
      end subroutine bcast_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_view_matrix_ctl(org_mat, new_mat)
!
      use copy_control_elements
!
      type(modeview_ctl), intent(in) :: org_mat
      type(modeview_ctl), intent(inout) :: new_mat
!
!
      new_mat%i_view_transform = org_mat%i_view_transform
!
      call dup_control_array_c_r(org_mat%lookpoint_ctl,                 &
     &                           new_mat%lookpoint_ctl)
      call dup_control_array_c_r(org_mat%viewpoint_ctl,                 &
     &                           new_mat%viewpoint_ctl)
      call dup_control_array_c_r(org_mat%up_dir_ctl,                    &
     &                           new_mat%up_dir_ctl)
!
      call dup_control_array_c_r(org_mat%view_rot_vec_ctl,              &
     &                           new_mat%view_rot_vec_ctl)
      call dup_control_array_c_r(org_mat%scale_vector_ctl,              &
     &                           new_mat%scale_vector_ctl)
      call dup_control_array_c_r(org_mat%viewpt_in_viewer_ctl,          &
     &                           new_mat%viewpt_in_viewer_ctl)
!
      call dup_control_array_c2_r(org_mat%modelview_mat_ctl,            &
     &                            new_mat%modelview_mat_ctl)
!
      call copy_real_ctl(org_mat%view_rotation_deg_ctl,                 &
     &                   new_mat%view_rotation_deg_ctl)
      call copy_real_ctl(org_mat%scale_factor_ctl,                      &
     &                   new_mat%scale_factor_ctl)
!
      end subroutine dup_view_matrix_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_projection_mat_ctl(org_mat, new_mat)
!
      use copy_control_elements
!
      type(modeview_ctl), intent(in) :: org_mat
      type(modeview_ctl), intent(inout) :: new_mat
!
!
      new_mat%i_project_mat = org_mat%i_project_mat
!
      call copy_real_ctl(org_mat%perspective_angle_ctl,                 &
     &                   new_mat%perspective_angle_ctl)
      call copy_real_ctl(org_mat%perspective_xy_ratio_ctl,              &
     &                   new_mat%perspective_xy_ratio_ctl)
      call copy_real_ctl(org_mat%perspective_near_ctl,                  &
     &                   new_mat%perspective_near_ctl)
      call copy_real_ctl(org_mat%perspective_far_ctl,                   &
     &                   new_mat%perspective_far_ctl)
!
      end subroutine copy_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_image_size_ctl(org_mat, new_mat)
!
      use copy_control_elements
!
      type(modeview_ctl), intent(in) :: org_mat
      type(modeview_ctl), intent(inout) :: new_mat
!
!
      new_mat%i_image_size = org_mat%i_image_size
!
      call copy_integer_ctl(org_mat%num_xpixel_ctl,                     &
     &                      new_mat%num_xpixel_ctl)
      call copy_integer_ctl(org_mat%num_ypixel_ctl,                     &
     &                      new_mat%num_ypixel_ctl)
!
      end subroutine copy_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_stereo_view_ctl(org_mat, new_mat)
!
      use copy_control_elements
!
      type(modeview_ctl), intent(in) :: org_mat
      type(modeview_ctl), intent(inout) :: new_mat
!
!
      new_mat%i_stereo_view = org_mat%i_stereo_view
!
      call copy_real_ctl(org_mat%focalpoint_ctl,                        &
     &                   new_mat%focalpoint_ctl)
      call copy_real_ctl(org_mat%eye_separation_ctl,                    &
     &                   new_mat%eye_separation_ctl)
!
      end subroutine copy_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_dup_view_transfer_ctl
