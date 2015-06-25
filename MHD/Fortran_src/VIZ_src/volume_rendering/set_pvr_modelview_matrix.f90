!>@file  set_pvr_modelview_matrix.f90
!!       module set_pvr_modelview_matrix
!!
!!@author H. Matsui
!!@date   Programmed in May. 2009
!
!> @brief Evaluate model view matirx
!!
!!@verbatim
!!      subroutine s_set_pvr_modelview_matrix(mat, view_param)
!!@endverbatim
!
      module set_pvr_modelview_matrix
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use m_ctl_data_4_view_transfer
      use t_control_params_4_pvr
!
      implicit none
!
      private :: copy_pvr_modelview_matrix, set_viewpoint_vector_ctl
      private :: copy_pvr_perspective_matrix, copy_pvr_image_size
      private :: set_view_rotation_vect_ctl, set_view_scale_factor_ctl
      private :: set_viewpnt_in_viewer_ctl
!
      private :: set_4direction_flag, set_3direction_flag
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_pvr_modelview_matrix(mat, view_param)
!
      type(modeview_ctl), intent(inout) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      call copy_pvr_image_size(mat, view_param)
      call copy_pvr_perspective_matrix(mat, view_param)
!
      if (mat%modelview_mat_ctl%num .gt. 0) then
        call copy_pvr_modelview_matrix(mat, view_param)
        call dealloc_control_array_c2_r(mat%modelview_mat_ctl)
      else
        call set_viewpoint_vector_ctl(mat, view_param)
        call dealloc_control_array_c_r(mat%lookpoint_ctl)
        call dealloc_control_array_c_r(mat%viewpoint_ctl)
        call dealloc_control_array_c_r(mat%up_dir_ctl)
      end if
!
      if(mat%i_view_rot_deg.gt.0                                        &
     &    .and. mat%view_rot_vec_ctl%num.ge.3) then
        call set_view_rotation_vect_ctl(mat, view_param)
        call dealloc_control_array_c_r(mat%view_rot_vec_ctl)
      end if
!
      if(mat%i_scale_factor .gt. 0) then
        view_param%scale_factor_pvr(1:3) = mat%scale_factor_ctl
        view_param%iflag_scale_fact = 1
      else if(mat%scale_vector_ctl%num .ge. 3) then
        call set_view_scale_factor_ctl(mat, view_param)
        call dealloc_control_array_c_r(mat%scale_vector_ctl)
      end if
!
      if(mat%viewpt_in_viewer_ctl%num .ge. 3) then
        call set_viewpnt_in_viewer_ctl(mat, view_param)
        call dealloc_control_array_c_r(mat%viewpt_in_viewer_ctl)
      end if
!
      end subroutine s_set_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_image_size(mat, view_param)
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      if (mat%i_x_pixel .gt. 0) then
        view_param%n_pvr_pixel(1) = mat%num_pixel_ctl(1)
      else
        view_param%n_pvr_pixel(1) = 640
      end if
!
      if (mat%i_y_pixel .gt. 0) then
        view_param%n_pvr_pixel(2) = mat%num_pixel_ctl(2)
      else
        view_param%n_pvr_pixel(2) = 480
      end if
!
      end subroutine copy_pvr_image_size
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_perspective_matrix(mat, view_param)
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      if (mat%i_perspect_angle .gt. 0) then
        view_param%perspective_angle = mat%perspective_angle_ctl
      else
        view_param%perspective_angle = 10.0d0
      end if
!
      if (mat%i_perspect_xy .gt. 0) then
        view_param%perspective_xy_ratio = mat%perspective_xy_ratio_ctl
      else
        view_param%perspective_xy_ratio = one
      end if
!
      if (mat%i_perspect_near .gt. 0) then
        view_param%perspective_near = mat%perspective_near_ctl
      else
        view_param%perspective_near = 0.1d0
      end if
!
      if (mat%i_perspect_far .gt. 0) then
        view_param%perspective_far = mat%perspective_far_ctl
      else
        view_param%perspective_far = 1.0d2
      end if
!
      view_param%iflag_perspective                                      &
     &      = mat%i_perspect_angle*mat%i_perspect_xy                    &
     &       *mat%i_perspect_near*mat%i_perspect_far
!
!
      if (mat%i_focalpoint .gt. 0) then
        view_param%focalLength = mat%focalpoint_ctl
      else
        view_param%focalLength = 1.0d1
      end if
!
      if (mat%i_eye_separation .gt. 0) then
        view_param%eye_separation = mat%eye_separation_ctl
      else
        view_param%eye_separation = 1.0d-1
      end if
!
      view_param%iflag_stereo_pvr                                       &
     &      = mat%i_focalpoint*mat%i_eye_separation
!
      end subroutine copy_pvr_perspective_matrix
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_modelview_matrix(mat, view_param)
!
      use calypso_mpi
      use skip_comment_f
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
      integer(kind = kint) :: i, nd1, nd2
!
!
      if(mat%modelview_mat_ctl%num .ne. 16) then
        write(e_message,'(a)')                                          &
     &     'Modelview  Matrix should be 16 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      do i = 1, mat%modelview_mat_ctl%num
        nd1 = set_4direction_flag(mat%modelview_mat_ctl%c1_tbl(i))
        nd2 = set_4direction_flag(mat%modelview_mat_ctl%c2_tbl(i))
!
        if(nd1*nd2 .gt. 0) then
          view_param%modelview_mat(nd2,nd1)                             &
     &              = mat%modelview_mat_ctl%vect(i)
        end if
      end do
!
      view_param%iflag_modelview_mat = 1
!
      end subroutine copy_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
!
      subroutine set_viewpoint_vector_ctl(mat, view_param)
!
      use calypso_mpi
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: i, nd
!
!
!      if(mat%lookpoint_ctl%num .ne. 3) then
!        write(e_message,'(a)')                                         &
!     &     'Lookatpoint vector should be 3 components'
!        call calypso_MPI_abort(ierr_PVR, e_message)
!      end if
!      if(mat%viewpoint_ctl%num .ne. 3) then
!        write(e_message,'(a)')                                         &
!     &     'Viewpoint vector should be 3 components'
!        call calypso_MPI_abort(ierr_PVR, e_message)
!      end if
!      if(mat%up_dir_ctl%num .ne. 3) then
!        write(e_message,'(a)')                                         &
!     &     'Up-direction vector should be 3 components'
!        call calypso_MPI_abort(ierr_PVR, e_message)
!      end if
!
      do i = 1, mat%lookpoint_ctl%num
        nd = set_3direction_flag(mat%lookpoint_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%lookat_vec(nd) = mat%lookpoint_ctl%vect(i)
      end do
      if(mat%lookpoint_ctl%num .ge. 3) then
        view_param%iflag_lookpoint = 1
      end if
!
      do i = 1, mat%viewpoint_ctl%num
        nd = set_3direction_flag(mat%viewpoint_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%viewpoint_vec(nd) = mat%viewpoint_ctl%vect(i)
      end do
      if(mat%viewpoint_ctl%num .ge. 3) then
        view_param%iflag_viewpoint = 1
      end if
!
      do i = 1, mat%up_dir_ctl%num
        nd = set_3direction_flag(mat%up_dir_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
         view_param%up_direction_vec(nd) = mat%up_dir_ctl%vect(i)
      end do
      if(mat%up_dir_ctl%num .ge. 3) then
        view_param%iflag_updir = 1
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'iflag_lookpoint_vec', view_param%iflag_lookpoint
        write(*,*) 'lookat_vec', view_param%lookat_vec(1:3)
        write(*,*) 'iflag_viewpoint_vec', view_param%iflag_viewpoint
        write(*,*) 'viewpoint_vec', view_param%viewpoint_vec(1:3)
        write(*,*) 'iflag_updir_vec', view_param%iflag_updir
        write(*,*) 'up_direction_vec',                                  &
     &            view_param%up_direction_vec(1:3)
      end if
!
      end subroutine set_viewpoint_vector_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_view_rotation_vect_ctl(mat, view_param)
!
      use calypso_mpi
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: i, nd
!
!
      if(mat%view_rot_vec_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Rotaion of viewpoint vector should be 3 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      if (mat%i_view_rot_deg .gt. 0) then
        view_param%rotation_pvr(1) = mat%view_rotation_deg_ctl
      else
        view_param%rotation_pvr(1) = 0.0d0
      end if
!
      do i = 1, mat%view_rot_vec_ctl%num
        nd = set_3direction_flag(mat%view_rot_vec_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%rotation_pvr(nd+1) = mat%view_rot_vec_ctl%vect(i)
      end do
      if(mat%view_rot_vec_ctl%num .ge. 4) then
        view_param%iflag_rotation = 1
      end if
!
      end subroutine set_view_rotation_vect_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_view_scale_factor_ctl(mat, view_param)
!
      use calypso_mpi
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
      integer(kind = kint) :: i, nd
!
!
      if(mat%scale_vector_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Scale factor vector should be 3 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      do i = 1, mat%scale_vector_ctl%num
        nd = set_3direction_flag(mat%scale_vector_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%scale_factor_pvr(nd) = mat%scale_vector_ctl%vect(i)
      end do
      if (mat%scale_vector_ctl%num .ge. 3) then
        view_param%iflag_scale_fact = 1
      end if
!
      end subroutine set_view_scale_factor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_viewpnt_in_viewer_ctl(mat, view_param)
!
      use calypso_mpi
      use skip_comment_f
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: i, nd
!
!
      if(mat%viewpt_in_viewer_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Viewpoint in viewer should be 3 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      do i = 1, mat%viewpt_in_viewer_ctl%num
        nd = set_3direction_flag(mat%viewpt_in_viewer_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%viewpt_in_viewer_pvr(nd)                             &
     &      = mat%viewpt_in_viewer_ctl%vect(i)
      end do
      if (mat%viewpt_in_viewer_ctl%num .ge. 3)  then
        view_param%iflag_viewpt_in_view = 1
      end if
!
      view_param%lookat_vec(1:2) = view_param%lookat_vec(1:2)           &
     &                         - view_param%viewpt_in_viewer_pvr(1:2)
!
      end subroutine set_viewpnt_in_viewer_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer function set_4direction_flag(dir_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: dir_ctl
!
!
      if     (cmp_no_case(dir_ctl,'x') .or. dir_ctl.eq.'1') then
        set_4direction_flag = 1
      else if(cmp_no_case(dir_ctl,'y') .or. dir_ctl.eq.'2') then
        set_4direction_flag = 2
      else if(cmp_no_case(dir_ctl,'z') .or. dir_ctl.eq.'3') then
        set_4direction_flag = 3
      else if(cmp_no_case(dir_ctl,'w') .or. dir_ctl.eq.'4') then
        set_4direction_flag = 4
      else
        set_4direction_flag = 0
      end if
!
      end function set_4direction_flag
!
! -----------------------------------------------------------------------
!
      integer function set_3direction_flag(dir_ctl)
!
      character(len = kchara), intent(in) :: dir_ctl
!
!
      set_3direction_flag = set_4direction_flag(dir_ctl)
      if(set_4direction_flag(dir_ctl) .eq. 4) set_3direction_flag = 0
!
      end function set_3direction_flag
!
! -----------------------------------------------------------------------
!
      end module set_pvr_modelview_matrix
