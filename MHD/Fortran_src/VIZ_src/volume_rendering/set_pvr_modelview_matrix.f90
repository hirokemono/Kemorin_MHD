!
!set_pvr_modelview_matrix.f90
!      module set_pvr_modelview_matrix
!
!        programmed by H.Matsui on May. 2009
!
!      subroutine set_pvr_projection_matrix(i_pvr, mat)
!      subroutine s_set_pvr_modelview_matrix(i_pvr, mat)
!
      module set_pvr_modelview_matrix
!
      use m_precision
!
      use m_constants
      use m_ctl_data_4_view_transfer
      use m_control_params_4_pvr
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
      subroutine s_set_pvr_modelview_matrix(i_pvr, mat)
!
      type(modeview_ctl), intent(inout) :: mat
      integer(kind = kint), intent(in) :: i_pvr
!
!
      call copy_pvr_image_size(i_pvr, mat)
      call copy_pvr_perspective_matrix(i_pvr, mat)
!
      if (mat%modelview_mat_ctl%num .gt. 0) then
        call copy_pvr_modelview_matrix(i_pvr, mat)
        call dealloc_control_array_c2_r(mat%modelview_mat_ctl)
      else
        call set_viewpoint_vector_ctl(i_pvr, mat)
        call dealloc_control_array_c_r(mat%lookpoint_ctl)
        call dealloc_control_array_c_r(mat%viewpoint_ctl)
        call dealloc_control_array_c_r(mat%up_dir_ctl)
      end if
!
      if(mat%i_view_rot_deg.gt.0                                        &
     &    .and. mat%view_rot_vec_ctl%num.ge.3) then
        call set_view_rotation_vect_ctl(i_pvr, mat)
        call dealloc_control_array_c_r(mat%view_rot_vec_ctl)
      end if
!
      if(mat%i_scale_factor .gt. 0) then
        scale_factor_pvr(1:3,i_pvr) = mat%scale_factor_ctl
        iflag_scale_fact_pvr(i_pvr) = 1
      else if(mat%scale_vector_ctl%num .ge. 3) then
        call set_view_scale_factor_ctl(i_pvr, mat)
        call dealloc_control_array_c_r(mat%scale_vector_ctl)
      end if
!
      if(mat%viewpt_in_viewer_ctl%num .ge. 3) then
        call set_viewpnt_in_viewer_ctl(i_pvr, mat)
        call dealloc_control_array_c_r(mat%viewpt_in_viewer_ctl)
      end if
!
      end subroutine s_set_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_image_size(i_pvr, mat)
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
!
!
      if (mat%i_x_pixel .gt. 0) then
        n_pvr_pixel(1,i_pvr) = mat%num_pixel_ctl(1)
      else
        n_pvr_pixel(1,i_pvr) = 640
      end if
!
      if (mat%i_y_pixel .gt. 0) then
        n_pvr_pixel(2,i_pvr) = mat%num_pixel_ctl(2)
      else
        n_pvr_pixel(2,i_pvr) = 480
      end if
!
      end subroutine copy_pvr_image_size
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_perspective_matrix(i_pvr, mat)
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
!
!
      if (mat%i_perspect_angle .gt. 0) then
        perspective_angle(i_pvr) =    mat%perspective_angle_ctl
      else
        perspective_angle(i_pvr) =    10.0d0
      end if
!
      if (mat%i_perspect_xy .gt. 0) then
        perspective_xy_ratio(i_pvr) = mat%perspective_xy_ratio_ctl
      else
        perspective_xy_ratio(i_pvr) = one
      end if
!
      if (mat%i_perspect_near .gt. 0) then
        perspective_near(i_pvr) =     mat%perspective_near_ctl
      else
        perspective_near(i_pvr) = 0.1d0
      end if
!
      if (mat%i_perspect_far .gt. 0) then
        perspective_far(i_pvr) =      mat%perspective_far_ctl
      else
        perspective_far(i_pvr) = 1.0d2
      end if
!
      iflag_perspective_mat(i_pvr)                                      &
     &      = mat%i_perspect_angle*mat%i_perspect_xy                    &
     &       *mat%i_perspect_near*mat%i_perspect_far
!
!
      if (mat%i_focalpoint .gt. 0) then
        focalLength(i_pvr) =      mat%focalpoint_ctl
      else
        focalLength(i_pvr) = 1.0d1
      end if
!
      if (mat%i_eye_separation .gt. 0) then
        eye_separation(i_pvr) = mat%eye_separation_ctl
      else
        eye_separation(i_pvr) = 1.0d-1
      end if
!
      iflag_stereo_pvr(i_pvr)                                           &
     &      = mat%i_focalpoint*mat%i_eye_separation
!
      end subroutine copy_pvr_perspective_matrix
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_modelview_matrix(i_pvr, mat)
!
      use calypso_mpi
      use skip_comment_f
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i, nd1, nd2, j
!
!
      if(mat%modelview_mat_ctl%num .ne. 16) then
        write(e_message,'(a)')                                          &
     &     'Modelview  Matrix should be 16 components'
        call calypso_MPI_abort(10, e_message)
      end if
!
      do i = 1, mat%modelview_mat_ctl%num
        nd1 = set_4direction_flag(mat%modelview_mat_ctl%c1_tbl(i))
        nd2 = set_4direction_flag(mat%modelview_mat_ctl%c2_tbl(i))
!
        if(nd1*nd2 .gt. 0) then
          j = nd2 + 4*(nd1-1)
          modelview_mat(j,i_pvr) = mat%modelview_mat_ctl%vect(i)
        end if
      end do
!
      iflag_modelview_mat(i_pvr) = 1
!
      end subroutine copy_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
!
      subroutine set_viewpoint_vector_ctl(i_pvr, mat)
!
      use calypso_mpi
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i, nd
!
!
      if(mat%lookpoint_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Lookatpoint vector should be 3 components'
        call calypso_MPI_abort(10, e_message)
      end if
      if(mat%viewpoint_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Viewpoint vector should be 3 components'
        call calypso_MPI_abort(10, e_message)
      end if
      if(mat%up_dir_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Up-direction vector should be 3 components'
        call calypso_MPI_abort(10, e_message)
      end if
!
      do i = 1, mat%lookpoint_ctl%num
        nd = set_3direction_flag(mat%lookpoint_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        lookat_vec(nd,i_pvr) = mat%lookpoint_ctl%vect(i)
      end do
      if(mat%lookpoint_ctl%num .ge. 3) iflag_lookpoint_vec(i_pvr) = 1
!
      do i = 1, mat%viewpoint_ctl%num
        nd = set_3direction_flag(mat%viewpoint_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        viewpoint_vec(nd,i_pvr) = mat%viewpoint_ctl%vect(i)
      end do
      if(mat%viewpoint_ctl%num .ge. 3) iflag_viewpoint_vec(i_pvr) = 1
!
      do i = 1, mat%up_dir_ctl%num
        nd = set_3direction_flag(mat%up_dir_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        up_direction_vec(nd,i_pvr) = mat%up_dir_ctl%vect(i)
      end do
      if (mat%up_dir_ctl%num .ge. 3) iflag_updir_vec(i_pvr) = 1
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'iflag_lookpoint_vec', iflag_lookpoint_vec(i_pvr)
        write(*,*) 'lookat_vec', lookat_vec(1:3,i_pvr)
        write(*,*) 'iflag_viewpoint_vec', iflag_viewpoint_vec(i_pvr)
        write(*,*) 'viewpoint_vec', viewpoint_vec(1:3,i_pvr)
        write(*,*) 'iflag_updir_vec', iflag_updir_vec(i_pvr)
        write(*,*) 'up_direction_vec', up_direction_vec(1:3,i_pvr)
      end if
!
      end subroutine set_viewpoint_vector_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_view_rotation_vect_ctl(i_pvr, mat)
!
      use calypso_mpi
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i, nd
!
!
      if(mat%view_rot_vec_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Rotaion of viewpoint vector should be 3 components'
        call calypso_MPI_abort(10, e_message)
      end if
!
      if (mat%i_view_rot_deg .gt. 0) then
        rotation_pvr(1,i_pvr) = mat%view_rotation_deg_ctl
      else
        rotation_pvr(1,i_pvr) = 0.0d0
      end if
!
      do i = 1, mat%view_rot_vec_ctl%num
        nd = set_3direction_flag(mat%view_rot_vec_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        rotation_pvr(nd+1,i_pvr) = mat%view_rot_vec_ctl%vect(i)
      end do
      if(mat%view_rot_vec_ctl%num .ge. 3) iflag_rotation_pvr(i_pvr) = 1
!
      end subroutine set_view_rotation_vect_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_view_scale_factor_ctl(i_pvr, mat)
!
      use calypso_mpi
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i, nd
!
!
      if(mat%scale_vector_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Scale factor vector should be 3 components'
        call calypso_MPI_abort(10, e_message)
      end if
!
      do i = 1, mat%scale_vector_ctl%num
        nd = set_3direction_flag(mat%scale_vector_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        scale_factor_pvr(nd,i_pvr) = mat%scale_vector_ctl%vect(i)
      end do
      if (mat%scale_vector_ctl%num .ge. 3) then
        iflag_scale_fact_pvr(i_pvr) = 1
      end if
!
      end subroutine set_view_scale_factor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_viewpnt_in_viewer_ctl(i_pvr, mat)
!
      use calypso_mpi
      use skip_comment_f
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i, nd
!
!
      if(mat%viewpt_in_viewer_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Viewpoint in viewer should be 3 components'
        call calypso_MPI_abort(10, e_message)
      end if
!
      do i = 1, mat%viewpt_in_viewer_ctl%num
        nd = set_3direction_flag(mat%viewpt_in_viewer_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        viewpt_in_viewer_pvr(nd,i_pvr)                                  &
     &      = mat%viewpt_in_viewer_ctl%vect(i)
      end do
      if (mat%viewpt_in_viewer_ctl%num .ge. 3)                          &
     &        iflag_viewpt_in_view_pvr(i_pvr) = 1
!
      lookat_vec(1:2,i_pvr) = lookat_vec(1:2,i_pvr)                     &
     &                       - viewpt_in_viewer_pvr(1:2,i_pvr)
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
      if     (cmp_no_case(dir_ctl,'x').gt.0 .or. dir_ctl.eq.'1') then
        set_4direction_flag = 1
      else if(cmp_no_case(dir_ctl,'y').gt.0 .or. dir_ctl.eq.'2') then
        set_4direction_flag = 2
      else if(cmp_no_case(dir_ctl,'z').gt.0 .or. dir_ctl.eq.'3') then
        set_4direction_flag = 3
      else if(cmp_no_case(dir_ctl,'w').gt.0 .or. dir_ctl.eq.'4') then
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
