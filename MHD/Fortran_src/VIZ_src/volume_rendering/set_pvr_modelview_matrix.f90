!
!set_pvr_modelview_matrix.f90
!      module set_pvr_modelview_matrix
!
      module set_pvr_modelview_matrix
!
!        programmed by H.Matsui on May. 2009
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
!      subroutine set_pvr_projection_matrix(i_pvr, mat)
!      subroutine s_set_pvr_modelview_matrix(i_pvr, mat)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_pvr_modelview_matrix(i_pvr, mat)
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
!
!
      call copy_pvr_image_size(i_pvr, mat)
      call copy_pvr_perspective_matrix(i_pvr, mat)
!
      if (mat%num_modelview_mat_ctl .eq. 16) then
        call copy_pvr_modelview_matrix(i_pvr, mat)
      else
        call set_viewpoint_vector_ctl(i_pvr, mat)
      end if
!
      if(mat%i_view_rot_deg.gt.0                                        &
     &    .and. mat%num_view_rot_dir_ctl.ge.3) then
        call set_view_rotation_vect_ctl(i_pvr, mat)
      end if
!
      if(mat%num_scale_factor_ctl .ge. 3) then
        call set_view_scale_factor_ctl(i_pvr, mat)
      end if
!
      if(mat%num_viewpt_in_viewer_ctl .ge. 3) then
        call set_viewpnt_in_viewer_ctl(i_pvr, mat)
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
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i
!
!
        do i = 1, mat%num_modelview_mat_ctl
!
          if     (mat%modelview_dir_ctl(i,1) .eq. 'x'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. 'X'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. '1') then
            if     (mat%modelview_dir_ctl(i,2) .eq. 'x'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'X'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '1') then
              modelview_mat(1,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'Y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '2') then
              modelview_mat(2,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '3') then
              modelview_mat(3,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'w'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'W'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '4') then
              modelview_mat(4,i_pvr) = mat%modelview_mat_ctl(i)
            end if
          else if(mat%modelview_dir_ctl(i,1) .eq. 'y'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. 'Y'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. '2') then
            if     (mat%modelview_dir_ctl(i,2) .eq. 'x'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'X'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '1') then
              modelview_mat(5,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'Y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '2') then
              modelview_mat(6,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '3') then
              modelview_mat(7,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'w'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'W'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '4') then
              modelview_mat(8,i_pvr) = mat%modelview_mat_ctl(i)
            end if
          else if(mat%modelview_dir_ctl(i,1) .eq. 'z'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. 'Z'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. '3') then
            if     (mat%modelview_dir_ctl(i,2) .eq. 'x'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'X'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '1') then
              modelview_mat(9, i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'Y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '2') then
              modelview_mat(10,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '3') then
              modelview_mat(11,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'w'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'W'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '4') then
              modelview_mat(12,i_pvr) = mat%modelview_mat_ctl(i)
            end if
          else if(mat%modelview_dir_ctl(i,1) .eq. 'w'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. 'Z'                   &
     &       .or. mat%modelview_dir_ctl(i,1) .eq. '4') then
            if     (mat%modelview_dir_ctl(i,2) .eq. 'x'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'X'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '1') then
              modelview_mat(13,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'Y'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '2') then
              modelview_mat(14,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'z'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. '3') then
              modelview_mat(15,i_pvr) = mat%modelview_mat_ctl(i)
            else if(mat%modelview_dir_ctl(i,2) .eq. 'w'                 &
     &         .or. mat%modelview_dir_ctl(i,2) .eq. 'W'                 &
     &         .or. mat%modelview_dir_ctl(2,i) .eq. '4') then
              modelview_mat(16,i_pvr) = mat%modelview_mat_ctl(i)
            end if
          end if
!
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
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i
!
!
        do i = 1, mat%num_lookpoint_ctl
          if     (mat%lookpoint_dir_ctl(i) .eq. 'x'                     &
     &       .or. mat%lookpoint_dir_ctl(i) .eq. 'X'                     &
     &       .or. mat%lookpoint_dir_ctl(i) .eq. '1') then
            lookat_vec(1,i_pvr) = mat%lookpoint_ctl(i)
          else if(mat%lookpoint_dir_ctl(i) .eq. 'y'                     &
     &       .or. mat%lookpoint_dir_ctl(i) .eq. 'Y'                     &
     &       .or. mat%lookpoint_dir_ctl(i) .eq. '2') then
            lookat_vec(2,i_pvr) = mat%lookpoint_ctl(i)
          else if(mat%lookpoint_dir_ctl(i) .eq. 'z'                     &
     &       .or. mat%lookpoint_dir_ctl(i) .eq. 'Z'                     &
     &       .or. mat%lookpoint_dir_ctl(i) .eq. '3') then
            lookat_vec(3,i_pvr) = mat%lookpoint_ctl(i)
          end if
        end do
        if(mat%num_lookpoint_ctl .ge. 3) iflag_lookpoint_vec(i_pvr) = 1
!
        do i = 1, mat%num_viewpoint_ctl
          if     (mat%viewpoint_dir_ctl(i) .eq. 'x'                     &
     &       .or. mat%viewpoint_dir_ctl(i) .eq. 'X'                     &
     &       .or. mat%viewpoint_dir_ctl(i) .eq. '1') then
            viewpoint_vec(1,i_pvr) = mat%viewpoint_ctl(i)
          else if(mat%viewpoint_dir_ctl(i) .eq. 'y'                     &
     &       .or. mat%viewpoint_dir_ctl(i) .eq. 'Y'                     &
     &       .or. mat%viewpoint_dir_ctl(i) .eq. '2') then
            viewpoint_vec(2,i_pvr) = mat%viewpoint_ctl(i)
          else if(mat%viewpoint_dir_ctl(i) .eq. 'z'                     &
     &       .or. mat%viewpoint_dir_ctl(i) .eq. 'Z'                     &
     &       .or. mat%viewpoint_dir_ctl(i) .eq. '3') then
            viewpoint_vec(3,i_pvr) = mat%viewpoint_ctl(i)
          end if
        end do
        if(mat%num_viewpoint_ctl .ge. 3) iflag_viewpoint_vec(i_pvr) = 1
!
        do i = 1, mat%num_up_direction_ctl
          if     (mat%up_direction_dir_ctl(i) .eq. 'x'                  &
     &       .or. mat%up_direction_dir_ctl(i) .eq. 'X'                  &
     &       .or. mat%up_direction_dir_ctl(i) .eq. '1') then
            up_direction_vec(1,i_pvr) = mat%up_direction_ctl(i)
          else if(mat%up_direction_dir_ctl(i) .eq. 'y'                  &
     &       .or. mat%up_direction_dir_ctl(i) .eq. 'Y'                  &
     &       .or. mat%up_direction_dir_ctl(i) .eq. '2') then
            up_direction_vec(2,i_pvr) = mat%up_direction_ctl(i)
          else if(mat%up_direction_dir_ctl(i) .eq. 'z'                  &
     &       .or. mat%up_direction_dir_ctl(i) .eq. 'Z'                  &
     &       .or. mat%up_direction_dir_ctl(i) .eq. '3') then
            up_direction_vec(3,i_pvr) = mat%up_direction_ctl(i)
          end if
        end do
        if (mat%num_up_direction_ctl .ge. 3) iflag_updir_vec(i_pvr) = 1
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
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i
!
!
      if (mat%i_view_rot_deg .gt. 0) then
        rotation_pvr(1,i_pvr) = mat%view_rotation_deg_ctl
      else
        rotation_pvr(1,i_pvr) = 0.0d0
      end if
!
      do i = 1, mat%num_view_rot_dir_ctl
        if     (mat%view_rotation_dir_ctl(i) .eq. 'x'                   &
     &     .or. mat%view_rotation_dir_ctl(i) .eq. 'X'                   &
     &     .or. mat%view_rotation_dir_ctl(i) .eq. '1') then
            rotation_pvr(2,i_pvr) = mat%view_rotation_vec_ctl(i)
        else if(mat%view_rotation_dir_ctl(i) .eq. 'y'                   &
     &     .or. mat%view_rotation_dir_ctl(i) .eq. 'Y'                   &
     &     .or. mat%view_rotation_dir_ctl(i) .eq. '2') then
            rotation_pvr(3,i_pvr) = mat%view_rotation_vec_ctl(i)
        else if(mat%view_rotation_dir_ctl(i) .eq. 'z'                   &
     &     .or. mat%view_rotation_dir_ctl(i) .eq. 'Z'                   &
     &     .or. mat%view_rotation_dir_ctl(i) .eq. '3') then
          rotation_pvr(4,i_pvr) = mat%view_rotation_vec_ctl(i)
        end if
      end do
      if(mat%num_view_rot_dir_ctl .ge. 3) iflag_rotation_pvr(i_pvr) = 1
!
      end subroutine set_view_rotation_vect_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_view_scale_factor_ctl(i_pvr, mat)
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i
!
!
      do i = 1, mat%num_scale_factor_ctl
        if     (mat%scale_factor_dir_ctl(i) .eq. 'x'                    &
     &     .or. mat%scale_factor_dir_ctl(i) .eq. 'X'                    &
     &     .or. mat%scale_factor_dir_ctl(i) .eq. '1') then
            scale_factor_pvr(1,i_pvr) = mat%scale_factor_pvr_ctl(i)
        else if(mat%scale_factor_dir_ctl(i) .eq. 'y'                    &
     &     .or. mat%scale_factor_dir_ctl(i) .eq. 'Y'                    &
     &     .or. mat%scale_factor_dir_ctl(i) .eq. '2') then
          scale_factor_pvr(2,i_pvr) = mat%scale_factor_pvr_ctl(i)
        else if(mat%scale_factor_dir_ctl(i) .eq. 'z'                    &
     &     .or. mat%scale_factor_dir_ctl(i) .eq. 'Z'                    &
     &     .or. mat%scale_factor_dir_ctl(i) .eq. '3') then
          scale_factor_pvr(3,i_pvr) = mat%scale_factor_pvr_ctl(i)
        end if
      end do
      if (mat%num_scale_factor_ctl .ge. 3) then
        iflag_scale_fact_pvr(i_pvr) = 1
      end if
!
      end subroutine set_view_scale_factor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_viewpnt_in_viewer_ctl(i_pvr, mat)
!
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint) :: i
!
!
      do i = 1, mat%num_viewpt_in_viewer_ctl
       if     (mat%viewpoint_in_view_dir_ctl(i) .eq. 'x'                &
     &    .or. mat%viewpoint_in_view_dir_ctl(i) .eq. 'X'                &
     &    .or. mat%viewpoint_in_view_dir_ctl(i) .eq. '1') then
         viewpt_in_viewer_pvr(1,i_pvr) = mat%viewpoint_in_viewer_ctl(i)
       else if(mat%viewpoint_in_view_dir_ctl(i) .eq. 'y'                &
     &    .or. mat%viewpoint_in_view_dir_ctl(i) .eq. 'Y'                &
     &    .or. mat%viewpoint_in_view_dir_ctl(i) .eq. '2') then
         viewpt_in_viewer_pvr(2,i_pvr) = mat%viewpoint_in_viewer_ctl(i)
       else if(mat%viewpoint_in_view_dir_ctl(i) .eq. 'z'                &
     &    .or. mat%viewpoint_in_view_dir_ctl(i) .eq. 'Z'                &
     &    .or. mat%viewpoint_in_view_dir_ctl(i) .eq. '3') then
         viewpt_in_viewer_pvr(3,i_pvr) = mat%viewpoint_in_viewer_ctl(i)
       end if
      end do
      if (mat%num_viewpt_in_viewer_ctl .ge. 3)                          &
     &        iflag_viewpt_in_view_pvr(i_pvr) = 1
!
      lookat_vec(1:2,i_pvr) = lookat_vec(1:2,i_pvr)                     &
     &                       - viewpt_in_viewer_pvr(1:2,i_pvr)
!
      end subroutine set_viewpnt_in_viewer_ctl
!
! -----------------------------------------------------------------------
!
      end module set_pvr_modelview_matrix
