!m_ctl_data_4_view_transfer.f90
!      module m_ctl_data_4_view_transfer
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine read_view_transfer_ctl(mat, ierr)
!      subroutine reset_view_transfer_ctl(mat)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  example
!!  begin view_transform_ctl
!!
!!     begin image_size_ctl
!!       x_pixel_ctl  640
!!       y_pixel_ctl  480
!!     end
!!
!!    array look_at_point_ctl   3
!!      look_at_point_ctl  x      3.0
!!      look_at_point_ctl  y     -8.0
!!      look_at_point_ctl  z      6.0 
!!    end  array look_at_point_ctl
!!
!!    array viewpoint_ctl    3
!!      viewpoint_ctl  x      3.0
!!      viewpoint_ctl  y     -8.0
!!      viewpoint_ctl  z      6.0 
!!    end array viewpoint_ctl
!!
!!    array up_direction_ctl    3
!!      up_direction_ctl  x      0.0
!!      up_direction_ctl  y      0.0
!!      up_direction_ctl  z      1.0
!!    end array up_direction_ctl
!!
!!    array view_rotation_vec_ctl      3
!!      view_rotation_vec_ctl  x      0.0
!!      view_rotation_vec_ctl  y      0.0
!!      view_rotation_vec_ctl  z      1.0
!!    end array view_rotation_vec_ctl
!!
!!    view_rotation_deg_ctl    60.0
!!
!!    scale_factor_ctl            1.0
!!    array scale_factor_vec_ctl       3
!!      scale_factor_vec_ctl  x      0.0
!!      scale_factor_vec_ctl  y      0.0
!!      scale_factor_vec_ctl  z      1.0
!!    end array scale_factor_vec_ctl
!!
!!    array viewpoint_in_viewer_ctl   3
!!      viewpoint_in_viewer_ctl  x      0.0
!!      viewpoint_in_viewer_ctl  y      0.0
!!      viewpoint_in_viewer_ctl  z      10.0
!!    end array viewpoint_in_viewer_ctl
!!
!!    array  modelview_matrix_ctl  16
!!      modelview_matrix_ctl   1  1  1.0  end
!!      modelview_matrix_ctl   2  1  0.0  end
!!      modelview_matrix_ctl   3  1  0.0  end
!!      modelview_matrix_ctl   4  1  0.0  end
!!
!!      modelview_matrix_ctl   1  2  0.0  end
!!      modelview_matrix_ctl   2  2  1.0  end
!!      modelview_matrix_ctl   3  2  0.0  end
!!      modelview_matrix_ctl   4  2  0.0  end
!!
!!      modelview_matrix_ctl   1  3  0.0  end
!!      modelview_matrix_ctl   2  3  0.0  end
!!      modelview_matrix_ctl   3  3  1.0  end
!!      modelview_matrix_ctl   4  3  0.0  end
!!
!!      modelview_matrix_ctl   1  4  0.0  end
!!      modelview_matrix_ctl   2  4  0.0  end
!!      modelview_matrix_ctl   3  4  0.0  end
!!      modelview_matrix_ctl   4  4  1.0  end
!!    end array modelview_matrix_ctl
!!
!!    Orthogonal view....( perspective_near_ctl = perspective_far_ctl)
!!
!!    begin projection_matrix_ctl
!!      perspective_angle_ctl     10.0
!!      perspective_xy_ratio_ctl   1.0
!!      perspective_near_ctl       0.5
!!      perspective_far_ctl     1000.0
!!    end projection_matrix_ctl
!!  end view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module m_ctl_data_4_view_transfer
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      type modeview_ctl
        integer(kind = kint) :: num_pixel_ctl(2)
!
        integer(kind = kint) :: num_modelview_mat_ctl
        character(len=kchara) :: modelview_dir_ctl(16,2)
        real(kind = kreal) :: modelview_mat_ctl(16)
!
        real(kind = kreal) :: perspective_angle_ctl
        real(kind = kreal) :: perspective_xy_ratio_ctl
        real(kind = kreal) :: perspective_near_ctl
        real(kind = kreal) :: perspective_far_ctl
!
        real(kind = kreal) :: focalpoint_ctl
        real(kind = kreal) :: eye_separation_ctl
!
!
        integer(kind = kint) :: num_lookpoint_ctl
        character(len=kchara) :: lookpoint_dir_ctl(3)
        real(kind = kreal) :: lookpoint_ctl(3) = 0.0d0
!
        integer(kind = kint) :: num_viewpoint_ctl
        character(len=kchara) :: viewpoint_dir_ctl(3)
        real(kind = kreal) :: viewpoint_ctl(3) = 0.0d0
!
        integer(kind = kint) :: num_up_direction_ctl
        character(len=kchara) :: up_direction_dir_ctl(3)
        real(kind = kreal) :: up_direction_ctl(3) = 0.0d0
!
        integer(kind = kint) :: num_view_rot_dir_ctl
        character(len=kchara) :: view_rotation_dir_ctl(3)
        real(kind = kreal) :: view_rotation_vec_ctl(3) = 0.0d0
!
        real(kind = kreal) :: view_rotation_deg_ctl = 0.0d0
        real(kind = kreal) :: scale_factor_ctl =      1.0d0
!
        integer(kind = kint) :: num_scale_factor_ctl
        character(len=kchara) :: scale_factor_dir_ctl(3)
        real(kind = kreal) :: scale_factor_vec_ctl(3) = 0.0d0
!
        integer(kind = kint) :: num_viewpt_in_viewer_ctl
        character(len=kchara) :: viewpoint_in_view_dir_ctl(3)
        real(kind = kreal) :: viewpoint_in_viewer_ctl(3)
!
!
!   entry label for this block
        integer (kind=kint) :: i_view_transform = 0
!
!     3rd level for view_transform_define
        integer (kind=kint) :: i_image_size =  0
        integer (kind=kint) :: i_model_mat =   0
        integer (kind=kint) :: i_project_mat = 0
!
        integer (kind=kint) :: i_stereo_view = 0
!
        integer (kind=kint) :: i_look_point =     0
        integer (kind=kint) :: i_view_point =     0
        integer (kind=kint) :: i_up_dir =         0
        integer (kind=kint) :: i_view_rot_deg =   0
        integer (kind=kint) :: i_view_rot_dir =   0
        integer (kind=kint) :: i_scale_factor =   0
        integer (kind=kint) :: i_scale_fac_dir =  0
        integer (kind=kint) :: i_viewpt_in_view = 0
!
!     4th level for projection_matrix
        integer (kind=kint) :: i_perspect_angle =  0
        integer (kind=kint) :: i_perspect_xy =     0
        integer (kind=kint) :: i_perspect_near =   0
        integer (kind=kint) :: i_perspect_far =    0
!
!     4th level for image size
        integer (kind=kint) :: i_x_pixel = 0
        integer (kind=kint) :: i_y_pixel = 0
!
!     4th level for stereo view
        integer (kind=kint) :: i_focalpoint =     0
        integer (kind=kint) :: i_eye_separation = 0
      end type modeview_ctl
!
!
!   entry label for this block
!
      character(len=kchara) :: hd_view_transform = 'view_transform_ctl'
!
!     3rd level for view_transform_define
      character(len=kchara) :: hd_image_size =    'image_size_ctl'
      character(len=kchara) :: hd_model_mat =   'modelview_matrix_ctl'
      character(len=kchara) :: hd_project_mat = 'projection_matrix_ctl'
!
      character(len=kchara) :: hd_look_point =  'look_at_point_ctl'
      character(len=kchara) :: hd_view_point =  'viewpoint_ctl'
      character(len=kchara) :: hd_up_dir =      'up_direction_ctl'
      character(len=kchara) :: hd_up_dir_comp = 'vector_comp_ctl'
!
!
      character(len=kchara) :: hd_view_rot_deg                          &
     &                        = 'view_rotation_deg_ctl'
      character(len=kchara) :: hd_view_rot_dir                          &
     &                        = 'view_rotation_vec_ctl'
!
      character(len=kchara) :: hd_scale_factor                          &
     &                             = 'scale_factor_ctl'
      character(len=kchara) :: hd_scale_fac_dir                         &
     &                        = 'scale_factor_vec_ctl'
      character(len=kchara) :: hd_viewpt_in_view                        &
     &                        = 'viewpoint_in_viewer_ctl'
!
      character(len=kchara) :: hd_stereo_view                           &
     &                        = 'streo_view_parameter_ctl'
!
!     4th level for projection_matrix
      character(len=kchara) :: hd_perspect_angle                        &
     &                        = 'perspective_angle_ctl'
      character(len=kchara) :: hd_perspect_xy =                         &
     &                        'perspective_xy_ratio_ctl'
      character(len=kchara) :: hd_perspect_near =                       &
     &                        'perspective_near_ctl'
      character(len=kchara) :: hd_perspect_far =                        &
     &                        'perspective_far_ctl'
!
!     4th level for image size
      character(len=kchara) :: hd_x_pixel = 'x_pixel_ctl'
      character(len=kchara) :: hd_y_pixel = 'y_pixel_ctl'
!
!     4th level for stereo view
      character(len=kchara) :: hd_focalpoint =     'focal_point_ctl'
      character(len=kchara) :: hd_eye_separation = 'eye_separation_ctl'
!
!
      private :: hd_view_transform
      private :: hd_x_pixel, hd_y_pixel
      private :: hd_project_mat, hd_view_point, hd_up_dir, hd_image_size
      private :: hd_model_mat
      private :: hd_perspect_angle, hd_perspect_xy
      private :: hd_perspect_near, hd_perspect_far
      private :: hd_up_dir_comp, hd_viewpt_in_view, hd_stereo_view
      private :: hd_focalpoint,  hd_eye_separation
      private :: hd_view_rot_deg, hd_view_rot_dir, hd_scale_fac_dir
!
      private :: read_projection_mat_ctl, read_stereo_view_ctl
      private :: read_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_view_transfer_ctl(mat, ierr)
!
      type(modeview_ctl), intent(inout) :: mat
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
!
      if(right_begin_flag(hd_view_transform) .eq. 0) return
      if (mat%i_view_transform .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_view_transform,                   &
     &      mat%i_view_transform)
        if(mat%i_view_transform .gt. 0) exit
!
        call read_projection_mat_ctl(mat)
        call read_image_size_ctl(mat)
        call read_stereo_view_ctl(mat)
!
!
        call find_control_array_flag(hd_look_point,                     &
     &      mat%num_lookpoint_ctl)
        if(mat%num_lookpoint_ctl.gt.0 .and. mat%i_look_point.eq.0) then
!
          if(mat%num_lookpoint_ctl .ne. 3) then
            ierr = 10
            write(e_message,'(a)')                                      &
     &          'lookpoint vector should have 3 components'
            return
          end if
!
          call read_control_array_vect_list(hd_look_point,              &
     &        mat%num_lookpoint_ctl, mat%i_look_point,                  &
     &        mat%lookpoint_dir_ctl, mat%lookpoint_ctl)
        end if
!
        call find_control_array_flag(hd_view_point,                     &
     &      mat%num_viewpoint_ctl)
        if(mat%num_viewpoint_ctl.gt.0 .and. mat%i_view_point.eq.0) then
!
          if(mat%num_viewpoint_ctl .ne. 3) then
            ierr = 10
            write(e_message,'(a)')                                      &
     &          'viewpoint vector should have 3 components'
            return
          end if
!
          call read_control_array_vect_list(hd_view_point,              &
     &        mat%num_viewpoint_ctl, mat%i_view_point,                  &
     &        mat%viewpoint_dir_ctl, mat%viewpoint_ctl)
        end if
!
        call find_control_array_flag(hd_up_dir,                         &
     &      mat%num_up_direction_ctl)
        if(mat%num_up_direction_ctl.gt.0 .and. mat%i_up_dir.eq.0) then
!
          if(mat%num_up_direction_ctl .ne. 3) then
            ierr = 10
            write(e_message,'(a)')                                      &
     &          'up-direction vector should have 3 components'
            return
          end if
!
          call read_control_array_vect_list(hd_up_dir,                  &
     &        mat%num_up_direction_ctl, mat%i_up_dir,                   &
     &        mat%up_direction_dir_ctl, mat%up_direction_ctl)
        end if
!
        call find_control_array_flag(hd_view_rot_dir,                   &
     &      mat%num_view_rot_dir_ctl)
        if(mat%num_view_rot_dir_ctl.gt.0                                &
     &      .and. mat%i_view_rot_dir.eq.0) then
!
          if(mat%num_view_rot_dir_ctl .ne. 3) then
            ierr = 10
            write(e_message,'(a)')                                      &
     &          'Rotation vector should have 3 components'
            return
          end if
!
          call read_control_array_vect_list(hd_view_rot_dir,            &
     &        mat%num_view_rot_dir_ctl, mat%i_view_rot_dir,             &
     &        mat%view_rotation_dir_ctl, mat%view_rotation_vec_ctl)
        end if
!
        call find_control_array_flag(hd_scale_fac_dir,                  &
     &       mat%num_scale_factor_ctl)
        if(mat%num_scale_factor_ctl.gt.0                                &
     &      .and. mat%i_scale_fac_dir.eq.0) then
!
          if(mat%num_scale_factor_ctl .ne. 3) then
            ierr = 10
            write(e_message,'(a)')                                      &
     &          'Scale factor should be 3 components'
            return
          end if
!
          call read_control_array_vect_list(hd_scale_fac_dir,           &
     &        mat%num_scale_factor_ctl, mat%i_scale_fac_dir,            &
     &        mat%scale_factor_dir_ctl, mat%scale_factor_vec_ctl)
        end if
!
        call find_control_array_flag(hd_viewpt_in_view,                 &
     &      mat%num_viewpt_in_viewer_ctl)
        if(mat%num_viewpt_in_viewer_ctl.gt.0                            &
     &      .and. mat%i_viewpt_in_view.eq.0) then
!
          if(mat%num_viewpt_in_viewer_ctl .ne. 3) then
            ierr = 10
            write(e_message,'(a)')                                      &
     &          'Viewpoint vector should be 3 components'
            return
          end if
!
          call read_control_array_vect_list(hd_viewpt_in_view,          &
     &        mat%num_viewpt_in_viewer_ctl, mat%i_viewpt_in_view,       &
     &        mat%viewpoint_in_view_dir_ctl,                            &
     &        mat%viewpoint_in_viewer_ctl)
        end if
!
        call find_control_array_flag(hd_model_mat,                      &
     &      mat%num_modelview_mat_ctl)
        if(mat%num_modelview_mat_ctl.gt.0                               &
     &      .and. mat%i_model_mat.eq.0) then
!
          if(mat%num_modelview_mat_ctl .ne. 16) then
            ierr = 10
            write(e_message,'(a)')                                      &
     &          'Modelview  Matrix should be 16 components'
            return
          end if
!
          call read_control_array_c2_r_list(hd_model_mat,               &
     &        mat%num_modelview_mat_ctl, mat%i_model_mat,               &
     &        mat%modelview_dir_ctl(1:16,1),                            &
     &        mat%modelview_dir_ctl(1:16,2), mat%modelview_mat_ctl)
        end if
!
        call read_real_ctl_item(hd_view_rot_deg,                        &
     &        mat%i_view_rot_deg,  mat%view_rotation_deg_ctl)
        call read_real_ctl_item(hd_scale_factor,                        &
     &        mat%i_scale_factor,  mat%scale_factor_ctl)
      end do
!
      end subroutine read_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_projection_mat_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      if(right_begin_flag(hd_project_mat) .eq. 0) return
      if (mat%i_project_mat.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_project_mat, mat%i_project_mat)
        if(mat%i_project_mat .gt. 0) exit
!
        call read_real_ctl_item(hd_perspect_angle,                      &
     &          mat%i_perspect_angle,  mat%perspective_angle_ctl )
        call read_real_ctl_item(hd_perspect_xy,                         &
     &          mat%i_perspect_xy,  mat%perspective_xy_ratio_ctl )
        call read_real_ctl_item(hd_perspect_near,                       &
     &          mat%i_perspect_near, mat%perspective_near_ctl )
        call read_real_ctl_item(hd_perspect_far,                        &
     &          mat%i_perspect_far, mat%perspective_far_ctl )
      end do
!
      end subroutine read_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_image_size_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      if(right_begin_flag(hd_image_size) .eq. 0) return
      if (mat%i_image_size.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_image_size, mat%i_image_size)
        if(mat%i_image_size .gt. 0) exit
!
        call read_integer_ctl_item(hd_x_pixel,                          &
     &        mat%i_x_pixel,  mat%num_pixel_ctl(1) )
        call read_integer_ctl_item(hd_y_pixel,                          &
     &        mat%i_y_pixel,  mat%num_pixel_ctl(2) )
      end do
!
      end subroutine read_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_stereo_view_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      if(right_begin_flag(hd_stereo_view) .eq. 0) return
      if (mat%i_stereo_view.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_stereo_view, mat%i_stereo_view)
        if(mat%i_stereo_view .gt. 0) exit
!
        call read_real_ctl_item(hd_focalpoint,                          &
     &        mat%i_focalpoint,  mat%focalpoint_ctl )
        call read_real_ctl_item(hd_eye_separation,                      &
     &        mat%i_eye_separation,  mat%eye_separation_ctl )
      end do
!
      end subroutine read_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_view_transfer_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      mat%i_view_transform = 0
!
      mat%num_modelview_mat_ctl = 0
      mat%num_lookpoint_ctl = 0
      mat%num_viewpoint_ctl = 0
      mat%num_up_direction_ctl = 0
      mat%num_view_rot_dir_ctl = 0
      mat%num_scale_factor_ctl = 0
      mat%num_viewpt_in_viewer_ctl = 0
!
      mat%modelview_mat_ctl(1:16) = 0.0d0
!
      mat%perspective_angle_ctl =    0.0d0
      mat%perspective_xy_ratio_ctl = 0.0d0
      mat%perspective_near_ctl =     0.0d0
      mat%perspective_far_ctl =      0.0d0
!
      mat%lookpoint_ctl(1:3) =    0.0d0
      mat%viewpoint_ctl(1:3) =    0.0d0
      mat%up_direction_ctl(1:3) = 0.0d0
!
      mat%view_rotation_deg_ctl =      0.0d0
      mat%view_rotation_vec_ctl(1:3) = 0.0d0
      mat%scale_factor_vec_ctl(1:3) =  0.0d0
      mat%scale_factor_ctl =           1.0d0
!
!
      mat%i_model_mat =   0
      mat%i_project_mat = 0
!
      mat%i_look_point =  0
      mat%i_view_point =  0
      mat%i_up_dir =      0
!
      mat%i_view_rot_dir =   0
      mat%i_scale_fac_dir =  0
      mat%i_viewpt_in_view = 0
!
      mat%i_view_rot_deg = 0
      mat%i_scale_factor = 0
!
      mat%i_image_size =  0
      mat%i_stereo_view = 0
!
      end subroutine reset_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_view_transfer
