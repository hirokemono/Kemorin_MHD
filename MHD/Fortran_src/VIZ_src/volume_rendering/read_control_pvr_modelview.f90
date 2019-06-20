!>@file   read_control_pvr_modelview.f90
!!@brief  module read_control_pvr_modelview
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine read_control_modelview_file                          &
!!     &         (id_control, viewctl_file_name, mat)
!!      subroutine read_view_transfer_ctl                               &
!!     &         (id_control, hd_block, mat, c_buf)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Input example
!
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
!!
!!    begin streo_view_parameter_ctl
!!      focal_point_ctl           40.0
!!      eye_separation_ctl        0.5
!!    end streo_view_parameter_ctl
!!
!!  end view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module read_control_pvr_modelview
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_ctl_data_4_view_transfer
      use t_read_control_elements
      use t_control_elements
      use t_control_array_charareal
      use t_control_array_chara2real
      use skip_comment_f
!
      implicit  none
!
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
      private :: hd_viewpt_in_view, hd_stereo_view
      private :: hd_focalpoint,  hd_eye_separation
      private :: hd_view_rot_deg, hd_view_rot_dir, hd_scale_fac_dir
!
      private :: read_projection_mat_ctl
      private :: read_stereo_view_ctl
      private :: read_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_modelview_file                            &
     &         (id_control, viewctl_file_name, mat)
!
      use calypso_mpi
      use m_error_IDs
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: viewctl_file_name
      type(modeview_ctl), intent(inout) :: mat
!
      type(buffer_for_control) :: c_buf1
!
!
      if(viewctl_file_name .eq. 'NO_FILE') then
        write(*,*)  'Modelview control is included'
        return
      end if
!
      write(*,*) 'Modelview control:', trim(viewctl_file_name)
!
      open(id_control, file = viewctl_file_name, status='old')
!
      call load_one_line_from_control(id_control, c_buf1)
      if(check_begin_flag(c_buf1, hd_view_transform)) then
        call read_view_transfer_ctl(id_control, hd_view_transform,      &
     &      mat, c_buf1)
      else
        call calypso_mpi_abort(ierr_PVR, 'Set view matrix file')
      end if
!
      close(id_control)
!
      end subroutine read_control_modelview_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_view_transfer_ctl                                 &
     &         (id_control, hd_block, mat, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(modeview_ctl), intent(inout) :: mat
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if (mat%i_view_transform .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_projection_mat_ctl                                    &
     &     (id_control, hd_project_mat, mat, c_buf)
        call read_image_size_ctl                                        &
     &     (id_control, hd_image_size, mat, c_buf)
        call read_stereo_view_ctl                                       &
     &     (id_control, hd_stereo_view, mat, c_buf)
!
!
        call read_control_array_c_r(id_control,                         &
     &      hd_look_point, mat%lookpoint_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_view_point, mat%viewpoint_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_up_dir, mat%up_dir_ctl, c_buf)
!
        call read_control_array_c_r(id_control,                         &
     &      hd_view_rot_dir, mat%view_rot_vec_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_scale_fac_dir, mat%scale_vector_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_viewpt_in_view, mat%viewpt_in_viewer_ctl, c_buf)
!
        call read_control_array_c2_r(id_control,                        &
     &      hd_model_mat, mat%modelview_mat_ctl, c_buf)
!
        call read_real_ctl_type(c_buf, hd_view_rot_deg,                 &
     &        mat%view_rotation_deg_ctl)
        call read_real_ctl_type(c_buf, hd_scale_factor,                 &
     &        mat%scale_factor_ctl)
      end do
      mat%i_view_transform = 1
!
      end subroutine read_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_projection_mat_ctl                                &
     &         (id_control, hd_block, mat, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(modeview_ctl), intent(inout) :: mat
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (mat%i_project_mat.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_perspect_angle,               &
     &      mat%perspective_angle_ctl)
        call read_real_ctl_type(c_buf, hd_perspect_xy,                  &
     &      mat%perspective_xy_ratio_ctl)
        call read_real_ctl_type(c_buf, hd_perspect_near,                &
     &      mat%perspective_near_ctl)
        call read_real_ctl_type(c_buf, hd_perspect_far,                 &
     &      mat%perspective_far_ctl)
      end do
      mat%i_project_mat = 1
!
      end subroutine read_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_image_size_ctl(id_control, hd_block, mat, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(modeview_ctl), intent(inout) :: mat
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (mat%i_image_size.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_x_pixel, mat%num_xpixel_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_y_pixel, mat%num_ypixel_ctl)
      end do
      mat%i_image_size = 1
!
      end subroutine read_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_stereo_view_ctl(id_control, hd_block, mat, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(modeview_ctl), intent(inout) :: mat
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (mat%i_stereo_view.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_focalpoint,                   &
     &      mat%focalpoint_ctl)
        call read_real_ctl_type(c_buf, hd_eye_separation,               &
     &      mat%eye_separation_ctl)
      end do
      mat%i_stereo_view = 1
!
      end subroutine read_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      end module read_control_pvr_modelview
