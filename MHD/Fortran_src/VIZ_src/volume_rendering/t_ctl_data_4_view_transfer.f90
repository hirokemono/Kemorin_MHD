!>@file   t_ctl_data_4_view_transfer.f90
!!@brief  module t_ctl_data_4_view_transfer
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine dealloc_view_transfer_ctl(mat)
!!        type(modeview_ctl), intent(inout) :: mat
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
      module t_ctl_data_4_view_transfer
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_elements
      use t_control_array_charareal
      use t_control_array_chara2real
      use skip_comment_f
!
      implicit  none
!
!
!>    Structure for modelview marices
      type modeview_ctl
!
!>    Structure for number of horizontal pixels
        type(read_integer_item) :: num_xpixel_ctl
!>    Structure for number of vertical pixels
        type(read_integer_item) :: num_ypixel_ctl
!
!>    Structure for opacity controls
!!@n      modelview_mat_ctl%c1_tbl:  1st component name for matrix
!!@n      modelview_mat_ctl%c2_tbl:  2nd component name for matrix
!!@n      modelview_mat_ctl%vect:    Modelview matrix
        type(ctl_array_c2r) :: modelview_mat_ctl
!
!>    Structure for perspective view angle
        type(read_real_item) :: perspective_angle_ctl
!>    Structure for aspect ration of screen
        type(read_real_item) :: perspective_xy_ratio_ctl
!>    Structure for Near point of view
        type(read_real_item) :: perspective_near_ctl
!>    Structure for Far point of view
        type(read_real_item) :: perspective_far_ctl
!
!>    Structure for focal point
        type(read_real_item) :: focalpoint_ctl
!>    Structure for eye separation
        type(read_real_item) :: eye_separation_ctl
!
!
!>      Structure for look at  controls
!!@n      lookpoint_ctl%c_tbl:   component of lookpoint
!!@n      lookpoint_ctl%vect:    Position of lookpoint
        type(ctl_array_cr) :: lookpoint_ctl
!
!>      Structure for viewpoint controls
!!@n      viewpoint_ctl%c_tbl:   Direction of viewpoint
!!@n      viewpoint_ctl%vect:    Position of viewpoint
        type(ctl_array_cr) :: viewpoint_ctl
!
!>      Structure for Up-directions controls
!!@n      up_dir_ctl%c_tbl:   Direction of  Up-directions
!!@n      up_dir_ctl%vect:    Position of  Up-directions
        type(ctl_array_cr) :: up_dir_ctl
!
!>      Structure for rotation of object
!!@n      view_rot_vec_ctl%c_tbl:   Direction of rotatin vector
!!@n      view_rot_vec_ctl%vect:    rotation vector
        type(ctl_array_cr) :: view_rot_vec_ctl
!
!>      Structure for rotation of rotatin angle of view
        type(read_real_item) :: view_rotation_deg_ctl
!>      Structure for scale factor
        type(read_real_item) :: scale_factor_ctl
!
!>      Structure for scale factor controls
!!@n      scale_vector_ctl%c_tbl:   Direction of scale factor
!!@n      scale_vector_ctl%vect:    Position of scale factor
        type(ctl_array_cr) :: scale_vector_ctl
!
!>      Structure for viewpoint in viewer controls
!!@n      viewpt_in_viewer_ctl%c_tbl:   Direction of viewpoint in viewer
!!@n      viewpt_in_viewer_ctl%vect:    Position of viewpoint in viewer
        type(ctl_array_cr) :: viewpt_in_viewer_ctl
!
!
!   entry label for this block
        integer (kind=kint) :: i_view_transform = 0
!
!     3rd level for view_transform_define
        integer (kind=kint) :: i_image_size =  0
        integer (kind=kint) :: i_project_mat = 0
!
        integer (kind=kint) :: i_stereo_view = 0
      end type modeview_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_view_transfer_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      mat%i_view_transform = 0
!
      call dealloc_control_array_c2_r(mat%modelview_mat_ctl)
      mat%modelview_mat_ctl%num =    0
      mat%modelview_mat_ctl%icou =   0
!
      call dealloc_control_array_c_r(mat%lookpoint_ctl)
      call dealloc_control_array_c_r(mat%viewpoint_ctl)
      call dealloc_control_array_c_r(mat%up_dir_ctl)
      mat%lookpoint_ctl%num =   0
      mat%viewpoint_ctl%num =   0
      mat%up_dir_ctl%num =      0
      mat%lookpoint_ctl%icou =  0
      mat%viewpoint_ctl%icou =  0
      mat%up_dir_ctl%icou =     0
!
      call dealloc_control_array_c_r(mat%view_rot_vec_ctl)
      call dealloc_control_array_c_r(mat%scale_vector_ctl)
      call dealloc_control_array_c_r(mat%viewpt_in_viewer_ctl)
      mat%view_rot_vec_ctl%num =     0
      mat%scale_vector_ctl%num =     0
      mat%viewpt_in_viewer_ctl%num = 0
      mat%view_rot_vec_ctl%icou =     0
      mat%scale_vector_ctl%icou =     0
      mat%viewpt_in_viewer_ctl%icou = 0
!
!
      mat%perspective_angle_ctl%realvalue =    0.0d0
      mat%perspective_xy_ratio_ctl%realvalue = 0.0d0
      mat%perspective_near_ctl%realvalue =     0.0d0
      mat%perspective_far_ctl%realvalue =      0.0d0
!
      mat%perspective_far_ctl%realvalue =      0.0d0
!
      mat%focalpoint_ctl%realvalue =    0.0d0
      mat%eye_separation_ctl%realvalue = 0.0d0
!
      mat%view_rotation_deg_ctl%realvalue =      0.0d0
      mat%scale_factor_ctl%realvalue =           1.0d0
!
!
      mat%i_project_mat = 0
!
!
      mat%view_rotation_deg_ctl%iflag = 0
      mat%scale_factor_ctl%iflag = 0
!
      mat%i_image_size =  0
      mat%i_stereo_view = 0
!
      end subroutine dealloc_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_4_view_transfer
