!>@file   set_control_pvr_movie.f90
!!@brief  module set_control_pvr_movie
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set each PVR parameters from control
!!
!!@verbatim
!!      subroutine s_set_control_pvr_movie                              &
!!     &         (movie_ctl, view_param, movie_def)
!!        type(pvr_movie_ctl), intent(in) :: movie_ctl
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(pvr_movie_parameter), intent(inout) :: movie_def
!!@endverbatim
!
      module set_control_pvr_movie
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_control_pvr_movie                                &
     &         (movie_ctl, view_param, movie_def)
!
      use t_control_data_pvr_movie
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use output_image_sel_4_png
      use skip_comment_f
!
      type(pvr_movie_ctl), intent(in) :: movie_ctl
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_movie_parameter), intent(inout) :: movie_def
!
      character(len = kchara) :: tmpchara
!
!
      view_param%iflag_movie_fmt = iflag_UNDEFINED
      if(movie_ctl%movie_format_ctl%iflag .gt. 0) then
        tmpchara = movie_ctl%movie_format_ctl%charavalue
        if(cmp_no_case(tmpchara, hd_PNG)) then
          view_param%iflag_movie_fmt = iflag_PNG
        else if(cmp_no_case(tmpchara, hd_BMP)) then
          view_param%iflag_movie_fmt = iflag_BMP
        else if(cmp_no_case(tmpchara, hd_QUILT_BMP)) then
          view_param%iflag_movie_fmt = iflag_QUILT_BMP
        else
          view_param%iflag_movie_fmt = iflag_UNDEFINED
        end if
      end if
!
      view_param%iflag_movie_mode = IFLAG_NO_MOVIE
      if(movie_ctl%movie_mode_ctl%iflag .gt. 0) then
        tmpchara = movie_ctl%movie_mode_ctl%charavalue
        if(cmp_no_case(tmpchara, FLAG_ROTATE_MOVIE)) then
          view_param%iflag_movie_mode = I_ROTATE_MOVIE
        else if(cmp_no_case(tmpchara, FLAG_ZOOM)) then
          view_param%iflag_movie_mode = I_ZOOM
        else if(cmp_no_case(tmpchara, FLAG_START_END_VIEW)) then
          view_param%iflag_movie_mode = I_START_END_VIEW
        else if(cmp_no_case(tmpchara, FLAG_LOOKINGLASS)) then
          view_param%iflag_movie_mode = I_LOOKINGLASS
        else if(cmp_no_case(tmpchara, FLAG_LIC_KERNEL)) then
          view_param%iflag_movie_mode = I_LIC_KERNEL
        else
          view_param%iflag_movie_mode = I_ROTATE_MOVIE
        end if
      end if
!
      if((view_param%iflag_movie_fmt .eq. iflag_QUILT_BMP)) then
        if(movie_ctl%quilt_row_column_ctl%iflag .eq. 0) then
          view_param%n_row =     1
          view_param%n_column =  movie_ctl%num_frames_ctl%intvalue
        else
          view_param%n_row = movie_ctl%quilt_row_column_ctl%intvalue(1)
          view_param%n_column                                           &
     &          = movie_ctl%quilt_row_column_ctl%intvalue(2)
        end if
        view_param%num_frame = view_param%n_row * view_param%n_column
      else
        if(movie_ctl%num_frames_ctl%iflag .eq. 0) then
          view_param%iflag_movie_mode = IFLAG_NO_MOVIE
        else
          view_param%num_frame = movie_ctl%num_frames_ctl%intvalue
        end if
      end if
!
      if(view_param%iflag_movie_mode .eq. I_ROTATE_MOVIE) then
!
        if(movie_ctl%rotation_axis_ctl%iflag .eq. 0) then
          view_param%iflag_movie_mode = IFLAG_NO_MOVIE
        else
          tmpchara = movie_ctl%rotation_axis_ctl%charavalue
          if     (cmp_no_case(tmpchara, 'x')) then
            movie_def%id_rot_axis = 1
          else if(cmp_no_case(tmpchara, 'y')) then
            movie_def%id_rot_axis = 2
          else if(cmp_no_case(tmpchara, 'z')) then
            movie_def%id_rot_axis = 3
          end if
        end if
        if(movie_ctl%angle_range_ctl%iflag .eq. 0) then
          movie_def%angle_range(1) =   0.0d0
          movie_def%angle_range(2) = 360.0d0
        else
          movie_def%angle_range(1:2)                                    &
     &          = movie_ctl%angle_range_ctl%realvalue(1:2)
        end if
      else if(view_param%iflag_movie_mode .eq. I_LOOKINGLASS) then
        movie_def%id_rot_axis = 2
!
        if(movie_ctl%angle_range_ctl%iflag .eq. 0) then
          movie_def%angle_range(1) =  -17.5d0
          movie_def%angle_range(2) =   17.5d0
        else
          movie_def%angle_range(1:2)                                    &
     &          = movie_ctl%angle_range_ctl%realvalue(1:2)
        end if
!
      else if(view_param%iflag_movie_mode .eq. I_ZOOM) then
        if(movie_ctl%apature_range_ctl%iflag .eq. 0) then
          view_param%iflag_movie_mode = IFLAG_NO_MOVIE
        else
          movie_def%apature_range(1:2)                                  &
     &          = movie_ctl%apature_range_ctl%realvalue(1:2)
        end if
!
      else if(view_param%iflag_movie_mode .eq. I_LIC_KERNEL) then
        if(movie_ctl%LIC_kernel_peak_range_ctl%iflag .eq. 0) then
          view_param%iflag_movie_mode = IFLAG_NO_MOVIE
          movie_def%peak_range(1) =  -0.5d0
          movie_def%peak_range(2) =   0.5d0
        else
          movie_def%peak_range(1:2)                                     &
     &          = movie_ctl%LIC_kernel_peak_range_ctl%realvalue(1:2)
        end if
!
      else if(view_param%iflag_movie_mode .eq. I_START_END_VIEW) then
        if(movie_ctl%view_start_ctl%i_view_transform .eq. 0) then
          view_param%iflag_movie_mode = IFLAG_NO_MOVIE
        end if
        if(movie_ctl%view_end_ctl%i_view_transform .eq. 0) then
          view_param%iflag_movie_mode = IFLAG_NO_MOVIE
        end if
      end if
!
!
      end subroutine s_set_control_pvr_movie
!
!  ---------------------------------------------------------------------
!
      end module set_control_pvr_movie
