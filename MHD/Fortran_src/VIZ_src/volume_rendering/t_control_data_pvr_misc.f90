!t_control_data_pvr_misc.f90
!      module t_control_data_pvr_misc
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine read_pvr_colorbar_ctl(colorbar)
!!      subroutine read_pvr_rotation_ctl(movie)
!!      subroutine reset_pvr_misc_control_flags(colorbar, movie)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
!!    iflag_zeromarker       ON
!!    colorbar_range     0.0   1.0
!!    font_size_ctl         3
!!    num_grid_ctl     4
!!  end colorbar_ctl
!!
!!  begin image_rotation_ctl
!!    rotation_axis_ctl       z
!!    rotation_axis_ctl       1
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module t_control_data_pvr_misc
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!
!
      type pvr_colorbar_ctl
        type(read_character_item) :: colorbar_switch_ctl
        type(read_character_item) :: colorbar_scale_ctl
        type(read_character_item) :: zeromarker_flag_ctl
        type(read_integer_item) ::   font_size_ctl
        type(read_integer_item) ::   ngrid_cbar_ctl
        type(read_real2_item) ::     cbar_range_ctl
!
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_pvr_colorbar = 0
      end type pvr_colorbar_ctl
!
      type pvr_movie_ctl
        type(read_character_item) :: rotation_axis_ctl
        type(read_integer_item) ::   num_frames_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_pvr_rotation = 0
      end type pvr_movie_ctl
!
!     2nd level for volume_rendering
!
      character(len=kchara) :: hd_pvr_colorbar =  'colorbar_ctl'
      character(len=kchara) :: hd_pvr_rotation =  'image_rotation_ctl'
!
!     3rd level for colorbar
!
      character(len=kchara)                                             &
     &                    :: hd_colorbar_switch = 'colorbar_switch_ctl'
      character(len=kchara) :: hd_colorbar_scale = 'colorbar_scale_ctl'
      character(len=kchara) :: hd_pvr_font_size = 'font_size_ctl'
      character(len=kchara) :: hd_pvr_numgrid_cbar = 'num_grid_ctl'
      character(len=kchara) :: hd_zeromarker_flag = 'iflag_zeromarker'
      character(len=kchara) :: hd_cbar_range = 'colorbar_range'
!
!     3rd level for rotation
!
      character(len=kchara) :: hd_movie_rot_axis =  'rotation_axis_ctl'
      character(len=kchara) :: hd_movie_rot_frame = 'num_frames_ctl'
!
      private :: hd_pvr_colorbar, hd_pvr_rotation
      private :: hd_colorbar_switch
      private :: hd_pvr_numgrid_cbar, hd_zeromarker_flag
      private :: hd_colorbar_scale, hd_pvr_font_size, hd_cbar_range
      private :: hd_movie_rot_axis, hd_movie_rot_frame
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colorbar_ctl(colorbar)
!
      type(pvr_colorbar_ctl), intent(inout) :: colorbar
!
!
      if(right_begin_flag(hd_pvr_colorbar) .eq. 0) return
      if (colorbar%i_pvr_colorbar.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag                                      &
     &     (hd_pvr_colorbar, colorbar%i_pvr_colorbar)
        if(colorbar%i_pvr_colorbar .gt. 0) exit
!
!
        call read_integer_ctl_type                                      &
     &     (hd_pvr_font_size, colorbar%font_size_ctl)
        call read_integer_ctl_type(hd_pvr_numgrid_cbar,                 &
     &      colorbar%ngrid_cbar_ctl)
!
!
        call read_chara_ctl_type(hd_colorbar_switch,                    &
     &      colorbar%colorbar_switch_ctl)
        call read_chara_ctl_type(hd_colorbar_scale,                     &
     &      colorbar%colorbar_scale_ctl)
        call read_chara_ctl_type(hd_zeromarker_flag,                    &
     &      colorbar%zeromarker_flag_ctl)
!
        call read_real2_ctl_type                                        &
     &     (hd_cbar_range, colorbar%cbar_range_ctl)
      end do
!
      end subroutine read_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_rotation_ctl(movie)
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      if(right_begin_flag(hd_pvr_rotation) .eq. 0) return
      if (movie%i_pvr_rotation.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag                                      &
     &     (hd_pvr_rotation, movie%i_pvr_rotation)
        if(movie%i_pvr_rotation .gt. 0) exit
!
        call read_integer_ctl_type(hd_movie_rot_frame,                  &
     &      movie%num_frames_ctl)
        call read_chara_ctl_type(hd_movie_rot_axis,                     &
     &      movie%rotation_axis_ctl)
      end do
!
      end subroutine read_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_misc_control_flags(colorbar, movie)
!
      type(pvr_colorbar_ctl), intent(inout) :: colorbar
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      movie%num_frames_ctl%iflag =    0
      movie%rotation_axis_ctl%iflag = 0
!
      colorbar%colorbar_switch_ctl%iflag = 0
      colorbar%colorbar_scale_ctl%iflag =  0
      colorbar%font_size_ctl%iflag =       0
      colorbar%ngrid_cbar_ctl%iflag =      0
      colorbar%zeromarker_flag_ctl%iflag = 0
      colorbar%cbar_range_ctl%iflag =      0
!
      movie%i_pvr_rotation = 0
      colorbar%i_pvr_colorbar = 0
!
      end subroutine reset_pvr_misc_control_flags
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_misc
