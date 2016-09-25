!m_control_data_4_pvr.f90
!      module m_control_data_4_pvr
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine deallocate_cont_dat_pvr(pvr)
!
!      subroutine read_control_data_pvr(pvr)
!      subroutine read_control_data_colormap(pvr)
!      subroutine read_vr_psf_ctl(pvr)
!      subroutine reset_pvr_control_flags
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  pvr_file_head        pvr_temp
!!  pvr_output_type      PNG
!!  image_tranceparency  tranceparent
!!!
!!  output_field    temperature    end
!!  output component     scalar
!!!
!!  begin plot_area_ctl
!!    array chosen_ele_grp_ctl  1
!!      chosen_ele_grp_ctl   outer_core   end
!!    end array chosen_ele_grp_ctl
!!  end  plot_area_ctl
!!!
!!  begin view_transform_ctl
!!   ...
!!  end view_transform_ctl
!!
!!  begin lighting_ctl
!!   ...
!!  end lighting_ctl
!!
!!  begin pvr_color_ctl
!!   ...
!!  end   pvr_color_ctl
!!!
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
!!    iflag_zeromarker       ON
!!    colorbar_range     0.0   1.0
!!!    font_size_ctl         3
!!    num_grid_ctl     4
!!  end colorbar_ctl
!!!
!!  begin image_rotation_ctl
!!    hd_movie_rot_axis       z
!!    num_of_frames           1
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_control_data_4_pvr
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use m_ctl_data_4_view_transfer
      use t_control_elements
      use t_read_control_arrays
      use t_ctl_data_pvr_colormap
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_ctl
!>  file name for modelves matrix
        character(len=kchara) :: view_file_ctl
!>  file name for modelves matrix
        character(len=kchara) :: color_file_ctl
!
!>    Structure for modelview marices
        type(modeview_ctl) :: mat
!>    STructure for colormap
        type(pvr_colormap_ctl) :: color
!
        type(read_character_item) :: file_head_ctl
        type(read_character_item) :: file_fmt_ctl
        type(read_character_item) :: transparent_ctl
!
!>      Structure for element group list for PVR
!!@n      group_4_monitor_ctl%c_tbl: Name of element group for PVR
        type(ctl_array_chara) :: pvr_area_ctl
!
        character(len=kchara) :: pvr_field_ctl(1)
        character(len=kchara) :: pvr_comp_ctl(1)
!
       type(read_character_item) :: rotation_axis_ctl
        type(read_integer_item) ::   num_frames_ctl
!
        type(read_character_item) :: colorbar_switch_ctl
        type(read_character_item) :: colorbar_scale_ctl
        type(read_character_item) :: zeromarker_flag_ctl
        type(read_integer_item) ::   font_size_ctl
        type(read_integer_item) ::   ngrid_cbar_ctl
        type(read_real2_item) ::     cbar_range_ctl
!
!     Top level
!
        integer (kind=kint) :: i_pvr_ctl = 0
!
        integer (kind=kint) :: i_view_file =  0
        integer (kind=kint) :: i_color_file = 0
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_output_field_def =    0
        integer (kind=kint) :: i_output_comp_def =     0
        integer (kind=kint) :: i_plot_area =           0
!
        integer (kind=kint) :: i_pvr_lighting =        0
        integer (kind=kint) :: i_pvr_colordef =        0
        integer (kind=kint) :: i_pvr_colorbar =        0
        integer (kind=kint) :: i_pvr_rotation =        0
      end type pvr_ctl
!
!
!     Top level
!
      character(len=kchara) :: hd_vr_psf_ctl = 'volume_rendering'
!
!     2nd level for volume_rendering
!
      character(len=kchara) :: hd_pvr_file_head =     'pvr_file_head'
      character(len=kchara) :: hd_pvr_out_type =      'pvr_output_type'
      character(len=kchara) :: hd_pvr_rgba_type = 'image_tranceparency'
      character(len=kchara) :: hd_output_field_def = 'output_field'
      character(len=kchara) :: hd_output_comp_def =  'output_component'
!
      character(len=kchara) :: hd_pvr_lighting =  'lighting_ctl'
      character(len=kchara) :: hd_pvr_colordef =  'pvr_color_ctl'
      character(len=kchara) :: hd_pvr_colorbar =  'colorbar_ctl'
      character(len=kchara) :: hd_pvr_rotation =  'image_rotation_ctl'
!
!     3rd level for surface_define
!
      character(len=kchara) :: hd_plot_area =   'plot_area_ctl'
!
!     4th level for plot_area
!
      character(len=kchara) :: hd_plot_grp = 'chosen_ele_grp_ctl'
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
      private :: hd_pvr_file_head, hd_pvr_out_type, hd_pvr_rgba_type
      private :: hd_output_field_def
      private :: hd_plot_area, hd_output_comp_def
      private :: hd_plot_grp
      private :: hd_pvr_lighting
      private :: hd_pvr_colorbar, hd_pvr_rotation
      private :: hd_pvr_colordef
      private :: hd_pvr_numgrid_cbar, hd_zeromarker_flag
!
      private :: read_plot_area_ctl
      private :: read_pvr_rotation_ctl
      private :: read_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_pvr(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      call dealloc_pvr_color_crl(pvr%color)
!
      call dealloc_control_array_chara(pvr%pvr_area_ctl)
!
      end subroutine deallocate_cont_dat_pvr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_pvr(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      call load_ctl_label_and_line
!
      call read_vr_psf_ctl(pvr)
!
      end subroutine read_control_data_pvr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_vr_psf_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_vr_psf_ctl) .eq. 0) return
      if (pvr%i_pvr_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_vr_psf_ctl, pvr%i_pvr_ctl)
        if(pvr%i_pvr_ctl .gt. 0) exit
!
!
        if(right_file_flag(hd_view_transform) .gt. 0) then
          call read_file_name_from_ctl_line(pvr%i_view_file,            &
     &        pvr%view_file_ctl)
        else if(right_begin_flag(hd_view_transform) .gt. 0) then
          pvr%view_file_ctl = 'NO_FILE'
          call read_view_transfer_ctl(pvr%mat)
        end if
!
        if(right_file_flag(hd_pvr_colordef) .gt. 0) then
          call read_file_name_from_ctl_line(pvr%i_color_file,           &
     &        pvr%color_file_ctl)
        else if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
          pvr%color_file_ctl = 'NO_FILE'
          call read_pvr_colordef_ctl(pvr%color)
        end if
!
        call read_plot_area_ctl(pvr)
        call read_lighting_ctl(pvr%color)
        call read_pvr_colorbar_ctl(pvr)
        call read_pvr_rotation_ctl(pvr)
!
!
        call read_chara_ctl_type(hd_pvr_file_head, pvr%file_head_ctl)
        call read_chara_ctl_type(hd_pvr_out_type,  pvr%file_fmt_ctl )
        call read_chara_ctl_type(hd_pvr_rgba_type, pvr%transparent_ctl)
        call read_character_ctl_item(hd_output_field_def,               &
     &          pvr%i_output_field_def, pvr%pvr_field_ctl(1) )
        call read_character_ctl_item(hd_output_comp_def,                &
     &          pvr%i_output_comp_def, pvr%pvr_comp_ctl(1) )
      end do
!
      end subroutine read_vr_psf_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_plot_area_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_plot_area) .eq. 0) return
      if (pvr%i_plot_area.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_plot_area, pvr%i_plot_area)
        if(pvr%i_plot_area .gt. 0) exit
!
        call read_control_array_c1(hd_plot_grp, pvr%pvr_area_ctl)
      end do
!
      end subroutine read_plot_area_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colorbar_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_pvr_colorbar) .eq. 0) return
      if (pvr%i_pvr_colorbar.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_colorbar, pvr%i_pvr_colorbar)
        if(pvr%i_pvr_colorbar .gt. 0) exit
!
!
        call read_integer_ctl_type(hd_pvr_font_size, pvr%font_size_ctl)
        call read_integer_ctl_type(hd_pvr_numgrid_cbar,                 &
     &      pvr%ngrid_cbar_ctl )
!
!
        call read_chara_ctl_type(hd_colorbar_switch,                    &
     &      pvr%colorbar_switch_ctl )
        call read_chara_ctl_type(hd_colorbar_scale,                     &
     &      pvr%colorbar_scale_ctl )
        call read_chara_ctl_type(hd_zeromarker_flag,                    &
     &      pvr%zeromarker_flag_ctl )
!
        call read_real2_ctl_type(hd_cbar_range, pvr%cbar_range_ctl )
      end do
!
      end subroutine read_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_rotation_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_pvr_rotation) .eq. 0) return
      if (pvr%i_pvr_rotation.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_rotation, pvr%i_pvr_rotation)
        if(pvr%i_pvr_rotation .gt. 0) exit
!
        call read_integer_ctl_type(hd_movie_rot_frame,                  &
     &      pvr%num_frames_ctl )
        call read_chara_ctl_type(hd_movie_rot_axis,                     &
     &      pvr%rotation_axis_ctl )
      end do
!
!
      end subroutine read_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_control_flags(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      pvr%pvr_area_ctl%num =       0
!
      pvr%file_head_ctl%iflag =   0
      pvr%file_fmt_ctl%iflag =    0
      pvr%transparent_ctl%iflag = 0
      pvr%i_output_field_def =    0
      pvr%i_output_comp_def =     0
      pvr%num_frames_ctl%iflag =    0
      pvr%rotation_axis_ctl%iflag = 0
!
      pvr%colorbar_switch_ctl%iflag = 0
      pvr%colorbar_scale_ctl%iflag =  0
      pvr%font_size_ctl%iflag =       0
      pvr%ngrid_cbar_ctl%iflag =      0
      pvr%zeromarker_flag_ctl%iflag = 0
      pvr%cbar_range_ctl%iflag =      0
!
      pvr%i_pvr_ctl = 0
      pvr%i_plot_area =   0
      pvr%pvr_area_ctl%icou = 0
!
      pvr%i_pvr_rotation = 0
      pvr%i_pvr_colorbar = 0
!
      call reset_pvr_colormap_flags(pvr%color)
      call reset_view_transfer_ctl(pvr%mat)
!
      end subroutine reset_pvr_control_flags
!
!  ---------------------------------------------------------------------
!
      end module m_control_data_4_pvr
