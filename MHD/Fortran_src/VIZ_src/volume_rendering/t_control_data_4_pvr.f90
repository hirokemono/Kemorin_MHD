!>@file   t_control_data_4_pvr.f90
!!@brief  module t_control_data_4_pvr
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine deallocate_cont_dat_pvr(pvr_ctl)
!!
!!      subroutine reset_pvr_update_flags(pvr_ctl)
!!      subroutine read_pvr_ctl(hd_block, hd_pvr_colordef, pvr_ctl)
!!      subroutine read_pvr_update_flag(hd_block, pvr_ctl)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  updated_sign         go
!!  pvr_file_head        pvr_temp
!!  pvr_output_type      PNG
!!  monitoring_mode      YES
!!  image_tranceparency  tranceparent
!!
!!  streo_imaging        YES
!!  anaglyph_image       YES
!!!
!!  output_field    temperature
!!  output_component     scalar
!!!
!!  begin plot_area_ctl
!!   ...
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
!!   ...
!!  end colorbar_ctl
!!!
!!  array section_ctl  2
!!    file section_ctl     ctl_psf_eq
!!    begin section_ctl
!!      ...
!!    end section_ctl
!!  end array section_ctl
!!!
!!  array isosurface_ctl  2
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!     ...
!!  end array isosurface_ctl
!!!
!!  begin image_rotation_ctl
!!   ...
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_4_pvr
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_view_transfer
      use t_control_elements
      use t_control_array_character
      use t_control_array_chara2real
      use t_ctl_data_pvr_colormap
      use t_control_data_pvr_misc
      use skip_comment_f
!
      implicit  none
!
!
!>  Structure of control data for PVR rendering
      type pvr_parameter_ctl
!>  file name for modelves matrix
        character(len=kchara) :: view_file_ctl
!>  file name for modelves matrix
        character(len=kchara) :: color_file_ctl
!
!>    Structure for modelview marices
        type(modeview_ctl) :: mat
!>    Structure for lighting
        type(pvr_light_ctl) :: light
!
!>    Structure for colormap and colorbar
        type(pvr_colormap_bar_ctl) :: cmap_cbar_c
!
!>    Structure for image rotation
        type(pvr_movie_ctl) :: movie
!
        type(read_character_item) :: updated_ctl
!
        type(read_character_item) :: file_head_ctl
        type(read_character_item) :: file_fmt_ctl
        type(read_character_item) :: monitoring_ctl
        type(read_character_item) :: transparent_ctl
!
        type(read_character_item) :: streo_ctl
        type(read_character_item) :: anaglyph_ctl
!
!>      Structure for element group list for PVR
!!@n      group_4_monitor_ctl%c_tbl: Name of element group for PVR
        type(ctl_array_chara) :: pvr_area_ctl
!
        type(ctl_array_c2r) :: surf_enhanse_ctl
!
        type(read_character_item) :: pvr_field_ctl
        type(read_character_item) :: pvr_comp_ctl
!
        integer(kind = kint) :: num_pvr_sect_ctl = 0
        type(pvr_sections_ctl), pointer :: pvr_sect_ctl(:)
!
        integer(kind = kint) :: num_pvr_iso_ctl = 0
        type(pvr_isosurf_ctl), pointer :: pvr_iso_ctl(:)
!
!     Top level
!
        integer (kind=kint) :: i_pvr_ctl = 0
!
        integer (kind=kint) :: i_view_file =  0
        integer (kind=kint) :: i_color_file = 0
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_plot_area =           0
        integer (kind=kint) :: i_pvr_sect =            0
        integer (kind=kint) :: i_pvr_iso =             0
      end type pvr_parameter_ctl
!
!
!     2nd level for volume_rendering
!
      character(len=kchara) :: hd_pvr_updated =     'updated_sign'
      character(len=kchara) :: hd_pvr_file_head =   'pvr_file_head'
      character(len=kchara) :: hd_pvr_out_type =    'pvr_output_type'
      character(len=kchara) :: hd_pvr_monitor =   'monitoring_mode'
      character(len=kchara) :: hd_pvr_rgba_type = 'image_tranceparency'
!
      character(len=kchara) :: hd_pvr_streo =    'streo_imaging'
      character(len=kchara) :: hd_pvr_anaglyph = 'anaglyph_image'
!
      character(len=kchara) :: hd_output_field_def = 'output_field'
      character(len=kchara) :: hd_output_comp_def =  'output_component'
!
      character(len=kchara) :: hd_pvr_sections = 'section_ctl'
      character(len=kchara) :: hd_pvr_isosurf =  'isosurface_ctl'
!
      character(len=kchara) :: hd_pvr_colorbar =  'colorbar_ctl'
      character(len=kchara) :: hd_pvr_rotation =  'image_rotation_ctl'
!
!     3rd level for surface_define
!
      character(len=kchara) :: hd_plot_area =   'plot_area_ctl'
!
!     3rd level for rotation
!
      character(len=kchara) :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara) :: hd_colormap =      'colormap_ctl'
      character(len=kchara) :: hd_pvr_lighting =  'lighting_ctl'
      private :: hd_view_transform, hd_pvr_lighting
      private :: hd_colormap
!
      private :: hd_pvr_file_head, hd_pvr_out_type, hd_pvr_rgba_type
      private :: hd_pvr_streo, hd_pvr_anaglyph, hd_pvr_updated
      private :: hd_output_field_def, hd_pvr_monitor
      private :: hd_plot_area, hd_output_comp_def
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_pvr(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      integer(kind = kint) :: i
!
!
      call reset_pvr_light_flags(pvr_ctl%light)
      call reset_pvr_movie_control_flags(pvr_ctl%movie)
!
      call dealloc_view_transfer_ctl(pvr_ctl%mat)
      call dealloc_pvr_light_crl(pvr_ctl%light)
      call deallocate_pvr_cmap_cbar(pvr_ctl%cmap_cbar_c)
!
      call dealloc_control_array_chara(pvr_ctl%pvr_area_ctl)
      call dealloc_control_array_c2_r(pvr_ctl%surf_enhanse_ctl)
!
      pvr_ctl%pvr_area_ctl%num =  0
      pvr_ctl%pvr_area_ctl%icou = 0
      pvr_ctl%surf_enhanse_ctl%num =  0
      pvr_ctl%surf_enhanse_ctl%icou = 0
!
      if(pvr_ctl%num_pvr_sect_ctl .gt. 0) then
        do i = 1, pvr_ctl%num_pvr_sect_ctl
          call deallocate_cont_dat_4_psf(pvr_ctl%pvr_sect_ctl(i)%psf_c)
        end do
        deallocate(pvr_ctl%pvr_sect_ctl)
      end if
      pvr_ctl%num_pvr_sect_ctl = 0
      pvr_ctl%i_pvr_sect = 0
!
      if(pvr_ctl%num_pvr_iso_ctl .gt. 0) then
        deallocate(pvr_ctl%pvr_iso_ctl)
      end if
      pvr_ctl%num_pvr_iso_ctl = 0
      pvr_ctl%i_pvr_iso = 0
!
      pvr_ctl%updated_ctl%iflag =     0
      pvr_ctl%file_head_ctl%iflag =   0
      pvr_ctl%file_fmt_ctl%iflag =    0
      pvr_ctl%transparent_ctl%iflag = 0
      pvr_ctl%pvr_field_ctl%iflag =   0
      pvr_ctl%pvr_comp_ctl%iflag =    0
!
      pvr_ctl%i_pvr_ctl = 0
      pvr_ctl%i_plot_area =   0
!
      end subroutine deallocate_cont_dat_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_update_flags(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
      pvr_ctl%i_pvr_ctl = 0
      pvr_ctl%updated_ctl%iflag =     0
!
      end subroutine reset_pvr_update_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_ctl(hd_block, hd_pvr_colordef, pvr_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(in) :: hd_pvr_colordef
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (pvr_ctl%i_pvr_ctl.gt.0) return
!
      pvr_ctl%view_file_ctl = 'NO_FILE'
      pvr_ctl%color_file_ctl = 'NO_FILE'
      do
        call load_ctl_label_and_line
!
        pvr_ctl%i_pvr_ctl = find_control_end_flag(hd_block)
        if(pvr_ctl%i_pvr_ctl .gt. 0) exit
!
!
        if(right_file_flag(hd_view_transform) .gt. 0) then
          call read_file_name_from_ctl_line(pvr_ctl%i_view_file,        &
     &        pvr_ctl%view_file_ctl)
        else if(right_begin_flag(hd_view_transform) .gt. 0) then
          call read_view_transfer_ctl(hd_view_transform, pvr_ctl%mat)
        end if
!
        if(right_file_flag(hd_pvr_colordef) .gt. 0) then
          call read_file_name_from_ctl_line(pvr_ctl%i_color_file,       &
     &        pvr_ctl%color_file_ctl)
        end if
!
        if(pvr_ctl%color_file_ctl .eq. 'NO_FILE') then
          call read_pvr_colordef_ctl                                    &
     &       (hd_pvr_colordef, pvr_ctl%cmap_cbar_c%color)
          call read_pvr_colordef_ctl                                    &
     &       (hd_colormap, pvr_ctl%cmap_cbar_c%color)
!
          call read_pvr_colorbar_ctl                                    &
     &       (hd_pvr_colorbar, pvr_ctl%cmap_cbar_c%cbar_ctl)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_pvr_sections, pvr_ctl%num_pvr_sect_ctl)
        if(pvr_ctl%num_pvr_sect_ctl .gt. 0) then
          call read_pvr_sections_ctl(pvr_ctl)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_pvr_isosurf, pvr_ctl%num_pvr_iso_ctl)
        if(pvr_ctl%num_pvr_iso_ctl .gt. 0) then
          call read_pvr_isosurfs_ctl(pvr_ctl)
        end if
!
        call read_plot_area_ctl(hd_plot_area, pvr_ctl%i_plot_area,      &
     &      pvr_ctl%pvr_area_ctl, pvr_ctl%surf_enhanse_ctl)
        call read_lighting_ctl(hd_pvr_lighting, pvr_ctl%light)
        call read_pvr_rotation_ctl(hd_pvr_rotation, pvr_ctl%movie)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_updated, pvr_ctl%updated_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_file_head, pvr_ctl%file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_out_type, pvr_ctl%file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_monitor, pvr_ctl%monitoring_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_rgba_type, pvr_ctl%transparent_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_streo, pvr_ctl%streo_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_anaglyph, pvr_ctl%anaglyph_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_output_field_def, pvr_ctl%pvr_field_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_output_comp_def, pvr_ctl%pvr_comp_ctl)
      end do
!
      end subroutine read_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_update_flag(hd_block, pvr_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (pvr_ctl%i_pvr_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        pvr_ctl%i_pvr_ctl = find_control_end_flag(hd_block)
        if(pvr_ctl%i_pvr_ctl .gt. 0) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_pvr_updated, pvr_ctl%updated_ctl)
      end do
!
      end subroutine read_pvr_update_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_sections_ctl(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
!
      if (pvr_ctl%i_pvr_sect .gt. 0) return
      allocate(pvr_ctl%pvr_sect_ctl(pvr_ctl%num_pvr_sect_ctl))
!
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(hd_pvr_sections,               &
     &      pvr_ctl%num_pvr_sect_ctl, pvr_ctl%i_pvr_sect)
        if(pvr_ctl%i_pvr_sect .ge. pvr_ctl%num_pvr_sect_ctl) exit
!
        if(right_begin_flag(hd_pvr_sections) .gt. 0) then
          pvr_ctl%i_pvr_sect = pvr_ctl%i_pvr_sect + 1
          call read_pvr_section_ctl                                     &
     &      (hd_pvr_sections, pvr_ctl%pvr_sect_ctl(pvr_ctl%i_pvr_sect))
        end if
      end do
!
      end subroutine read_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurfs_ctl(pvr_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!
!
      if (pvr_ctl%i_pvr_iso .gt. 0) return
      allocate(pvr_ctl%pvr_iso_ctl(pvr_ctl%num_pvr_iso_ctl))
!
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag                                &
     &     (hd_pvr_isosurf, pvr_ctl%num_pvr_iso_ctl, pvr_ctl%i_pvr_iso)
        if(pvr_ctl%i_pvr_iso .ge. pvr_ctl%num_pvr_iso_ctl) exit
!
        if(right_begin_flag(hd_pvr_isosurf) .gt. 0) then
          pvr_ctl%i_pvr_iso = pvr_ctl%i_pvr_iso + 1
          call read_pvr_isosurface_ctl                                  &
     &       (hd_pvr_isosurf, pvr_ctl%pvr_iso_ctl(pvr_ctl%i_pvr_iso))
        end if
      end do
!
      end subroutine read_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_pvr
