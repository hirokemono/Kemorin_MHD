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
!!      subroutine read_pvr_ctl                                         &
!!     &         (id_control, hd_block, hd_pvr_colordef, pvr_ctl, c_buf)
!!      subroutine read_pvr_update_flag                                 &
!!     &         (id_control, hd_block, pvr_ctl, c_buf)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
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
      use t_read_control_elements
      use t_ctl_data_4_view_transfer
      use t_control_elements
      use t_control_array_character
      use t_control_array_chara2real
      use t_ctl_data_pvr_colormap_bar
      use t_ctl_data_pvr_light
      use t_control_data_pvr_sections
      use t_control_data_pvr_movie
      use t_control_data_pvr_isosurfs
      use t_control_data_pvr_area
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
        type(pvr_render_area_ctl) :: render_area_c
!
        type(read_character_item) :: pvr_field_ctl
        type(read_character_item) :: pvr_comp_ctl
!
        type(pvr_sections_ctl) :: pvr_scts_c
!
!>       constrol structure for isosurfaces in PVR
        type(pvr_isosurfs_ctl) :: pvr_isos_c
!
!     Top level flag
        integer(kind = kint) :: i_pvr_ctl = 0
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
!
!
      call reset_pvr_light_flags(pvr_ctl%light)
      call reset_pvr_movie_control_flags(pvr_ctl%movie)
!
      call dealloc_view_transfer_ctl(pvr_ctl%mat)
      call dealloc_pvr_light_crl(pvr_ctl%light)
      call deallocate_pvr_cmap_cbar(pvr_ctl%cmap_cbar_c)
!
      call dealloc_pvr_render_area_ctl(pvr_ctl%render_area_c)
      call dealloc_pvr_isosurfs_ctl(pvr_ctl%pvr_isos_c)
      call dealloc_pvr_sections_ctl(pvr_ctl%pvr_scts_c)
!
      pvr_ctl%updated_ctl%iflag =     0
      pvr_ctl%file_head_ctl%iflag =   0
      pvr_ctl%file_fmt_ctl%iflag =    0
      pvr_ctl%transparent_ctl%iflag = 0
      pvr_ctl%pvr_field_ctl%iflag =   0
      pvr_ctl%pvr_comp_ctl%iflag =    0
!
      pvr_ctl%i_pvr_ctl = 0
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
      subroutine read_pvr_ctl                                           &
     &         (id_control, hd_block, hd_pvr_colordef, pvr_ctl, c_buf)
!
      use read_control_pvr_modelview
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(in) :: hd_pvr_colordef
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pvr_ctl%i_pvr_ctl .gt. 0) return
!
      pvr_ctl%view_file_ctl = 'NO_FILE'
      pvr_ctl%color_file_ctl = 'NO_FILE'
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_view_transform)) then
          pvr_ctl%view_file_ctl = third_word(c_buf)
        else if(check_begin_flag(c_buf, hd_view_transform)) then
          call read_view_transfer_ctl(id_control, hd_view_transform,    &
      &       pvr_ctl%mat, c_buf)
        end if
!
        if(check_file_flag(c_buf, hd_pvr_colordef)) then
          pvr_ctl%color_file_ctl = third_word(c_buf)
        end if
!
        if(pvr_ctl%color_file_ctl .eq. 'NO_FILE') then
          call read_pvr_colordef_ctl(id_control, hd_pvr_colordef,       &
     &        pvr_ctl%cmap_cbar_c%color, c_buf)
          call read_pvr_colordef_ctl(id_control, hd_colormap,           &
     &        pvr_ctl%cmap_cbar_c%color, c_buf)
!
          call read_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,       &
     &        pvr_ctl%cmap_cbar_c%cbar_ctl, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_pvr_sections)) then
          call read_pvr_sections_ctl(id_control, hd_pvr_sections,       &
     &        pvr_ctl%pvr_scts_c, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_pvr_isosurf)) then
          call read_pvr_isosurfs_ctl(id_control, hd_pvr_isosurf,        &
     &        pvr_ctl%pvr_isos_c, c_buf)
        end if
!
        call read_pvr_render_area_ctl(id_control, hd_plot_area,         &
     &      pvr_ctl%render_area_c, c_buf)
        call read_lighting_ctl(id_control, hd_pvr_lighting,             &
     &      pvr_ctl%light, c_buf)
        call read_pvr_rotation_ctl(id_control, hd_pvr_rotation,         &
     &      pvr_ctl%movie, c_buf)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr_ctl%updated_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_file_head, pvr_ctl%file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_out_type, pvr_ctl%file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_monitor, pvr_ctl%monitoring_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_rgba_type, pvr_ctl%transparent_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_streo, pvr_ctl%streo_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_anaglyph, pvr_ctl%anaglyph_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_output_field_def, pvr_ctl%pvr_field_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_output_comp_def, pvr_ctl%pvr_comp_ctl)
      end do
      pvr_ctl%i_pvr_ctl = 1
!
      end subroutine read_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_update_flag                                   &
     &         (id_control, hd_block, pvr_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (pvr_ctl%i_pvr_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr_ctl%updated_ctl)
      end do
      pvr_ctl%i_pvr_ctl = 1
!
      end subroutine read_pvr_update_flag
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_pvr
