!>@file   t_control_data_lic_pvr.f90
!!@brief  module t_control_data_lic_pvr
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine dealloc_lic_count_data(pvr, lic_ctl)
!!      subroutine read_lic_pvr_ctl                                     &
!!     &         (hd_block, hd_lic_colordef, pvr, lic_ctl)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  updated_sign         go
!!  lic_file_prefix      pvr_temp
!!  lic_image_format     PNG
!!  monitoring_mode      YES
!!  image_tranceparency  tranceparent
!!
!!  streo_imaging        YES
!!  anaglyph_image       YES
!!!
!!  begin LIC_ctl
!!   ...
!!  end  LIC_ctl
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
!!  begin image_rotation_ctl
!!   ...
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_lic_pvr
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_data_4_pvr
      use t_control_data_LIC
      use skip_comment_f
!
      implicit  none
!
!     2nd level for volume_rendering
!
      character(len=kchara) :: hd_pvr_updated =     'updated_sign'
      character(len=kchara) :: hd_lic_file_head =   'lic_file_prefix'
      character(len=kchara) :: hd_lic_out_type =    'lic_image_format'
      character(len=kchara) :: hd_pvr_monitor =   'monitoring_mode'
      character(len=kchara) :: hd_pvr_rgba_type = 'image_tranceparency'
!
      character(len=kchara) :: hd_pvr_streo =    'streo_imaging'
      character(len=kchara) :: hd_pvr_anaglyph = 'anaglyph_image'
!
      character(len=kchara) :: hd_lic_ctl = 'LIC_ctl'
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
!
!
      private :: hd_view_transform, hd_pvr_lighting
      private :: hd_colormap
!
      private :: hd_lic_file_head, hd_lic_out_type, hd_pvr_rgba_type
      private :: hd_pvr_streo, hd_pvr_anaglyph, hd_pvr_updated
      private :: hd_lic_ctl, hd_pvr_monitor
      private :: hd_plot_area
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_count_data(pvr, lic_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      call dealloc_lic_control_flags(lic_ctl)
      call deallocate_cont_dat_pvr(pvr)
!
      end subroutine dealloc_lic_count_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_lic_pvr_ctl                                       &
     &         (hd_block, hd_lic_colordef, pvr, lic_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(in) :: hd_lic_colordef
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (pvr%i_pvr_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        pvr%i_pvr_ctl = find_control_end_flag(hd_block)
        if(pvr%i_pvr_ctl .gt. 0) exit
!
!
        if(right_file_flag(hd_view_transform) .gt. 0) then
          call read_file_name_from_ctl_line(pvr%i_view_file,            &
     &        pvr%view_file_ctl)
        else if(right_begin_flag(hd_view_transform) .gt. 0) then
          pvr%view_file_ctl = 'NO_FILE'
          call read_view_transfer_ctl(hd_view_transform, pvr%mat)
        end if
!
        if(right_file_flag(hd_colormap) .gt. 0                          &
     &      .or. right_file_flag(hd_lic_colordef) .gt. 0) then
          call read_file_name_from_ctl_line(pvr%i_color_file,           &
     &        pvr%color_file_ctl)
        else if(right_begin_flag(hd_colormap) .gt. 0) then
          pvr%color_file_ctl = 'NO_FILE'
          call read_pvr_colordef_ctl(hd_colormap, pvr%color)
        else if(right_begin_flag(hd_lic_colordef) .gt. 0) then
          pvr%color_file_ctl = 'NO_FILE'
          call read_pvr_colordef_ctl(hd_lic_colordef, pvr%color)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_pvr_sections, pvr%num_pvr_sect_ctl)
        if(pvr%num_pvr_sect_ctl .gt. 0) call read_pvr_sections_ctl(pvr)
!
        call find_control_array_flag                                    &
     &     (hd_pvr_isosurf, pvr%num_pvr_iso_ctl)
        if(pvr%num_pvr_iso_ctl .gt. 0) call read_pvr_isosurfs_ctl(pvr)
!
        call read_plot_area_ctl(hd_plot_area, pvr%i_plot_area,          &
     &      pvr%pvr_area_ctl, pvr%surf_enhanse_ctl)
        call read_lighting_ctl(hd_pvr_lighting, pvr%color)
        call read_pvr_colorbar_ctl(pvr%cbar_ctl)
        call read_pvr_rotation_ctl(pvr%movie)
!
        call read_lic_control_data(hd_lic_ctl, lic_ctl)
!
        call read_chara_ctl_type(hd_pvr_updated, pvr%updated_ctl)
        call read_chara_ctl_type(hd_lic_file_head, pvr%file_head_ctl)
        call read_chara_ctl_type(hd_lic_out_type, pvr%file_fmt_ctl )
        call read_chara_ctl_type(hd_pvr_monitor, pvr%monitoring_ctl)
        call read_chara_ctl_type(hd_pvr_rgba_type, pvr%transparent_ctl)
!
        call read_chara_ctl_type(hd_pvr_streo, pvr%streo_ctl)
        call read_chara_ctl_type(hd_pvr_anaglyph, pvr%anaglyph_ctl)
      end do
!
      end subroutine read_lic_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_lic_pvr
