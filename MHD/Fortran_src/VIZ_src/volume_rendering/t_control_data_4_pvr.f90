!t_control_data_4_pvr.f90
!      module t_control_data_4_pvr
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine deallocate_cont_dat_pvr(pvr)
!
!!      subroutine read_control_data_pvr(pvr)
!!      subroutine reset_pvr_update_flags(pvr)
!!      subroutine read_control_data_colormap(pvr)
!!      subroutine read_vr_psf_ctl(pvr)
!!      subroutine read_pvr_update_flag(pvr)
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
      use t_read_control_arrays
      use t_ctl_data_pvr_colormap
      use t_control_data_pvr_misc
      use skip_comment_f
!
      implicit  none
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
!>    Structure for colorbar
        type(pvr_colorbar_ctl) :: colorbar
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
        character(len=kchara) :: pvr_field_ctl(1)
        character(len=kchara) :: pvr_comp_ctl(1)
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
        integer (kind=kint) :: i_output_field_def =    0
        integer (kind=kint) :: i_output_comp_def =     0
        integer (kind=kint) :: i_plot_area =           0
        integer (kind=kint) :: i_pvr_sect =            0
        integer (kind=kint) :: i_pvr_iso =             0
      end type pvr_ctl
!
!
!     Top level
!
      character(len=kchara) :: hd_vr_psf_ctl = 'volume_rendering'
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
      character(len=kchara) :: hd_pvr_colordef =  'pvr_color_ctl'
!
!     3rd level for surface_define
!
      character(len=kchara) :: hd_plot_area =   'plot_area_ctl'
!
!     4th level for plot_area
!
      character(len=kchara) :: hd_plot_grp = 'chosen_ele_grp_ctl'
      character(len=kchara) :: hd_sf_enhanse = 'surface_enhanse_ctl'
!
!     3rd level for rotation
!
      private :: hd_pvr_file_head, hd_pvr_out_type, hd_pvr_rgba_type
      private :: hd_pvr_streo, hd_pvr_anaglyph, hd_pvr_updated
      private :: hd_output_field_def, hd_pvr_monitor
      private :: hd_plot_area, hd_output_comp_def, hd_plot_grp
      private :: hd_sf_enhanse, hd_pvr_colordef
!
      private :: read_pvr_sections_ctl, read_pvr_isosurfs_ctl
      private :: read_plot_area_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_pvr(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      call load_ctl_label_and_line
      call read_vr_psf_ctl(pvr)
!
      end subroutine read_control_data_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_pvr(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
      integer(kind = kint) :: i
!
!
      call reset_pvr_colormap_flags(pvr%color)
      call reset_pvr_misc_control_flags(pvr%colorbar, pvr%movie)
!
      call dealloc_view_transfer_ctl(pvr%mat)
      call dealloc_pvr_color_crl(pvr%color)
!
      if(pvr%pvr_area_ctl%num .gt. 0) then
        call dealloc_control_array_chara(pvr%pvr_area_ctl)
      end if
      if(pvr%surf_enhanse_ctl%num .gt. 0) then
        call dealloc_control_array_c2_r(pvr%surf_enhanse_ctl)
      end if
!
      pvr%pvr_area_ctl%num =  0
      pvr%pvr_area_ctl%icou = 0
      pvr%surf_enhanse_ctl%num =  0
      pvr%surf_enhanse_ctl%icou = 0
!
      if(pvr%num_pvr_sect_ctl .gt. 0) then
        do i = 1, pvr%num_pvr_sect_ctl
          call deallocate_cont_dat_4_psf(pvr%pvr_sect_ctl(i)%psf)
        end do
        deallocate(pvr%pvr_sect_ctl)
      end if
      pvr%num_pvr_sect_ctl = 0
      pvr%i_pvr_sect = 0
!
      if(pvr%num_pvr_iso_ctl .gt. 0) deallocate(pvr%pvr_iso_ctl)
      pvr%num_pvr_iso_ctl = 0
      pvr%i_pvr_iso = 0
!
      pvr%updated_ctl%iflag =     0
      pvr%file_head_ctl%iflag =   0
      pvr%file_fmt_ctl%iflag =    0
      pvr%transparent_ctl%iflag = 0
      pvr%i_output_field_def =    0
      pvr%i_output_comp_def =     0
!
      pvr%i_pvr_ctl = 0
      pvr%i_plot_area =   0
!
      end subroutine deallocate_cont_dat_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_update_flags(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
      pvr%i_pvr_ctl = 0
      pvr%updated_ctl%iflag =     0
!
      end subroutine reset_pvr_update_flags
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
        call find_control_array_flag                                    &
     &     (hd_pvr_sections, pvr%num_pvr_sect_ctl)
        if(pvr%num_pvr_sect_ctl .gt. 0) call read_pvr_sections_ctl(pvr)
!
        call find_control_array_flag                                    &
     &     (hd_pvr_isosurf, pvr%num_pvr_iso_ctl)
        if(pvr%num_pvr_iso_ctl .gt. 0) call read_pvr_isosurfs_ctl(pvr)
!
        call read_plot_area_ctl(pvr)
        call read_lighting_ctl(pvr%color)
        call read_pvr_colorbar_ctl(pvr%colorbar)
        call read_pvr_rotation_ctl(pvr%movie)
!
!
        call read_chara_ctl_type(hd_pvr_updated, pvr%updated_ctl)
        call read_chara_ctl_type(hd_pvr_file_head, pvr%file_head_ctl)
        call read_chara_ctl_type(hd_pvr_out_type, pvr%file_fmt_ctl )
        call read_chara_ctl_type(hd_pvr_monitor, pvr%monitoring_ctl)
        call read_chara_ctl_type(hd_pvr_rgba_type, pvr%transparent_ctl)
!
        call read_chara_ctl_type(hd_pvr_streo, pvr%streo_ctl)
        call read_chara_ctl_type(hd_pvr_anaglyph, pvr%anaglyph_ctl)
!
        call read_character_ctl_item(hd_output_field_def,               &
     &          pvr%i_output_field_def, pvr%pvr_field_ctl(1) )
        call read_character_ctl_item(hd_output_comp_def,                &
     &          pvr%i_output_comp_def, pvr%pvr_comp_ctl(1) )
      end do
!
      end subroutine read_vr_psf_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_update_flag(pvr)
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
        call read_chara_ctl_type(hd_pvr_updated, pvr%updated_ctl)
      end do
!
      end subroutine read_pvr_update_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_sections_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if (pvr%i_pvr_sect .gt. 0) return
      allocate(pvr%pvr_sect_ctl(pvr%num_pvr_sect_ctl))
!
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag                                &
     &     (hd_pvr_sections, pvr%num_pvr_sect_ctl, pvr%i_pvr_sect)
        if(pvr%i_pvr_sect .ge. pvr%num_pvr_sect_ctl) exit
!
        if(right_begin_flag(hd_pvr_sections) .gt. 0) then
          pvr%i_pvr_sect = pvr%i_pvr_sect + 1
          call read_pvr_section_ctl(pvr%pvr_sect_ctl(pvr%i_pvr_sect))
        end if
      end do
!
      end subroutine read_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurfs_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if (pvr%i_pvr_iso .gt. 0) return
      allocate(pvr%pvr_iso_ctl(pvr%num_pvr_iso_ctl))
!
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag                                &
     &     (hd_pvr_isosurf, pvr%num_pvr_iso_ctl, pvr%i_pvr_iso)
        if(pvr%i_pvr_iso .ge. pvr%num_pvr_iso_ctl) exit
!
        if(right_begin_flag(hd_pvr_isosurf) .gt. 0) then
          pvr%i_pvr_iso = pvr%i_pvr_iso + 1
          call read_pvr_isosurface_ctl(pvr%pvr_iso_ctl(pvr%i_pvr_iso))
        end if
      end do
!
      end subroutine read_pvr_isosurfs_ctl
!
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
        call read_control_array_c2_r                                    &
     &     (hd_sf_enhanse, pvr%surf_enhanse_ctl)
      end do
      call calypso_mpi_barrier
!
      end subroutine read_plot_area_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_pvr
