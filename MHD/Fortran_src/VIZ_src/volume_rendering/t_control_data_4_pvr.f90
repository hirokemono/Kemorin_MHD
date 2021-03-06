!>@file   t_control_data_4_pvr.f90
!!@brief  module t_control_data_4_pvr
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine deallocate_cont_dat_pvr(pvr_ctl)
!!      subroutine reset_pvr_update_flags(pvr_ctl)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!!
!!      subroutine add_field_4_pvr_to_fld_ctl(pvr_ctl, field_ctl)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
!!  max_pe_4_composit     32
!!
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
      use t_control_array_integer
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
!>  file name for modelview matrix
        character(len=kchara) :: view_file_ctl
!>  file name for coloemap file
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
        type(read_integer_item) :: maxpe_composit_ctl
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
      pvr_ctl%maxpe_composit_ctl%iflag = 0
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
      subroutine add_field_4_pvr_to_fld_ctl(pvr_ctl, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(pvr_ctl%pvr_field_ctl%iflag .gt. 0) then
        call add_viz_name_ctl                                           &
     &     (my_rank, pvr_ctl%pvr_field_ctl%charavalue, field_ctl)
      end if
!
      end subroutine add_field_4_pvr_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_pvr
