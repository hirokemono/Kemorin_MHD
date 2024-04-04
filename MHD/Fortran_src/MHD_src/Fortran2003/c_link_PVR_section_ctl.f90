!>@file   c_link_PVR_section_ctl.f90
!!@brief  module c_link_PVR_section_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_VIZ_PVR_sects_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_PVR_sects_block_name')
!!      integer(c_int) function c_VIZ_PVR_num_pvr_sect_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_PVR_num_pvr_sect_ctl')
!!      type(c_ptr) function c_VIZ_PVR_section_ctl(idx_in, c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_PVR_section_ctl')
!!        integer(c_int), value :: idx_in
!!        type(c_ptr), value, intent(in) :: c_ctl
!!      type(c_ptr) function c_append_PVR_sections_ctls                 &
!!     &                  (idx, c_name, c_ctl)                          &
!!     &                  bind(C, NAME = 'c_append_PVR_sections_ctls')
!!      type(c_ptr) function c_delete_PVR_sections_ctls(idx, c_ctl)     &
!!     &                  bind(C, NAME = 'c_delete_PVR_sections_ctls')
!!        integer(c_int), value, intent(in) :: idx
!!        character(C_char), intent(in) :: c_name(*)
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        type(pvr_sections_ctl), pointer :: f_ctl
!!!!
!!      type(c_ptr) function c_VIZ_PVR_isos_block_name(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_PVR_isos_block_name')
!!      integer(c_int) function c_VIZ_PVR_num_pvr_iso_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_VIZ_PVR_num_pvr_iso_ctl')
!!      type(c_ptr) function c_VIZ_PVR_isosurface_ctl(idx_in, c_ctl)    &
!!     &          bind(C, NAME = 'c_VIZ_PVR_isosurface_ctl')
!!        integer(c_int), value :: idx_in
!!        type(c_ptr), value, intent(in) :: c_ctl
!!      type(c_ptr) function c_append_PVR_isosurface_ctls               &
!!     &                  (idx, c_name, c_ctl)                          &
!!     &                  bind(C, NAME = 'c_append_PVR_isosurface_ctls')
!!      type(c_ptr) function c_delete_PVR_isosurface_ctls(idx, c_ctl)   &
!!     &                  bind(C, NAME = 'c_delete_PVR_isosurface_ctls')
!!        integer(c_int), value, intent(in) :: idx
!!        character(C_char), intent(in) :: c_name(*)
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_PVR_section_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_PVR_section_ctl_block_name')
!!      type(c_ptr) function c_PVR_section_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_PVR_section_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_PVR_section_fname_sect_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_PVR_section_fname_sect_ctl')
!!      type(c_ptr) function c_PVR_section_psf_def_c(c_ctl)             &
!!     &          bind(C, NAME = 'c_PVR_section_psf_def_c')
!!      type(c_ptr) function c_PVR_section_opacity_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_PVR_section_opacity_ctl')
!!      type(c_ptr) function c_PVR_section_zeroline_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_PVR_section_zeroline_ctl')
!!!!
!!      type(c_ptr) function c_MAP_section_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_MAP_section_ctl_block_name')
!!      type(c_ptr) function c_MAP_section_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_MAP_section_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_MAP_section_fname_sect_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MAP_section_fname_sect_ctl')
!!      type(c_ptr) function c_MAP_section_psf_def_c(c_ctl)             &
!!     &          bind(C, NAME = 'c_MAP_section_psf_def_c')
!!      type(c_ptr) function c_MAP_section_zeroline_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_MAP_section_zeroline_ctl')
!!      type(c_ptr) function c_MAP_section_iso_color_mode(c_ctl)        &
!!     &          bind(C, NAME = 'c_MAP_section_iso_color_mode')
!!      type(c_ptr) function c_MAP_section_iso_number_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MAP_section_iso_number_ctl')
!!      type(c_ptr) function c_MAP_section_iso_range_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MAP_section_iso_range_ctl')
!!      type(c_ptr) function c_MAP_section_iso_width_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MAP_section_iso_width_ctl')
!!      type(c_ptr) function c_MAP_section_grid_width_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MAP_section_grid_width_ctl')
!!      type(c_ptr) function c_MAP_section_tan_cyl_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_MAP_section_tan_cyl_ctl')
!!      type(c_ptr) function c_MAP_section_tan_cyl_in_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MAP_section_tan_cyl_in_ctl')
!!      type(c_ptr) function c_MAP_section_tan_cyl_out_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_MAP_section_tan_cyl_out_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_PVR_isosurf_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_PVR_isosurf_ctl_block_name')
!!      type(c_ptr) function c_PVR_isosurf_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_PVR_isosurf_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_PVR_isosurf_type_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_PVR_isosurf_type_ctl')
!!      type(c_ptr) function c_PVR_isosurf_iso_value_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_PVR_isosurf_iso_value_ctl')
!!      type(c_ptr) function c_PVR_isosurf_opacity_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_PVR_isosurf_opacity_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_PVR_section_ctl
!
      use iso_c_binding
      use t_control_data_pvr_sections
      use t_control_data_pvr_isosurfs
      use t_ctl_data_map_section
      use t_ctl_data_pvr_section
      use t_ctl_data_pvr_isosurface
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_sects_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_PVR_sects_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_sections_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_sects_block_name = c_loc(f_ctl%block_name)
      end function c_VIZ_PVR_sects_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_VIZ_PVR_num_pvr_sect_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_PVR_num_pvr_sect_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_sections_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_num_pvr_sect_ctl = f_ctl%num_pvr_sect_ctl
      end function c_VIZ_PVR_num_pvr_sect_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_section_ctl(idx_in, c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_PVR_section_ctl')
      integer(c_int), value :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_sections_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_section_ctl = C_loc(f_ctl%pvr_sect_ctl(idx_in+1))
      end function c_VIZ_PVR_section_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_append_PVR_sections_ctls                   &
     &                  (idx, c_name, c_ctl)                            &
     &                  bind(C, NAME = 'c_append_PVR_sections_ctls')
      use ctl_array_chara_to_c
!
      integer(c_int), value, intent(in) :: idx
      character(C_char), intent(in) :: c_name(*)
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_sections_ctl), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call append_pvr_section_ctl(idx, copy_char_from_c(c_name), f_ctl)
      c_append_PVR_sections_ctls = C_loc(f_ctl%pvr_sect_ctl)
      f_ctl%pvr_sect_ctl(idx+1)%i_pvr_sect_ctl = 1
!
      end function c_append_PVR_sections_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_delete_PVR_sections_ctls(idx, c_ctl)       &
     &                  bind(C, NAME = 'c_delete_PVR_sections_ctls')
      integer(c_int), value, intent(in) :: idx
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_sections_ctl), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call delete_pvr_section_ctl((idx+1), f_ctl)
      c_delete_PVR_sections_ctls = C_loc(f_ctl%pvr_sect_ctl)
!
      end function c_delete_PVR_sections_ctls
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_isos_block_name(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_PVR_isos_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurfs_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_isos_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_PVR_isos_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_VIZ_PVR_num_pvr_iso_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_VIZ_PVR_num_pvr_iso_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurfs_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_num_pvr_iso_ctl = f_ctl%num_pvr_iso_ctl
      end function c_VIZ_PVR_num_pvr_iso_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_isosurface_ctl(idx_in, c_ctl)      &
     &          bind(C, NAME = 'c_VIZ_PVR_isosurface_ctl')
      integer(c_int), value :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurfs_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_isosurface_ctl = C_loc(f_ctl%pvr_iso_ctl(idx_in+1))
      end function c_VIZ_PVR_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_append_PVR_isosurface_ctls                 &
     &                  (idx, c_name, c_ctl)                            &
     &                  bind(C, NAME = 'c_append_PVR_isosurface_ctls')
      use ctl_array_chara_to_c
!
      integer(c_int), value, intent(in) :: idx
      character(C_char), intent(in) :: c_name(*)
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurfs_ctl), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call append_pvr_isosurf_ctl(idx, copy_char_from_c(c_name), f_ctl)
      c_append_PVR_isosurface_ctls = C_loc(f_ctl%pvr_iso_ctl)
      f_ctl%pvr_iso_ctl(idx+1)%i_pvr_isosurf_ctl = 1
!
      end function c_append_PVR_isosurface_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_delete_PVR_isosurface_ctls(idx, c_ctl)     &
     &                  bind(C, NAME = 'c_delete_PVR_isosurface_ctls')
      integer(c_int), value, intent(in) :: idx
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurfs_ctl), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call delete_pvr_isosurf_ctl((idx+1), f_ctl)
      c_delete_PVR_isosurface_ctls = C_loc(f_ctl%pvr_iso_ctl)
!
      end function c_delete_PVR_isosurface_ctls
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_section_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_section_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_section_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_section_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_section_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_PVR_section_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_section_ctl_iflag = C_loc(f_ctl%i_pvr_sect_ctl)
      end function c_PVR_section_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_section_fname_sect_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_section_fname_sect_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_section_fname_sect_ctl = C_loc(f_ctl%fname_sect_ctl)
      end function c_PVR_section_fname_sect_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_section_psf_def_c(c_ctl)               &
     &          bind(C, NAME = 'c_PVR_section_psf_def_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_section_psf_def_c = C_loc(f_ctl%psf_def_c)
      end function c_PVR_section_psf_def_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_section_opacity_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_PVR_section_opacity_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_section_opacity_ctl = C_loc(f_ctl%opacity_ctl)
      end function c_PVR_section_opacity_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_section_zeroline_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_PVR_section_zeroline_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_section_zeroline_ctl = C_loc(f_ctl%zeroline_switch_ctl)
      end function c_PVR_section_zeroline_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MAP_section_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MAP_section_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MAP_section_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_ctl_iflag = C_loc(f_ctl%i_map_sect_ctl)
      end function c_MAP_section_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_fname_sect_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MAP_section_fname_sect_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_fname_sect_ctl = C_loc(f_ctl%fname_sect_ctl)
      end function c_MAP_section_fname_sect_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_psf_def_c(c_ctl)               &
     &          bind(C, NAME = 'c_MAP_section_psf_def_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_psf_def_c = C_loc(f_ctl%psf_def_c)
      end function c_MAP_section_psf_def_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_zeroline_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_MAP_section_zeroline_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_zeroline_ctl = C_loc(f_ctl%zeroline_switch_ctl)
      end function c_MAP_section_zeroline_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_iso_color_mode(c_ctl)          &
     &          bind(C, NAME = 'c_MAP_section_iso_color_mode')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_iso_color_mode = C_loc(f_ctl%isoline_color_mode)
      end function c_MAP_section_iso_color_mode
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_iso_number_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MAP_section_iso_number_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_iso_number_ctl = C_loc(f_ctl%isoline_number_ctl)
      end function c_MAP_section_iso_number_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_iso_range_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MAP_section_iso_range_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_iso_range_ctl = C_loc(f_ctl%isoline_range_ctl)
      end function c_MAP_section_iso_range_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_iso_width_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MAP_section_iso_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_iso_width_ctl = C_loc(f_ctl%isoline_width_ctl)
      end function c_MAP_section_iso_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_grid_width_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MAP_section_grid_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_grid_width_ctl = C_loc(f_ctl%grid_width_ctl)
      end function c_MAP_section_grid_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_tan_cyl_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_MAP_section_tan_cyl_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_tan_cyl_ctl = C_loc(f_ctl%tan_cyl_switch_ctl)
      end function c_MAP_section_tan_cyl_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_tan_cyl_in_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MAP_section_tan_cyl_in_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_tan_cyl_in_ctl                                      &
     &            = C_loc(f_ctl%tangent_cylinder_inner_ctl)
      end function c_MAP_section_tan_cyl_in_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MAP_section_tan_cyl_out_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_MAP_section_tan_cyl_out_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_section_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MAP_section_tan_cyl_out_ctl                                     &
     &            = C_loc(f_ctl%tangent_cylinder_outer_ctl)
      end function c_MAP_section_tan_cyl_out_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_isosurf_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_isosurf_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_isosurf_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_isosurf_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_isosurf_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_PVR_isosurf_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_isosurf_ctl_iflag = C_loc(f_ctl%i_pvr_isosurf_ctl)
      end function c_PVR_isosurf_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_isosurf_type_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_PVR_isosurf_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_isosurf_type_ctl = C_loc(f_ctl%isosurf_type_ctl)
      end function c_PVR_isosurf_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_isosurf_iso_value_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_PVR_isosurf_iso_value_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_isosurf_iso_value_ctl = C_loc(f_ctl%iso_value_ctl)
      end function c_PVR_isosurf_iso_value_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_isosurf_opacity_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_PVR_isosurf_opacity_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_isosurf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_isosurf_opacity_ctl = C_loc(f_ctl%opacity_ctl)
      end function c_PVR_isosurf_opacity_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_PVR_section_ctl
