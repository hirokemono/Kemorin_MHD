!>@file   c_link_PVR_colorbar_ctl.f90
!!@brief  module c_link_PVR_colorbar_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_PVR_cmap_cbar_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_PVR_cmap_cbar_ctl_block_name')
!!      type(c_ptr) function c_PVR_cmap_cbar_ctl_iflag(c_ctl)           &
!!     &          bind(C, NAME = 'c_PVR_cmap_cbar_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_PVR_cmap_cbar_color_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_PVR_cmap_cbar_color_ctl')
!!      type(c_ptr) function c_PVR_cmap_cbar_cbar_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_PVR_cmap_cbar_cbar_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_PVR_colorbar_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_colorbar_ctl_block_name')
!!      type(c_ptr) function c_PVR_colorbar_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_PVR_colorbar_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!|      type(c_ptr) function c_PVR_colorbar_switch_ctl(c_ctl)           &
!|     &          bind(C, NAME = 'c_PVR_colorbar_switch_ctl')
!|      type(c_ptr) function c_PVR_colorbar_scale_ctl(c_ctl)            &
!|     &          bind(C, NAME = 'c_PVR_colorbar_scale_ctl')
!|      type(c_ptr) function c_PVR_colorbar_position_ctl(c_ctl)         &
!|     &          bind(C, NAME = 'c_PVR_colorbar_position_ctl')
!|      type(c_ptr) function c_PVR_colorbar_zeromarker_ctl(c_ctl)       &
!|     &          bind(C, NAME = 'c_PVR_colorbar_zeromarker_ctl')
!|      type(c_ptr) function c_PVR_colorbar_font_size_ctl(c_ctl)        &
!|     &          bind(C, NAME = 'c_PVR_colorbar_font_size_ctl')
!|      type(c_ptr) function c_PVR_colorbar_ngrid_cbar_ctl(c_ctl)       &
!|     &          bind(C, NAME = 'c_PVR_colorbar_ngrid_cbar_ctl')
!|      type(c_ptr) function c_PVR_colorbar_cbar_range_ctl(c_ctl)       &
!|     &          bind(C, NAME = 'c_PVR_colorbar_cbar_range_ctl')
!|      type(c_ptr) function c_PVR_colorbar_axis_switch_ctl(c_ctl)      &
!|     &          bind(C, NAME = 'c_PVR_colorbar_axis_switch_ctl')
!|      type(c_ptr) function c_PVR_colorbar_time_ctl(c_ctl)             &
!|     &          bind(C, NAME = 'c_PVR_colorbar_time_ctl')
!|      type(c_ptr) function c_PVR_colorbar_mapgrid_ctl(c_ctl)          &
!|     &          bind(C, NAME = 'c_PVR_colorbar_mapgrid_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_PVR_colorbar_ctl
!
      use iso_c_binding
      use t_ctl_data_pvr_colormap_bar
      use t_ctl_data_pvr_colorbar
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_cbar_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_cmap_cbar_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_bar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_cbar_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_cmap_cbar_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_cbar_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_PVR_cmap_cbar_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_bar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_cbar_ctl_iflag = C_loc(f_ctl%i_cmap_cbar)
      end function c_PVR_cmap_cbar_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_cbar_color_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_PVR_cmap_cbar_color_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_bar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_cbar_color_ctl = C_loc(f_ctl%color)
      end function c_PVR_cmap_cbar_color_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_cbar_cbar_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_cmap_cbar_cbar_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_bar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_cbar_cbar_ctl = C_loc(f_ctl%cbar_ctl)
      end function c_PVR_cmap_cbar_cbar_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_colorbar_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_colorbar_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_colorbar_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_ctl_iflag = C_loc(f_ctl%i_pvr_colorbar)
      end function c_PVR_colorbar_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_switch_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_PVR_colorbar_switch_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_switch_ctl = C_loc(f_ctl%colorbar_switch_ctl)
      end function c_PVR_colorbar_switch_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_scale_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_colorbar_scale_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_scale_ctl = C_loc(f_ctl%colorbar_scale_ctl)
      end function c_PVR_colorbar_scale_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_position_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_PVR_colorbar_position_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_position_ctl = C_loc(f_ctl%colorbar_position_ctl)
      end function c_PVR_colorbar_position_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_zeromarker_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_colorbar_zeromarker_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_zeromarker_ctl = C_loc(f_ctl%zeromarker_flag_ctl)
      end function c_PVR_colorbar_zeromarker_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_font_size_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_colorbar_font_size_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_font_size_ctl = C_loc(f_ctl%font_size_ctl)
      end function c_PVR_colorbar_font_size_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_ngrid_cbar_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_colorbar_ngrid_cbar_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_ngrid_cbar_ctl = C_loc(f_ctl%ngrid_cbar_ctl)
      end function c_PVR_colorbar_ngrid_cbar_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_cbar_range_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_colorbar_cbar_range_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_cbar_range_ctl = C_loc(f_ctl%cbar_range_ctl)
      end function c_PVR_colorbar_cbar_range_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_axis_switch_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_colorbar_axis_switch_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_axis_switch_ctl = C_loc(f_ctl%axis_switch_ctl)
      end function c_PVR_colorbar_axis_switch_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_time_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_PVR_colorbar_time_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_time_ctl = C_loc(f_ctl%time_switch_ctl)
      end function c_PVR_colorbar_time_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colorbar_mapgrid_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_PVR_colorbar_mapgrid_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colorbar_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colorbar_mapgrid_ctl = C_loc(f_ctl%mapgrid_switch_ctl)
      end function c_PVR_colorbar_mapgrid_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_PVR_colorbar_ctl
