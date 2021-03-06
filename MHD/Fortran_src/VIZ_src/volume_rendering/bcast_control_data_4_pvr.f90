!>@file   bcast_control_data_4_pvr.f90
!!@brief  module bcast_control_data_4_pvr
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine bcast_vr_psf_ctl(pvr)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr
!!      subroutine dup_pvr_ctl(org_pvr, new_pvr)
!!      subroutine copy_pvr_update_flag(org_pvr, new_pvr)
!!        type(pvr_parameter_ctl), intent(in) :: org_pvr
!!        type(pvr_parameter_ctl), intent(inout) :: new_pvr
!!@end verbatim
!
!
      module bcast_control_data_4_pvr
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_vr_psf_ctl(pvr)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use bcast_dup_view_transfer_ctl
      use transfer_to_long_integers
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
!
!
      call calypso_mpi_bcast_one_int(pvr%i_pvr_ctl, 0)
!
!
      call calypso_mpi_bcast_character                                  &
     &   (pvr%view_file_ctl, cast_long(kchara), 0)
      call bcast_view_transfer_ctl(pvr%mat)
!
      call calypso_mpi_bcast_character                                  &
     &   (pvr%color_file_ctl, cast_long(kchara), 0)
!
      call bcast_pvr_isosurfs_ctl(pvr%pvr_isos_c)
      call bcast_pvr_sections_ctl(pvr%pvr_scts_c)
!
      call bcast_lighting_ctl(pvr%light)
      call bcast_pvr_colorbar_ctl(pvr%cmap_cbar_c%cbar_ctl)
      call bcast_pvr_colordef_ctl(pvr%cmap_cbar_c%color)
!
      call bcast_pvr_rotation_ctl(pvr%movie)
      call bcast_pvr_render_area_ctl(pvr%render_area_c)
!
      call bcast_ctl_type_c1(pvr%updated_ctl)
      call bcast_ctl_type_c1(pvr%file_head_ctl)
      call bcast_ctl_type_c1(pvr%file_fmt_ctl )
      call bcast_ctl_type_c1(pvr%monitoring_ctl)
      call bcast_ctl_type_c1(pvr%transparent_ctl)
!
      call bcast_ctl_type_c1(pvr%streo_ctl)
      call bcast_ctl_type_c1(pvr%anaglyph_ctl)
!
      call bcast_ctl_type_c1(pvr%pvr_field_ctl)
      call bcast_ctl_type_c1(pvr%pvr_comp_ctl)
!
      call bcast_ctl_type_i1(pvr%maxpe_composit_ctl)
!
      end subroutine bcast_vr_psf_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_update_flag(pvr)
!
      use bcast_control_arrays
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
!
!
      call bcast_ctl_type_c1(pvr%updated_ctl)
!
      end subroutine bcast_pvr_update_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_ctl(org_pvr, new_pvr)
!
      use bcast_control_arrays
      use bcast_dup_view_transfer_ctl
!
      type(pvr_parameter_ctl), intent(in) :: org_pvr
      type(pvr_parameter_ctl), intent(inout) :: new_pvr
!
!
      new_pvr%i_pvr_ctl = org_pvr%i_pvr_ctl
!
      new_pvr%view_file_ctl = org_pvr%view_file_ctl
      call dup_view_transfer_ctl(org_pvr%mat, new_pvr%mat)
!
      new_pvr%color_file_ctl = org_pvr%color_file_ctl
!
      call dup_pvr_isosurfs_ctl(org_pvr%pvr_isos_c, new_pvr%pvr_isos_c)
      call dup_pvr_sections_ctl(org_pvr%pvr_scts_c, new_pvr%pvr_scts_c)
!
      call dup_lighting_ctl(org_pvr%light, new_pvr%light)
      call dup_pvr_cmap_cbar(org_pvr%cmap_cbar_c, new_pvr%cmap_cbar_c)
!
      call dup_pvr_movie_control_flags(org_pvr%movie, new_pvr%movie)
      call dup_pvr_render_area_ctl(org_pvr%render_area_c,               &
     &                             new_pvr%render_area_c)
!
      call copy_chara_ctl(org_pvr%updated_ctl, new_pvr%updated_ctl)
      call copy_chara_ctl(org_pvr%file_head_ctl, new_pvr%file_head_ctl)
      call copy_chara_ctl(org_pvr%file_fmt_ctl, new_pvr%file_fmt_ctl)
      call copy_chara_ctl(org_pvr%monitoring_ctl,                       &
     &                    new_pvr%monitoring_ctl)
      call copy_chara_ctl(org_pvr%transparent_ctl,                      &
     &                    new_pvr%transparent_ctl)
!
      call copy_chara_ctl(org_pvr%streo_ctl, new_pvr%streo_ctl)
      call copy_chara_ctl(org_pvr%anaglyph_ctl, new_pvr%anaglyph_ctl)
!
      call copy_chara_ctl(org_pvr%pvr_field_ctl, new_pvr%pvr_field_ctl)
      call copy_chara_ctl(org_pvr%pvr_comp_ctl, new_pvr%pvr_comp_ctl)
!
      call copy_integer_ctl(org_pvr%maxpe_composit_ctl,                 &
     &    new_pvr%maxpe_composit_ctl)
!
      end subroutine dup_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_update_flag(org_pvr, new_pvr)
!
      type(pvr_parameter_ctl), intent(in) :: org_pvr
      type(pvr_parameter_ctl), intent(inout) :: new_pvr
!
!
      call copy_chara_ctl(org_pvr%updated_ctl, new_pvr%updated_ctl)
!
      end subroutine copy_pvr_update_flag
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_data_4_pvr
