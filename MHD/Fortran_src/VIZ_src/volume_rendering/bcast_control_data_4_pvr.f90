!bcast_control_data_4_pvr.f90
!      module bcast_control_data_4_pvr
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine bcast_vr_psf_ctl(pvr)
!!      subroutine bcast_pvr_colordef_ctl(color)
!!      subroutine bcast_view_transfer_ctl(mat)
!
!
      module bcast_control_data_4_pvr
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
      use t_ctl_data_pvr_colormap
      use t_control_data_pvr_misc
      use t_ctl_data_4_view_transfer
!
      use bcast_control_arrays
!
      implicit  none
!
      private :: bcast_pvr_sections_ctl, bcast_pvr_isosurfs_ctl
      private :: bcast_pvr_isosurface_ctl
      private :: bcast_pvr_colorbar_ctl, bcast_pvr_rotation_ctl
      private :: bcast_lighting_ctl, bcast_projection_mat_ctl
      private :: bcast_image_size_ctl, bcast_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_vr_psf_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
      integer(kind = kint) :: i_psf
!
!
      call MPI_BCAST(pvr%i_pvr_ctl,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
!
      call MPI_BCAST(pvr%view_file_ctl, kchara,                         &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call bcast_view_transfer_ctl(pvr%mat)
!
      call MPI_BCAST(pvr%color_file_ctl, kchara,                        &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call bcast_pvr_colordef_ctl(pvr%color)
!
      call bcast_pvr_isosurfs_ctl(pvr)
      call bcast_pvr_sections_ctl(pvr)
!
      call bcast_lighting_ctl(pvr%color)
      call bcast_pvr_colorbar_ctl(pvr%colorbar)
      call bcast_pvr_rotation_ctl(pvr%movie)
!
      call bcast_ctl_array_c1(pvr%pvr_area_ctl)
      call bcast_ctl_array_c2r(pvr%surf_enhanse_ctl)
!
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
      end subroutine bcast_vr_psf_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_update_flag(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      call bcast_ctl_type_c1(pvr%updated_ctl)
!
      end subroutine bcast_pvr_update_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_sections_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
      integer(kind = kint) :: i
!
!
      call MPI_BCAST(pvr%num_pvr_sect_ctl,  ione,                       &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      if(pvr%num_pvr_sect_ctl .gt. 0 .and. my_rank .gt. 0) then
        allocate(pvr%pvr_sect_ctl(pvr%num_pvr_sect_ctl))
      end if
!
      call MPI_BCAST(pvr%i_pvr_sect,  ione,                             &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      do i = 1, pvr%num_pvr_sect_ctl
        call MPI_BCAST(pvr%pvr_sect_ctl(i)%fname_sect_ctl, kchara,      &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
        call MPI_BCAST(pvr%pvr_sect_ctl(i)%psf%i_psf_ctl,  ione,        &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
        call bcast_section_def_control(pvr%pvr_sect_ctl(i)%psf)
        call bcast_ctl_type_r1(pvr%pvr_sect_ctl(i)%opacity_ctl)
      end do
!
      end subroutine bcast_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurfs_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
      integer(kind = kint) :: i
!
!
      call MPI_BCAST(pvr%i_pvr_iso,  ione,                              &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(pvr%num_pvr_iso_ctl,  ione,                        &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      if(pvr%num_pvr_iso_ctl .gt. 0 .and. my_rank .gt. 0) then
        allocate(pvr%pvr_iso_ctl(pvr%num_pvr_iso_ctl))
      end if
!
      do i = 1, pvr%num_pvr_iso_ctl
        call bcast_pvr_isosurface_ctl(pvr%pvr_iso_ctl(i))
      end do
!
      end subroutine bcast_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurface_ctl(pvr_iso_ctl)
!
      use bcast_control_arrays
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      call bcast_ctl_type_c1(pvr_iso_ctl%isosurf_type_ctl)
      call bcast_ctl_type_r1(pvr_iso_ctl%isosurf_value_ctl)
      call bcast_ctl_type_r1(pvr_iso_ctl%opacity_ctl)
!
      end subroutine bcast_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_colorbar_ctl(colorbar)
!
      use bcast_control_arrays
!
      type(pvr_colorbar_ctl), intent(inout) :: colorbar
!
!
      call MPI_BCAST(colorbar%i_pvr_colorbar,  ione,                    &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_i1(colorbar%font_size_ctl)
      call bcast_ctl_type_i1(colorbar%ngrid_cbar_ctl)
!
      call bcast_ctl_type_c1(colorbar%colorbar_switch_ctl)
      call bcast_ctl_type_c1(colorbar%colorbar_scale_ctl)
      call bcast_ctl_type_c1(colorbar%zeromarker_flag_ctl)
!
      call bcast_ctl_type_c1(colorbar%axis_switch_ctl)
!!
      call bcast_ctl_type_r2(colorbar%cbar_range_ctl)
!
      end subroutine bcast_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_rotation_ctl(movie)
!
      use bcast_control_arrays
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      call MPI_BCAST(movie%i_pvr_rotation,  ione,                       &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_i1(movie%num_frames_ctl)
      call bcast_ctl_type_c1(movie%rotation_axis_ctl)
!
      end subroutine bcast_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_lighting_ctl(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      call MPI_BCAST(color%i_pvr_lighting,  ione,                       &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_array_r3(color%light_position_ctl)
!
      call bcast_ctl_type_r1(color%ambient_coef_ctl )
      call bcast_ctl_type_r1(color%diffuse_coef_ctl )
      call bcast_ctl_type_r1(color%specular_coef_ctl)
!
      end subroutine bcast_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_colordef_ctl(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      call MPI_BCAST(color%i_pvr_colordef,  ione,                       &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_array_r2(color%colortbl_ctl)
      call bcast_ctl_array_r2(color%linear_opacity_ctl)
!
      call bcast_ctl_array_r3(color%step_opacity_ctl)
!
      call bcast_ctl_type_c1(color%colormap_ctl)
      call bcast_ctl_type_c1(color%data_mapping_ctl)
      call bcast_ctl_type_c1(color%opacity_style_ctl)
!
      call bcast_ctl_type_r1(color%range_min_ctl)
      call bcast_ctl_type_r1(color%range_max_ctl)
      call bcast_ctl_type_r1(color%fix_opacity_ctl)
!
      end subroutine bcast_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_view_transfer_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_view_transform,  ione,                       &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_projection_mat_ctl(mat)
      call bcast_image_size_ctl(mat)
      call bcast_stereo_view_ctl(mat)
!
!
      call bcast_ctl_array_cr(mat%lookpoint_ctl)
      call bcast_ctl_array_cr(mat%viewpoint_ctl)
      call bcast_ctl_array_cr(mat%up_dir_ctl)
!
      call bcast_ctl_array_cr(mat%view_rot_vec_ctl)
      call bcast_ctl_array_cr(mat%scale_vector_ctl)
      call bcast_ctl_array_cr(mat%viewpt_in_viewer_ctl)
!
      call bcast_ctl_array_c2r(mat%modelview_mat_ctl)
!
      call bcast_ctl_type_r1(mat%view_rotation_deg_ctl)
      call bcast_ctl_type_r1(mat%scale_factor_ctl)
!
      end subroutine bcast_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_projection_mat_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_project_mat,  ione,                          &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_r1(mat%perspective_angle_ctl)
      call bcast_ctl_type_r1(mat%perspective_xy_ratio_ctl)
      call bcast_ctl_type_r1(mat%perspective_near_ctl)
      call bcast_ctl_type_r1(mat%perspective_far_ctl)
!
      end subroutine bcast_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_image_size_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_image_size,  ione,                           &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_i1(mat%num_xpixel_ctl)
      call bcast_ctl_type_i1(mat%num_ypixel_ctl)
!
      end subroutine bcast_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_stereo_view_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call MPI_BCAST(mat%i_stereo_view,  ione,                          &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_r1(mat%focalpoint_ctl)
      call bcast_ctl_type_r1(mat%eye_separation_ctl)
!
      end subroutine bcast_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_data_4_pvr
