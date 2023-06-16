!>@file   multi_map_projections.f90
!!@brief  module multi_map_projections
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief loop for map projections
!!
!!@verbatim
!!      subroutine init_multi_map_projections(num_map, view_param,      &
!!     &          psf_mesh, psf_dat, map_data, map_rgb, SR_sig)
!!        integer(kind= kint), intent(in) :: num_map
!!        type(psf_local_data), intent(in) :: psf_mesh(num_map)
!!        type(pvr_view_parameter), intent(in):: view_param(num_map)
!!        type(psf_results), intent(inout) :: psf_dat(num_map)
!!        type(map_rendering_data), intent(inout) :: map_data(num_map)
!!        type(pvr_image_type), intent(inout) :: map_rgb(num_map)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine s_multi_map_projections                              &
!!     &         (num_map, istep_psf, time_d, psf_mesh, color_param,    &
!!     &          cbar_param, psf_dat, map_data, map_rgb, SR_sig)
!!        integer(kind= kint), intent(in) :: num_map
!!        integer(kind= kint), intent(in) ::  istep_psf
!!        type(time_data), intent(in) :: time_d
!!        type(psf_local_data), intent(in) :: psf_mesh(num_map)
!!        type(pvr_colormap_parameter), intent(in)                      &
!!     &                                :: color_param(num_map)
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_map)
!!        type(psf_results), intent(inout) :: psf_dat(num_map)
!!        type(map_rendering_data), intent(inout) :: map_data(num_map)
!!        type(pvr_image_type), intent(inout) :: map_rgb(num_map)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
      module multi_map_projections
!
      use calypso_mpi
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_solver_SR
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
      use t_map_rendering_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_multi_map_projections(num_map, view_param,        &
     &          psf_mesh, psf_dat, map_data, map_rgb, SR_sig)
!
      use m_elapsed_labels_4_VIZ
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      use collect_psf_mesh_field
!
      integer(kind= kint), intent(in) :: num_map
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(in):: view_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(map_rendering_data), intent(inout) :: map_data(num_map)
      type(pvr_image_type), intent(inout) :: map_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_map
!
!
      do i_map = 1, num_map
        map_rgb(i_map)%irank_image_file = mod(i_map,nprocs)
        call alloc_pvr_image_array(view_param(i_map)%n_pvr_pixel,       &
     &                             map_rgb(i_map))
        call init_map_rendering_data(view_param(i_map),                 &
     &                               map_rgb(i_map), map_data(i_map))
      end do
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+1)
      do i_map = 1, num_map
        call init_merge_psf_mesh                                        &
     &     (map_rgb(i_map)%irank_image_file, psf_mesh(i_map),           &
     &      psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,             &
     &      psf_dat(i_map)%psf_phys, SR_sig)
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+1)
!
      end subroutine init_multi_map_projections
!
!  ---------------------------------------------------------------------
!
      subroutine s_multi_map_projections                                &
     &         (num_map, istep_psf, time_d, psf_mesh, view_param, color_param,      &
     &          cbar_param, psf_dat, map_data, map_rgb, SR_sig)
!
      use m_elapsed_labels_4_VIZ
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      use collect_psf_mesh_field
      use xyz_plane_rendering
      use draw_pixels_on_map
      use draw_pvr_colorbar
      use write_PVR_image
!
      integer(kind= kint), intent(in) :: num_map
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(in):: view_param(num_map)
      type(pvr_colormap_parameter), intent(in)                          &
     &                                :: color_param(num_map)
      type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(map_rendering_data), intent(inout) :: map_data(num_map)
      type(pvr_image_type), intent(inout) :: map_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      type(pvr_image_type) :: map_rgb2
      integer(kind= kint) :: i_map
!
!
      do i_map = 1, num_map
        if(psf_mesh(i_map)%node%numnod .eq. 0) cycle
!
        map_rgb2%irank_image_file = my_rank
        call alloc_pvr_image_array(view_param(i_map)%n_pvr_pixel,       &
     &                             map_rgb2)
        if(map_data(i_map)%iflag_2d_projection_mode                     &
     &                              .eq. iflag_aitoff) then
          call aitoff_projection_rendering                              &
     &       (time_d, psf_mesh(i_map)%node, psf_mesh(i_map)%patch,      &
     &        psf_mesh(i_map)%field, color_param(i_map),                &
     &        map_data(i_map), map_rgb2)
        else
          call s_xyz_plane_rendering                                    &
     &       (time_d, psf_mesh(i_map)%node, psf_mesh(i_map)%patch,      &
     &        psf_mesh(i_map)%field, color_param(i_map),                &
     &        map_data(i_map), map_rgb2)
        end if
!
        call fill_background                                            &
     &     (map_rgb2%num_pixels(1), map_rgb2%num_pixels(2),             &
     &      color_param(i_map)%bg_rgba_real, map_rgb2%rgba_real_gl)
        if(cbar_param(i_map)%flag_pvr_colorbar) then
          call set_pvr_colorbar                                         &
     &       (map_rgb2%num_pixel_xy, map_rgb2%num_pixels,               &
     &        color_param(i_map), cbar_param(i_map),                    &
     &        map_rgb2%rgba_real_gl(1,1))
        end if
        if(cbar_param(i_map)%flag_draw_time) then
          call set_pvr_timelabel(time_d%time,                           &
     &        map_rgb2%num_pixel_xy, map_rgb2%num_pixels,               &
     &        cbar_param(i_map), map_rgb2%rgba_real_gl(1,1))
        end if
!
        call sel_write_pvr_image_file(istep_psf, my_rank, map_rgb2)
        call dealloc_pvr_image_array(map_rgb2)
      end do
!
!
!
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+1)
      do i_map = 1, num_map
        call collect_psf_scalar(map_rgb(i_map)%irank_image_file, ione,  &
     &      psf_mesh(i_map)%node, psf_mesh(i_map)%field,                &
     &      psf_dat(i_map)%psf_phys%d_fld(1,1), SR_sig)
        call collect_psf_scalar(map_rgb(i_map)%irank_image_file, itwo,  &
     &      psf_mesh(i_map)%node, psf_mesh(i_map)%field,                &
     &      psf_dat(i_map)%psf_phys%d_fld(1,2), SR_sig)
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+1)
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+2)
      do i_map = 1, num_map
        if(my_rank .ne. map_rgb(i_map)%irank_image_file) cycle
        if(map_data(i_map)%iflag_2d_projection_mode                     &
     &                              .eq. iflag_aitoff) then
          call aitoff_projection_rendering                              &
     &       (time_d, psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,   &
     &        psf_dat(i_map)%psf_phys, color_param(i_map),              &
     &        map_data(i_map), map_rgb(i_map))
        else
          call s_xyz_plane_rendering                                    &
     &       (time_d, psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,   &
     &        psf_dat(i_map)%psf_phys, color_param(i_map),              &
     &        map_data(i_map), map_rgb(i_map))
        end if
!
        call fill_background                                            &
     &   (map_rgb(i_map)%num_pixels(1), map_rgb(i_map)%num_pixels(2),   &
     &    color_param(i_map)%bg_rgba_real, map_rgb(i_map)%rgba_real_gl)
        if(cbar_param(i_map)%flag_pvr_colorbar) then
          call set_pvr_colorbar                                         &
     &       (map_rgb(i_map)%num_pixel_xy, map_rgb(i_map)%num_pixels,   &
     &        color_param(i_map), cbar_param(i_map),                    &
     &        map_rgb(i_map)%rgba_real_gl(1,1))
        end if
        if(cbar_param(i_map)%flag_draw_time) then
          call set_pvr_timelabel(time_d%time,                           &
     &        map_rgb(i_map)%num_pixel_xy, map_rgb(i_map)%num_pixels,   &
     &        cbar_param(i_map), map_rgb(i_map)%rgba_real_gl(1,1))
        end if
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+2)
!
      if(iflag_MAP_time) call start_elapsed_time(ist_elapsed_MAP+3)
      do i_map = 1, num_map
        call sel_write_pvr_image_file(istep_psf, -1, map_rgb(i_map))
      end do
      if(iflag_MAP_time) call end_elapsed_time(ist_elapsed_MAP+3)
!
      end subroutine s_multi_map_projections
!
!  ---------------------------------------------------------------------
!
      end module multi_map_projections
