!>@file   collect_psf_data.f90
!!@brief  module collect_psf_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine collect_psf_scalar(irank_draw, ifld_img, node, field,&
!!     &                              d_img, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        integer(kind = kint) , intent(in) :: ifld_img
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: field
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_img(node%istack_internod(nprocs))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_psf_node(irank_draw, node, xx_out, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        type(node_data), intent(in) :: node
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: xx_out(node%istack_internod(nprocs),n_vector)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_psf_element(irank_draw, ele, ie_out, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        type(element_data), intent(in) :: ele
!!        integer(kind = kinds), intent(inout)                          &
!!     &            :: ie_out(node%istack_internod(nprocs),num_triangle)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
      module collect_psf_data
!
      use calypso_mpi
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_solver_SR
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_mesh(num_map, view_param, psf_mesh,         &
     &                           psf_dat, pvr_rgb, SR_sig)
!
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
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_map
!
!
      do i_map = 1, num_map
        pvr_rgb(i_map)%irank_image_file = mod(i_map,nprocs)
        call init_merge_psf_mesh                                        &
     &     (pvr_rgb(i_map)%irank_image_file, psf_mesh(i_map),           &
     &      psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,             &
     &      psf_dat(i_map)%psf_phys, SR_sig)
        call alloc_pvr_image_array(view_param(i_map)%n_pvr_pixel,       &
     &                             pvr_rgb(i_map))
      end do
!
      end subroutine output_map_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_file(num_map, istep_psf, time_d,            &
     &          psf_mesh, view_param, color_param, cbar_param,          &
     &          psf_dat, pvr_rgb, SR_sig)
!
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      use collect_psf_mesh_field
      use write_PVR_image
!
      integer(kind= kint), intent(in) :: num_map
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(in):: view_param(num_map)
      type(pvr_colormap_parameter), intent(in) :: color_param(num_map)
      type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_map
!
!
      do i_map = 1, num_map
        call collect_psf_scalar(pvr_rgb(i_map)%irank_image_file, ione,  &
     &      psf_mesh(i_map)%node, psf_mesh(i_map)%field,                &
     &      psf_dat(i_map)%psf_phys%d_fld(1,1), SR_sig)
      end do
!
      do i_map = 1, num_map
        if(my_rank .ne. pvr_rgb%irank_image_file) cycle
        call merge_write_psf_file(istep_psf, psf_mesh(i_map), time_d,   &
     &      psf_dat(i_map)%psf_nod, psf_dat(i_map)%psf_ele,             &
     &      psf_dat(i_map)%psf_phys, view_param(i_map),                 &
     &      color_param(i_map), cbar_param(i_map),                      &
     &      pvr_rgb(i_map), SR_sig)
!
        call sel_write_pvr_image_file(istep_psf, -1, pvr_rgb(i_map))
      end do
!
      end subroutine output_map_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine merge_write_psf_file                                   &
     &         (istep_psf, psf_mesh, time_d, psf_nod, psf_ele,          &
     &          psf_phys, view_param, color_param, cbar_param,          &
     &          pvr_rgb, SR_sig)
!
      use t_psf_patch_data
      use t_time_data
      use t_file_IO_parameter
      use t_map_patch_from_1patch
      use t_pvr_image_array
!
      use set_ucd_data_to_type
      use ucd_IO_select
!
      use draw_aitoff_map
      use draw_lines_on_map
      use draw_pvr_colorbar
!
      integer(kind = kint), intent(in) :: istep_psf
      type(psf_local_data), intent(in) :: psf_mesh
      type(time_data), intent(in) :: time_d
      type(pvr_view_parameter), intent(in):: view_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(phys_data), intent(in) :: psf_phys
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
!
      type(map_patches_for_1patch) :: map_e1
!
      real(kind= kreal), parameter :: xframe = 2.4, yframe = 1.8
      real(kind= kreal) :: xmin_frame = -xframe
      real(kind= kreal) :: xmax_frame =  xframe
      real(kind= kreal) :: ymin_frame = -yframe
      real(kind= kreal) :: ymax_frame =  yframe
!
      real(kind = kreal), allocatable :: d_map(:)
!
      real(kind = kreal) :: xtmp, ytmp
      real(kind = kreal) :: aspect, pi, theta_ref
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      aspect =  view_param%perspective_xy_ratio
!
      ytmp = xframe / aspect
      xtmp = yframe * aspect
      if(ytmp .le. 1.0) then
        xmin_frame = -xtmp
        xmax_frame =  xtmp
        ymin_frame = -yframe
        ymax_frame =  yframe
      else
        xmin_frame = -xframe
        xmax_frame =  xframe
        ymin_frame = -ytmp
        ymax_frame =  ytmp
      end if
!
      allocate(d_map(pvr_rgb%num_pixel_xy))
!$omp parallel workshare
      d_map(1:pvr_rgb%num_pixel_xy) = 0.0d0
!$omp end parallel workshare
!
      call alloc_map_patch_from_1patch(ione, map_e1)
      call set_scalar_on_map_image(psf_nod, psf_ele, psf_phys,          &
     &    xmin_frame, xmax_frame, ymin_frame, ymax_frame,               &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, d_map, pvr_rgb%rgba_real_gl(1,1),       &
     &    map_e1)
      call map_value_to_rgb                                             &
     &   (color_param, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),    &
     &    pvr_rgb%num_pixel_xy, d_map, pvr_rgb%rgba_real_gl)
!
        call draw_aitoff_map_zeroline                                   &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, d_map, pvr_rgb%rgba_real_gl)
      call dealloc_map_patch_from_1patch(map_e1)
!
!      if(cbar_param%flag_draw_tangent_cylinder) then
        pi = four*atan(one)
        theta_ref = asin(cbar_param%tangent_cylinder_radius(2)          &
     &                 / cbar_param%tangent_cylinder_radius(1))
        call draw_aitoff_lat_line                                       &
     &     (xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      theta_ref, cbar_param%tangent_cylinder_rgba,                &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
        call draw_aitoff_lat_line                                       &
     &     (xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      (pi-theta_ref), cbar_param%tangent_cylinder_rgba,           &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
!      end if
!
      if(cbar_param%flag_draw_mapgrid) then
        call draw_aitoff_map_frame                                      &
     &     (xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
      end if
!
      if(cbar_param%flag_pvr_colorbar) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      if(cbar_param%flag_draw_time) then
        call set_pvr_timelabel                                          &
     &     (time_d%time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,      &
     &      cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
      deallocate(d_map)
!
      end subroutine merge_write_psf_file
!
!  ---------------------------------------------------------------------
!
      end module collect_psf_data
