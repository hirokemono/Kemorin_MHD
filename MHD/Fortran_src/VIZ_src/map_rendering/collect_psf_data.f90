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
      integer(kind= kint), intent(in) :: num_map
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(in):: view_param(num_map)
!
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_psf
!
!
      do i_psf = 1, num_map
        pvr_rgb(i_psf)%irank_image_file = mod(i_psf,nprocs)
        call merge_write_psf_mesh                                       &
     &     (pvr_rgb(i_psf)%irank_image_file, psf_mesh(i_psf),           &
     &      psf_dat(i_psf)%psf_nod, psf_dat(i_psf)%psf_ele,             &
     &      psf_dat(i_psf)%psf_phys, SR_sig)
        call alloc_pvr_image_array(view_param(i_psf)%n_pvr_pixel,       &
     &                             pvr_rgb(i_psf))
      end do
!
      end subroutine output_map_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_file(num_map, istep_psf, time_d,            &
     &          psf_mesh, view_param, color_param, cbar_param,          &
     &          t_IO, psf_dat, pvr_rgb, SR_sig)
!
      use t_psf_patch_data
      use t_psf_results
      use t_pvr_image_array
!
      integer(kind= kint), intent(in) :: num_map
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: psf_mesh(num_map)
      type(pvr_view_parameter), intent(in):: view_param(num_map)
      type(pvr_colormap_parameter), intent(in) :: color_param(num_map)
      type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_map)
!
      type(time_data), intent(inout) :: t_IO
      type(psf_results), intent(inout) :: psf_dat(num_map)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_map)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_psf
!
!
      call copy_time_step_size_data(time_d, t_IO)
!
      do i_psf = 1, num_map
        call merge_write_psf_file(istep_psf, psf_mesh(i_psf), t_IO,     &
     &      psf_dat(i_psf)%psf_nod, psf_dat(i_psf)%psf_ele,             &
     &      psf_dat(i_psf)%psf_phys, view_param(i_psf),                 &
     &      color_param(i_psf), cbar_param(i_psf),                      &
     &      pvr_rgb(i_psf), SR_sig)
      end do
!
      end subroutine output_map_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine merge_write_psf_mesh(irank_draw, psf_mesh,             &
     &          psf_nod, psf_ele, psf_phys, SR_sig)
!
      use t_psf_patch_data
      use t_file_IO_parameter
!
      use append_phys_data
      use cal_mesh_position
      use set_ucd_data_to_type
      use ucd_IO_select
!
      integer, intent(in) :: irank_draw
      type(psf_local_data), intent(in) :: psf_mesh
      type(node_data), intent(inout) ::    psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(phys_data), intent(inout) ::    psf_phys
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: i
!
      psf_nod%numnod = 0
      psf_ele%numele = 0
      psf_ele%nnod_4_ele = psf_mesh%patch%nnod_4_ele
      if(my_rank .eq. irank_draw) then
        psf_nod%numnod = int(psf_mesh%node%istack_internod(nprocs))
        psf_ele%numele = int(psf_mesh%patch%istack_interele(nprocs))
      end if
      psf_nod%internal_node = psf_nod%numnod
!
      call alloc_node_geometry_w_sph(psf_nod)
      call collect_psf_node(irank_draw, psf_mesh%node,                  &
     &                      psf_nod%xx, SR_sig)
      call set_spherical_position(psf_nod)
!
      call copy_field_name(psf_mesh%field, psf_phys)
      call alloc_phys_data(psf_nod%numnod, psf_phys)
      call calypso_mpi_barrier
!
!$omp parallel do
      do i = 1, psf_nod%numnod
        psf_nod%inod_global(i) = i
      end do
!$omp end parallel do
!
!
      call alloc_element_types(psf_ele)
      psf_ele%first_ele_type = psf_mesh%patch%first_ele_type
!$omp parallel do
      do i = 1, psf_ele%numele
        psf_ele%iele_global(i) = i
        psf_ele%elmtyp(i) = psf_mesh%patch%elmtyp(1)
        psf_ele%nodelm(i) = psf_mesh%patch%nodelm(1)
      end do
!$omp end parallel do
!
      call alloc_ele_connectivity(psf_ele)
      call collect_psf_element(irank_draw, psf_mesh%patch,              &
     &                         psf_ele%ie, SR_sig)
      call calypso_mpi_barrier
!
      end subroutine merge_write_psf_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine merge_write_psf_file                                   &
     &         (istep_psf, psf_mesh, t_IO, psf_nod, psf_ele,            &
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
      use write_PVR_image
      use set_parallel_file_name
      use draw_pvr_colorbar
!
      integer(kind = kint), intent(in) :: istep_psf
      type(psf_local_data), intent(in) :: psf_mesh
      type(time_data), intent(in) :: t_IO
      type(pvr_view_parameter), intent(in):: view_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      type(phys_data), intent(inout) :: psf_phys
      type(node_data), intent(inout) :: psf_nod
      type(element_data), intent(inout) :: psf_ele
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
      call collect_psf_scalar(pvr_rgb%irank_image_file, ione,           &
     &    psf_mesh%node, psf_mesh%field, psf_phys%d_fld(1,1), SR_sig)
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
      call set_scalar_on_map_image                                      &
     &   (pvr_rgb%irank_image_file, psf_nod, psf_ele, psf_phys,         &
     &    xmin_frame, xmax_frame, ymin_frame, ymax_frame,               &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, d_map, pvr_rgb%rgba_real_gl(1,1),       &
     &    map_e1)
      call map_value_to_rgb(pvr_rgb%irank_image_file, color_param,      &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, d_map, pvr_rgb%rgba_real_gl)
!
        call draw_aitoff_map_zeroline(pvr_rgb%irank_image_file,         &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, d_map, pvr_rgb%rgba_real_gl)
      call dealloc_map_patch_from_1patch(map_e1)
!
!      if(cbar_param%flag_draw_tangent_cylinder) then
        pi = four*atan(one)
        theta_ref = asin(cbar_param%tangent_cylinder_radius(2)          &
     &                 / cbar_param%tangent_cylinder_radius(1))
        call draw_aitoff_lat_line(pvr_rgb%irank_image_file,             &
     &      xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      theta_ref, cbar_param%tangent_cylinder_rgba,                &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
        call draw_aitoff_lat_line(pvr_rgb%irank_image_file,             &
     &      xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      (pi-theta_ref), cbar_param%tangent_cylinder_rgba,           &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
!      end if
!
      if(cbar_param%flag_draw_mapgrid) then
        call draw_aitoff_map_frame(pvr_rgb%irank_image_file,            &
     &      xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
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
     &     (t_IO%time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,        &
     &      cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      call sel_write_pvr_image_file(istep_psf, -1, pvr_rgb)
      deallocate(d_map)
!
      end subroutine merge_write_psf_file
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_scalar(irank_draw, ifld_img, node, field,  &
     &                              d_img, SR_sig)
!
      use collect_SR_N
!
      integer, intent(in) :: irank_draw
      integer(kind = kint) , intent(in) :: ifld_img
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: field
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_img(node%istack_internod(nprocs))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: nnod
!
!
      nnod = int(node%istack_internod(my_rank+1)                        &
     &          - node%istack_internod(my_rank))
      call collect_send_recv_N(irank_draw, n_scalar, nnod,              &
     &                         field%d_fld(1,ifld_img),                 &
     &                         node%istack_internod, d_img(1), SR_sig)
!
      end subroutine collect_psf_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_node(irank_draw, node, xx_out, SR_sig)
!
      use m_phys_constants
      use collect_SR_N
!
      integer, intent(in) :: irank_draw
      type(node_data), intent(in) :: node
!
      real(kind = kreal), intent(inout)                                 &
     &           :: xx_out(node%istack_internod(nprocs),n_vector)
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      integer(kind = kint) :: nnod, nd
!
      nnod = int(node%istack_internod(my_rank+1)                        &
     &          - node%istack_internod(my_rank))
      do nd = 1, n_vector
        call collect_send_recv_N(irank_draw, ione, nnod,                &
     &                           node%xx(1,nd), node%istack_internod,   &
     &                           xx_out(1,nd), SR_sig)
      end do
!
      end subroutine collect_psf_node
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_element(irank_draw, ele, ie_out, SR_sig)
!
      use m_geometry_constants
      use t_solver_SR_int
      use collect_SR_int
!
      integer, intent(in) :: irank_draw
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(inout)                               &
     &              :: ie_out(ele%istack_interele(nprocs),num_triangle)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: nele, nd
!
!
      nele = int(ele%istack_interele(my_rank+1)                         &
     &           - ele%istack_interele(my_rank))
      do nd = 1, num_triangle
        call collect_send_recv_int(irank_draw, ione, nele,              &
     &                             ele%ie(1,nd), ele%istack_interele,   &
     &                             ie_out(1,nd), SR_sig)
      end do
!
      end subroutine collect_psf_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_map_image                                &
     &         (irank_draw, psf_nod, psf_ele, psf_phys,                 &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!
      use t_geometry_data
      use t_phys_data
      use t_map_patch_from_1patch
      use map_patch_from_1patch
!
      integer, intent(in) :: irank_draw
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: d_map(npix)
      real(kind = kreal), intent(inout) :: rgba(4,npix)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, i, ix, iy, i_img
      integer(kind = kint) :: ix_min, ix_max
      integer(kind = kint) :: iy_min, iy_mid, iy_max
      integer(kind = kint) :: k_ymin, k_ymid, k_ymax
      integer(kind = kint) :: kmin, kmax
      real(kind = kreal) :: x(2), d(2)
      real(kind = kreal) :: ratio_ymid, ratio_ymax, ratio_x
!
!
      if(my_rank .ne. irank_draw) return
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, psf_phys%d_fld(1,1), map_e%n_map_patch,               &
     &      map_e%x_map_patch, map_e%d_map_patch)
        do i = 1, map_e%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (map_e%x_map_patch(1,1,i), map_e%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e%rtp_map_patch(1,1,i),              &
     &                         map_e%xy_map(1,1,i))
        end do
!
        do i = 1, map_e%n_map_patch
          call find_map_path_orientation(map_e%xy_map(1,1,i),           &
     &                                   k_ymin, k_ymid, k_ymax)
!
          iy_min = int(1 + dble(nypixel-1)                              &
     &                    * (map_e%xy_map(2,k_ymin,i) - ymin_frame)     &
     &                      / (ymax_frame - ymin_frame))
          iy_mid = int(1 + dble(nypixel-1)                              &
     &                    * (map_e%xy_map(2,k_ymid,i) - ymin_frame)     &
     &                      / (ymax_frame - ymin_frame))
          iy_max = int(1 + dble(nypixel-1)                              &
     &                    * (map_e%xy_map(2,k_ymax,i) - ymin_frame)     &
     &                      / (ymax_frame - ymin_frame))
          do iy = iy_min, iy_mid
            if(iy_max.eq.iy_min .or. iy_mid.eq.iy_min) then
              x(1) = map_e%xy_map(1,k_ymin,i)
              x(2) = map_e%xy_map(1,k_ymid,i)
              d(1) = map_e%d_map_patch(k_ymin,1,i)
              d(2) = map_e%d_map_patch(k_ymid,1,i)
            else
              ratio_ymid = dble(iy-iy_min) / dble(iy_mid-iy_min)
              ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
              x(1) = (one-ratio_ymid) * map_e%xy_map(1,k_ymin,i)        &
     &                  + ratio_ymid *  map_e%xy_map(1,k_ymid,i)
              x(2) = (one-ratio_ymax) * map_e%xy_map(1,k_ymin,i)        &
     &                  + ratio_ymax *  map_e%xy_map(1,k_ymax,i)
              d(1) = (one-ratio_ymid) * map_e%d_map_patch(k_ymin,1,i)   &
     &                  + ratio_ymid *  map_e%d_map_patch(k_ymid,1,i)
              d(2) = (one-ratio_ymax) * map_e%d_map_patch(k_ymin,1,i)   &
     &                  + ratio_ymax *  map_e%d_map_patch(k_ymax,1,i)
            end if
            if(x(1) .le. x(2)) then
              kmin = 1
              kmax = 2
            else
              kmin = 2
              kmax = 1
            end if
            ix_min = int(1 + dble(nxpixel-1)*(x(kmin) - xmin_frame)     &
     &                      / (xmax_frame - xmin_frame))
            ix_max = int(1 + dble(nxpixel-1)*(x(kmax) - xmin_frame)     &
     &                      / (xmax_frame - xmin_frame))
!
            i_img = ix_min + (iy-1) * nxpixel
            d_map(i_img) =  d(kmin)
            rgba(4,i_img) = one
!
            do ix = ix_min+1, ix_max
              i_img = ix + (iy-1) * nxpixel
              ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
              d_map(i_img) = (one - ratio_x) * d(kmin)                  &
     &                            + ratio_x *  d(kmax)
              rgba(4,i_img) = one
            end do
          end do
!
          do iy = iy_mid+1, iy_max
            if(iy_max.eq.iy_min) then
              x(1) = map_e%xy_map(1,k_ymid,i)
              x(2) = map_e%xy_map(1,k_ymax,i)
              d(1) = map_e%d_map_patch(k_ymid,1,i)
              d(2) = map_e%d_map_patch(k_ymax,1,i)
            else
              ratio_ymid = dble(iy-iy_mid) / dble(iy_max-iy_mid)
              ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
              x(1) = (one-ratio_ymid) * map_e%xy_map(1,k_ymid,i)        &
     &                  + ratio_ymid *  map_e%xy_map(1,k_ymax,i)
              x(2) = (one-ratio_ymax) * map_e%xy_map(1,k_ymin,i)        &
     &                  + ratio_ymax *  map_e%xy_map(1,k_ymax,i)
              d(1) = (one-ratio_ymid) * map_e%d_map_patch(k_ymid,1,i)   &
     &                  + ratio_ymid *  map_e%d_map_patch(k_ymax,1,i)
              d(2) = (one-ratio_ymax) * map_e%d_map_patch(k_ymin,1,i)   &
     &                  + ratio_ymax *  map_e%d_map_patch(k_ymax,1,i)
            end if
            if(x(1) .le. x(2)) then
              kmin = 1
              kmax = 2
            else
              kmin = 2
              kmax = 1
            end if
            ix_min = int(1 + dble(nxpixel-1)*(x(kmin) - xmin_frame)     &
     &                      / (xmax_frame - xmin_frame))
            ix_max = int(1 + dble(nxpixel-1)*(x(kmax) - xmin_frame)     &
     &                      / (xmax_frame - xmin_frame))
!
            i_img = ix_min + (iy-1) * nxpixel
            d_map(i_img) =  d(kmin)
            rgba(4,i_img) = one
!
            do ix = ix_min+1, ix_max
              i_img = ix + (iy-1) * nxpixel
              ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
              d_map(i_img) = (one - ratio_x) * d(kmin)                  &
     &                            + ratio_x *  d(kmax)
              rgba(4,i_img) = one
            end do
          end do
        end do
      end do
!
      end subroutine set_scalar_on_map_image
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_zeroline                               &
     &         (irank_draw, nxpixel, nypixel, npix, d_map, rgba)
!
      integer, intent(in) :: irank_draw
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      real(kind = kreal), intent(in) :: d_map(npix)
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: i_img, i, j
!
!
      if(my_rank .ne. irank_draw) return
!$omp parallel do private(i,j,i_img)
      do j = 1, nypixel
        do i = 2, nxpixel-1
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
!
          if((d_map(i_img-1)*d_map(i_img+1)) .le. 0) then
            rgba(1:4,i_img) = zero
            rgba(4,  i_img) = one
          end if
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(i,j,i_img)
      do j = 2, nypixel-2
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
!
          if((d_map(i_img-nxpixel)*d_map(i_img+nxpixel)) .le. 0) then
            rgba(1:4,i_img) = zero
            rgba(4,  i_img) = one
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_map_zeroline
!
!  ---------------------------------------------------------------------
!
      subroutine map_value_to_rgb(irank_draw, color_param,              &
     &                            nxpixel, nypixel, npix, d_map, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      integer, intent(in) :: irank_draw
      type(pvr_colormap_parameter), intent(in) :: color_param
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      real(kind = kreal), intent(in) :: d_map(npix)
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: i_img, i, j
!
!
      if(my_rank .ne. irank_draw) return
!$omp parallel do private(i,j,i_img)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
!
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_map(i_img), rgba(1,i_img))
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_rgb
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_frame(irank_draw,                      &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, rgba)
!
      use aitoff
!
      integer, intent(in) :: irank_draw
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: ii, jj
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, x_pix2, y_pix1, y_pix2
      real(kind = kreal) :: phi_ref, theta_ref, pi
      real(kind = kreal) :: theta(4), phi(4)
!
!
      if(my_rank .ne. irank_draw) return
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,i_img,  &
!$omp&                    phi_ref,theta_ref,theta,phi)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          if(mod(j,6).ge.3 .and. mod(i,6).lt.3) cycle
          if(mod(j,6).lt.3 .and. mod(i,6).ge.3) cycle
!
!
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          x_pix2 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i) /   dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          y_pix2 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          call reverse_aitoff(x_pix2, y_pix1, theta(2), phi(2))
          call reverse_aitoff(x_pix1, y_pix2, theta(3), phi(3))
          call reverse_aitoff(x_pix2, y_pix2, theta(4), phi(4))
!
          if(    (theta(1)*theta(2)) .le. 0.0d0                         &
     &      .or. (theta(1)*theta(3)) .le. 0.0d0                         &
     &      .or. (theta(1)*theta(4)) .le. 0.0d0) then
            i_img = i + (j-1) * nxpixel
            rgba(1:4,i_img) = one
            rgba(4,  i_img) = one
          end if
!
          if(theta(1) .le. zero) cycle
          if(theta(2) .le. zero) cycle
          if(theta(3) .le. zero) cycle
          if(theta(4) .le. zero) cycle
          do ii = 1, 5
            phi_ref = pi * dble(ii-3) / 3.0d0
            if(     (phi(1)-phi_ref)*(phi(2)-phi_ref) .le. zero         &
     &         .or. (phi(1)-phi_ref)*(phi(3)-phi_ref) .le. zero         &
     &         .or. (phi(1)-phi_ref)*(phi(4)-phi_ref) .le. zero) then
              i_img = i + (j-1) * nxpixel
              rgba(1:4,i_img) = zero
              rgba(4,  i_img) = one
!              rgba(1:4,i_img+1,j) = zero
!              rgba(4,  i_img+1,j) = one
            end if
          end do
!
          do jj = 1, 5
            theta_ref = pi * dble(jj) / 6.0d0
            if(    (theta(1)-theta_ref)*(theta(2)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(3)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(4)-theta_ref) .le. zero) &
     &         then
              i_img = i + (j-1) * nxpixel
              rgba(1:4,i_img) = zero
              rgba(4,  i_img) = one
!              rgba(1:4,i_img+nxpixel) = zero
!              rgba(4,  i_img+nxpixel) = one
            end if
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_map_frame
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_lat_line(irank_draw,                       &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          theta_ref, rgba_in, nxpixel, nypixel, npix, rgba)
!
      use aitoff
!
      integer, intent(in) :: irank_draw
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      real(kind = kreal), intent(in) :: theta_ref
      real(kind = kreal), intent(in) :: rgba_in(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: ii, jj
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, x_pix2, y_pix1, y_pix2
      real(kind = kreal) :: phi_ref, pi
      real(kind = kreal) :: theta(4), phi(4)
!
!
      if(my_rank .ne. irank_draw) return
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,i_img,  &
!$omp&                    phi_ref,theta,phi)
      do j = 2, nypixel-1
        do i = 2, nxpixel-1
          if(mod(i,12).ge.6) cycle
!
!
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          x_pix2 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i) /   dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          y_pix2 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          call reverse_aitoff(x_pix2, y_pix1, theta(2), phi(2))
          call reverse_aitoff(x_pix1, y_pix2, theta(3), phi(3))
          call reverse_aitoff(x_pix2, y_pix2, theta(4), phi(4))
!
          if(    (theta(1)-theta_ref)*(theta(2)-theta_ref) .le. zero    &
     &      .or. (theta(1)-theta_ref)*(theta(3)-theta_ref) .le. zero    &
     &      .or. (theta(1)-theta_ref)*(theta(4)-theta_ref) .le. zero)   &
     &       then
             i_img = i + (j-1) * nxpixel
             rgba(1:3,i_img) = rgba_in(1:3)
             rgba(4,  i_img) = one
             if(rgba(4,i_img-nxpixel).gt.0) then
               rgba(1:3,i_img-nxpixel) = rgba_in(1:3)
             end if
             if(rgba(4,i_img+nxpixel).gt.0) then
               rgba(1:3,i_img+nxpixel) = rgba_in(1:3)
             end if
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_lat_line
!
!  ---------------------------------------------------------------------
!
      end module collect_psf_data
