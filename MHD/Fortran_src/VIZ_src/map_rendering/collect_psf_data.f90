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
      subroutine output_map_mesh(num_psf, psf_mesh, psf_file_IO,        &
     &                           psf_dat, psf_out, SR_sig)
!
      use t_psf_patch_data
      use t_psf_results
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
!
      integer(kind= kint), intent(in) :: num_psf
      type(psf_local_data), intent(in) :: psf_mesh(num_psf)
!
      type(field_IO_params), intent(inout) :: psf_file_IO(num_psf)
      type(psf_results), intent(inout) :: psf_dat(num_psf)
      type(ucd_data), intent(inout) :: psf_out(num_psf)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_psf
      integer :: irank_draw
!
!
      do i_psf = 1, num_psf
        irank_draw = mod(i_psf,nprocs)
        call merge_write_psf_mesh                                       &
     &     (irank_draw, psf_mesh(i_psf), psf_file_IO(i_psf),            &
     &      psf_dat(i_psf)%psf_nod, psf_dat(i_psf)%psf_ele,             &
     &      psf_dat(i_psf)%psf_phys, psf_out(i_psf), SR_sig)
      end do
!
      end subroutine output_map_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_map_file(num_psf, psf_file_IO, istep_psf,       &
     &          time_d, psf_mesh, t_IO, psf_dat, psf_out,               &
     &          view_param, color_param, cbar_param, SR_sig)
!
      use t_psf_patch_data
      use t_psf_results
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
!
      integer(kind= kint), intent(in) :: num_psf
      integer(kind= kint), intent(in) ::  istep_psf
      type(time_data), intent(in) :: time_d
      type(psf_local_data), intent(in) :: psf_mesh(num_psf)
      type(field_IO_params), intent(in) :: psf_file_IO(num_psf)
      type(pvr_view_parameter), intent(in):: view_param(num_psf)
      type(pvr_colormap_parameter), intent(in) :: color_param(num_psf)
      type(pvr_colorbar_parameter), intent(in) :: cbar_param(num_psf)
!
      type(time_data), intent(inout) :: t_IO
      type(psf_results), intent(inout) :: psf_dat(num_psf)
      type(ucd_data), intent(inout) :: psf_out(num_psf)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind= kint) :: i_psf
      integer :: irank_draw
!
!
      call copy_time_step_size_data(time_d, t_IO)
!
      do i_psf = 1, num_psf
        irank_draw = mod(i_psf,nprocs)
        call merge_write_psf_file(irank_draw, istep_psf,                &
     &      psf_file_IO(i_psf), psf_mesh(i_psf), t_IO,                  &
     &      psf_dat(i_psf)%psf_nod, psf_dat(i_psf)%psf_ele,             &
     &      psf_dat(i_psf)%psf_phys, psf_out(i_psf), view_param(i_psf), &
     &      color_param(i_psf), cbar_param(i_psf), SR_sig)
      end do
!
      end subroutine output_map_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine merge_write_psf_mesh(irank_draw, psf_mesh,             &
     &          psf_file_IO, psf_nod, psf_ele, psf_phys, psf_ucd,       &
     &          SR_sig)
!
      use t_psf_patch_data
      use t_ucd_data
      use t_file_IO_parameter
!
      use append_phys_data
      use cal_mesh_position
      use set_ucd_data_to_type
      use ucd_IO_select
!
      integer, intent(in) :: irank_draw
      type(psf_local_data), intent(in) :: psf_mesh
      type(field_IO_params), intent(inout) :: psf_file_IO
      type(node_data), intent(inout) ::    psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(phys_data), intent(inout) ::    psf_phys
      type(ucd_data), intent(inout) ::     psf_ucd
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
      if(my_rank .eq. irank_draw) then
        call link_node_data_2_ucd(psf_nod, psf_ucd)
        call link_ele_data_2_ucd(psf_ele, psf_ucd)
        call link_field_data_to_ucd(psf_phys, psf_ucd)
!
        if(psf_file_IO%iflag_format .gt. iflag_single) then
          psf_file_IO%iflag_format = psf_file_IO%iflag_format           &
     &                              - iflag_single
        end if
        call sel_write_grd_file(-1, psf_file_IO, psf_ucd)
      end if
!
!      call dealloc_ele_connect(psf_ele)
!      call dealloc_node_geometry_w_sph(psf_nod)
!
      end subroutine merge_write_psf_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine merge_write_psf_file(irank_draw, istep_psf,            &
     &          psf_file_IO, psf_mesh, t_IO, psf_nod, psf_ele,          &
     &          psf_phys, psf_ucd, view_param, color_param, cbar_param, &
     &          SR_sig)
!
      use t_psf_patch_data
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
      use t_map_patch_from_1patch
      use map_patch_from_1patch
!
      use set_ucd_data_to_type
      use ucd_IO_select
      use aitoff
!
      use convert_real_rgb_2_bite
      use output_image_sel_4_png
      use set_parallel_file_name
      use set_color_4_pvr
      use draw_pvr_colorbar
!
      integer, intent(in) :: irank_draw
      integer(kind = kint), intent(in) :: istep_psf
      type(psf_local_data), intent(in) :: psf_mesh
      type(field_IO_params), intent(in) :: psf_file_IO
      type(time_data), intent(in) :: t_IO
      type(pvr_view_parameter), intent(in):: view_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      type(phys_data), intent(inout) :: psf_phys
      type(node_data), intent(inout) :: psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(ucd_data), intent(inout) :: psf_ucd
      type(send_recv_status), intent(inout) :: SR_sig
!
      type(map_patches_for_1patch) :: map_e1
      integer(kind = kint) :: k_ymin, k_ymid, k_ymax, k_xmin, k_xmax
      integer(kind = kint) :: ix_map, iy_map, inod_map
!
      real(kind= kreal), parameter :: xframe = 2.4, yframe = 1.8
      real(kind= kreal), parameter :: xmin_frame = -xframe
      real(kind= kreal), parameter :: xmax_frame =  xframe
      real(kind= kreal), parameter :: ymin_frame = -yframe
      real(kind= kreal), parameter :: ymax_frame =  yframe
!
      integer(kind = kint) :: nxpixel, nypixel, npix
      real(kind = kreal), allocatable :: d_map(:,:)
      real(kind = kreal), allocatable :: rgba(:,:,:)
      character(len = 1), allocatable :: cimage(:,:)
!
      integer(kind = kint) :: ii, jj
      integer(kind = kint) :: i_img, iele, i, j, k1, ix, iy
      integer(kind = kint) :: ix_min, ix_max
      integer(kind = kint) :: iy_min, iy_mid, iy_max
      real(kind = kreal) :: x1, x2, x_min, x_max, x_mid
      real(kind = kreal) :: d1, d2, d_min, d_max, d_mid
      real(kind = kreal) :: ar, ratio_ymid, ratio_ymax, ratio_x
      real(kind = kreal) :: x_pix1, x_pix2, y_pix1, y_pix2
      real(kind = kreal) :: phi_ref, theta_ref, pi
      real(kind = kreal) :: theta(4), phi(4)
!
!
      call collect_psf_scalar(irank_draw, ione, psf_mesh%node,          &
     &    psf_mesh%field, psf_phys%d_fld(1,1), SR_sig)
!
      if(my_rank .eq. irank_draw) then
        call sel_write_ucd_file                                         &
     &     (-1, istep_psf, psf_file_IO, t_IO, psf_ucd)
      end if
!
      if(my_rank .ne. irank_draw) return
!
      nxpixel = view_param%n_pvr_pixel(1)
      nypixel = view_param%n_pvr_pixel(2)
      npix = (nxpixel*nypixel)
      allocate(d_map(nxpixel,nypixel))
      allocate(rgba(4,nxpixel,nypixel))
      allocate(cimage(3,npix))
!$omp parallel workshare
      d_map(1:nxpixel,1:nypixel) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      rgba(1:4,1:nxpixel,1:nypixel) = 0.0d0
!$omp end parallel workshare
!
      call alloc_map_patch_from_1patch(ione, map_e1)
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, psf_phys%d_fld(1,1), map_e1%n_map_patch,              &
     &      map_e1%x_map_patch, map_e1%d_map_patch)
        do i = 1, map_e1%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (map_e1%x_map_patch(1,1,i), map_e1%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e1%rtp_map_patch(1,1,i),             &
     &                         map_e1%xy_map(1,1,i))
        end do
!
        do i = 1, map_e1%n_map_patch
          call find_map_path_orientation(map_e1%xy_map(1,1,i),          &
     &        k_ymin, k_ymid, k_ymax, k_xmin, k_xmax)
!
          iy_min = int(1 + dble(nypixel-1)                              &
     &                    * (map_e1%xy_map(2,k_ymin,i) - ymin_frame)    &
     &                      / (ymax_frame - ymin_frame))
          iy_mid = int(1 + dble(nypixel-1)                              &
     &                    * (map_e1%xy_map(2,k_ymid,i) - ymin_frame)    &
     &                      / (ymax_frame - ymin_frame))
          iy_max = int(1 + dble(nypixel-1)                              &
     &                    * (map_e1%xy_map(2,k_ymax,i) - ymin_frame)    &
     &                      / (ymax_frame - ymin_frame))
          do iy = iy_min, iy_mid
            if(iy_max.eq.iy_min .or. iy_mid.eq.iy_min) then
              x1 = map_e1%xy_map(1,k_ymin,i)
              x2 = map_e1%xy_map(1,k_ymid,i)
              d1 = map_e1%d_map_patch(k_ymin,1,i)
              d2 = map_e1%d_map_patch(k_ymid,1,i)
            else
              ratio_ymid = dble(iy-iy_min) / dble(iy_mid-iy_min)
              ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
              x1 = (one - ratio_ymid) * map_e1%xy_map(1,k_ymin,i)       &
     &                  + ratio_ymid *  map_e1%xy_map(1,k_ymid,i)
              x2 = (one - ratio_ymax) * map_e1%xy_map(1,k_ymin,i)       &
     &                  + ratio_ymax *  map_e1%xy_map(1,k_ymax,i)
              d1 = (one - ratio_ymid) * map_e1%d_map_patch(k_ymin,1,i)  &
     &                  + ratio_ymid *  map_e1%d_map_patch(k_ymid,1,i)
              d2 = (one - ratio_ymax) * map_e1%d_map_patch(k_ymin,1,i)  &
     &                  + ratio_ymax *  map_e1%d_map_patch(k_ymax,1,i)
            end if
            if(x1 .le. x2) then
              x_min = x1
              x_max = x2
              d_min = d1
              d_max = d2
            else
              x_min = x2
              x_max = x1
              d_min = d2
              d_max = d1
            end if
            ix_min = int(1 + dble(nxpixel-1)*(x_min - xmin_frame)       &
     &                      / (xmax_frame - xmin_frame))
            ix_max = int(1 + dble(nxpixel-1)*(x_max - xmin_frame)       &
     &                      / (xmax_frame - xmin_frame))
!
            d_map(ix_min,iy) = d_min
            rgba(4,ix_min,iy) = one
!
            do ix = ix_min+1, ix_max
              ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
              d_map(ix,iy) = (one - ratio_x) * d_min + ratio_x * d_max
              rgba(4,ix,iy) = one
            end do
          end do
!
          do iy = iy_mid+1, iy_max
            if(iy_max.eq.iy_min) then
              x1 = map_e1%xy_map(1,k_ymid,i)
              x2 = map_e1%xy_map(1,k_ymax,i)
              d1 = map_e1%d_map_patch(k_ymid,1,i)
              d2 = map_e1%d_map_patch(k_ymax,1,i)
            else
              ratio_ymid = dble(iy-iy_mid) / dble(iy_max-iy_mid)
              ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
              x1 = (one - ratio_ymid) * map_e1%xy_map(1,k_ymid,i)       &
     &                  + ratio_ymid *  map_e1%xy_map(1,k_ymax,i)
              x2 = (one - ratio_ymax) * map_e1%xy_map(1,k_ymin,i)       &
     &                  + ratio_ymax *  map_e1%xy_map(1,k_ymax,i)
              d1 = (one - ratio_ymid) * map_e1%d_map_patch(k_ymid,1,i)  &
     &                  + ratio_ymid *  map_e1%d_map_patch(k_ymax,1,i)
              d2 = (one - ratio_ymax) * map_e1%d_map_patch(k_ymin,1,i)  &
     &                  + ratio_ymax *  map_e1%d_map_patch(k_ymax,1,i)
            end if
            if(x1 .le. x2) then
              x_min = x1
              x_max = x2
              d_min = d1
              d_max = d2
            else
              x_min = x2
              x_max = x1
              d_min = d2
              d_max = d1
            end if
            ix_min = int(1 + dble(nxpixel-1)*(x_min - xmin_frame)       &
     &                      / (xmax_frame - xmin_frame))
            ix_max = int(1 + dble(nxpixel-1)*(x_max - xmin_frame)       &
     &                      / (xmax_frame - xmin_frame))
!
            d_map(ix_min,iy) = d_min
            rgba(4,ix_min,iy) = one
!
            do ix = ix_min+1, ix_max
              ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
              d_map(ix,iy) = (one - ratio_x) * d_min + ratio_x * d_max
              rgba(4,ix,iy) = one
            end do
          end do
        end do
      end do
!
!$omp parallel do private(i,j)
      do j = 1, nypixel
        do i = 1, nxpixel
          if(rgba(4,i,j) .eq. zero) cycle
!
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_map(i,j), rgba(1,i,j))
        end do
      end do
!$omp end parallel do
!
!
!$omp parallel do private(i,j)
      do j = 1, nypixel
        do i = 2, nxpixel-1
          if(rgba(4,i,j) .eq. zero) cycle
!
          if((d_map(i-1,j)*d_map(i+1,j)) .le. 0) then
            rgba(1:4,i,j) = zero
            rgba(4,  i,j) = one
          end if
        end do
      end do
!$omp end parallel do
!$omp parallel do private(i,j)
      do j = 2, nypixel-2
        do i = 1, nxpixel
          if(rgba(4,i,j) .eq. zero) cycle
!
          if((d_map(i,j-1)*d_map(i,j+2)) .le. 0) then
            rgba(1:4,i,j) = zero
            rgba(4,  i,j) = one
          end if
        end do
      end do
!$omp end parallel do
!
      call dealloc_map_patch_from_1patch(map_e1)
!
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,        &
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
            rgba(1:4,i,j) = one
            rgba(4,  i,j) = one
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
              rgba(1:4,i,j) = zero
              rgba(4,  i,j) = one
!              rgba(1:4,i+1,j) = zero
!              rgba(4,  i+1,j) = one
            end if
          end do
!
          do jj = 1, 5
            theta_ref = pi * dble(jj) / 6.0d0
            if(    (theta(1)-theta_ref)*(theta(2)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(3)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(4)-theta_ref) .le. zero) &
     &         then
              rgba(1:4,i,j) = zero
              rgba(4,  i,j) = one
!              rgba(1:4,i,j+1) = zero
!              rgba(4,  i,j+1) = one
            end if
          end do
        end do
      end do
!$omp end parallel do
!
      if(cbar_param%iflag_pvr_colorbar) then
        call set_pvr_colorbar(npix, view_param%n_pvr_pixel,             &
     &                        color_param, cbar_param, rgba(1,1,1))
      end if
!
      if(cbar_param%iflag_draw_time) then
        call set_pvr_timelabel(t_IO%time, npix,view_param%n_pvr_pixel,  &
     &                         cbar_param, rgba(1,1,1))
      end if
!
!
      call cvt_double_rgba_to_char_rgb(npix, rgba(1,1,1), cimage)
      call sel_output_image_file(psf_file_IO%iflag_format,              &
     &    add_int_suffix(istep_psf, psf_file_IO%file_prefix),           &
     &    nxpixel, nypixel, cimage(1,1))
      deallocate(d_map, rgba, cimage)
!
!      call dealloc_phys_data(psf_phys)
!      call dealloc_phys_name(psf_phys)
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
!
      end module collect_psf_data
