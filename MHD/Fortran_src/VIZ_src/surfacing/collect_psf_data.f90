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
     &          time_d, psf_mesh, t_IO, psf_dat, psf_out, SR_sig)
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
     &      psf_dat(i_psf)%psf_phys, psf_out(i_psf), SR_sig)
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
     &          psf_phys, psf_ucd, SR_sig)
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
      use calypso_png_file_IO
!
      integer, intent(in) :: irank_draw
      integer(kind = kint), intent(in) :: istep_psf
      type(psf_local_data), intent(in) :: psf_mesh
      type(field_IO_params), intent(in) :: psf_file_IO
      type(time_data), intent(in) :: t_IO
!
      type(phys_data), intent(inout) :: psf_phys
      type(node_data), intent(inout) :: psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(ucd_data), intent(inout) :: psf_ucd
      type(send_recv_status), intent(inout) :: SR_sig
!
      type(map_patches_for_1patch) :: map_e1
      integer(kind = kint) :: k_ymin, k_ymid, k_ymax, k_xmin, k_xmax
      integer(kind = kint) :: ix_map, iy_map, inod_map, npix
!
      integer(kind = kint), parameter :: nxpixel = 3200
      integer(kind = kint), parameter :: nypixel = 2400
      real(kind= kreal), parameter :: xframe = 2.4, yframe = 1.8
      real(kind= kreal), parameter :: xmin_frame = -xframe
      real(kind= kreal), parameter :: xmax_frame =  xframe
      real(kind= kreal), parameter :: ymin_frame = -yframe
      real(kind= kreal), parameter :: ymax_frame =  yframe
!
      real(kind = kreal), allocatable :: rgba(:,:)
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
      real(kind = kreal) :: theta1,theta2,phi1,phi2
!
!
      do i_img = 1, psf_phys%ntot_phys
        call collect_psf_scalar(irank_draw, i_img, psf_mesh%node,       &
     &      psf_mesh%field, psf_phys%d_fld(1,i_img), SR_sig)
      end do
!
      if(my_rank .eq. irank_draw) then
        call sel_write_ucd_file                                         &
     &     (-1, istep_psf, psf_file_IO, t_IO, psf_ucd)
      end if
!
      if(my_rank .ne. irank_draw) return
!
      npix = (nxpixel*nypixel)
      allocate(rgba(4,npix))
      allocate(cimage(3,npix))
      rgba(1:4,1:npix) = 0.0d0
!
      i_img = 1
      call alloc_map_patch_from_1patch(ione, map_e1)
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, psf_phys%d_fld(1,i_img), map_e1%n_map_patch,          &
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
          do k1 = 1, 3
            ix_map = int(1 + dble(nxpixel-1)                            &
     &                      * (map_e1%xy_map(1,k1,i) - xmin_frame)      &
     &                      / (xmax_frame - xmin_frame))
            iy_map = int(1 + dble(nypixel-1)                            &
     &                      * (map_e1%xy_map(2,k1,i) - ymin_frame)      &
     &                      / (ymax_frame - ymin_frame))
!
            ar = sqrt(map_e1%x_map_patch(k1,1,i)**2                     &
     &              + map_e1%x_map_patch(k1,2,i)**2                     &
     &              + map_e1%x_map_patch(k1,3,i)**2)
            if(irank_draw .gt. 1) cycle
            inod_map = ix_map + (iy_map-1) * nxpixel
            rgba(1,inod_map) = map_e1%x_map_patch(k1,1,i) / ar + half
            rgba(2,inod_map) = map_e1%x_map_patch(k1,2,i) / ar + half
            rgba(3,inod_map) = map_e1%x_map_patch(k1,3,i) / ar + half
            rgba(4,inod_map) = one
          end do
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
            if(irank_draw .le. 1) cycle
            d_mid = d_min
            inod_map = ix_min + (iy-1) * nxpixel
            rgba(1,inod_map) = map_e1%x_map_patch(1,1,i) / ar + half
            rgba(2,inod_map) = map_e1%x_map_patch(1,2,i) / ar + half
            rgba(3,inod_map) = map_e1%x_map_patch(1,3,i) / ar + half
            rgba(4,inod_map) = one
!
            do ix = ix_min+1, ix_max
              ratio_x = dble(ix-ix_min) / dble(ix_max-iy_min)
              x_mid = (one - ratio_x) * x_min + ratio_x * x_max
              d_mid = (one - ratio_x) * d_min + ratio_x * d_max
!
              inod_map = ix + (iy-1) * nxpixel
              rgba(1,inod_map) = map_e1%x_map_patch(1,1,i) / ar + half
              rgba(2,inod_map) = map_e1%x_map_patch(1,2,i) / ar + half
              rgba(3,inod_map) = map_e1%x_map_patch(1,3,i) / ar + half
              rgba(4,inod_map) = one
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
            if(irank_draw .le. 1) cycle
            d_mid = d_min
            inod_map = ix_min + (iy-1) * nxpixel
            rgba(1,inod_map) = map_e1%x_map_patch(1,1,i) / ar + half
            rgba(2,inod_map) = map_e1%x_map_patch(1,2,i) / ar + half
            rgba(3,inod_map) = map_e1%x_map_patch(1,3,i) / ar + half
            rgba(4,inod_map) = one
!
            do ix = ix_min+1, ix_max
              ratio_x = dble(ix-ix_min) / dble(ix_max-iy_min)
              x_mid = (one - ratio_x) * x_min + ratio_x * x_max
              d_mid = (one - ratio_x) * d_min + ratio_x * d_max
!
              inod_map = ix + (iy-1) * nxpixel
              rgba(1,inod_map) = map_e1%x_map_patch(1,1,i) / ar + half
              rgba(2,inod_map) = map_e1%x_map_patch(1,2,i) / ar + half
              rgba(3,inod_map) = map_e1%x_map_patch(1,3,i) / ar + half
              rgba(4,inod_map) = one
            end do
          end do
        end do
      end do
      call dealloc_map_patch_from_1patch(map_e1)
!
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,        &
!$omp&        inod_map,phi_ref,theta_ref,theta1,theta2,phi1,phi2)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          x_pix2 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i) /   dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          y_pix2 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta1, phi1)
          call reverse_aitoff(x_pix2, y_pix1, theta2, phi2)
          call reverse_aitoff(x_pix1, y_pix1, theta1, phi2)
          call reverse_aitoff(x_pix1, y_pix2, theta1, phi2)
!
          if(theta1.lt.0.0d0 .and. theta2.ge.0.0d0) then
            inod_map = i + (j-1) * nxpixel
            rgba(1,inod_map) = one
            rgba(2,inod_map) = one
            rgba(3,inod_map) = one
            rgba(4,inod_map) = one
          end if
          if(theta1.ge.0.0d0 .and. theta2.lt.0.0d0) then
            inod_map = i+1 + (j-1) * nxpixel
            rgba(1,inod_map) = one
            rgba(2,inod_map) = one
            rgba(3,inod_map) = one
            rgba(4,inod_map) = one
          end if
!
          do ii = 1, 5
            phi_ref = pi * dble(ii) / 3.0d0
            if(phi1.lt.phi_ref .and. phi2.ge.phi_ref) then
              inod_map = i + (j-1) * nxpixel
              rgba(1,inod_map) = one
              rgba(2,inod_map) = one
              rgba(3,inod_map) = one
              rgba(4,inod_map) = half
              inod_map = i+1 + (j-1) * nxpixel
              rgba(1,inod_map) = one
              rgba(2,inod_map) = one
              rgba(3,inod_map) = one
              rgba(4,inod_map) = one
            end if
          end do
!
          do jj = 1, 5
            theta_ref = pi * dble(ii) / 6.0d0
            if(theta1.lt.theta_ref .and. theta2.ge.theta_ref) then
              inod_map = i + (j-1) * nxpixel
              rgba(1,inod_map) = one
              rgba(2,inod_map) = one
              rgba(3,inod_map) = one
              rgba(4,inod_map) = half
              inod_map = i + (j)   * nxpixel
              rgba(1,inod_map) = one
              rgba(2,inod_map) = one
              rgba(3,inod_map) = one
              rgba(4,inod_map) = one
            end if
          end do
        end do
      end do
!$omp end parallel do
!
!
      call cvt_double_rgba_to_char_rgb(npix, rgba, cimage)
      call calypso_write_png(psf_file_IO%file_prefix, ithree,           &
     &                       nxpixel, nypixel, cimage(1,1))
      deallocate(rgba, cimage)
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
