!ray_trace_4_each_image.f90
!
!      module ray_trace_4_each_image
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine cont_overlap_in_each_domain                          &
!!     &         (num_pvr_ray, id_pixel_start,                          &
!!     &          num_pixel_xy, iflag_mapped, num_overlap)
!!      subroutine cal_average_image_depth(num_pvr_ray,                 &
!!     &         id_pixel_start, xx_pvr_ray_start, num_overlap,         &
!!     &         num_pixel_xy, iflag_mapped, depth_lc)
!!      subroutine copy_segmented_image                                 &
!!     &        (num_pvr_ray, id_pixel_start, rgba_ray,                 &
!!     &         num_overlap, num_pixel_xy, iflag_mapped, rgba_lc)
!!
!!      subroutine s_ray_trace_4_each_image                             &
!!     &         (numnod, numele, numsurf, nnod_4_surf,                 &
!!     &          ie_surf, isf_4_ele, iele_4_surf, interior_ele,        &
!!     &          xx, vnorm_surf, iflag_pvr_used_ele,                   &
!!     &          x_nod_screen, d_nod_pvr, grad_ele_pvr,                &
!!     &          viewpoint_vec, arccos_sf, color_param,                &
!!     &          ray_vec, num_pvr_ray, icount_pvr_trace,               &
!!     &          isf_pvr_ray_start, xi_pvr_start, xx_pvr_start,        &
!!     &          xx_pvr_ray_start, rgba_ray)
!!      subroutine blend_overlapped_area(num_pvr_ray,                   &
!!     &         id_pixel_start, xx_pvr_ray_start, rgba_ray,            &
!!     &         num_pixel_xy, iflag_mapped, rgba_lc, depth_lc)
!
      module ray_trace_4_each_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use set_rgba_4_each_pixel
!
      implicit  none
!
      real(kind = kint), parameter :: SMALL = 0.1
!
      private :: ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cont_overlap_in_each_domain                            &
     &         (num_pvr_ray, id_pixel_start,                            &
     &          num_pixel_xy, iflag_mapped, num_overlap)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      integer(kind = kint), intent(inout) :: num_overlap
!
      integer(kind = kint) :: inum, ipix
!
!
!$omp parallel workshare
      iflag_mapped = 0
!$omp end parallel workshare
      do inum = 1, num_pvr_ray
        ipix = id_pixel_start(inum)
        iflag_mapped(ipix) = iflag_mapped(ipix) + 1
      end do
      num_overlap = maxval(iflag_mapped,1)
!
      end subroutine cont_overlap_in_each_domain
!
!  ---------------------------------------------------------------------
!
      subroutine cal_average_image_depth(num_pvr_ray,                   &
     &         id_pixel_start, xx_pvr_ray_start, num_overlap,           &
     &         num_pixel_xy, iflag_mapped, depth_lc)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      real(kind = kreal), intent(in)                                    &
     &                    ::  xx_pvr_ray_start(3,num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_overlap, num_pixel_xy
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(inout)                                 &
     &                   :: depth_lc(num_pixel_xy,num_overlap)
!
      integer(kind = kint) :: inum, ipix, icou
!
!
!$omp parallel workshare
      iflag_mapped(1:num_pixel_xy) = 0
!$omp end parallel workshare
!$omp parallel workshare
      depth_lc(1:num_pixel_xy,1:num_overlap) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(inum,ipix,icou)
      do inum = 1, num_pvr_ray
        ipix = id_pixel_start(inum)
        iflag_mapped(ipix) = iflag_mapped(ipix) + 1
        icou = iflag_mapped(ipix)
        depth_lc(ipix,icou) =  xx_pvr_ray_start(3,inum)
      end do
!$omp end parallel do
!
      end subroutine cal_average_image_depth
!
!  ---------------------------------------------------------------------
!
      subroutine copy_segmented_image                                   &
     &        (num_pvr_ray, id_pixel_start, rgba_ray,                   &
     &         num_overlap, num_pixel_xy, iflag_mapped, rgba_lc)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      real(kind = kreal), intent(in) ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_overlap, num_pixel_xy
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(inout)                                 &
     &                    :: rgba_lc(4,num_overlap,num_pixel_xy)
!
      integer(kind = kint) :: inum, ipix, icou
!
!
!$omp parallel workshare
      iflag_mapped(1:num_pixel_xy) = 0
!$omp end parallel workshare
!$omp parallel workshare
      rgba_lc(1:4,1:num_overlap,1:num_pixel_xy) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(inum,ipix,icou)
      do inum = 1, num_pvr_ray
        ipix = id_pixel_start(inum)
        iflag_mapped(ipix) = iflag_mapped(ipix) + 1
        icou = iflag_mapped(ipix)
!
        rgba_lc(1,icou,ipix) = rgba_ray(1,inum)
        rgba_lc(2,icou,ipix) = rgba_ray(2,inum)
        rgba_lc(3,icou,ipix) = rgba_ray(3,inum)
        rgba_lc(4,icou,ipix) = rgba_ray(4,inum)
      end do
!$omp end parallel do
!
      end subroutine copy_segmented_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_ray_trace_4_each_image                               &
     &         (numnod, numele, numsurf, nnod_4_surf,                   &
     &          ie_surf, isf_4_ele, iele_4_surf, interior_ele,          &
     &          xx, vnorm_surf,  x_nod_screen, viewpoint_vec,           &
     &          field_pvr, color_param, ray_vec, num_pvr_ray,           &
     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,      &
     &          xx_pvr_start, xx_pvr_ray_start, rgba_ray)
!
      use t_geometries_in_pvr_screen
      use t_control_params_4_pvr
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
!
      real(kind = kreal), intent(in) :: x_nod_screen(numnod,4)
!
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec(3)
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(inout)                               &
     &                    :: icount_pvr_trace(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: isf_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xi_pvr_start(2,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xx_pvr_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  xx_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint) :: inum, iflag_comm
      real(kind = kreal) :: rgba_tmp(4)
!
!
!$omp parallel do private(inum, iflag_comm,rgba_tmp)
      do inum = 1, num_pvr_ray
        rgba_tmp(1:4) = zero
          call ray_trace_each_pixel                                     &
     &      (numnod, numele, numsurf, nnod_4_surf, ie_surf, isf_4_ele,  &
     &       iele_4_surf, interior_ele, xx, vnorm_surf, x_nod_screen,   &
     &       viewpoint_vec, field_pvr, color_param, ray_vec,            &
     &       isf_pvr_ray_start(1,inum), xx_pvr_ray_start(1,inum),       &
     &       xx_pvr_start(1,inum), xi_pvr_start(1,inum),                &
     &       rgba_tmp(1), icount_pvr_trace(inum), iflag_comm)
        rgba_ray(1:4,inum) = rgba_tmp(1:4)
      end do
!$omp end parallel do
!
      end subroutine s_ray_trace_4_each_image
!
!  ---------------------------------------------------------------------
!
      subroutine blend_overlapped_area(num_pvr_ray,                     &
     &         id_pixel_start, xx_pvr_ray_start, rgba_ray,              &
     &         num_pixel_xy, iflag_mapped, rgba_lc, depth_lc)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      real(kind = kreal), intent(in)                                    &
     &                    ::  xx_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(in)                                    &
     &                    ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(inout) :: rgba_lc(4,num_pixel_xy)
      real(kind = kreal), intent(inout) :: depth_lc(num_pixel_xy)
!
      integer(kind = kint) :: inum, id_pixel
!
!
      iflag_mapped = 0
      rgba_lc =      0.0d0
      depth_lc =     0.0d0
      do inum = 1, num_pvr_ray
        id_pixel = id_pixel_start(inum)
        call composite_alpha_blending                                   &
     &     (rgba_ray(1,inum), rgba_lc(1,id_pixel) )
!
        if(iflag_mapped(id_pixel) .eq. 0) then
          depth_lc(id_pixel)  = xx_pvr_ray_start(3,inum)
          iflag_mapped(id_pixel) = 1
        else
          depth_lc(id_pixel)                                            &
     &      = min(depth_lc(id_pixel),xx_pvr_ray_start(3,inum))
        end if
      end do
!
      end subroutine blend_overlapped_area
!
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_each_pixel                                   &
     &       (numnod, numele, numsurf,  nnod_4_surf, ie_surf,           &
     &        isf_4_ele, iele_4_surf, interior_ele, xx, vnorm_surf,     &
     &        x_nod_screen, viewpoint_vec, field_pvr, color_param,      &
     &        ray_vec, isurf_org, screen_st, xx_st, xi, rgba_ray,       &
     &        icount_line, iflag_comm)
!
      use t_geometries_in_pvr_screen
      use t_control_params_4_pvr
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use set_coefs_of_sections
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
!
      real(kind = kreal), intent(in) :: x_nod_screen(numnod,4)
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec(3)
!
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) :: screen_st(3)
      real(kind = kreal), intent(inout) :: xx_st(3), xi(2)
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
      integer(kind = kint), parameter :: iflag_back = 0
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      integer(kind = kint) :: i_iso, i_psf
      real(kind = kreal) :: screen_tgt(3), c_tgt(1), c_org(1)
      real(kind = kreal) :: grad_tgt(3), xx_tgt(3), rflag, rflag2
!
!
      if(isurf_org(1) .eq. 0) return
!
      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_end = abs(isf_4_ele(iele,isf_org))
      call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,       &
     &    ie_surf, isurf_end, xi, field_pvr%d_pvr, c_org(1) )
!
      do
        icount_line = icount_line + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
        if(field_pvr%iflag_used_ele(iele).eq.0) then
          iflag_comm = 2
          exit
        end if
!
!   extend to surface of element
!
        call find_line_end_in_1ele(iflag_back, numnod, numele,          &
     &     numsurf, nnod_4_surf, isf_4_ele, ie_surf, x_nod_screen(1,1), &
     &     iele, isf_org, ray_vec, screen_st, isf_tgt, screen_tgt, xi)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
!   set backside element and surface 
!
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
!
        if(isf_4_ele(iele,isf_tgt) .lt. 0) then
          isurf_org(1) = iele_4_surf(isurf_end,1,1)
          isurf_org(2) = iele_4_surf(isurf_end,1,2)
        else
          isurf_org(1) = iele_4_surf(isurf_end,2,1)
          isurf_org(2) = iele_4_surf(isurf_end,2,2)
        end if
!
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, xx, xx_tgt)
        call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, field_pvr%d_pvr, c_tgt(1))
!
        if(interior_ele(iele) .gt. 0) then
          if(field_pvr%arccos_sf(isurf_end) .gt. SMALL) then
            grad_tgt(1:3) = vnorm_surf(isurf_end,1:3)
            call plane_rendering_with_light                             &
     &         (viewpoint_vec, xx_tgt, grad_tgt,                        &
     &          field_pvr%arccos_sf(isurf_end),  color_param, rgba_ray)
          end if
!
          do i_psf = 1, field_pvr%num_sections
            rflag =  side_of_plane(field_pvr%coefs(1:10,i_psf), xx_st)  &
     &             * side_of_plane(field_pvr%coefs(1:10,i_psf), xx_tgt)
            rflag2 = side_of_plane(field_pvr%coefs(1:10,i_psf), xx_tgt)
            if(rflag2 .eq. zero .or. rflag .lt. zero) then
              call cal_normal_of_plane                                  &
     &           (field_pvr%coefs(1:10,i_psf), xx_tgt, grad_tgt)
              call color_plane_with_light                               &
     &           (viewpoint_vec, xx_tgt, c_tgt(1), grad_tgt,            &
     &            field_pvr%sect_opacity(i_psf), color_param, rgba_ray)
            end if
          end do
!
          do i_iso = 1, field_pvr%num_isosurf
            rflag =  (c_org(1) - field_pvr%iso_value(i_iso))            &
     &             * (c_tgt(1) - field_pvr%iso_value(i_iso))
            if((c_tgt(1) - field_pvr%iso_value(i_iso)) .eq. zero        &
     &        .or. rflag .lt. zero) then
              grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)              &
     &                       * field_pvr%itype_isosurf(i_iso)
              call color_plane_with_light                               &
     &           (viewpoint_vec, xx_tgt, field_pvr%iso_value(i_iso),    &
     &            grad_tgt, field_pvr%iso_opacity(i_iso),               &
     &            color_param, rgba_ray)
            end if
          end do
!
          grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)
          c_tgt(1) = half*(c_tgt(1) + c_org(1))
          call s_set_rgba_4_each_pixel(viewpoint_vec, xx_st, xx_tgt,    &
     &        c_tgt(1), grad_tgt, color_param, rgba_ray)
        end if
!
        if(isurf_org(1).eq.0) then
          iflag_comm = 0
          exit
        end if
!
        screen_st(1:3) = screen_tgt(1:3)
        xx_st(1:3) = xx_tgt(1:3)
        c_org(1) =   c_tgt(1)
      end do
!
      end subroutine ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      end module ray_trace_4_each_image
