!ray_trace_4_each_image.f90
!
!      module ray_trace_4_each_image
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_ray_trace_4_each_image                             &
!!     &        (i_pvr, numnod, numele, numsurf, nnod_4_surf,           &
!!     &         ie_surf, isf_4_ele, iele_4_surf, e_multi, xx,          &
!!     &         nnod_pvr, nele_pvr, iflag_pvr_used_ele, x_nod_screen,  &
!!     &         d_nod_pvr, grad_ele_pvr, viewpoint_vec, ray_vec,       &
!!     &         num_pvr_ray, icount_pvr_trace, isf_pvr_ray_start,      &
!!     &         xi_pvr_start, xx_pvr_start, xx_pvr_ray_start, rgba_ray)
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
      private :: ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_ray_trace_4_each_image                               &
     &        (i_pvr, numnod, numele, numsurf, nnod_4_surf,             &
     &         ie_surf, isf_4_ele, iele_4_surf, e_multi, xx,            &
     &         nnod_pvr, nele_pvr, iflag_pvr_used_ele, x_nod_screen,    &
     &         d_nod_pvr, grad_ele_pvr, viewpoint_vec, ray_vec,         &
     &         num_pvr_ray, icount_pvr_trace, isf_pvr_ray_start,        &
     &         xi_pvr_start, xx_pvr_start, xx_pvr_ray_start, rgba_ray)
!
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      real(kind = kreal), intent(in) :: e_multi(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nnod_pvr, nele_pvr
      integer(kind = kint), intent(in) :: iflag_pvr_used_ele(numele)
      real(kind = kreal), intent(in) :: x_nod_screen(nnod_pvr,4)
      real(kind = kreal), intent(in) :: d_nod_pvr(nnod_pvr)
      real(kind = kreal), intent(in) :: grad_ele_pvr(nele_pvr,3)
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
!
!
!$omp parallel do private(inum, iflag_comm)
      do inum = 1, num_pvr_ray
          rgba_ray(1:4,inum) = zero
          call ray_trace_each_pixel(i_pvr, numnod, numele, numsurf,     &
     &        nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf,             &
     &        e_multi, xx, nnod_pvr, nele_pvr, iflag_pvr_used_ele,      &
     &        x_nod_screen, d_nod_pvr, grad_ele_pvr,                    &
     &        viewpoint_vec, ray_vec, isf_pvr_ray_start(1,inum),        &
     &        xx_pvr_ray_start(1,inum), xx_pvr_start(1,inum),           &
     &        xi_pvr_start(1,inum), rgba_ray(1,inum),                   &
     &        icount_pvr_trace(inum), iflag_comm)
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
      subroutine ray_trace_each_pixel(i_pvr, numnod, numele, numsurf,   &
     &          nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf,           &
     &          e_multi, xx, nnod_pvr, nele_pvr, iflag_used_ele,        &
     &          x_nod_screen, color_nod, grad_ele,                      &
     &          viewpoint_vec, ray_vec, isurf_org, screen_st, xx_st,    &
     &          xi, rgba_ray, icount_line, iflag_comm)
!
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      real(kind = kreal), intent(in) :: e_multi(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: iflag_used_ele(numele)
      integer(kind = kint), intent(in) :: nnod_pvr, nele_pvr
      real(kind = kreal), intent(in) :: x_nod_screen(nnod_pvr,4)
      real(kind = kreal), intent(in) :: color_nod(nnod_pvr)
      real(kind = kreal), intent(in) :: grad_ele(nele_pvr,3)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec(3)
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) :: screen_st(3)
      real(kind = kreal), intent(inout) :: xx_st(3), xi(2)
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
      integer(kind = kint), parameter :: iflag_back = 0
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      real(kind = kreal) :: screen_tgt(3), c_tgt(1), c_org(1)
      real(kind = kreal) :: grad_tgt(3), xx_tgt(3)
!
!
      if(isurf_org(1) .eq. 0) return
!
      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_end = abs(isf_4_ele(iele,isf_org))
      call cal_field_on_surf_scalar(nnod_pvr, numsurf,                  &
     &      nnod_4_surf, ie_surf, isurf_end, xi, color_nod, c_org(1) )
!
      do
        icount_line = icount_line + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
        if(iflag_used_ele(iele).eq.0) then
          iflag_comm = 2
          exit
        end if
!
!   extend to surface of element
!
        call find_line_end_in_1ele(iflag_back, nnod_pvr, numele,        &
     &      numsurf, nnod_4_surf, isf_4_ele, ie_surf, x_nod_screen,     &
     &      iele, isf_org, ray_vec, screen_st, isf_tgt, screen_tgt, xi)
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
        call cal_field_on_surf_vector(nnod_pvr, numsurf, nnod_4_surf,   &
     &      ie_surf, isurf_end, xi, xx, xx_tgt)
        call cal_field_on_surf_scalar(nnod_pvr, numsurf, nnod_4_surf,   &
     &      ie_surf, isurf_end, xi, color_nod, c_tgt(1))
        grad_tgt(1:3) = grad_ele(iele,1:3)
!
        c_tgt(1) = half*(c_tgt(1) + c_org(1))
!
        if(e_multi(iele) .gt. 0.0d0) then
          call s_set_rgba_4_each_pixel(i_pvr, viewpoint_vec,            &
     &        xx_st, xx_tgt, c_tgt(1), grad_tgt, rgba_ray)
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
