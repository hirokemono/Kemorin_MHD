!set_pvr_ray_start_point.f90
!      module set_pvr_ray_start_point
!
!        programmed by H.Matsui on Aug., 2011
!
!      subroutine s_set_pvr_ray_start_point(i_pvr,                      &
!     &          numnod, numele, numsurf, nnod_4_surf, xx,              &
!     &          ie_surf, isf_4_ele)
!
      module set_pvr_ray_start_point
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_control_params_4_pvr
      use m_geometries_in_pvr_screen
      use m_surf_grp_4_pvr_domain
      use m_parallel_var_dof
      use m_pvr_ray_startpoints
!
      implicit  none
!
      private :: count_each_pvr_ray_start_point
      private :: set_each_pvr_ray_start_point
      private :: count_each_pvr_ray_start, set_each_pvr_ray_start
      private :: cal_coefs_on_surf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_ray_start_point(i_pvr,                       &
     &          numnod, numele, numsurf, nnod_4_surf, xx,               &
     &          ie_surf, isf_4_ele)
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      real(kind = kreal), intent(in)  :: xx(numnod,3)
!
!
      call count_each_pvr_ray_start_point(i_pvr, numele, isf_4_ele)
!
      call reallocate_item_pvr_ray_start
!
      call set_each_pvr_ray_start_point(i_pvr,                          &
     &          numnod, numele, numsurf, nnod_4_surf, xx,               &
     &          ie_surf, isf_4_ele)
!
      end subroutine s_set_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      subroutine count_each_pvr_ray_start_point(i_pvr, numele, isf_4_ele)
!
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: inum,  icou
!
!
      ist = istack_pvr_surf_domain(i_pvr-1) + 1
      ied = istack_pvr_surf_domain(i_pvr  )
      num_pvr_ray_sf(ist:ied) = 0
!$omp parallel do private (inum, icou)
      do inum = ist, ied
        call count_each_pvr_ray_start(i_pvr, inum,                      &
     &      numele, isf_4_ele, icou)
        num_pvr_ray_sf(inum) = icou
      end do
!$omp end parallel do
!
      num_pvr_ray = 0
      istack_pvr_ray_sf(ist-1) = 0
      do inum = ist, ied
        istack_pvr_ray_sf(inum) = istack_pvr_ray_sf(inum-1)             &
     &                           + num_pvr_ray_sf(inum)
      end do
      num_pvr_ray = istack_pvr_ray_sf(ied)
!
      end subroutine count_each_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      subroutine set_each_pvr_ray_start_point(i_pvr,                    &
     &          numnod, numele, numsurf, nnod_4_surf, xx,               &
     &          ie_surf, isf_4_ele)
!
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      real(kind = kreal), intent(in)  :: xx(numnod,3)
!
      integer(kind = kint) :: ist, ied, inum
!
!
      ist = istack_pvr_surf_domain(i_pvr-1) + 1
      ied = istack_pvr_surf_domain(i_pvr  )
!$omp parallel do private (inum)
      do inum = ist, ied
        call set_each_pvr_ray_start(i_pvr, inum,  numnod, numele,       &
     &     numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele)
      end do
!$omp end parallel do
!
      end subroutine set_each_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      subroutine count_each_pvr_ray_start(i_pvr, inum,                  &
     &          numele, isf_4_ele, num_ray)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: i_pvr, inum
      integer(kind = kint), intent(inout) :: num_ray
!
      integer(kind = kint) :: iele, k1, isurf, iflag
      integer(kind = kint) :: ist_pix, ied_pix, jst_pix, jed_pix
      integer(kind = kint) :: ipix, jpix, ipix_s, jpix_s
      real(kind = kreal) :: x_pix(2)
      real(kind = kreal) :: xi(2), x_surf(2,4)
!
!
      num_ray = 0
      iele = item_pvr_surf_domain(1,inum)
      k1 =   item_pvr_surf_domain(2,inum)
      isurf = abs(isf_4_ele(iele,k1))
!
      if( (screen_norm_pvr_domain(3,inum)*ray_vec(3)) .gt. zero) then
        x_surf(1:2,1) = xx_screen_pvr_domain(4*inum-3,1:2)
        x_surf(1:2,2) = xx_screen_pvr_domain(4*inum-2,1:2)
        x_surf(1:2,3) = xx_screen_pvr_domain(4*inum-1,1:2)
        x_surf(1:2,4) = xx_screen_pvr_domain(4*inum,  1:2)
!
        ist_pix = isurf_xrng_pvr_domain(1,inum)
        ied_pix = isurf_xrng_pvr_domain(2,inum)
        jst_pix = jsurf_yrng_pvr_domain(1,inum)
        jed_pix = jsurf_yrng_pvr_domain(2,inum)
        do ipix = ist_pix, ied_pix
          ipix_s = ipix + istack_pixel_x(i_pvr-1)
          do jpix = jst_pix, jed_pix
            jpix_s = jpix + istack_pixel_y(i_pvr-1)
!
            x_pix(1) = pixel_point_x(ipix_s)
            x_pix(2) = pixel_point_y(jpix_s)
!
            call cal_coefs_on_surf(x_surf, x_pix, iflag, xi)
            num_ray = num_ray + iflag
!
          end do
        end do
      end if
!
      end subroutine count_each_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine set_each_pvr_ray_start(i_pvr, inum, numnod,            &
     &          numele, numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele)
!
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: i_pvr, inum
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      real(kind = kreal), intent(in)  :: xx(numnod,3)
!
      integer(kind = kint) :: icou, iele, k1, isurf, iflag
      integer(kind = kint) :: ist_pix, ied_pix, jst_pix, jed_pix
      integer(kind = kint) :: ipix, jpix, ipix_s, jpix_s, id_pixel
      real(kind = kreal) :: a(2,2), x_pix(2)
      real(kind = kreal) :: x_surf(2,4), xi(2)
!
!
      icou = istack_pvr_ray_sf(inum-1)
      iele = item_pvr_surf_domain(1,inum)
      k1 =   item_pvr_surf_domain(2,inum)
      isurf = abs(isf_4_ele(iele,k1))
!
      if( (screen_norm_pvr_domain(3,inum)*ray_vec(3)) .gt. zero) then
        x_surf(1:2,1) = xx_screen_pvr_domain(4*inum-3,1:2)
        x_surf(1:2,2) = xx_screen_pvr_domain(4*inum-2,1:2)
        x_surf(1:2,3) = xx_screen_pvr_domain(4*inum-1,1:2)
        x_surf(1:2,4) = xx_screen_pvr_domain(4*inum,  1:2)
!
        ist_pix = isurf_xrng_pvr_domain(1,inum)
        ied_pix = isurf_xrng_pvr_domain(2,inum)
        jst_pix = jsurf_yrng_pvr_domain(1,inum)
        jed_pix = jsurf_yrng_pvr_domain(2,inum)
        do ipix = ist_pix, ied_pix
          ipix_s = ipix + istack_pixel_x(i_pvr-1)
          do jpix = jst_pix, jed_pix
            jpix_s = jpix + istack_pixel_y(i_pvr-1)
!
            id_pixel = ipix + (jpix-1)*n_pvr_pixel(1,i_pvr)
            x_pix(1) = pixel_point_x(ipix_s)
            x_pix(2) = pixel_point_y(jpix_s)
            call cal_coefs_on_surf(x_surf, x_pix, iflag, xi)
!
            if(iflag .gt. 0) then
              icou = icou + 1
              icount_pvr_trace(icou) = 0
              id_pixel_start(icou) = id_pixel
              isf_pvr_ray_start(1,icou) = iele
              isf_pvr_ray_start(2,icou) = k1
              isf_pvr_ray_start(3,icou) = ie_surf(isurf,1)
              xi_pvr_start(1,icou) = xi(1)
              xi_pvr_start(2,icou) = xi(2)
              xx_pvr_ray_start(1,icou) = x_pix(1)
              xx_pvr_ray_start(2,icou) = x_pix(2)
!
              call cal_field_on_surf_scalar(nnod_pvr, numsurf,          &
     &              nnod_4_surf, ie_surf, isurf, xi_pvr_start(1,icou),  &
     &              x_nod_screen(1,3), xx_pvr_ray_start(3,icou) )
              call cal_field_on_surf_vector(nnod_pvr, numsurf,          &
     &              nnod_4_surf, ie_surf, isurf, xi_pvr_start(1,icou),  &
     &              xx(1,1), xx_pvr_start(1,icou) )
              pvr_ray_dir(1:3,icou) = viewpoint_vec(1:3,i_pvr)          &
     &                             - xx_pvr_start(1:3,icou)
            end if
          end do
        end do
      end if
!
      end subroutine set_each_pvr_ray_start
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_coefs_on_surf(x_surf, x_tgt, iflag_inside, xi)
!
      real(kind = kreal), intent(in) ::    x_tgt(2)
      real(kind = kreal), intent(in) ::    x_surf(2,4)
      integer(kind = kint), intent(inout) :: iflag_inside
      real(kind = kreal), intent(inout) :: xi(2)
!
      real(kind = kreal) :: xt1(2), a(2,2), coef(2,2)
      real(kind = kreal) :: c1, c3, aj
!
!
      xt1(1:2) = x_tgt(1:2) - x_surf(1:2,1)
      a(1:2,1) = x_surf(1:2,2) - x_surf(1:2,1)
      a(1:2,2) = x_surf(1:2,4) - x_surf(1:2,1)
      aj = one / (a(1,1)*a(2,2) - a(2,1)*a(1,2))
!
      coef(1,1) = ( a(2,2)*xt1(1) - a(1,2)*xt1(2) ) * aj
      coef(2,1) = (-a(2,1)*xt1(1) + a(1,1)*xt1(2) ) * aj
!
      xt1(1:2) = x_tgt(1:2) - x_surf(1:2,3)
      a(1:2,1) = x_surf(1:2,2) - x_surf(1:2,3)
      a(1:2,2) = x_surf(1:2,4) - x_surf(1:2,3)
      aj = one / (a(1,1)*a(2,2) - a(2,1)*a(1,2))
!
      coef(1,2) = ( a(2,2)*xt1(1) - a(1,2)*xt1(2) ) * aj
      coef(2,2) = (-a(2,1)*xt1(1) + a(1,1)*xt1(2) ) * aj
!
      c1 = one - (coef(1,1) + coef(2,1))
      c3 = one - (coef(1,2) + coef(2,2))
!
      if(coef(1,1).ge.zero .and. coef(2,1).ge.zero                      &
     &      .and. c1 .ge. zero) then
        iflag_inside = 1
        xi(1) = -one + two*coef(1,1)
        xi(2) = -one + two*coef(2,1)
!
      else if(coef(1,2).ge.zero .and. coef(2,2).ge.zero                 &
     &      .and. c3 .ge. zero) then
        iflag_inside = 1
        xi(1) = one - two*coef(2,2)
        xi(2) = one - two*coef(1,2)
      else
        iflag_inside = 0
      end if
!
      end subroutine cal_coefs_on_surf
!
!-----------------------------------------------------------------------
!
      end module set_pvr_ray_start_point
