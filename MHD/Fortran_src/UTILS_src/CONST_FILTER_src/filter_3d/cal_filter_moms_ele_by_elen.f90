!cal_filter_moms_ele_by_elen.f90
!     module cal_filter_moms_ele_by_elen
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine cal_fmoms_ele_by_elen_1st(ifil)
!!      subroutine correct_fmoms_ele_by_elen_1st(ifil)
!!      subroutine delete_x_products_of_elen_1st
!!
!!      subroutine s_cal_filter_moms_ele_by_elen(ifil)
!!      subroutine correct_filter_moms_ele_by_elen(ifil)
!!      subroutine delete_cross_products_of_elen
!
      module cal_filter_moms_ele_by_elen
!
      use m_precision
!
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_fmoms_ele_by_elen_1st(ifil)
!
      use m_geometry_parameter
      use m_filter_elength
!
      integer(kind = kint), intent(in) :: ifil
!
      call s_cal_filter_moms_ele_by_elen                                &
     &   (ifil, numele, nf_type, filter_conf1%xmom_1d_org,              &
     &    elen_1%f_x2, elen_1%f_y2, elen_1%f_z2,                   &
     &    elen_1%f_xy, elen_1%f_yz, elen_1%f_zx,                   &
     &    diff1_1%df_x2,  diff1_1%df_y2,  diff1_1%df_z2,          &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,          &
     &    diff2_1%df_x2,  diff2_1%df_y2,  diff2_1%df_z2,       &
     &    diff2_1%df_xy,  diff2_1%df_yz,  diff2_1%df_zx)
!
      end subroutine cal_fmoms_ele_by_elen_1st
!
!  ---------------------------------------------------------------------
!
      subroutine correct_fmoms_ele_by_elen_1st(ifil)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_filter_elength
!
      integer(kind = kint), intent(in) :: ifil
!
      call correct_filter_moms_ele_by_elen(ifil, numele, nnod_4_ele,    &
     &    ie, nf_type, filter_conf1%xmom_1d_org,           &
     &    elen_1%f_x2, elen_1%f_y2, elen_1%f_z2,                   &
     &    elen_1%f_xy, elen_1%f_yz, elen_1%f_zx,                   &
     &    diff1_1%df_x2,  diff1_1%df_y2,  diff1_1%df_z2,          &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,          &
     &    diff2_1%df_x2,  diff2_1%df_y2,  diff2_1%df_z2,       &
     &    diff2_1%df_xy,  diff2_1%df_yz,  diff2_1%df_zx)
!
      end subroutine correct_fmoms_ele_by_elen_1st
!
!  ---------------------------------------------------------------------
!
      subroutine delete_x_products_of_elen_1st
!
      use m_geometry_parameter
      use m_filter_elength
!
      call delete_cross_products_of_elen(numele,                        &
     &    elen_1%f_xy, elen_1%f_yz, elen_1%f_zx,                   &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,          &
     &    diff2_1%df_xy,  diff2_1%df_yz,  diff2_1%df_zx)
!
      end subroutine delete_x_products_of_elen_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_cal_filter_moms_ele_by_elen                          &
     &   (ifil, numele, nf_type, xmom_1d_org,                           &
     &    elen_dx2_ele,      elen_dy2_ele,      elen_dz2_ele,           &
     &    elen_dxdy_ele,     elen_dydz_ele,     elen_dzdx_ele,          &
     &    elen_dx2_ele_dx,   elen_dy2_ele_dx,   elen_dz2_ele_dx,        &
     &    elen_dxdy_ele_dx,  elen_dydz_ele_dx,  elen_dzdx_ele_dx,       &
     &    elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2,       &
     &    elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2)
!
      use m_filter_moments
!
      integer(kind = kint), intent(in) :: ifil
!
      integer (kind = kint), intent(in) :: numele
!
      integer (kind = kint), intent(in) :: nf_type
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
      real(kind=kreal), intent(in) :: elen_dx2_ele(numele)
      real(kind=kreal), intent(in) :: elen_dy2_ele(numele)
      real(kind=kreal), intent(in) :: elen_dz2_ele(numele)
      real(kind=kreal), intent(in) :: elen_dxdy_ele(numele)
      real(kind=kreal), intent(in) :: elen_dydz_ele(numele)
      real(kind=kreal), intent(in) :: elen_dzdx_ele(numele)
!
      real(kind=kreal), intent(in) :: elen_dx2_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dy2_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dz2_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dxdy_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dydz_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dzdx_ele_dx(numele,3)
!
      real(kind=kreal), intent(in) :: elen_dx2_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dy2_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dz2_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dxdy_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dydz_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dzdx_ele_dx2(numele,3)
!
      integer(kind = kint) :: iele, nd
!
!$omp parallel do
      do iele = 1, numele
        filter_x_ele(iele,ifil) = zero
        filter_y_ele(iele,ifil) = zero
        filter_z_ele(iele,ifil) = zero
        filter_x2_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dx2_ele(iele)
        filter_y2_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dy2_ele(iele)
        filter_z2_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dz2_ele(iele)
        filter_xy_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dxdy_ele(iele)
        filter_yz_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dydz_ele(iele)
        filter_zx_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dzdx_ele(iele)
      end do
!$omp end parallel do
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          filter_x_ele_dx(iele,nd,ifil) = zero
          filter_y_ele_dx(iele,nd,ifil) = zero
          filter_z_ele_dx(iele,nd,ifil) = zero
          filter_x2_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dx2_ele_dx(iele,nd)
          filter_y2_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dy2_ele_dx(iele,nd)
          filter_z2_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dz2_ele_dx(iele,nd)
          filter_xy_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dxdy_ele_dx(iele,nd)
          filter_yz_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dydz_ele_dx(iele,nd)
          filter_zx_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dzdx_ele_dx(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          filter_x_ele_dx2(iele,nd,ifil) = zero
          filter_y_ele_dx2(iele,nd,ifil) = zero
          filter_z_ele_dx2(iele,nd,ifil) = zero
          filter_x2_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dx2_ele_dx2(iele,nd)
          filter_y2_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dy2_ele_dx2(iele,nd)
          filter_z2_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dz2_ele_dx2(iele,nd)
          filter_xy_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dxdy_ele_dx2(iele,nd)
          filter_yz_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dydz_ele_dx2(iele,nd)
          filter_zx_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dzdx_ele_dx2(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine s_cal_filter_moms_ele_by_elen
!
!  ---------------------------------------------------------------------
!
      subroutine correct_filter_moms_ele_by_elen                        &
     &   (ifil, numele, nnod_4_ele, ie, nf_type, xmom_1d_org,           &
     &    elen_dx2_ele,      elen_dy2_ele,      elen_dz2_ele,           &
     &    elen_dxdy_ele,     elen_dydz_ele,     elen_dzdx_ele,          &
     &    elen_dx2_ele_dx,   elen_dy2_ele_dx,   elen_dz2_ele_dx,        &
     &    elen_dxdy_ele_dx,  elen_dydz_ele_dx,  elen_dzdx_ele_dx,       &
     &    elen_dx2_ele_dx2,  elen_dy2_ele_dx2,  elen_dz2_ele_dx2,       &
     &    elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2)
!
      use m_filter_moments
      use m_filter_coefs
!
      integer(kind = kint), intent(in) :: ifil
!
      integer (kind = kint), intent(in) :: numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer (kind = kint), intent(in) :: nf_type
      real(kind=kreal), intent(in) :: xmom_1d_org(nf_type,0:2)
!
      real(kind=kreal), intent(in) :: elen_dx2_ele(numele)
      real(kind=kreal), intent(in) :: elen_dy2_ele(numele)
      real(kind=kreal), intent(in) :: elen_dz2_ele(numele)
      real(kind=kreal), intent(in) :: elen_dxdy_ele(numele)
      real(kind=kreal), intent(in) :: elen_dydz_ele(numele)
      real(kind=kreal), intent(in) :: elen_dzdx_ele(numele)
!
      real(kind=kreal), intent(in) :: elen_dx2_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dy2_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dz2_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dxdy_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dydz_ele_dx(numele,3)
      real(kind=kreal), intent(in) :: elen_dzdx_ele_dx(numele,3)
!
      real(kind=kreal), intent(in) :: elen_dx2_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dy2_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dz2_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dxdy_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dydz_ele_dx2(numele,3)
      real(kind=kreal), intent(in) :: elen_dzdx_ele_dx2(numele,3)
!
      integer(kind = kint) :: iele, nd, inum, inod, k1
!
!
      do iele = 1, numele
        do k1 = 1, nnod_4_ele
          inod = ie(iele,k1)
          if( iflag_make_whole_filter(inod) .eq. 1) then
            iflag_make_moment_ele(iele) = 1
            exit
          end if
          if( iflag_make_fluid_filter(inod) .eq. 1) then
            iflag_make_moment_ele(iele) = 1
            exit
          end if
        end do
      end do
!
      nele_make_moment_again = 0
      do iele = 1, numele
        if ( iflag_make_fluid_filter(iele) .eq. 0) then
          nele_make_moment_again = nele_make_moment_again + 1
          iele_make_moment_again(nele_make_moment_again) = iele
        end if
      end do
!
!
!$omp parallel do private(inum,iele)
      do inum = 1, nele_make_moment_again
        iele = iele_make_moment_again(inum)
!
        filter_x_ele(iele,ifil) = zero
        filter_y_ele(iele,ifil) = zero
        filter_z_ele(iele,ifil) = zero
        filter_x2_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dx2_ele(iele)
        filter_y2_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dy2_ele(iele)
        filter_z2_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dz2_ele(iele)
        filter_xy_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dxdy_ele(iele)
        filter_yz_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dydz_ele(iele)
        filter_zx_ele(iele,ifil)                                        &
     &        = xmom_1d_org(1,2) * elen_dzdx_ele(iele)
      end do
!$omp end parallel do
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(inum,iele)
        do inum = 1, nele_make_moment_again
          iele = iele_make_moment_again(inum)
!
          filter_x_ele_dx(iele,nd,ifil) = zero
          filter_y_ele_dx(iele,nd,ifil) = zero
          filter_z_ele_dx(iele,nd,ifil) = zero
          filter_x2_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dx2_ele_dx(iele,nd)
          filter_y2_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dy2_ele_dx(iele,nd)
          filter_z2_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dz2_ele_dx(iele,nd)
          filter_xy_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dxdy_ele_dx(iele,nd)
          filter_yz_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dydz_ele_dx(iele,nd)
          filter_zx_ele_dx(iele,nd,ifil)                                &
     &        = xmom_1d_org(1,2) * elen_dzdx_ele_dx(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(inum,iele)
        do inum = 1, nele_make_moment_again
          iele = iele_make_moment_again(inum)
!
          filter_x_ele_dx2(iele,nd,ifil) = zero
          filter_y_ele_dx2(iele,nd,ifil) = zero
          filter_z_ele_dx2(iele,nd,ifil) = zero
          filter_x2_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dx2_ele_dx2(iele,nd)
          filter_y2_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dy2_ele_dx2(iele,nd)
          filter_z2_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dz2_ele_dx2(iele,nd)
          filter_xy_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dxdy_ele_dx2(iele,nd)
          filter_yz_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dydz_ele_dx2(iele,nd)
          filter_zx_ele_dx2(iele,nd,ifil)                               &
     &        = xmom_1d_org(1,2) * elen_dzdx_ele_dx2(iele,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine correct_filter_moms_ele_by_elen
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine delete_cross_products_of_elen(numele,                  &
     &    elen_dxdy_ele,     elen_dydz_ele,     elen_dzdx_ele,          &
     &    elen_dxdy_ele_dx,  elen_dydz_ele_dx,  elen_dzdx_ele_dx,       &
     &    elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2)
!
      integer(kind = kint), intent(in) :: numele
      real(kind=kreal), intent(inout) :: elen_dxdy_ele(numele)
      real(kind=kreal), intent(inout) :: elen_dydz_ele(numele)
      real(kind=kreal), intent(inout) :: elen_dzdx_ele(numele)
!
      real(kind=kreal), intent(inout) :: elen_dxdy_ele_dx(numele,3)
      real(kind=kreal), intent(inout) :: elen_dydz_ele_dx(numele,3)
      real(kind=kreal), intent(inout) :: elen_dzdx_ele_dx(numele,3)
!
      real(kind=kreal), intent(inout) :: elen_dxdy_ele_dx2(numele,3)
      real(kind=kreal), intent(inout) :: elen_dydz_ele_dx2(numele,3)
      real(kind=kreal), intent(inout) :: elen_dzdx_ele_dx2(numele,3)
!
      integer(kind = kint) :: iele, nd
!
!$omp parallel do
      do iele = 1, numele
        elen_dxdy_ele(iele) = zero
        elen_dydz_ele(iele) = zero
        elen_dzdx_ele(iele) = zero
      end do
!$omp end parallel do
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          elen_dxdy_ele_dx(iele,nd) = zero
          elen_dydz_ele_dx(iele,nd) = zero
          elen_dzdx_ele_dx(iele,nd) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(nd)
      do nd = 1, 3
!$omp do private(iele)
        do iele = 1, numele
          elen_dxdy_ele_dx2(iele,nd) = zero
          elen_dydz_ele_dx2(iele,nd) = zero
          elen_dzdx_ele_dx2(iele,nd) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine delete_cross_products_of_elen
!
!  ---------------------------------------------------------------------
!
      end module cal_filter_moms_ele_by_elen
