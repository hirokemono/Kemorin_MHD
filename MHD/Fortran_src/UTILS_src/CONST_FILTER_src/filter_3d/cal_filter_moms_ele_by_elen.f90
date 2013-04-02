!cal_filter_moms_ele_by_elen.f90
!     module cal_filter_moms_ele_by_elen
!
      module cal_filter_moms_ele_by_elen
!
!     Written by H. Matsui on Mar., 2008
!
      use m_precision
!
      use m_constants
!
      implicit none
!
!      subroutine s_cal_filter_moms_ele_by_elen(ifil)
!      subroutine correct_filter_moms_ele_by_elen(ifil)
!      subroutine delete_cross_products_of_elen
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_filter_moms_ele_by_elen(ifil)
!
      use m_geometry_parameter
      use m_filter_elength
      use m_filter_moments
!
      integer(kind = kint), intent(in) :: ifil
      integer(kind = kint) :: iele, nd
!
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
!
      do nd = 1, 3
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
      end do
!
      do nd = 1, 3
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
      end do
!
      end subroutine s_cal_filter_moms_ele_by_elen
!
!  ---------------------------------------------------------------------
!
      subroutine correct_filter_moms_ele_by_elen(ifil)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_filter_elength
      use m_filter_moments
      use m_filter_coefs
!
      integer(kind = kint), intent(in) :: ifil
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
!
      do nd = 1, 3
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
      end do
!
      do nd = 1, 3
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
      end do
!
      end subroutine correct_filter_moms_ele_by_elen
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine delete_cross_products_of_elen
!
      use m_geometry_parameter
      use m_filter_elength
!
      integer(kind = kint) :: iele, nd
!
        do iele = 1, numele
          elen_dxdy_ele(iele) = zero
          elen_dydz_ele(iele) = zero
          elen_dzdx_ele(iele) = zero
        end do
!
      do nd = 1, 3
        do iele = 1, numele
          elen_dxdy_ele_dx(iele,nd) = zero
          elen_dydz_ele_dx(iele,nd) = zero
          elen_dzdx_ele_dx(iele,nd) = zero
        end do
      end do
!
      do nd = 1, 3
        do iele = 1, numele
          elen_dxdy_ele_dx2(iele,nd) = zero
          elen_dydz_ele_dx2(iele,nd) = zero
          elen_dzdx_ele_dx2(iele,nd) = zero
        end do
      end do
!
      end subroutine delete_cross_products_of_elen
!
!  ---------------------------------------------------------------------
!
      end module cal_filter_moms_ele_by_elen
