!copy_filter_moms_from_2nd.f90
!      module copy_filter_moms_from_2nd
!
      module copy_filter_moms_from_2nd
!
!     Written by H. Matsui on Nov., 2008
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: n_vector = 3
      private :: n_vector
!
!      subroutine copy_elength_ele_from_2nd
!      subroutine copy_filter_moments_from_2nd
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine copy_elength_ele_from_2nd
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_filter_ele_length
      use m_filter_elength
!
      integer(kind = kint) :: iele, nd
!
!
      nnod_filter_mom = nnod_2nd
      nele_filter_mom = ele_2nd%numele
      call allocate_ele_length
!
      do iele = 1, ele_2nd%numele
        elen_dx2_ele(iele) = elen_dx2_ele_2nd(iele)
        elen_dy2_ele(iele) = elen_dy2_ele_2nd(iele)
        elen_dz2_ele(iele) = elen_dz2_ele_2nd(iele)
        elen_dxdy_ele(iele) = elen_dxdy_ele_2nd(iele)
        elen_dydz_ele(iele) = elen_dydz_ele_2nd(iele)
        elen_dzdx_ele(iele) = elen_dzdx_ele_2nd(iele)
      end do
!
      do nd = 1, n_vector
        do iele = 1, ele_2nd%numele
          elen_dx2_ele_dx(iele,nd) =  elen_dx2_ele_dx_2nd(iele,nd)
          elen_dy2_ele_dx(iele,nd) =  elen_dy2_ele_dx_2nd(iele,nd)
          elen_dz2_ele_dx(iele,nd) =  elen_dz2_ele_dx_2nd(iele,nd)
          elen_dxdy_ele_dx(iele,nd) = elen_dxdy_ele_dx_2nd(iele,nd)
          elen_dydz_ele_dx(iele,nd) = elen_dydz_ele_dx_2nd(iele,nd)
          elen_dzdx_ele_dx(iele,nd) = elen_dzdx_ele_dx_2nd(iele,nd)
!
          elen_dx2_ele_dx2(iele,nd) =  elen_dx2_ele_dx2_2nd(iele,nd)
          elen_dy2_ele_dx2(iele,nd) =  elen_dy2_ele_dx2_2nd(iele,nd)
          elen_dz2_ele_dx2(iele,nd) =  elen_dz2_ele_dx2_2nd(iele,nd)
          elen_dxdy_ele_dx2(iele,nd) = elen_dxdy_ele_dx2_2nd(iele,nd)
          elen_dydz_ele_dx2(iele,nd) = elen_dydz_ele_dx2_2nd(iele,nd)
          elen_dzdx_ele_dx2(iele,nd) = elen_dzdx_ele_dx2_2nd(iele,nd)
        end do
      end do
!
      call deallocate_2nd_ele_length
!
      end subroutine copy_elength_ele_from_2nd
!
!   --------------------------------------------------------------------
!
      subroutine copy_filter_moments_from_2nd
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_filter_moments
      use m_filter_moments
!
      integer(kind = kint) :: iele, ifil, nd
!
!
      nnod_fmom = nnod_2nd
      call allocate_filter_moms_ele(ele_2nd%numele)
!
      do ifil = 1, num_filter_moms
        do iele = 1, ele_2nd%numele
          filter_x2_ele(iele,ifil) = filter_x2_ele_2nd(iele,ifil)
          filter_y2_ele(iele,ifil) = filter_y2_ele_2nd(iele,ifil)
          filter_z2_ele(iele,ifil) = filter_z2_ele_2nd(iele,ifil)
          filter_xy_ele(iele,ifil) = filter_xy_ele_2nd(iele,ifil)
          filter_yz_ele(iele,ifil) = filter_yz_ele_2nd(iele,ifil)
          filter_zx_ele(iele,ifil) = filter_zx_ele_2nd(iele,ifil)
          filter_x_ele(iele,ifil) =  filter_x_ele_2nd(iele,ifil)
          filter_y_ele(iele,ifil) =  filter_y_ele_2nd(iele,ifil)
          filter_z_ele(iele,ifil) =  filter_z_ele_2nd(iele,ifil)
        end do
      end do
!
      do ifil = 1, num_filter_moms
        do nd = 1, n_vector
          do iele = 1, ele_2nd%numele
            filter_x2_ele_dx(iele,nd,ifil)                              &
     &          = filter_x2_ele_dx_2nd(iele,nd,ifil)
            filter_y2_ele_dx(iele,nd,ifil)                              &
     &          = filter_y2_ele_dx_2nd(iele,nd,ifil)
            filter_z2_ele_dx(iele,nd,ifil)                              &
     &          = filter_z2_ele_dx_2nd(iele,nd,ifil)
            filter_xy_ele_dx(iele,nd,ifil)                              &
     &          = filter_xy_ele_dx_2nd(iele,nd,ifil)
            filter_yz_ele_dx(iele,nd,ifil)                              &
     &          = filter_yz_ele_dx_2nd(iele,nd,ifil)
            filter_zx_ele_dx(iele,nd,ifil)                              &
     &          = filter_zx_ele_dx_2nd(iele,nd,ifil)
            filter_x_ele_dx(iele,nd,ifil)                               &
     &          =  filter_x_ele_dx_2nd(iele,nd,ifil)
            filter_y_ele_dx(iele,nd,ifil)                               &
     &          =  filter_y_ele_dx_2nd(iele,nd,ifil)
            filter_z_ele_dx(iele,nd,ifil)                               &
     &          =  filter_z_ele_dx_2nd(iele,nd,ifil)
!
            filter_x2_ele_dx2(iele,nd,ifil)                             &
     &          = filter_x2_ele_dx2_2nd(iele,nd,ifil)
            filter_y2_ele_dx2(iele,nd,ifil)                             &
     &          = filter_y2_ele_dx2_2nd(iele,nd,ifil)
            filter_z2_ele_dx2(iele,nd,ifil)                             &
     &          = filter_z2_ele_dx2_2nd(iele,nd,ifil)
            filter_xy_ele_dx2(iele,nd,ifil)                             &
     &          = filter_xy_ele_dx2_2nd(iele,nd,ifil)
            filter_yz_ele_dx2(iele,nd,ifil)                             &
     &          = filter_yz_ele_dx2_2nd(iele,nd,ifil)
            filter_zx_ele_dx2(iele,nd,ifil)                             &
     &          = filter_zx_ele_dx2_2nd(iele,nd,ifil)
            filter_x_ele_dx2(iele,nd,ifil)                              &
     &          =  filter_x_ele_dx2_2nd(iele,nd,ifil)
            filter_y_ele_dx2(iele,nd,ifil)                              &
     &          =  filter_y_ele_dx2_2nd(iele,nd,ifil)
            filter_z_ele_dx2(iele,nd,ifil)                              &
     &          =  filter_z_ele_dx2_2nd(iele,nd,ifil)
          end do
        end do
      end do
!
      call deallocate_2nd_filter_moms_ele
!
      end subroutine copy_filter_moments_from_2nd
!
!   --------------------------------------------------------------------
!
      end module copy_filter_moms_from_2nd
