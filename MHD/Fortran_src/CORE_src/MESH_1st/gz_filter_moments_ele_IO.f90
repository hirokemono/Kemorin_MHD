!gz_filter_moments_ele_IO.f90
!     module gz_filter_moments_ele_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_base_filter_info_gz
!      subroutine write_base_filter_info_gz
!
!      subroutine read_elength_ele_gz
!      subroutine write_elength_ele_gz
!      subroutine read_filter_moments_ele_gz(ifil)
!      subroutine write_filter_moments_ele_gz(ifil)
!
      module gz_filter_moments_ele_IO
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_base_filter_info_gz
!
      use m_filter_elength
      use gz_filter_moms_elen_data_IO
!
!
      call read_ref_filter_param_gz(nf_type,                            &
     &    filter_type, f_width, xmom_1d_org)
!
      end subroutine read_base_filter_info_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_base_filter_info_gz
!
      use m_filter_elength
      use gz_filter_moms_elen_data_IO
!
!
      call write_ref_filter_param_gz(nf_type,                           &
     &    filter_type, f_width, xmom_1d_org)
!
      end subroutine write_base_filter_info_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elength_ele_gz
!
      use m_filter_elength
      use gz_filter_moms_elen_data_IO
!
!
      call read_elens_ele_gz(nele_filter_mom,                           &
     &    elen_dx2_ele, elen_dy2_ele, elen_dz2_ele,                     &
     &    elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,                  &
     &    elen_dx2_ele_dx, elen_dy2_ele_dx, elen_dz2_ele_dx,            &
     &    elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,         &
     &    elen_dx2_ele_dx2, elen_dy2_ele_dx2, elen_dz2_ele_dx2,         &
     &    elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2)
!
      end subroutine read_elength_ele_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_elength_ele_gz
!
      use m_filter_elength
      use gz_filter_moms_elen_data_IO
!
!    output coefs for filters for each node
!
      call write_elens_ele_gz(nele_filter_mom,                          &
     &    elen_dx2_ele, elen_dy2_ele, elen_dz2_ele,                     &
     &    elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele,                  &
     &    elen_dx2_ele_dx, elen_dy2_ele_dx, elen_dz2_ele_dx,            &
     &    elen_dxdy_ele_dx, elen_dydz_ele_dx, elen_dzdx_ele_dx,         &
     &    elen_dx2_ele_dx2, elen_dy2_ele_dx2, elen_dz2_ele_dx2,         &
     &    elen_dxdy_ele_dx2, elen_dydz_ele_dx2, elen_dzdx_ele_dx2)
!
      end subroutine write_elength_ele_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moments_ele_gz(ifil)
!
      use m_filter_moments
      use gz_filter_moms_elen_data_IO
!
      integer(kind = kint), intent(in) :: ifil
!
!
      call read_filter_moms_ele_gz(nele_fmom,                           &
     &    filter_x2_ele(1,ifil), filter_y2_ele(1,ifil),                 &
     &    filter_z2_ele(1,ifil), filter_xy_ele(1,ifil),                 &
     &    filter_yz_ele(1,ifil), filter_zx_ele(1,ifil),                 &
     &    filter_x_ele(1,ifil), filter_y_ele(1,ifil),                   &
     &    filter_z_ele(1,ifil), filter_x2_ele_dx(1,1,ifil),             &
     &    filter_y2_ele_dx(1,1,ifil), filter_z2_ele_dx(1,1,ifil),       &
     &    filter_xy_ele_dx(1,1,ifil), filter_yz_ele_dx(1,1,ifil),       &
     &    filter_zx_ele_dx(1,1,ifil), filter_x_ele_dx(1,1,ifil),        &
     &    filter_y_ele_dx(1,1,ifil),  filter_z_ele_dx(1,1,ifil),        &
     &    filter_x2_ele_dx2(1,1,ifil), filter_y2_ele_dx2(1,1,ifil),     &
     &    filter_z2_ele_dx2(1,1,ifil), filter_xy_ele_dx2(1,1,ifil),     &
     &    filter_yz_ele_dx2(1,1,ifil), filter_zx_ele_dx2(1,1,ifil),     &
     &    filter_x_ele_dx2(1,1,ifil), filter_y_ele_dx2(1,1,ifil),       &
     &    filter_z_ele_dx2(1,1,ifil) )
!
      end subroutine read_filter_moments_ele_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moments_ele_gz(ifil)
!
      use m_filter_moments
      use gz_filter_moms_elen_data_IO
!
      integer(kind = kint), intent(in) :: ifil
!
!
      call write_filter_moms_ele_gz(nele_fmom,                          &
     &    filter_x2_ele(1,ifil), filter_y2_ele(1,ifil),                 &
     &    filter_z2_ele(1,ifil), filter_xy_ele(1,ifil),                 &
     &    filter_yz_ele(1,ifil), filter_zx_ele(1,ifil),                 &
     &    filter_x_ele(1,ifil), filter_y_ele(1,ifil),                   &
     &    filter_z_ele(1,ifil), filter_x2_ele_dx(1,1,ifil),             &
     &    filter_y2_ele_dx(1,1,ifil), filter_z2_ele_dx(1,1,ifil),       &
     &    filter_xy_ele_dx(1,1,ifil), filter_yz_ele_dx(1,1,ifil),       &
     &    filter_zx_ele_dx(1,1,ifil), filter_x_ele_dx(1,1,ifil),        &
     &    filter_y_ele_dx(1,1,ifil),  filter_z_ele_dx(1,1,ifil),        &
     &    filter_x2_ele_dx2(1,1,ifil), filter_y2_ele_dx2(1,1,ifil),     &
     &    filter_z2_ele_dx2(1,1,ifil), filter_xy_ele_dx2(1,1,ifil),     &
     &    filter_yz_ele_dx2(1,1,ifil), filter_zx_ele_dx2(1,1,ifil),     &
     &    filter_x_ele_dx2(1,1,ifil), filter_y_ele_dx2(1,1,ifil),       &
     &    filter_z_ele_dx2(1,1,ifil) )
!
      end subroutine write_filter_moments_ele_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_filter_moments_ele_IO
