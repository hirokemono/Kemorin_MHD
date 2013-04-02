!cal_1st_diff_deltax_4_nod.f90
!     module cal_1st_diff_deltax_4_nod
!
!     Written by H. Matsui on Mar., 2008
!
      module cal_1st_diff_deltax_4_nod
!
      use m_precision
      use m_phys_constants
!
      implicit none
!
      private :: take_1st_diffs_nod_by_consist
      private :: take_1st_diffs_nod_by_lump
!
!      subroutine cal_1st_diffs_dx_by_consist
!      subroutine cal_1st_diffs_dx_by_consist(ifil)
!      subroutine cal_1st_diffs_dx_by_lump
!      subroutine cal_1st_diffs_dx_by_lump(ifil)
!      subroutine cal_diff_dx_by_consist
!      subroutine cal_diff_dx_by_lump
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_1st_diffs_dx_by_consist
!
      use m_filter_elength
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_consist                                &
     &    ( elen_dx2_nod(1), elen_dx2_nod_dx(1,1) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( elen_dy2_nod(1), elen_dy2_nod_dx(1,1) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( elen_dz2_nod(1), elen_dz2_nod_dx(1,1) )
!
      call take_1st_diffs_nod_by_consist                                &
     &    ( elen_dxdy_nod(1), elen_dxdy_nod_dx(1,1) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( elen_dydz_nod(1), elen_dydz_nod_dx(1,1) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( elen_dzdx_nod(1), elen_dzdx_nod_dx(1,1) )
!
      end subroutine cal_1st_diffs_dx_by_consist
!
!-----------------------------------------------------------------------
!
      subroutine cal_diffs_filter_nod_consist(ifil)
!
      use m_filter_moments
!
      integer(kind = kint), intent(in) :: ifil
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_x_nod(1,ifil), filter_x_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_y_nod(1,ifil), filter_y_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_z_nod(1,ifil), filter_z_nod_dx(1,1,ifil) )
!
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_x2_nod(1,ifil), filter_x2_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_y2_nod(1,ifil), filter_y2_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_z2_nod(1,ifil), filter_z2_nod_dx(1,1,ifil) )
!
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_xy_nod(1,ifil), filter_xy_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_yz_nod(1,ifil), filter_yz_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_consist                                &
     &    ( filter_zx_nod(1,ifil), filter_zx_nod_dx(1,1,ifil) )
!
      end subroutine cal_diffs_filter_nod_consist
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_1st_diffs_dx_by_lump
!
      use m_filter_elength
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_lump                                   &
     &    ( elen_dx2_nod(1), elen_dx2_nod_dx(1,1) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( elen_dy2_nod(1), elen_dy2_nod_dx(1,1) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( elen_dz2_nod(1), elen_dz2_nod_dx(1,1) )
!
      call take_1st_diffs_nod_by_lump                                   &
     &    ( elen_dxdy_nod(1), elen_dxdy_nod_dx(1,1) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( elen_dydz_nod(1), elen_dydz_nod_dx(1,1) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( elen_dzdx_nod(1), elen_dzdx_nod_dx(1,1) )
!
      end subroutine cal_1st_diffs_dx_by_lump
!
!-----------------------------------------------------------------------
!
      subroutine cal_diffs_filter_nod_lump(ifil)
!
      use m_filter_moments
!
      integer(kind = kint), intent(in) :: ifil
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_x_nod(1,ifil), filter_x_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_y_nod(1,ifil), filter_y_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_z_nod(1,ifil), filter_z_nod_dx(1,1,ifil) )
!
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_x2_nod(1,ifil), filter_x2_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_y2_nod(1,ifil), filter_y2_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_z2_nod(1,ifil), filter_z2_nod_dx(1,1,ifil) )
!
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_xy_nod(1,ifil), filter_xy_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_yz_nod(1,ifil), filter_yz_nod_dx(1,1,ifil) )
      call take_1st_diffs_nod_by_lump                                   &
     &    ( filter_zx_nod(1,ifil), filter_zx_nod_dx(1,1,ifil) )
!
      end subroutine cal_diffs_filter_nod_lump
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine take_1st_diffs_nod_by_consist(org_field, diff_field)
!
      use m_geometry_parameter
      use m_finite_element_matrix
      use int_vol_elesize_on_node
      use cal_ff_smp_to_ffs
      use cal_sol_deltax_by_consist
!
      real(kind = kreal), intent(inout) :: org_field(numnod)
      real(kind = kreal), intent(inout) :: diff_field(numnod,3)
      integer(kind = kint) :: nd
!
      ff = 0.0d0
      ff_nl_smp = 0.0d0
      call int_vol_diff_dxs(org_field)
      call cal_ff_smp_2_ff(n_vector, ff_nl_smp, ff)
      do nd = 1, n_vector
        call cal_sol_dx_by_consist(diff_field(1,nd), nd)
      end do
!
      end subroutine take_1st_diffs_nod_by_consist
!
!-----------------------------------------------------------------------
!
      subroutine take_1st_diffs_nod_by_lump(org_field, diff_field)
!
      use m_geometry_parameter
      use m_finite_element_matrix
      use int_vol_elesize_on_node
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(inout) :: org_field(numnod)
      real(kind = kreal), intent(inout) :: diff_field(numnod,3)
!
      ff_nl_smp = 0.0d0
      call int_vol_diff_dxs(org_field)
      call cal_ff_smp_2_vector(diff_field, ff_nl_smp, ml)
!
      end subroutine take_1st_diffs_nod_by_lump
!
!-----------------------------------------------------------------------
!
      end module cal_1st_diff_deltax_4_nod
