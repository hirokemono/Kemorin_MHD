!cal_1st_diff_deltax_4_nod.f90
!     module cal_1st_diff_deltax_4_nod
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine cal_1st_diffs_dx_by_consist
!      subroutine cal_diffs_filter_nod_consist(mom_nod)
!      subroutine cal_diffs_filter_nod_lump(mom_nod)
!      subroutine cal_1st_diffs_dx_by_lump
!      subroutine cal_1st_diffs_dx_by_lump(ifil)
!      subroutine cal_diff_dx_by_consist
!      subroutine cal_diff_dx_by_lump
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
     &   (FEM1_elen%elen_nod%moms%f_x2, FEM1_elen%elen_nod%diff%df_x2)
      call take_1st_diffs_nod_by_consist                                &
     &   (FEM1_elen%elen_nod%moms%f_y2, FEM1_elen%elen_nod%diff%df_y2)
      call take_1st_diffs_nod_by_consist                                &
     &   (FEM1_elen%elen_nod%moms%f_z2, FEM1_elen%elen_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_consist                                &
     &   (FEM1_elen%elen_nod%moms%f_xy, FEM1_elen%elen_nod%diff%df_xy)
      call take_1st_diffs_nod_by_consist                                &
     &   (FEM1_elen%elen_nod%moms%f_yz, FEM1_elen%elen_nod%diff%df_yz)
      call take_1st_diffs_nod_by_consist                                &
     &   (FEM1_elen%elen_nod%moms%f_zx, FEM1_elen%elen_nod%diff%df_zx)
!
      end subroutine cal_1st_diffs_dx_by_consist
!
!-----------------------------------------------------------------------
!
      subroutine cal_diffs_filter_nod_consist(mom_nod)
!
      use t_filter_moments
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_x,  mom_nod%diff%df_x)
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_y,  mom_nod%diff%df_y)
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_z,  mom_nod%diff%df_z)
!
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_x2, mom_nod%diff%df_x2)
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_y2, mom_nod%diff%df_y2)
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_z2, mom_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_xy, mom_nod%diff%df_xy)
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_yz, mom_nod%diff%df_yz)
      call take_1st_diffs_nod_by_consist                                &
     &   (mom_nod%moms%f_zx, mom_nod%diff%df_zx)
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
     &   (FEM1_elen%elen_nod%moms%f_x2, FEM1_elen%elen_nod%diff%df_x2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (FEM1_elen%elen_nod%moms%f_y2, FEM1_elen%elen_nod%diff%df_y2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (FEM1_elen%elen_nod%moms%f_z2, FEM1_elen%elen_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (FEM1_elen%elen_nod%moms%f_xy, FEM1_elen%elen_nod%diff%df_xy)
      call take_1st_diffs_nod_by_lump                                   &
     &   (FEM1_elen%elen_nod%moms%f_yz, FEM1_elen%elen_nod%diff%df_yz)
      call take_1st_diffs_nod_by_lump                                   &
     &   (FEM1_elen%elen_nod%moms%f_zx, FEM1_elen%elen_nod%diff%df_zx)
!
      end subroutine cal_1st_diffs_dx_by_lump
!
!-----------------------------------------------------------------------
!
      subroutine cal_diffs_filter_nod_lump(mom_nod)
!
      use t_filter_moments
!
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_x,  mom_nod%diff%df_x)
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_y,  mom_nod%diff%df_y)
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_z,  mom_nod%diff%df_z)
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_x2, mom_nod%diff%df_x2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_y2, mom_nod%diff%df_y2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_z2, mom_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_xy, mom_nod%diff%df_xy)
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_yz, mom_nod%diff%df_yz)
      call take_1st_diffs_nod_by_lump                                   &
     &   (mom_nod%moms%f_zx, mom_nod%diff%df_zx)
!
      end subroutine cal_diffs_filter_nod_lump
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine take_1st_diffs_nod_by_consist(org_field, diff_field)
!
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
      use int_vol_elesize_on_node
      use cal_ff_smp_to_ffs
      use cal_sol_deltax_by_consist
!
      real(kind = kreal), intent(inout) :: org_field(node1%numnod)
      real(kind = kreal), intent(inout) :: diff_field(node1%numnod,3)
      integer(kind = kint) :: nd
!
!
      call reset_ff(node1%numnod, f1_l)
      call reset_ff_smp(node1%max_nod_smp, f1_nl)
!
      call int_vol_diff_dxs(org_field)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    f1_nl%ff_smp, f1_l%ff)
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
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
      use int_vol_elesize_on_node
      use cal_ff_smp_to_ffs
!
      real(kind = kreal), intent(inout) :: org_field(node1%numnod)
      real(kind = kreal), intent(inout)                                 &
     &                   :: diff_field(node1%numnod,n_vector)
!
      call reset_ff_smp(node1%max_nod_smp, f1_nl)
!
      call int_vol_diff_dxs(org_field)
      call cal_ff_smp_2_vector(node1, rhs_tbl1,                         &
     &    f1_nl%ff_smp, m1_lump%ml, n_vector, ione, diff_field)
!
      end subroutine take_1st_diffs_nod_by_lump
!
!-----------------------------------------------------------------------
!
      end module cal_1st_diff_deltax_4_nod
