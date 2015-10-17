!
!     module int_vol_elesize_on_node
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Mar., 2008
!
!      subroutine allocate_scalar_ele_4_int
!      subroutine deallocate_scalar_ele_4_int
!
!      subroutine int_dx_ele2_node(nd, nele_filter_mom, elen_ele)
!      subroutine int_vol_diff_dxs(elen_org_nod)
!
      module int_vol_elesize_on_node
!
      use m_precision
      use m_constants
!
      use m_ctl_params_4_gen_filter
      use m_geometry_data
      use m_machine_parameter
      use m_phys_constants
!
      use m_finite_element_matrix
!
      implicit none
!
      real(kind = kreal), allocatable :: scalar_ele(:)
      private :: scalar_ele
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_scalar_ele_4_int(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      allocate( scalar_ele(numele) )
      scalar_ele = 0.0d0
!
      end subroutine allocate_scalar_ele_4_int
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_scalar_ele_4_int
!
      deallocate( scalar_ele )
!
      end subroutine deallocate_scalar_ele_4_int
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
      subroutine int_dx_ele2_node(itype_mass, elen_ele, elen_nod)
!
      use m_jacobians
      use m_sorted_node
      use m_finite_element_matrix
      use m_element_list_4_filter
      use int_element_field_2_node
      use cal_ff_smp_to_ffs
      use cal_sol_deltax_by_consist
!
      integer(kind = kint), intent(in) :: itype_mass
      real(kind = kreal), intent(in) :: elen_ele(ele1%numele)
      real(kind = kreal), intent(inout) :: elen_nod(node1%numnod)
!
!
      if (id_filter_area_grp(1) .eq. -1) then
        call int_area_ele_scalar_2_node                                 &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1,  ele1%istack_ele_smp,     &
     &      elen_ele, fem1_wk, f1_l)
      else
        call int_grp_ele_scalar_2_node                                  &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, iele_filter_smp_stack,    &
     &      nele_4_filter, iele_4_filter, elen_ele, fem1_wk, f1_l)
      end if
!
!
      if (itype_mass .eq. 1) then
        call cal_ff_smp_2_scalar(node1, rhs_tbl1, f1_l%ff_smp,          &
     &      m1_lump%ml, n_scalar, ione, elen_nod)
      else
        call reset_ff(node1%numnod, f1_l)
        call cal_ff_smp_2_ff                                            &
     &     (node1, rhs_tbl1, n_scalar, f1_l%ff_smp, f1_l%ff)
        call cal_sol_dx_by_consist(elen_nod, ione)
      end if
!
      end subroutine int_dx_ele2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_diff_dxs(elen_org_nod)
!
      use m_jacobians
      use m_sorted_node
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_vector_diff_type
!
      real(kind = kreal), intent(inout) :: elen_org_nod(node1%numnod)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_2_each_element(node1, ele1,                         &
     &      k2, elen_org_nod, scalar_ele)
        call fem_skv_gradient(ele1%istack_ele_smp, num_int_points,      &
     &      k2, ele1, jac1_3d_q, scalar_ele, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_diff_dxs
!
!-----------------------------------------------------------------------
!
      end module int_vol_elesize_on_node
