!
!     module int_vol_elesize_on_node
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Mar., 2008
!
!      subroutine allocate_scalar_ele_4_int
!      subroutine deallocate_scalar_ele_4_int
!
!      subroutine int_dx_ele2_node(nod_comm, node, ele,                 &
!     &          jac_3d, rhs_tbl, tbl_crs, m_lump, itype_mass,          &
!     &          mass, elen_ele, elen_nod, fem_wk, f_l)
!      subroutine int_vol_diff_dxs(node, ele, jac_3d,                   &
!     &          rhs_tbl, fem_wk, f_nl, elen_org_nod)
!
      module int_vol_elesize_on_node
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_ctl_params_4_gen_filter
      use m_phys_constants
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
      subroutine int_dx_ele2_node(nod_comm, node, ele,                  &
     &          jac_3d, rhs_tbl, tbl_crs, m_lump, itype_mass,           &
     &          mass, elen_ele, elen_nod, fem_wk, f_l)
!
      use m_element_list_4_filter
!
      use t_comm_table
      use t_geometry_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_crs_matrix
!
      use int_element_field_2_node
      use cal_ff_smp_to_ffs
      use cal_sol_deltax_by_consist
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      integer(kind = kint), intent(in) :: itype_mass
      real(kind = kreal), intent(in) :: elen_ele(ele%numele)
!
      type(CRS_matrix), intent(inout) :: mass
      real(kind = kreal), intent(inout) :: elen_nod(node%numnod)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (id_filter_area_grp(1) .eq. -1) then
        call int_area_ele_scalar_2_node                                 &
     &     (node, ele, g_FEM1, jac_3d, rhs_tbl,  ele%istack_ele_smp,    &
     &      elen_ele, fem_wk, f_l)
      else
        call int_grp_ele_scalar_2_node                                  &
     &     (node, ele, g_FEM1, jac_3d, rhs_tbl, iele_filter_smp_stack,  &
     &      nele_4_filter, iele_4_filter, elen_ele, fem_wk, f_l)
      end if
!
!
      if (itype_mass .eq. 1) then
        call cal_ff_smp_2_scalar(node, rhs_tbl, f_l%ff_smp,           &
     &      m_lump%ml, n_scalar, ione, elen_nod)
      else
        call reset_ff(node%numnod, f_l)
        call cal_ff_smp_2_ff                                            &
     &     (node, rhs_tbl, n_scalar, f_l%ff_smp, f_l%ff)
        call cal_sol_dx_by_consist                                      &
     &     (ione, node, nod_comm, tbl_crs, f_l, mass, elen_nod)
      end if
!
      end subroutine int_dx_ele2_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_diff_dxs(node, ele, jac_3d,                    &
     &          rhs_tbl, fem_wk, f_nl, elen_org_nod)
!
      use t_geometry_data
      use m_fem_gauss_int_coefs
      use t_jacobians
!
      use t_table_FEM_const
      use t_finite_element_mat
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_vector_diff_type
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      real(kind = kreal), intent(inout) :: elen_org_nod(node%numnod)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_2_each_element(node, ele,                          &
     &      k2, elen_org_nod, scalar_ele)
        call fem_skv_gradient(ele%istack_ele_smp, num_int_points,      &
     &      k2, ele, g_FEM1, jac_3d, scalar_ele, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                    &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_diff_dxs
!
!-----------------------------------------------------------------------
!
      end module int_vol_elesize_on_node
