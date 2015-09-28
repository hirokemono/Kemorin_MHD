!
!     module int_vol_coriolis_type
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_coriolis_pg_t(mesh, jac_3d, nod_fld, rhs_tbl, &
!     &          iele_fsmp_stack, n_int, fem_wk, f_nl)
!      subroutine int_vol_coriolis_upw_t(mesh, jac_3d, nod_fld, rhs_tbl,&
!     &          iele_fsmp_stack, n_int, vxe_up, fem_wk, f_nl)
!
      module int_vol_coriolis_type
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use t_mesh_data
      use t_phys_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      use m_node_phys_address
      use m_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_pg_t(mesh, jac_3d, nod_fld, rhs_tbl,  &
     &          iele_fsmp_stack, n_int, fem_wk, f_nl)
!
      use nodal_fld_cst_to_ele_type
      use cal_skv_to_ff_smp_type
      use fem_skv_nonlinear_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, iphys%i_velo, coef_cor, fem_wk%vector_1)
        call fem_skv_coriolis_type(iele_fsmp_stack, n_int, k2,          &
     &      fem_wk%vector_1, angular, mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_coriolis_pg_t
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_upw_t(mesh, jac_3d, nod_fld, rhs_tbl, &
     &          iele_fsmp_stack, n_int, vxe_up, fem_wk, f_nl)
!
      use nodal_fld_cst_to_ele_type
      use cal_skv_to_ff_smp_type
      use fem_skv_nonlinear_upw_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: vxe_up(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, iphys%i_velo, coef_cor, fem_wk%vector_1)
        call fem_skv_coriolis_upw_type(iele_fsmp_stack, n_int, k2,      &
     &      angular, vxe_up, mesh%ele, jac_3d, fem_wk)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_coriolis_upw_t
!
!-----------------------------------------------------------------------
!
      end module int_vol_coriolis_type
