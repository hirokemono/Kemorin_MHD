!
!     module int_vol_Lorentz_type
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_Lorentz_pg_t(mesh, jac_3d, nod_fld, rhs_tbl,  &
!     &          iele_fsmp_stack, n_int, bxe, fem_wk, f_nl)
!      subroutine int_vol_full_Lorentz_pg_t(mesh, jac_3d, nod_fld,      &
!     &          rhs_tbl, iele_fsmp_stack, n_int, bxe, fem_wk, f_nl)
!      subroutine int_vol_full_rot_Lorentz_pg_t(mesh, jac_3d, nod_fld,  &
!     &           rhs_tbl, iele_fsmp_stack, n_int, bxe, fem_wk, f_nl)
!
!      subroutine int_vol_Lorentz_upw_t(mesh, jac_3d, nod_fld, rhs_tbl, &
!     &         iele_fsmp_stack, n_int, bxe, vxe_up, fem_wk, f_nl)
!      subroutine int_vol_full_Lorentz_upw_t(mesh, jac_3d, nod_fld,     &
!     &          rhs_tbl, iele_fsmp_stack, n_int, bxe, vxe_up, fem_wk,  &
!     &          f_nl)
!
      module int_vol_Lorentz_type
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
      subroutine int_vol_Lorentz_pg_t(mesh, jac_3d, nod_fld, rhs_tbl,   &
     &          iele_fsmp_stack, n_int, bxe, fem_wk, f_nl)
!
      use cal_add_smp
      use int_vol_inertia_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: bxe(mesh%ele%numele,3)
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
!$omp parallel
      call add_const_to_vector_smp(np_smp, mesh%ele%numele,             &
     &    iele_fsmp_stack, bxe, ex_magne, fem_wk%vxe)
!$omp end parallel
!
      call int_vol_vector_inertia_type(mesh, jac_3d,                    &
     &    nod_fld, rhs_tbl, iele_fsmp_stack, n_int, iphys%i_magne,      &
     &    fem_wk%vxe, coef_lor, fem_wk, f_nl)
!
      end subroutine int_vol_Lorentz_pg_t
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_Lorentz_pg_t(mesh, jac_3d, nod_fld,       &
     &          rhs_tbl, iele_fsmp_stack, n_int, bxe, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_ele_type
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: bxe(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_ele_type(mesh, nod_fld, k2,             &
     &      iphys%i_magne, fem_wk%vector_1)
        call fem_skv_lorentz_full_galerkin(iele_fsmp_stack, n_int, k2,  &
     &      coef_lor, fem_wk%vector_1, bxe, ex_magne,                   &
     &      mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_full_Lorentz_pg_t
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_rot_Lorentz_pg_t(mesh, jac_3d, nod_fld,   &
     &           rhs_tbl, iele_fsmp_stack, n_int, bxe, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_ele_type
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: bxe(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_cst_phys_each_ele_type(mesh, nod_fld, k2,           &
     &      iphys%i_vecp, coef_lor, fem_wk%vector_1)
!
!$omp parallel
        call add_const_to_vector_smp(np_smp, mesh%ele%numele,           &
     &      iele_fsmp_stack, bxe, ex_magne, fem_wk%vxe)
!$omp end parallel
!
        call fem_skv_lorentz_rot_galerkin(iele_fsmp_stack, n_int, k2,   &
     &      fem_wk%vector_1, fem_wk%vxe, mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_full_rot_Lorentz_pg_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_Lorentz_upw_t(mesh, jac_3d, nod_fld, rhs_tbl,  &
     &         iele_fsmp_stack, n_int, bxe, vxe_up, fem_wk, f_nl)
!
      use cal_add_smp
      use int_vol_inertia_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: bxe(mesh%ele%numele,3)
      real(kind = kreal), intent(in) :: vxe_up(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!

!$omp parallel
      call add_const_to_vector_smp(np_smp, mesh%ele%numele,             &
     &    iele_fsmp_stack, bxe, ex_magne, fem_wk%vxe)
!$omp end parallel
!
      call int_vol_vector_inertia_upw_type(mesh, jac_3d,                &
     &    nod_fld, rhs_tbl, iele_fsmp_stack, n_int, iphys%i_magne,      &
     &    fem_wk%vxe, vxe_up, coef_lor, fem_wk, f_nl)
!
      end subroutine int_vol_Lorentz_upw_t
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_Lorentz_upw_t(mesh, jac_3d, nod_fld,      &
     &          rhs_tbl, iele_fsmp_stack, n_int, bxe, vxe_up, fem_wk,   &
     &          f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_ele_type
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: bxe(mesh%ele%numele,3)
      real(kind = kreal), intent(in) :: vxe_up(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_ele_type(mesh, nod_fld, k2,             &
     &      iphys%i_magne, fem_wk%vector_1)
        call fem_skv_lorentz_full_upwind(iele_fsmp_stack, n_int, k2,    &
     &      coef_lor, fem_wk%vector_1, vxe_up, bxe, ex_magne,           &
     &      mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_full_Lorentz_upw_t
!
!-----------------------------------------------------------------------
!
      end module int_vol_Lorentz_type
