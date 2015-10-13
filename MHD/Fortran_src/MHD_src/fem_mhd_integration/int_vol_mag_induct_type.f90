!
!     module int_vol_mag_induct_type
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_mag_induct_pg_t(mesh, jac_3d, rhs_tbl,        &
!     &          nod_fld, iele_fsmp_stack, n_int, ncomp_ele, d_ele,     &
!     &          fem_wk, mhd_fem_wk, iphys_nod, iphys_ele, f_nl)
!      subroutine int_vol_mag_induct_upm_t(mesh, jac_3d, rhs_tbl,       &
!     &          nod_fld, iele_fsmp_stack, n_int, ncomp_ele, d_ele,     &
!     &          fem_wk, mhd_fem_wk, iphys_nod, iphys_ele, f_nl)
!
      module int_vol_mag_induct_type
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use t_mesh_data
      use t_phys_address
      use t_phys_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      use m_physical_property
      use m_node_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_pg_t(mesh, jac_3d, rhs_tbl,         &
     &          nod_fld, iele_fsmp_stack, n_int, ncomp_ele, d_ele,      &
     &          fem_wk, mhd_fem_wk, iphys_nod, iphys_ele, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_nod
      type(phys_address), intent(in) :: iphys_ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in)                                    &
     &                    :: d_ele(mesh%ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp(np_smp, mesh%ele%numele,             &
     &    mesh%ele%istack_ele_smp, d_ele(1,iphys_ele%i_magne),          &
     &    ex_magne, fem_wk%vxe)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &      k2, iphys_nod%i_velo,  mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &      k2, iphys_nod%i_magne, fem_wk%vector_1)
!
        call fem_skv_induction_galerkin(iele_fsmp_stack, n_int, k2,     &
     &      coef_induct, mhd_fem_wk%velo_1, fem_wk%vector_1,            &
     &      d_ele(1,iphys_ele%i_velo), fem_wk%vxe, mesh%ele, jac_3d,    &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_mag_induct_pg_t
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_upm_t(mesh, jac_3d, rhs_tbl,        &
     &          nod_fld, iele_fsmp_stack, n_int, ncomp_ele, d_ele,      &
     &          fem_wk, mhd_fem_wk, iphys_nod, iphys_ele, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_nod
      type(phys_address), intent(in) :: iphys_ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in)                                    &
     &                    :: d_ele(mesh%ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp(np_smp, mesh%ele%numele,             &
     &    mesh%ele%istack_ele_smp, d_ele(1,iphys_ele%i_magne),          &
     &    ex_magne, fem_wk%vxe)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &      k2, iphys_nod%i_velo,  mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &      k2, iphys_nod%i_magne, fem_wk%vector_1)
!
        call fem_skv_induction_upmagne(iele_fsmp_stack, n_int, k2,      &
     &      coef_induct, mhd_fem_wk%velo_1, fem_wk%vector_1,            &
     &      d_ele(1,iphys_ele%i_velo), fem_wk%vxe,                      &
     &      d_ele(1,iphys_ele%i_magne), mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_mag_induct_upm_t
!
!-----------------------------------------------------------------------
!
      end module int_vol_mag_induct_type
