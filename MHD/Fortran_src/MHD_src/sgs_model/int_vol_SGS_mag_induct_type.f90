!int_vol_SGS_mag_induct_type.f90
!     module int_vol_SGS_mag_induct_type
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_div_SGS_idct_mod_pg_t(mesh, jac_3d, rhs_tbl,  &
!     &          nod_fld, FEM_elens, iele_fsmp_stack, n_int,            &
!     &          i_filter, ak_diff, fem_wk, mhd_fem_wk, f_nl)
!      subroutine int_vol_div_SGS_idct_mod_upm_t(mesh, jac_3d, rhs_tbl, &
!     &          nod_fld, FEM_elens, iele_fsmp_stack, n_int,            &
!     &          i_filter, ak_diff, vxe_up, fem_wk, mhd_fem_wk, f_nl)
!
      module int_vol_SGS_mag_induct_type
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
      use t_MHD_finite_element_mat
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
      subroutine int_vol_div_SGS_idct_mod_pg_t(mesh, jac_3d, rhs_tbl,   &
     &          nod_fld, FEM_elens, iele_fsmp_stack, n_int,             &
     &          i_filter, ak_diff, fem_wk, mhd_fem_wk, f_nl)
!
      use sgs_terms_to_each_ele_type
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int, i_filter
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
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
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call SGS_induct_cst_each_ele_type(mesh, nod_fld, k2,            &
     &      iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,          &
     &      coef_induct, mhd_fem_wk%sgs_v1, fem_wk%vector_1)
         call fem_skv_div_sgs_asym_tsr(iele_fsmp_stack, n_int, k2,      &
     &       i_filter, ak_diff, mesh%ele, jac_3d, FEM_elens,            &
     &       mhd_fem_wk%sgs_v1, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_idct_mod_pg_t
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_idct_mod_upm_t(mesh, jac_3d, rhs_tbl,  &
     &          nod_fld, FEM_elens, iele_fsmp_stack, n_int,             &
     &          i_filter, ak_diff, vxe_up, fem_wk, mhd_fem_wk, f_nl)
!
      use sgs_terms_to_each_ele_type
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_upw
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int, i_filter
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real (kind=kreal), intent(in) :: vxe_up(mesh%ele%numele,3)
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
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call SGS_induct_cst_each_ele_type(mesh, nod_fld, k2,            &
     &      iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,          &
     &      coef_induct, mhd_fem_wk%sgs_v1, fem_wk%vector_1)
        call fem_skv_div_sgs_asym_t_upwind(iele_fsmp_stack, n_int, k2,  &
     &      i_filter, ak_diff, mesh%ele, jac_3d, FEM_elens,             &
     &      vxe_up, mhd_fem_wk%sgs_v1, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_idct_mod_upm_t
!
!-----------------------------------------------------------------------
!
      end module int_vol_SGS_mag_induct_type
