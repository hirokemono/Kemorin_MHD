!
!     module int_vol_commute_error
!
!     Written by H. Matsui
!
!!      subroutine int_vol_commute_grad(iele_fsmp_stack, n_int,         &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          i_filter, i_scalar, fem_wk, f_nl)
!!      subroutine int_vol_commute_div(iele_fsmp_stack, n_int,          &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          i_filter, i_vect, fem_wk, f_nl)
!!      subroutine int_vol_commute_rot(iele_fsmp_stack, n_int,          &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          i_filter, i_vect, fem_wk, f_nl)
!!
!!      subroutine int_vol_commute_div_v_flux(iele_fsmp_stack, n_int,   &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          i_filter, i_flux, i_vect, i_scalar, fem_wk, f_nl)
!!      subroutine int_vol_commute_div_m_flux(iele_fsmp_stack, n_int,   &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          i_filter, i_flux, i_vect, fem_wk, f_nl)
!!      subroutine int_vol_commute_induct_t(iele_fsmp_stack, n_int,     &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          i_filter, i_flux, i_v, i_b, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_commute_error
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use m_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      use fem_skv_commute_err_diffs
      use cal_skv_to_ff_smp
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_grad(iele_fsmp_stack, n_int,           &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          i_filter, i_scalar, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_scalar
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
       do k2 = 1, ele%nnod_4_ele
!
! --------- set temperature at each node in an element
!
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_scalar, fem_wk%scalar_1)
!
        call fem_skv_commute_err_grad_t(iele_fsmp_stack, n_int,         &
     &      k2, i_filter, ele, g_FEM1, jac_3d, FEM_elens, fem_wk)
       end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_commute_grad
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_div(iele_fsmp_stack, n_int,            &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          i_filter, i_vect, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_vect
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
      do k2 = 1, ele%nnod_4_ele
!
! --------- set temperature at each node in an element
!
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_vect, fem_wk%vector_1)
        call fem_skv_commute_err_div_t(iele_fsmp_stack, n_int,          &
     &      k2, i_filter, ele, g_FEM1, jac_3d, FEM_elens, fem_wk)
       end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_commute_div
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_rot(iele_fsmp_stack, n_int,            &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          i_filter, i_vect, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_vect
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_vect, fem_wk%vector_1)
        call fem_skv_commute_err_rot_t(iele_fsmp_stack, n_int,          &
     &      k2, i_filter, ele, g_FEM1, jac_3d, FEM_elens, fem_wk)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_commute_rot
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_commute_div_v_flux(iele_fsmp_stack, n_int,     &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          i_filter, i_flux, i_vect, i_scalar, fem_wk, f_nl)
!
      use sgs_terms_2_each_ele
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
      do k2 = 1, ele%nnod_4_ele
        call SGS_v_flux_2_each_element                                  &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      ele%istack_ele_smp, k2, nod_fld%ntot_phys,                  &
     &      i_vect, i_scalar, i_flux, nod_fld%d_fld, fem_wk%vector_1)
        call fem_skv_commute_err_div_t(iele_fsmp_stack, n_int,          &
     &      k2, i_filter, ele, g_FEM1, jac_3d, FEM_elens, fem_wk)
       end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_commute_div_v_flux
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_commute_div_m_flux(iele_fsmp_stack, n_int,     &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          i_filter, i_flux, i_vect, fem_wk, f_nl)
!
      use sgs_terms_2_each_ele
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_flux, i_vect
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
      do k2 = 1, ele%nnod_4_ele
        call SGS_m_flux_2_each_element                                  &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      ele%istack_ele_smp, k2, nod_fld%ntot_phys,                  &
     &      i_vect, i_flux, nod_fld%d_fld, fem_wk%tensor_1)
        call fem_skv_commute_err_div_tsr_t(iele_fsmp_stack, n_int,      &
     &      k2, i_filter, ele, g_FEM1, jac_3d, FEM_elens, fem_wk)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_commute_div_m_flux
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_commute_induct_t(iele_fsmp_stack, n_int,       &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          i_filter, i_flux, i_v, i_b, fem_wk, f_nl)
!
      use sgs_terms_2_each_ele
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_filter, n_int
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
      do k2 = 1, ele%nnod_4_ele
        call SGS_induct_2_each_element                                  &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,            &
     &      ele%istack_ele_smp, k2, nod_fld%ntot_phys,                  &
     &      i_b, i_v, i_flux, nod_fld%d_fld, fem_wk%vector_1)
        call fem_skv_commute_err_div_ast_t(iele_fsmp_stack, n_int,      &
     &      k2, i_filter, ele, g_FEM1, jac_3d, FEM_elens, fem_wk)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_commute_induct_t
!
!-----------------------------------------------------------------------
!
      end module int_vol_commute_error
