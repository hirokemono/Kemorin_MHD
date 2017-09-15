!int_sgs_vect_cst_diff_upw.f90
!      module int_sgs_vect_cst_diff_upw
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!!      subroutine int_sgs_grad_w_const_upw                             &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,      &
!!     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!      subroutine int_sgs_div_w_const_upw                              &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,      &
!!     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!      subroutine int_sgs_rot_w_const_upw                              &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,      &
!!     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!
!!      subroutine int_sgs_div_tsr_w_const_upw                          &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,      &
!!     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!      subroutine int_sgs_div_as_tsr_w_const_upw                       &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,      &
!!     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_sgs_vect_cst_diff_upw
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_diffs_sgs_upw_type
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_grad_w_const_upw                               &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,        &
     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef,  fem_wk%scalar_1)
        call fem_skv_grad_sgs_upwind(iele_fsmp_stack, num_int, k2,      &
     &      i_filter, dt, ak_diff, ele, g_FEM, jac_3d, FEM_elens,       &
     &      d_ele(1,iv_up), fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_grad_w_const_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_w_const_upw                                &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,        &
     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_div_sgs_upwind(iele_fsmp_stack, num_int, k2,       &
     &      i_filter, dt, ak_diff, ele, g_FEM, jac_3d, FEM_elens,       &
     &      d_ele(1,iv_up), fem_wk%vector_1, fem_wk%sk6)
   end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_w_const_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_rot_w_const_upw                                &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,        &
     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_rot_sgs_upwind(iele_fsmp_stack, num_int, k2,       &
     &      i_filter, dt, ak_diff, ele, g_FEM, jac_3d, FEM_elens,       &
     &      d_ele(1,iv_up), fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_rot_w_const_upw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_tsr_w_const_upw                            &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,        &
     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call tensor_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%tensor_1)
        call fem_skv_div_tsr_sgs_upwind(iele_fsmp_stack, num_int, k2,   &
     &      i_filter, dt, ak_diff, ele, g_FEM, jac_3d, FEM_elens,       &
     &      d_ele(1,iv_up), fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_tsr_w_const_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_as_tsr_w_const_upw                         &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, dt, i_filter, ak_diff,        &
     &          i_field, coef, ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_div_as_tsr_sgs_upwind                              &
     &     (iele_fsmp_stack, num_int, k2, i_filter, dt,                 &
     &      ak_diff, ele, g_FEM, jac_3d, FEM_elens, d_ele(1,iv_up),     &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_as_tsr_w_const_upw
!
!-----------------------------------------------------------------------
!
      end module int_sgs_vect_cst_diff_upw
