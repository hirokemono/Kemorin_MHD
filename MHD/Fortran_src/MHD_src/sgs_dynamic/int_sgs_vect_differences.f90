!int_sgs_vect_differences.f90
!      module int_sgs_vect_differences
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!!      subroutine int_sgs_gradient                                     &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, i_filter,                   &
!!     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
!!      subroutine int_sgs_divergence                                   &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, i_filter,                   &
!!     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
!!      subroutine int_sgs_rotation                                     &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, i_filter,                   &
!!     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
!!
!!      subroutine int_sgs_div_tensor                                   &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, i_filter,                   &
!!     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
!!      subroutine int_sgs_div_as_tsr                                   &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,&
!!     &          iele_fsmp_stack, num_int, i_filter,                   &
!!     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!
!
      module int_sgs_vect_differences
!
      use m_precision
!
      use m_phys_constants
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_diffs_sgs_type
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_gradient                                       &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, i_filter,                     &
     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
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
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%scalar_1)
        call fem_skv_grad_sgs_galerkin                                  &
     &    (iele_fsmp_stack, num_int, k2, i_filter, ak_diff(1,iak_diff), &
     &     ele, g_FEM, jac_3d, FEM_elens, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_gradient
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_divergence                                     &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, i_filter,                     &
     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
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
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%vector_1)
        call fem_skv_div_sgs_galerkin                                   &
     &    (iele_fsmp_stack, num_int, k2, i_filter, ak_diff(1,iak_diff), &
     &     ele, g_FEM, jac_3d, FEM_elens, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_divergence
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_rotation                                       &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, i_filter,                     &
     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
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
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%vector_1)
        call fem_skv_rot_sgs_galerkin                                   &
     &    (iele_fsmp_stack, num_int, k2, i_filter, ak_diff(1,iak_diff), &
     &     ele, g_FEM, jac_3d, FEM_elens, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_rotation
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_tensor                                     &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, i_filter,                     &
     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
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
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
        call tensor_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%tensor_1)
        call fem_skv_div_tensor_sgs_galerkin                            &
     &    (iele_fsmp_stack, num_int, k2, i_filter, ak_diff(1,iak_diff), &
     &     ele, g_FEM, jac_3d, FEM_elens, fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_tensor
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_as_tsr                                     &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld, FEM_elens,  &
     &          iele_fsmp_stack, num_int, i_filter,                     &
     &          ncomp_diff, iak_diff, ak_diff, i_field, fem_wk, f_nl)
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
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
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
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%vector_1)
        call fem_skv_div_as_tsr_sgs_galerkin                            &
     &    (iele_fsmp_stack, num_int, k2, i_filter, ak_diff(1,iak_diff), &
     &     ele, g_FEM, jac_3d, FEM_elens, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_as_tsr
!
!-----------------------------------------------------------------------
!
      end module int_sgs_vect_differences
