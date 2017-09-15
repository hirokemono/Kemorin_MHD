!int_vol_sgs_fractional.f90
!     module int_vol_sgs_fractional
!
!      Written by H. Matsui on june, 2005
!
!!      subroutine int_vol_sgs_div_v_linear(node, ele,                  &
!!     &          jac_3d, jac_3d_l, rhs_tbl, FEM_elens, nod_fld,        &
!!     &          iele_fsmp_stack, n_int, i_vector, i_filter,           &
!!     &          ncomp_diff, iak_diff, ak_diff, fem_wk, f_l)
!!      subroutine int_vol_sgs_solenoidal_co(node, ele,                 &
!!     &          jac_3d, jac_3d_l, rhs_tbl, FEM_elens, nod_fld,        &
!!     &          iele_fsmp_stack, n_int, i_scalar, i_filter,           &
!!     &          ncomp_diff, iak_diff, ak_diff, fem_wk, f_nl)
!!
!!      subroutine int_vol_scalar_sgs_diffuse                           &
!!     &         (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,       &
!!     &          iele_fsmp_stack, n_int, coef_crank, ak_d,             &
!!     &          i_scalar, i_filter, ncomp_diff, iak_diff, ak_diff,    &
!!     &          fem_wk, f_l)
!!      subroutine int_vol_vector_sgs_diffuse                           &
!!     &         (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,       &
!!     &          iele_fsmp_stack, n_int, coef_crank, ak_d,             &
!!     &          i_vector, i_filter, ncomp_diff, iak_diff, ak_diff,    &
!!     &          fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_vol_sgs_fractional
!
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use m_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_filter_elength
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sgs_div_v_linear(node, ele,                    &
     &          jac_3d, jac_3d_l, rhs_tbl, FEM_elens, nod_fld,          &
     &          iele_fsmp_stack, n_int, i_vector, i_filter,             &
     &          ncomp_diff, iak_diff, ak_diff, fem_wk, f_l)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_diffs_sgs_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2=1, num_t_linear
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_vector, fem_wk%vector_1)
        call fem_skv_div_sgs_linear                                     &
     &     (iele_fsmp_stack, n_int, k2, i_filter, ak_diff(1,iak_diff),  &
     &      ele, g_FEM1, jac_3d, jac_3d_l, FEM_elens, fem_wk%vector_1,  &
     &      fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_sgs_div_v_linear
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_solenoidal_co(node, ele,                   &
     &          jac_3d, jac_3d_l, rhs_tbl, FEM_elens, nod_fld,          &
     &          iele_fsmp_stack, n_int, i_scalar, i_filter,             &
     &          ncomp_diff, iak_diff, ak_diff, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_diffs_sgs_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2=1, num_t_linear
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_scalar, fem_wk%scalar_1)
        call fem_skv_grad_sgs_linear                                    &
     &     (iele_fsmp_stack, n_int, k2, i_filter, ak_diff(1,iak_diff),  &
     &      ele, g_FEM1, jac_3d, jac_3d_l, FEM_elens, fem_wk%scalar_1,  &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_sgs_solenoidal_co
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_scalar_sgs_diffuse                             &
     &         (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,         &
     &          iele_fsmp_stack, n_int, coef_crank, ak_d,               &
     &          i_scalar, i_filter, ncomp_diff, iak_diff, ak_diff,      &
     &          fem_wk, f_l)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_diffusion_sgs_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: i_filter
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_scalar, fem_wk%scalar_1)
        call fem_skv_scalar_diffuse_sgs_type(iele_fsmp_stack, n_int,    &
     &      k2, i_filter, ak_diff(1,iak_diff), ak_d,                    &
     &      ele, g_FEM1, jac_3d, FEM_elens, fem_wk%scalar_1,            &
     &      fem_wk%sk6)
      end do
!
      call add1_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_crank, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_scalar_sgs_diffuse
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_sgs_diffuse                             &
     &         (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,         &
     &          iele_fsmp_stack, n_int, coef_crank, ak_d,               &
     &          i_vector, i_filter, ncomp_diff, iak_diff, ak_diff,      &
     &          fem_wk, f_l)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_diffusion_sgs_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: i_filter
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      integer(kind=kint), intent(in) :: ncomp_diff, iak_diff
      real(kind=kreal), intent(in) :: ak_diff(ele%numele,ncomp_diff)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_vector, fem_wk%vector_1)
        call fem_skv_vector_diffuse_sgs_type(iele_fsmp_stack, n_int,    &
     &      k2, i_filter, ak_diff(1,iak_diff), ak_d,                    &
     &      ele, g_FEM1, jac_3d, FEM_elens, fem_wk%vector_1,            &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_crank, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_vector_sgs_diffuse
!
!  ---------------------------------------------------------------------
!
      end module int_vol_sgs_fractional
