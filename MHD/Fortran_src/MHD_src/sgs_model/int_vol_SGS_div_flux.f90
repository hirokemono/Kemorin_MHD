!int_vol_SGS_div_flux.f90
!     module int_vol_SGS_div_flux
!
!      Written by H. Matsui on june, 2005
!
!!      subroutine int_vol_div_SGS_vec_flux(node, ele, nod_fld,         &
!!     &           jac_3d, rhs_tbl, FEM_elens, diff_coefs,              &
!!     &           iele_fsmp_stack, n_int, i_vector, i_scalar,          &
!!     &           i_SGS_flux, i_filter, iak_diff, coef, fem_wk,        &
!!     &           mhd_fem_wk, f_nl)
!!      subroutine int_vol_div_SGS_tsr_flux(node, ele, nod_fld,         &
!!     &           jac_3d, rhs_tbl, FEM_elens, diff_coefs,              &
!!     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter,&
!!     &           iak_diff, coef, fem_wk, mhd_fem_wk, f_nl)
!!
!!      subroutine int_vol_div_SGS_vec_flux_upw                         &
!!     &          (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,      &
!!     &           iele_fsmp_stack, n_int, i_vector, i_scalar,          &
!!     &           i_SGS_flux, i_filter, ak_diff, ncomp_ele, ie_upw,    &
!!     &           d_ele, coef, fem_wk, mhd_fem_wk, f_nl)
!!      subroutine int_vol_div_SGS_tsr_flux_upw(node, ele, nod_fld,     &
!!     &           jac_3d, rhs_tbl, FEM_elens, diff_coefs,              &
!!     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter,&
!!     &           iak_diff, ncomp_ele, ie_upw, d_ele, coef,            &
!!     &           fem_wk, mhd_fem_wk, f_nl)
!
      module int_vol_SGS_div_flux
!
      use m_precision
      use m_control_parameter
!
      use m_phys_constants
      use t_mesh_data
      use t_phys_data
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_vec_flux(node, ele, nod_fld,           &
     &           jac_3d, rhs_tbl, FEM_elens, diff_coefs,                &
     &           iele_fsmp_stack, n_int, i_vector, i_scalar,            &
     &           i_SGS_flux, i_filter, iak_diff, coef, fem_wk,          &
     &           mhd_fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_scalar
      integer(kind = kint), intent(in) :: i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
!
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call SGS_const_vector_each_ele(node, ele, nod_fld,              &
     &      k2, i_vector, i_scalar, i_SGS_flux, coef,                   &
     &      mhd_fem_wk%sgs_v1, fem_wk%vector_1)
        call fem_skv_div_sgs_vector                                     &
     &     (iele_fsmp_stack, n_int, k2, i_filter,                       &
     &      diff_coefs%num_field, iak_diff, diff_coefs%ak,              &
     &      ele, jac_3d, FEM_elens, mhd_fem_wk%sgs_v1,                  &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_vec_flux
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_tsr_flux(node, ele, nod_fld,           &
     &           jac_3d, rhs_tbl, FEM_elens, diff_coefs,                &
     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter,  &
     &           iak_diff, coef, fem_wk, mhd_fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vect, i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
!
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call SGS_const_tensor_each_ele                                  &
     &     (node, ele, nod_fld, k2, i_vect, i_SGS_flux, coef,           &
     &      mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
        call fem_skv_div_sgs_tensor                                     &
     &     (iele_fsmp_stack, n_int, k2, i_filter,                       &
     &      diff_coefs%num_field, iak_diff, diff_coefs%ak,              &
     &      ele, jac_3d, FEM_elens, mhd_fem_wk%sgs_t1,                  &
     &      fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_tsr_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_vec_flux_upw(node, ele, nod_fld,       &
     &           jac_3d, rhs_tbl, FEM_elens, diff_coefs,                &
     &           iele_fsmp_stack, n_int, i_vector, i_scalar,            &
     &           i_SGS_flux, i_filter, iak_diff, ncomp_ele, ie_upw,     &
     &           d_ele, coef, fem_wk, mhd_fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data), intent(in) :: nod_fld
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_scalar
      integer(kind = kint), intent(in) :: i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
!
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call SGS_const_vector_each_ele(node, ele, nod_fld,              &
     &      k2, i_vector, i_scalar, i_SGS_flux, coef,                   &
     &      mhd_fem_wk%sgs_v1, fem_wk%vector_1)
        call fem_skv_div_sgs_vector_upwind                              &
     &     (iele_fsmp_stack, n_int, k2, i_filter,                       &
     &      diff_coefs%num_field, iak_diff, diff_coefs%ak,              &
     &      ele, jac_3d, FEM_elens, d_ele(1,ie_upw),                    &
     &      mhd_fem_wk%sgs_v1, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_vec_flux_upw
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_tsr_flux_upw(node, ele, nod_fld,       &
     &           jac_3d, rhs_tbl, FEM_elens, diff_coefs,                &
     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter,  &
     &           iak_diff, ncomp_ele, ie_upw, d_ele, coef,              &
     &           fem_wk, mhd_fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vect, i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter, iak_diff
!
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call SGS_const_tensor_each_ele                                  &
     &     (node, ele, nod_fld, k2, i_vect, i_SGS_flux, coef,           &
     &      mhd_fem_wk%sgs_t1, fem_wk%tensor_1)
        call fem_skv_div_sgs_tensor_upwind                              &
     &     (iele_fsmp_stack, n_int, k2, i_filter,                       &
     &      diff_coefs%num_field, iak_diff, diff_coefs%ak,              &
     &      ele, jac_3d, FEM_elens, d_ele(1,ie_upw),                    &
     &      mhd_fem_wk%sgs_t1, fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_tsr_flux_upw
!
! ----------------------------------------------------------------------
!
      end module int_vol_SGS_div_flux
