!fem_skv_vector_diff_type.f90
!      module fem_skv_vector_diff_type
!
!     programmed by H.Matsui on May 2012
!
!      subroutine fem_skv_gradient_type(iele_fsmp_stack, n_int, k2,     &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_divergence_type(iele_fsmp_stack, n_int, k2,   &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_rotation_type(iele_fsmp_stack, n_int, k2,     &
!     &          ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_div_flux_type(iele_fsmp_stack, n_int, k2,     &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_div_asym_t_type(iele_fsmp_stack, n_int, k2,   &
!     &          ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_grp_gradient_type(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!      subroutine fem_skv_grp_divergence_type(iele_fsmp_stack,          &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!      subroutine fem_skv_grp_rotation_type(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_grp_div_flux_type(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!      subroutine fem_skv_grp_div_asym_t_type(iele_fsmp_stack,          &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_linear_grad_type(iele_fsmp_stack, n_int, k2,  &
!     &          ele, jac_3d, jac_3d_l, fem_wk)
!      subroutine fem_skv_div_to_linear_type(iele_fsmp_stack, n_int, k2,&
!     &          ele, jac_3d, jac_3d_l, fem_wk)
!        integer(kind=kint), intent(in) :: n_int, k2
!        integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(jacobians_3d), intent(in) :: jac_3d_l
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      module fem_skv_vector_diff_type
!
      use m_precision
!
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
      use m_machine_parameter
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_gradient_type(iele_fsmp_stack, n_int, k2,      &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      call fem_skv_all_grad(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_gradient_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_divergence_type(iele_fsmp_stack, n_int, k2,    &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_div(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_divergence_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rotation_type(iele_fsmp_stack, n_int, k2,      &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_rot
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      call fem_all_skv_rot(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_rotation_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_flux_type(iele_fsmp_stack, n_int, k2,      &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_div_flux
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_div_flux                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_flux_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_asym_t_type(iele_fsmp_stack, n_int, k2,    &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_div_asym_t
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_div_asym_t                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_asym_t_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_gradient_type(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_grad(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_gradient_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence_type(iele_fsmp_stack,           &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_div(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_divergence_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_rotation_type(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!
      use fem_skv_rot
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_rot(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_rotation_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_flux_type(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!
      use fem_skv_div_flux
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_div_flux                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_div_flux_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_asym_t_type(iele_fsmp_stack,           &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d, fem_wk)
!
      use fem_skv_div_asym_t
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_div_asym_t                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_div_asym_t_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_linear_grad_type(iele_fsmp_stack, n_int, k2,   &
     &          ele, jac_3d, jac_3d_l, fem_wk)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_grad(ele%numele, ele%nnod_4_ele, num_t_linear,   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d_l%dnx,                         &
     &    fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_linear_grad_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_to_linear_type(iele_fsmp_stack, n_int, k2, &
     &          ele, jac_3d, jac_3d_l, fem_wk)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_div(ele%numele, num_t_linear, ele%nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d_l%an, jac_3d%dnx,                         &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_to_linear_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_vector_diff_type
