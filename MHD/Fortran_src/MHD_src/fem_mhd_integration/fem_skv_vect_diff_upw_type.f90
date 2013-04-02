!fem_skv_vect_diff_upw_type.f90
!      module fem_skv_vect_diff_upw_type
!
!     programmed by H.Matsui on May 2012
!
!      subroutine fem_skv_grad_upw_type(iele_fsmp_stack, n_int, k2, vxe,&
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_div_upw_type(iele_fsmp_stack, n_int, k2, vxe, &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_rot_upw_type(iele_fsmp_stack, n_int, k2, vxe, &
!     &          ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_div_tsr_upw_type(iele_fsmp_stack,             &
!     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!      subroutine fem_skv_div_asym_t_upw_type(iele_fsmp_stack,          &
!     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_grp_grad_upw_type(iele_fsmp_stack,            &
!     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!      subroutine fem_skv_grp_div_upw_type(iele_fsmp_stack,             &
!     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!      subroutine fem_skv_grp_rot_upw_type(iele_fsmp_stack,             &
!     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_grp_div_flux_upw_type(iele_fsmp_stack,        &
!     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!      subroutine fem_skv_grp_div_asym_t_upw_type(iele_fsmp_stack,      &
!     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_linear_grad_upw_type(iele_fsmp_stack,         &
!     &          n_int, k2, vxe, ele, jac_3d, jac_3d_l, fem_wk)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(jacobians_3d), intent(in) :: jac_3d_l
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        integer(kind=kint), intent(in) :: n_int, k2
!        integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
!
      module fem_skv_vect_diff_upw_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_fem_gauss_int_coefs
      use m_t_int_parameter
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grad_upw_type(iele_fsmp_stack, n_int, k2, vxe, &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_grad_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      call fem_skv_all_grad_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    vxe, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_grad_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_upw_type(iele_fsmp_stack, n_int, k2, vxe,  &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_div_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_div_upw                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    vxe, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_upw_type(iele_fsmp_stack, n_int, k2, vxe,  &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_rot_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      call fem_all_skv_rot_upw                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    vxe, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_rot_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tsr_upw_type(iele_fsmp_stack,              &
     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_div_flux_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_div_flux_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    vxe, fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_tsr_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_asym_t_upw_type(iele_fsmp_stack,           &
     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_div_asym_t_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_div_asym_t_upw                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    vxe, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_asym_t_upw_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_grad_upw_type(iele_fsmp_stack,             &
     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_grad_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_grad_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    n_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,                  &
     &    jac_3d%an, jac_3d%dnx, jac_3d%dnx,                            &
     &    vxe, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_grad_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_upw_type(iele_fsmp_stack,              &
     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_div_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_div_upw                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    n_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,                  &
     &    jac_3d%an, jac_3d%dnx, jac_3d%dnx,                            &
     &    vxe, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_div_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_rot_upw_type(iele_fsmp_stack,              &
     &         nele_grp, iele_grp, n_int, k2, ele, vxe, jac_3d, fem_wk)
!
      use fem_skv_rot_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_rot_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                  &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                 &
     &    n_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,                 &
     &    jac_3d%an, jac_3d%dnx, jac_3d%dnx,                           &
     &    vxe, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_rot_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_flux_upw_type(iele_fsmp_stack,         &
     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_div_flux_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_div_flux_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    n_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,                  &
     &    jac_3d%an, jac_3d%dnx, jac_3d%dnx,                            &
     &    vxe, fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_div_flux_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_asym_t_upw_type(iele_fsmp_stack,       &
     &         nele_grp, iele_grp, n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_div_asym_t_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grp_div_asym_t_upw                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    n_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,                  &
     &    jac_3d%an, jac_3d%dnx, jac_3d%dnx,                            &
     &    vxe, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_grp_div_asym_t_upw_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_linear_grad_upw_type(iele_fsmp_stack,          &
     &          n_int, k2, vxe, ele, jac_3d, jac_3d_l, fem_wk)
!
      use fem_skv_grad_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_all_grad_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, num_t_linear,                     &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d_l%dnx,             &
     &    vxe, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_linear_grad_upw_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_vect_diff_upw_type
