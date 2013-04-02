!fem_skv_lorentz_full_type.f90
!      module fem_skv_lorentz_full_type
!
!     programmed by H.Matsui on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_lorentz_rot_type(iele_fsmp_stack, n_int, k2,  &
!     &          bxe_ex, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_lorentz_full_pg_type(iele_fsmp_stack,         &
!     &          n_int, k2, coef_lor, bxe, ex_magne, ele, jac_3d,       &
!     &          fem_wk)
!      subroutine fem_skv_induction_type(iele_fsmp_stack,               &
!     &          n_int, k2, coef_uxb, vxe, bxe_ex, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_stratified_type(iele_fsmp_stack,              &
!     &          n_int, k2, temp_1, vxe, xe, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_lorentz_full_upw_type(iele_fsmp_stack,        &
!     &          n_int, k2, coef_lor, vxe, bxe, ex_magne, ele,          &
!     &          jac_3d, fem_wk)
!      subroutine fem_skv_induction_upm_type(iele_fsmp_stack,           &
!     &          n_int, k2, coef_uxb, vxe, bxe_ex, bxe_up, ele,         &
!     &          jac_3d, fem_wk)
!
!      subroutine fem_skv_stratified_upw_type(iele_fsmp_stack,          &
!     &          n_int, k2, temp_1, vxe, xe, ele, jac_3d, fem_wk)
!
      module fem_skv_lorentz_full_type
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use m_fem_gauss_int_coefs
      use m_t_int_parameter
!
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_rot_type(iele_fsmp_stack, n_int, k2,   &
     &          bxe_ex, ele, jac_3d, fem_wk)
!
      use fem_skv_lorentz_full
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: bxe_ex(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_lorentz_rot                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack,  jac_3d%ntot_int, n_int, k2,         &
     &    jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,                          &
     &    fem_wk%vector_1, bxe_ex, fem_wk%sk6)
!
      end subroutine fem_skv_lorentz_rot_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_pg_type(iele_fsmp_stack,          &
     &          n_int, k2, coef_lor, bxe, ex_magne, ele, jac_3d,        &
     &          fem_wk)
!
      use fem_skv_lorentz_full
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_lor
      real (kind=kreal), intent(in) :: bxe(ele%numele,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_lorentz_full_pg                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, n_int, k2,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, coef_lor,                 &
     &    fem_wk%vector_1, bxe, ex_magne, fem_wk%sk6)
!
      end subroutine fem_skv_lorentz_full_pg_type
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_induction_type(iele_fsmp_stack,                &
     &          n_int, k2, coef_uxb, vxe, bxe_ex, ele, jac_3d, fem_wk)
!
      use fem_skv_induction
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_uxb
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_induction_pg                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, n_int, k2,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    fem_wk%velo_1, fem_wk%vector_1, vxe, bxe_ex,                  &
     &    coef_uxb, fem_wk%sk6)
!
      end subroutine fem_skv_induction_type
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_type(iele_fsmp_stack,               &
     &          n_int, k2, temp_1, vxe, xe, ele, jac_3d, fem_wk)
!
      use fem_skv_stratified
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: temp_1(ele%numele)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: xe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_stratified_pg                                        &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, n_int, k2,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%an, temp_1, vxe, xe,           &
     &    fem_wk%sk6)
!
      end subroutine fem_skv_stratified_type
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_upw_type(iele_fsmp_stack,         &
     &          n_int, k2, coef_lor, vxe, bxe, ex_magne, ele,           &
     &          jac_3d, fem_wk)
!
      use fem_skv_lorentz_full
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_lor
      real (kind=kreal), intent(in) :: bxe(ele%numele,3)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_lorentz_full_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, n_int, k2, dt,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx, coef_lor,     &
     &    fem_wk%vector_1, vxe, bxe, ex_magne, fem_wk%sk6)
!
      end subroutine fem_skv_lorentz_full_upw_type
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_induction_upm_type(iele_fsmp_stack,            &
     &          n_int, k2, coef_uxb, vxe, bxe_ex, bxe_up, ele,          &
     &          jac_3d, fem_wk)
!
      use fem_skv_induction
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_uxb
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(ele%numele,3)
      real (kind=kreal), intent(in) :: bxe_up(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_induction_upm                                        &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, n_int, k2, dt,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    fem_wk%velo_1, fem_wk%vector_1, vxe, bxe_ex, bxe_up,          &
     &    coef_uxb, fem_wk%sk6)
!
      end subroutine fem_skv_induction_upm_type
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_upw_type(iele_fsmp_stack,           &
     &          n_int, k2, temp_1, vxe, xe, ele, jac_3d, fem_wk)
!
      use fem_skv_stratified
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: temp_1(ele%numele)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: xe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_stratified_upw                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, jac_3d%ntot_int, n_int, k2, dt,      &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%an, temp_1,        &
     &    vxe, xe,  fem_wk%sk6)
!
      end subroutine fem_skv_stratified_upw_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_lorentz_full_type
