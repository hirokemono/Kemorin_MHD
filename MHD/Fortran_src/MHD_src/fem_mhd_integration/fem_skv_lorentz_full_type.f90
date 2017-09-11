!fem_skv_lorentz_full_type.f90
!      module fem_skv_lorentz_full_type
!
!     programmed by H.Matsui on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_lorentz_rot_galerkin(iele_fsmp_stack,         &
!     &           n_int, k2, vector_1, bxe_ex, ele, jac_3d, sk_v)
!
!      subroutine fem_skv_lorentz_full_galerkin(iele_fsmp_stack,        &
!     &          n_int, k2, coef_lor, magne_1, bxe, ex_magne,           &
!     &          ele, jac_3d, sk_v)
!      subroutine fem_skv_induction_galerkin(iele_fsmp_stack,           &
!     &          n_int, k2, coef_uxb, velo_1, magne_1, vxe, bxe_ex,     &
!     &          ele, jac_3d, sk_v)
!
!      subroutine fem_skv_stratified_galerkin(iele_fsmp_stack,          &
!     &          n_int, k2, temp_1, vxe, xe, ele, jac_3d, sk_v)
!
!      subroutine fem_skv_lorentz_full_upwind(iele_fsmp_stack,          &
!     &          n_int, k2, dt, coef_lor, magne_1, vxe, bxe, ex_magne,  &
!     &          ele, jac_3d, sk_v)
!      subroutine fem_skv_induction_upmagne(iele_fsmp_stack, n_int, k2, &
!     &          dt, coef_uxb, velo_1, magne_1, vxe, bxe_ex, bxe_up,    &
!     &          ele, jac_3d, sk_v)
!
!      subroutine fem_skv_stratified_upwind(iele_fsmp_stack,            &
!     &          n_int, k2, dt, temp_1, vxe, xe, ele, jac_3d, sk_v)
!
      module fem_skv_lorentz_full_type
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use m_fem_gauss_int_coefs
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
      subroutine fem_skv_lorentz_rot_galerkin(iele_fsmp_stack,          &
     &           n_int, k2, vector_1, bxe_ex, ele, jac_3d, sk_v)
!
      use fem_skv_lorentz_full
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: vector_1(ele%numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_lorentz_rot                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, jac_3d%ntot_int, n_int, k2,                &
     &    jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,                          &
     &    vector_1, bxe_ex, sk_v)
!
      end subroutine fem_skv_lorentz_rot_galerkin
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_galerkin(iele_fsmp_stack,         &
     &          n_int, k2, coef_lor, magne_1, bxe, ex_magne,            &
     &          ele, jac_3d, sk_v)
!
      use fem_skv_lorentz_full
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_lor
      real (kind=kreal), intent(in) :: magne_1(ele%numele,3)
      real (kind=kreal), intent(in) :: bxe(ele%numele,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_lorentz_full_pg                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, jac_3d%ntot_int, n_int, k2,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, coef_lor,                 &
     &    magne_1, bxe, ex_magne, sk_v)
!
      end subroutine fem_skv_lorentz_full_galerkin
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_induction_galerkin(iele_fsmp_stack,            &
     &          n_int, k2, coef_uxb, velo_1, magne_1, vxe, bxe_ex,      &
     &          ele, jac_3d, sk_v)
!
      use fem_skv_induction
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_uxb
      real (kind=kreal), intent(in) :: velo_1(ele%numele,3)
      real (kind=kreal), intent(in) :: magne_1(ele%numele,3)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_induction_pg                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, jac_3d%ntot_int, n_int, k2,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    velo_1, magne_1, vxe, bxe_ex, coef_uxb, sk_v)
!
      end subroutine fem_skv_induction_galerkin
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_galerkin(iele_fsmp_stack,           &
     &          n_int, k2, temp_1, vxe, xe, ele, jac_3d, sk_v)
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
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_stratified_pg                                        &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, jac_3d%ntot_int, n_int, k2,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d%an, temp_1, vxe, xe, sk_v)
!
      end subroutine fem_skv_stratified_galerkin
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_upwind(iele_fsmp_stack,           &
     &          n_int, k2, dt, coef_lor, magne_1, vxe, bxe, ex_magne,   &
     &          ele, jac_3d, sk_v)
!
      use fem_skv_lorentz_full
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: coef_lor
      real(kind=kreal), intent(in) :: magne_1(ele%numele,3)
      real(kind=kreal), intent(in) :: bxe(ele%numele,3)
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: ex_magne(3)
!
      real(kind=kreal), intent(inout)                                   &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_lorentz_full_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, jac_3d%ntot_int, n_int, k2, dt,            &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx, coef_lor,     &
     &    magne_1, vxe, bxe, ex_magne, sk_v)
!
      end subroutine fem_skv_lorentz_full_upwind
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_induction_upmagne(iele_fsmp_stack, n_int, k2,  &
     &          dt, coef_uxb, velo_1, magne_1, vxe, bxe_ex, bxe_up,     &
     &          ele, jac_3d, sk_v)
!
      use fem_skv_induction
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: coef_uxb
      real(kind=kreal), intent(in) :: velo_1(ele%numele,3)
      real(kind=kreal), intent(in) :: magne_1(ele%numele,3)
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: bxe_ex(ele%numele,3)
      real(kind=kreal), intent(in) :: bxe_up(ele%numele,3)
!
      real(kind=kreal), intent(inout)                                   &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_induction_upm                                        &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, jac_3d%ntot_int, n_int, k2, dt,            &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    velo_1, magne_1, vxe, bxe_ex, bxe_up, coef_uxb, sk_v)
!
      end subroutine fem_skv_induction_upmagne
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_upwind(iele_fsmp_stack,             &
     &          n_int, k2, dt, temp_1, vxe, xe, ele, jac_3d, sk_v)
!
      use fem_skv_stratified
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: temp_1(ele%numele)
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: xe(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_stratified_upw                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, jac_3d%ntot_int, n_int, k2, dt,            &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%an, temp_1,        &
     &    vxe, xe, sk_v)
!
      end subroutine fem_skv_stratified_upwind
!
!-----------------------------------------------------------------------
!
      end module fem_skv_lorentz_full_type
