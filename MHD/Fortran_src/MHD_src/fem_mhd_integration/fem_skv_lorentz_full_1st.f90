!fem_skv_lorentz_full_1st.f90
!      module fem_skv_lorentz_full_1st
!
!     programmed by H.Matsui on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_lorentz_rot_1st(iele_fsmp_stack, n_int, k2,   &
!     &          vect_1, bxe_ex, sk_v)
!
!      subroutine fem_skv_lorentz_full_pg_1st(iele_fsmp_stack,          &
!     &          n_int, k2, coef_lor, magne_1, bxe, ex_magne, sk_v)
!      subroutine fem_skv_induction_1st(iele_fsmp_stack, n_int, k2,     &
!     &           coef_uxb, velo_1, magne_1, vxe, bxe_ex, sk_v)
!
!      subroutine fem_skv_stratified_1st(iele_fsmp_stack,               &
!     &          n_int, k2, temp_1, vxe, xe, sk_v)
!
!      subroutine fem_skv_lorentz_full_upw_1st(iele_fsmp_stack,         &
!     &          n_int, k2, coef_lor, magne_1, vxe, bxe, ex_magne, sk_v)
!      subroutine fem_skv_induction_upm_1st(iele_fsmp_stack, n_int, k2, &
!     &           coef_uxb, velo_1, magne_1, vxe, bxe_ex, bxe_up, sk_v)
!
!      subroutine fem_skv_stratified_upw_1st(iele_fsmp_stack,           &
!     &          n_int, k2, temp_1, vxe, xe, sk_v)
!
      module fem_skv_lorentz_full_1st
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_parameter
      use m_t_int_parameter
      use m_phys_constants
      use m_fem_gauss_int_coefs
      use m_jacobians
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_rot_1st(iele_fsmp_stack, n_int, k2,    &
     &          vect_1, bxe_ex, sk_v)
!
      use fem_skv_lorentz_full
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: vect_1(numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_lorentz_rot(numele, nnod_4_ele, nnod_4_ele,          &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, n_int, k2,        &
     &          xjac, dwx, dwx,  vect_1, bxe_ex, sk_v)
!
      end subroutine fem_skv_lorentz_rot_1st
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_pg_1st(iele_fsmp_stack,           &
     &          n_int, k2, coef_lor, magne_1, bxe, ex_magne, sk_v)
!
      use fem_skv_lorentz_full
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_lor
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: bxe(numele,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_lorentz_full_pg(numele, nnod_4_ele, nnod_4_ele,      &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, n_int, k2, xjac,  &
     &          aw, dwx, coef_lor, magne_1, bxe, ex_magne, sk_v)
!
      end subroutine fem_skv_lorentz_full_pg_1st
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_induction_1st(iele_fsmp_stack, n_int, k2,      &
     &           coef_uxb, velo_1, magne_1, vxe, bxe_ex, sk_v)
!
      use fem_skv_induction
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_uxb
      real (kind=kreal), intent(in) :: velo_1(numele,3)
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_induction_pg(numele, nnod_4_ele, nnod_4_ele,         &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, n_int, k2,        &
     &          xjac, aw, dwx, velo_1, magne_1, vxe, bxe_ex,            &
     &          coef_uxb, sk_v)
!
      end subroutine fem_skv_induction_1st
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_1st(iele_fsmp_stack,                &
     &          n_int, k2, temp_1, vxe, xe, sk_v)
!
      use fem_skv_stratified
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: temp_1(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: xe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_stratified_pg(numele, nnod_4_ele, nnod_4_ele,        &
     &    np_smp, iele_fsmp_stack, ntot_int_3d, n_int, k2,              &
     &    xjac, aw, aw, temp_1, vxe, xe, sk_v)
!
      end subroutine fem_skv_stratified_1st
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_skv_lorentz_full_upw_1st(iele_fsmp_stack,          &
     &          n_int, k2, coef_lor, magne_1, vxe, bxe, ex_magne, sk_v)
!
      use fem_skv_lorentz_full
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_lor
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: bxe(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: ex_magne(3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_lorentz_full_upw(numele, nnod_4_ele, nnod_4_ele,     &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, n_int, k2, dt,    &
     &          xjac, aw, dwx, dwx, coef_lor, magne_1,                  &
     &          vxe, bxe, ex_magne, sk_v)
!
      end subroutine fem_skv_lorentz_full_upw_1st
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_induction_upm_1st(iele_fsmp_stack, n_int, k2,  &
     &           coef_uxb, velo_1, magne_1, vxe, bxe_ex, bxe_up, sk_v)
!
      use fem_skv_induction
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: coef_uxb
      real (kind=kreal), intent(in) :: velo_1(numele,3)
      real (kind=kreal), intent(in) :: magne_1(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: bxe_ex(numele,3)
      real (kind=kreal), intent(in) :: bxe_up(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_induction_upm(numele, nnod_4_ele, nnod_4_ele,        &
     &          np_smp, iele_fsmp_stack, ntot_int_3d, n_int, k2, dt,    &
     &          xjac, aw, dwx, dwx, velo_1, magne_1, vxe, bxe_ex,       &
     &          bxe_up, coef_uxb, sk_v)
!
      end subroutine fem_skv_induction_upm_1st
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_stratified_upw_1st(iele_fsmp_stack,            &
     &          n_int, k2, temp_1, vxe, xe, sk_v)
!
      use fem_skv_stratified
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: temp_1(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
      real (kind=kreal), intent(in) :: xe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_stratified_upw(numele, nnod_4_ele, nnod_4_ele,       &
     &    np_smp, iele_fsmp_stack, ntot_int_3d, n_int, k2, dt,          &
     &    xjac, aw, dwx, aw, temp_1, vxe, xe, sk_v)
!
      end subroutine fem_skv_stratified_upw_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_lorentz_full_1st
