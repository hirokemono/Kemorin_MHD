!fem_skv_vect_diff_upw_1st.f90
!      module fem_skv_vect_diff_upw_1st
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_gradient_upw(iele_fsmp_stack, n_int, k2,      &
!     &          vxe, scalar_1, sk_v)
!      subroutine fem_skv_divergence_upw(iele_fsmp_stack, n_int, k2,    &
!     &          vxe, vect_1, sk_v)
!      subroutine fem_skv_rotation_upw(iele_fsmp_stack, n_int, k2,      &
!     &          vxe, vect_1, sk_v)
!
!      subroutine fem_skv_div_tsr_upw(iele_fsmp_stack, n_int, k2,       &
!     &          vxe, flux_1, sk_v)
!      subroutine fem_skv_div_as_tsr_upw(iele_fsmp_stack, n_int, k2,    &
!     &          vxe, flux_1, sk_v)
!
!      subroutine fem_skv_grp_gradient_upw(iele_fsmp_stack,             &
!     &          nele_grp, iele_grp, n_int, k2, vxe, scalar_1, sk_v)
!      subroutine fem_skv_grp_divergence_upw(iele_fsmp_stack,           &
!     &          nele_grp, iele_grp, n_int, k2, vxe, vect_1, sk_v)
!      subroutine fem_skv_grp_rotation_upw(iele_fsmp_stack,             &
!     &          nele_grp, iele_grp, n_int, k2, vxe, vect_1, sk_v)
!
!      subroutine fem_skv_grp_div_tsr_upw(iele_fsmp_stack,              &
!     &          nele_grp, iele_grp, n_int, k2, vxe, flux_1, sk_v)
!      subroutine fem_skv_grp_div_as_tsr_upw(iele_fsmp_stack,           &
!     &          nele_grp, iele_grp, n_int, k2, vxe, flux_1, sk_v)
!
!      subroutine fem_skv_linear_gradient_upw(iele_fsmp_stack,          &
!     &          n_int, k2, vxe, scalar_1, sk_v)
!
      module fem_skv_vect_diff_upw_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_t_int_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_gradient_upw(iele_fsmp_stack, n_int, k2,       &
     &          vxe, scalar_1, sk_v)
!
      use fem_skv_grad_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      call fem_skv_all_grad_upw(numele, nnod_4_ele, nnod_4_ele,         &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, ntot_int_3d,          &
     &    xjac, aw, dwx, dwx, vxe, scalar_1, sk_v)
!
      end subroutine fem_skv_gradient_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_divergence_upw(iele_fsmp_stack, n_int, k2,     &
     &          vxe, vect_1, sk_v)
!
      use fem_skv_div_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: vect_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_all_div_upw(numele, nnod_4_ele, nnod_4_ele, np_smp,  &
     &    iele_fsmp_stack, n_int, k2, dt, ntot_int_3d, xjac,            &
     &     aw, dwx, dwx, vxe, vect_1, sk_v)
!
      end subroutine fem_skv_divergence_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rotation_upw(iele_fsmp_stack, n_int, k2,       &
     &          vxe, vect_1, sk_v)
!
      use fem_skv_rot_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: vect_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      call fem_all_skv_rot_upw(numele, nnod_4_ele, nnod_4_ele, np_smp,  &
     &    iele_fsmp_stack, n_int, k2, dt, ntot_int_3d, xjac,            &
     &    aw, dwx, dwx, vxe, vect_1, sk_v)
!
      end subroutine fem_skv_rotation_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tsr_upw(iele_fsmp_stack, n_int, k2,        &
     &          vxe, flux_1, sk_v)
!
      use fem_skv_div_flux_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in)    :: flux_1(numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_all_div_flux_upw(numele, nnod_4_ele, nnod_4_ele,     &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, ntot_int_3d, xjac,    &
     &    aw, dwx, dwx, vxe, flux_1, sk_v)
!
      end subroutine fem_skv_div_tsr_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_as_tsr_upw(iele_fsmp_stack, n_int, k2,     &
     &          vxe, flux_1, sk_v)
!
      use fem_skv_div_asym_t_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in)    :: flux_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_all_div_asym_t_upw                                   &
     &   (numele, nnod_4_ele, nnod_4_ele, np_smp, iele_fsmp_stack,      &
     &    n_int, k2, dt, ntot_int_3d, xjac, aw, dwx, dwx,               &
     &    vxe, flux_1, sk_v)
!
      end subroutine fem_skv_div_as_tsr_upw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_gradient_upw(iele_fsmp_stack,              &
     &          nele_grp, iele_grp, n_int, k2, vxe, scalar_1, sk_v)
!
      use fem_skv_grad_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal),   intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grp_grad_upw(numele, nnod_4_ele, nnod_4_ele,         &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    n_int, k2, dt, ntot_int_3d, xjac, aw, dwx, dwx,               &
     &    vxe, scalar_1, sk_v)
!
      end subroutine fem_skv_grp_gradient_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence_upw(iele_fsmp_stack,            &
     &          nele_grp, iele_grp, n_int, k2, vxe, vect_1, sk_v)
!
      use fem_skv_div_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in)    :: vect_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grp_div_upw(numele, nnod_4_ele, nnod_4_ele, np_smp,  &
     &    iele_fsmp_stack, nele_grp, iele_grp, n_int, k2, dt,           &
     &    ntot_int_3d, xjac, aw, dwx, dwx, vxe, vect_1, sk_v)
!
      end subroutine fem_skv_grp_divergence_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_rotation_upw(iele_fsmp_stack,              &
     &          nele_grp, iele_grp, n_int, k2, vxe, vect_1, sk_v)
!
      use fem_skv_rot_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: vect_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grp_rot_upw(numele, nnod_4_ele, nnod_4_ele, np_smp,  &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int, k2, dt,     &
     &          ntot_int_3d, xjac, aw, dwx, dwx,  vxe, vect_1, sk_v)
!
      end subroutine fem_skv_grp_rotation_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_tsr_upw(iele_fsmp_stack,               &
     &          nele_grp, iele_grp, n_int, k2, vxe, flux_1, sk_v)
!
      use fem_skv_div_flux_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: flux_1(numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grp_div_flux_upw(numele, nnod_4_ele, nnod_4_ele,     &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2, dt,   &
     &    ntot_int_3d, xjac, aw, dwx, dwx,  vxe, flux_1, sk_v)
!
      end subroutine fem_skv_grp_div_tsr_upw
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_div_as_tsr_upw(iele_fsmp_stack,            &
     &          nele_grp, iele_grp, n_int, k2, vxe, flux_1, sk_v)
!
      use fem_skv_div_asym_t_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: flux_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grp_div_asym_t_upw(numele, nnod_4_ele, nnod_4_ele,   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2, dt,   &
     &    ntot_int_3d, xjac, aw, vxe, dwx, dwx,  flux_1, sk_v)
!
      end subroutine fem_skv_grp_div_as_tsr_upw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_linear_gradient_upw(iele_fsmp_stack,           &
     &          n_int, k2, vxe, scalar_1, sk_v)
!
      use fem_skv_grad_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal),   intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      call fem_skv_all_grad_upw(numele, nnod_4_ele, num_t_linear,       &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, ntot_int_3d,          &
     &    xjac, aw, dwx, dnx, vxe, scalar_1, sk_v)
!
      end subroutine fem_skv_linear_gradient_upw
!
!-----------------------------------------------------------------------
!
      end module fem_skv_vect_diff_upw_1st
