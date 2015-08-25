!fem_skv_vector_diff_1st.f90
!      module fem_skv_vector_diff_1st
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_gradient(iele_fsmp_stack, n_int, k2,          &
!     &          scalar_1, sk_v)
!      subroutine fem_skv_divergence(iele_fsmp_stack, n_int, k2,        &
!     &          vect_1, sk_v)
!      subroutine fem_skv_rotation(iele_fsmp_stack, n_int, k2,          &
!     &          vect_1, sk_v)
!      subroutine fem_skv_rot_rot_by_laplace(iele_fsmp_stack, n_int, k2,&
!     &          vect_1, sk_v)
!
!      subroutine fem_skv_div_tensor(iele_fsmp_stack, n_int, k2,        &
!     &          flux_1, sk_v)
!      subroutine fem_skv_div_asym_tsr(iele_fsmp_stack, n_int, k2,      &
!     &          flux_1, sk_v)
!
!      subroutine fem_skv_grp_gradient(iele_fsmp_stack,                 &
!     &          nele_grp, iele_grp, n_int, k2, scalar_1, sk_v)
!      subroutine fem_skv_grp_divergence(iele_fsmp_stack,               &
!     &          nele_grp, iele_grp, n_int, k2, vect_1, sk_v)
!      subroutine fem_skv_grp_rotation(iele_fsmp_stack,                 &
!     &          nele_grp, iele_grp, n_int, k2, vect_1, sk_v)
!      subroutine fem_skv_grp_rot_rot_by_laplace(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, n_int, k2, vect_1, sk_v)
!
!      subroutine fem_skv_grp_divergence_flux(iele_fsmp_stack,          &
!     &          nele_grp, iele_grp, n_int, k2, flux_1, sk_v)
!      subroutine fem_skv_grp_divergence_asym_t(iele_fsmp_stack,        &
!     &          nele_grp, iele_grp, n_int, k2, flux_1, sk_v)
!
!      subroutine fem_skv_linear_gradient(iele_fsmp_stack, n_int, k2,   &
!     &          scalar_1, sk_v)
!      subroutine fem_skv_div_to_linear(iele_fsmp_stack, n_int, k2,     &
!     &          vect_1, sk_v)
!
      module fem_skv_vector_diff_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_gradient(iele_fsmp_stack, n_int, k2,           &
     &          scalar_1, sk_v)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal),   intent(in) :: scalar_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      call fem_skv_all_grad                                             &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,       &
     &    jac1_3d_q%xjac, jac1_3d_q%an, jac1_3d_q%dnx,                  &
     &    scalar_1, sk_v)
!
      end subroutine fem_skv_gradient
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_divergence(iele_fsmp_stack, n_int, k2,         &
     &          vect_1, sk_v)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)    :: vect_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_all_div                                              &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele, np_smp,        &
     &    iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,               &
     &    jac1_3d_q%xjac, jac1_3d_q%an, jac1_3d_q%dnx,                  &
     &    vect_1, sk_v)
!
      end subroutine fem_skv_divergence
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rotation(iele_fsmp_stack, n_int, k2,           &
     &          vect_1, sk_v)
!
      use fem_skv_rot
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      call fem_all_skv_rot                                              &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele, np_smp,        &
     &    iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,               &
     &    jac1_3d_q%xjac, jac1_3d_q%an, jac1_3d_q%dnx,                  &
     &    vect_1, sk_v)
!
      end subroutine fem_skv_rotation
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_rot_by_laplace(iele_fsmp_stack, n_int, k2, &
     &          vect_1, sk_v)
!
      use fem_skv_rot2_laplace
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_all_skv_rot2_laplace                                     &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,       &
     &    jac1_3d_q%xjac, jac1_3d_q%dnx, jac1_3d_q%dnx, vect_1, sk_v)
!
      end subroutine fem_skv_rot_rot_by_laplace
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tensor(iele_fsmp_stack, n_int, k2,         &
     &          flux_1, sk_v)
!
      use fem_skv_div_flux
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: flux_1(ele1%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_all_div_flux                                         &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele, np_smp,        &
     &    iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,               &
     &    jac1_3d_q%xjac, jac1_3d_q%an, jac1_3d_q%dnx,                  &
     &    flux_1, sk_v)
!
      end subroutine fem_skv_div_tensor
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_asym_tsr(iele_fsmp_stack, n_int, k2,       &
     &          flux_1, sk_v)
!
      use fem_skv_div_asym_t
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)    :: flux_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_all_div_asym_t                                       &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,       &
     &    jac1_3d_q%xjac, jac1_3d_q%an, jac1_3d_q%dnx,                  &
     &    flux_1, sk_v)
!
      end subroutine fem_skv_div_asym_tsr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_gradient(iele_fsmp_stack,                  &
     &          nele_grp, iele_grp, n_int, k2, scalar_1, sk_v)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real(kind=kreal),   intent(in) :: scalar_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_grp_grad                                             &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac1_3d_q%ntot_int, jac1_3d_q%xjac,                           &
     &    jac1_3d_q%an, jac1_3d_q%dnx, scalar_1, sk_v)
!
      end subroutine fem_skv_grp_gradient
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence(iele_fsmp_stack,                &
     &          nele_grp, iele_grp, n_int, k2, vect_1, sk_v)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real(kind=kreal), intent(in)    :: vect_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_grp_div                                              &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele, np_smp,        &
     &    iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,               &
     &    jac1_3d_q%ntot_int, jac1_3d_q%xjac,                           &
     &    jac1_3d_q%an, jac1_3d_q%dnx, vect_1, sk_v)
!
      end subroutine fem_skv_grp_divergence
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_rotation(iele_fsmp_stack,                  &
     &          nele_grp, iele_grp, n_int, k2, vect_1, sk_v)
!
      use fem_skv_rot
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_grp_rot                                              &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele, np_smp,        &
     &    iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,               &
     &    jac1_3d_q%ntot_int, jac1_3d_q%xjac,                           &
     &    jac1_3d_q%an, jac1_3d_q%dnx, vect_1, sk_v)
!
      end subroutine fem_skv_grp_rotation
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_rot_rot_by_laplace(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, n_int, k2, vect_1, sk_v)
!
      use fem_skv_rot2_laplace
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_grp_rot2_laplace                                     &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac1_3d_q%ntot_int, jac1_3d_q%xjac,                           &
     &    jac1_3d_q%dnx, jac1_3d_q%dnx, vect_1, sk_v)
!
      end subroutine fem_skv_grp_rot_rot_by_laplace
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence_flux(iele_fsmp_stack,           &
     &          nele_grp, iele_grp, n_int, k2, flux_1, sk_v)
!
      use fem_skv_div_flux
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real(kind=kreal), intent(in) :: flux_1(ele1%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_grp_div_flux                                         &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac1_3d_q%ntot_int, jac1_3d_q%xjac,                           &
     &    jac1_3d_q%an, jac1_3d_q%dnx, flux_1, sk_v)
!
      end subroutine fem_skv_grp_divergence_flux
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence_asym_t(iele_fsmp_stack,         &
     &          nele_grp, iele_grp, n_int, k2, flux_1, sk_v)
!
      use fem_skv_div_asym_t
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      real(kind=kreal), intent(in) :: flux_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_grp_div_asym_t                                       &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp, n_int, k2,       &
     &    jac1_3d_q%ntot_int, jac1_3d_q%xjac,                           &
     &    jac1_3d_q%an, jac1_3d_q%dnx, flux_1, sk_v)
!
      end subroutine fem_skv_grp_divergence_asym_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_linear_gradient(iele_fsmp_stack, n_int, k2,    &
     &          scalar_1, sk_v)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal),   intent(in) :: scalar_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      call fem_skv_all_grad(ele1%numele, ele1%nnod_4_ele, num_t_linear, &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,       &
     &    jac1_3d_q%xjac, jac1_3d_q%an, jac1_3d_l%dnx,                  &
     &    scalar_1, sk_v)
!
      end subroutine fem_skv_linear_gradient
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_to_linear(iele_fsmp_stack, n_int, k2,      &
     &          vect_1, sk_v)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)    :: vect_1(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_all_div(ele1%numele, num_t_linear, ele1%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac1_3d_q%ntot_int,       &
     &    jac1_3d_q%xjac, jac1_3d_l%an, jac1_3d_q%dnx,                  &
     &    vect_1, sk_v(1,1,1) )
!
      end subroutine fem_skv_div_to_linear
!
!-----------------------------------------------------------------------
!
      end module fem_skv_vector_diff_1st
