!fem_skv_vector_diff_type.f90
!      module fem_skv_vector_diff_type
!
!     programmed by H.Matsui on May 2012
!
!      subroutine fem_skv_gradient(iele_fsmp_stack, n_int, k2,          &
!     &          ele, jac_3d, scalar_1, sk_v)
!      subroutine fem_skv_divergence(iele_fsmp_stack, n_int, k2,        &
!     &          ele, jac_3d, vector_1, sk_v)
!      subroutine fem_skv_rotation(iele_fsmp_stack, n_int, k2,          &
!     &          ele, jac_3d, vector_1, sk_v)
!
!      subroutine fem_skv_rot_rot_by_laplace(iele_fsmp_stack,           &
!     &          n_int, k2, ele, jac_3d, vect_1, sk_v)
!      subroutine fem_skv_div_tensor(iele_fsmp_stack, n_int, k2,        &
!     &          ele, jac_3d, flux_1, sk_v)
!      subroutine fem_skv_div_asym_tsr(iele_fsmp_stack, n_int, k2,      &
!     &          ele, jac_3d, flux_1, sk_v)
!
!      subroutine fem_skv_grp_gradient(iele_fsmp_stack,                 &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,            &
!     &          scalar_1, sk_v)
!      subroutine fem_skv_grp_divergence(iele_fsmp_stack,               &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,            &
!     &          vector_1, sk_v)
!      subroutine fem_skv_grp_rotation(iele_fsmp_stack,                 &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,            &
!     &          vector_1, sk_v)
!
!      subroutine fem_skv_grp_rot_rot_by_laplace(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,            &
!     &          vect_1, sk_v)
!      subroutine fem_skv_grp_divergence_flux(iele_fsmp_stack,          &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,            &
!     &          flux_1, sk_v)
!      subroutine fem_skv_grp_divergence_asym_t(iele_fsmp_stack,        &
!     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,            &
!     &          flux_1, sk_v)
!
!      subroutine fem_skv_linear_gradient(iele_fsmp_stack, n_int, k2,   &
!     &          ele, jac_3d, jac_3d_l, scalar_1, sk_v)
!      subroutine fem_skv_div_to_linear(iele_fsmp_stack, n_int, k2,     &
!     &          ele, jac_3d, jac_3d_l, vector_1, sk6)
!        integer(kind=kint), intent(in) :: n_int, k2
!        integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(jacobians_3d), intent(in) :: jac_3d_l
!
!
      module fem_skv_vector_diff_type
!
      use m_precision
!
      use t_geometry_data
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
      subroutine fem_skv_gradient(iele_fsmp_stack, n_int, k2,           &
     &          ele, jac_3d, scalar_1, sk_v)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal),   intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      call fem_skv_all_grad(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, jac_3d%ntot_int,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, scalar_1, sk_v)
!
      end subroutine fem_skv_gradient
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_divergence(iele_fsmp_stack, n_int, k2,         &
     &          ele, jac_3d, vector_1, sk_v)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in)    :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_all_div(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, jac_3d%ntot_int,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, vector_1, sk_v)
!
      end subroutine fem_skv_divergence
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rotation(iele_fsmp_stack, n_int, k2,           &
     &          ele, jac_3d, vector_1, sk_v)
!
      use fem_skv_rot
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      call fem_all_skv_rot(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2,                                 &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    vector_1, sk_v)
!
      end subroutine fem_skv_rotation
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_rot_by_laplace(iele_fsmp_stack,            &
     &          n_int, k2, ele, jac_3d, vect_1, sk_v)
!
      use fem_skv_rot2_laplace
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in) :: vect_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_all_skv_rot2_laplace                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, jac_3d%ntot_int,                &
     &    jac_3d%xjac, jac_3d%dnx, jac_3d%dnx, vect_1, sk_v)
!
      end subroutine fem_skv_rot_rot_by_laplace
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tensor(iele_fsmp_stack, n_int, k2,         &
     &          ele, jac_3d, flux_1, sk_v)
!
      use fem_skv_div_flux
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in) :: flux_1(ele%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_all_div_flux                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, jac_3d%ntot_int,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, flux_1, sk_v)
!
      end subroutine fem_skv_div_tensor
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_asym_tsr(iele_fsmp_stack, n_int, k2,       &
     &          ele, jac_3d, flux_1, sk_v)
!
      use fem_skv_div_asym_t
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in)    :: flux_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_all_div_asym_t                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, jac_3d%ntot_int,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, flux_1, sk_v)
!
      end subroutine fem_skv_div_asym_tsr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_gradient(iele_fsmp_stack,                  &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,             &
     &          scalar_1, sk_v)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal),   intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_grp_grad(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    scalar_1, sk_v)
!
      end subroutine fem_skv_grp_gradient
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence(iele_fsmp_stack,                &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,             &
     &          vector_1, sk_v)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in)    :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_grp_div(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    vector_1, sk_v)
!
      end subroutine fem_skv_grp_divergence
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_rotation(iele_fsmp_stack,                  &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,             &
     &          vector_1, sk_v)
!
      use fem_skv_rot
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_grp_rot(ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    vector_1, sk_v)
!
      end subroutine fem_skv_grp_rotation
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_rot_rot_by_laplace(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,             &
     &          vect_1, sk_v)
!
      use fem_skv_rot2_laplace
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in) :: vect_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_grp_rot2_laplace                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d,              &
     &    n_int, k2, jac_3d%ntot_int, jac_3d%xjac,                      &
     &    jac_3d%dnx, jac_3d%dnx, vect_1, sk_v)
!
      end subroutine fem_skv_grp_rot_rot_by_laplace
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence_flux(iele_fsmp_stack,           &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,             &
     &          flux_1, sk_v)
!
      use fem_skv_div_flux
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in) :: flux_1(ele%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_grp_div_flux                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    flux_1, sk_v)
!
      end subroutine fem_skv_grp_divergence_flux
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grp_divergence_asym_t(iele_fsmp_stack,         &
     &          nele_grp, iele_grp, n_int, k2, ele, jac_3d,             &
     &          flux_1, sk_v)
!
      use fem_skv_div_asym_t
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: nele_grp
      integer(kind=kint), intent(in) :: iele_grp(nele_grp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      real(kind=kreal), intent(in) :: flux_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_grp_div_asym_t                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, nele_grp, iele_grp,                  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    flux_1, sk_v)
!
      end subroutine fem_skv_grp_divergence_asym_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_linear_gradient(iele_fsmp_stack, n_int, k2,    &
     &          ele, jac_3d, jac_3d_l, scalar_1, sk_v)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      real(kind=kreal),   intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_all_grad(ele%numele, ele%nnod_4_ele, num_t_linear,   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, jac_3d%ntot_int,                &
     &    jac_3d%xjac, jac_3d%an, jac_3d_l%dnx, scalar_1, sk_v)
!
      end subroutine fem_skv_linear_gradient
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_to_linear(iele_fsmp_stack, n_int, k2,      &
     &          ele, jac_3d, jac_3d_l, vector_1, sk_v)
!
      use fem_skv_div
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)    :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_all_div(ele%numele, num_t_linear, ele%nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, jac_3d%ntot_int,                &
     &    jac_3d%xjac, jac_3d_l%an, jac_3d%dnx, vector_1, sk_v)
!
      end subroutine fem_skv_div_to_linear
!
!-----------------------------------------------------------------------
!
      end module fem_skv_vector_diff_type
