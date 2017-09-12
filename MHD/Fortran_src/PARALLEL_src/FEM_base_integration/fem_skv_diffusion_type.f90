!fem_skv_diffusion_type.f90
!      module fem_skv_diffusion_type
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!!      subroutine fem_skv_scalar_diffuse_type(iele_fsmp_stack,         &
!!     &          n_int, k2, ak_d, ele, jac_3d , scalar_1, sk_v)
!!      subroutine fem_skv_vector_diffuse_type(iele_fsmp_stack,         &
!!     &          n_int, k2, ak_d, ele, jac_3d, vect_1, sk_v)
!!
!!      subroutine fem_skv_poisson_type(iele_fsmp_stack,                &
!!     &          n_int, k2, ele, jac_3d, sk_v)
!!      subroutine fem_skv_poisson_linear_type(iele_fsmp_stack,         &
!!     &          n_int, k2, ele, jac_3d, sk_v)
!!        integer(kind=kint), intent(in) :: n_int, k2
!!        integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        real (kind=kreal), intent(in) :: ak_d(ele%numele)
!!        real(kind=kreal), intent(in) :: scalar_1(ele%numele)
!!        real (kind=kreal), intent(inout)                              &
!!     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      module fem_skv_diffusion_type
!
      use m_precision
!
      use t_geometry_data
      use t_jacobians
      use m_machine_parameter
      use m_geometry_constants
!
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
      subroutine fem_skv_scalar_diffuse_type(iele_fsmp_stack,           &
     &          n_int, k2, ak_d, ele, jac_3d, scalar_1, sk_v)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      real(kind=kreal), intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_diffuse                                       &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                  &
     &     np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,       &
     &     int_start3, owe3d, n_int, k2, jac_3d%ntot_int,               &
     &     jac_3d%xjac, jac_3d%dnx, jac_3d%dnx, ak_d, scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_diffuse_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_diffuse_type(iele_fsmp_stack,           &
     &          n_int, k2, ak_d, ele, jac_3d, vect_1, sk_v)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      real(kind=kreal), intent(in) :: vect_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_diffuse                                       &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                  &
     &     np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,       &
     &     int_start3, owe3d, n_int, k2, jac_3d%ntot_int,               &
     &     jac_3d%xjac, jac_3d%dnx, jac_3d%dnx, ak_d, vect_1, sk_v)
!
      end subroutine fem_skv_vector_diffuse_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_type(iele_fsmp_stack,                  &
     &          n_int, k2, ele, jac_3d, sk_v)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_poisson                                              &
     &    (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                  &
     &     np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,       &
     &     int_start3, owe3d, n_int, k2, jac_3d%ntot_int,               &
     &     jac_3d%xjac, jac_3d%dnx, jac_3d%dnx, sk_v)
!
      end subroutine fem_skv_poisson_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_linear_type(iele_fsmp_stack,           &
     &          n_int, k2, ele, jac_3d_l, sk_v)
!
      use fem_skv_diffusion
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_poisson                                              &
     &    (ele%numele, num_t_linear, num_t_linear,                      &
     &     np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,       &
     &     int_start3, owe3d, n_int, k2, jac_3d_l%ntot_int,             &
     &     jac_3d_l%xjac, jac_3d_l%dnx, jac_3d_l%dnx, sk_v)
!
      end subroutine fem_skv_poisson_linear_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffusion_type
