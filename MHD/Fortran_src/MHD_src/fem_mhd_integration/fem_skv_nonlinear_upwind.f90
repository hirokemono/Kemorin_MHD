!fem_skv_nonlinear_upwind.f90
!      module fem_skv_nonlinear_upwind
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!!      subroutine fem_skv_scalar_inertia_upwind(iele_fsmp_stack, n_int,&
!!     &          k2, dt, scalar_1, vxe, vxe_up, ele, jac_3d, sk_v)
!!      subroutine fem_skv_vector_inertia_upwind(iele_fsmp_stack, n_int,&
!!     &          k2, dt, vector_1, vxe, vxe_up, ele, jac_3d, sk_v)
!!      subroutine fem_skv_rot_inertia_upwind(iele_fsmp_stack, n_int,   &
!!     &          k2, dt, vector_1, wxe, vxe_up, ele, jac_3d, sk_v)
!!
!!      subroutine fem_skv_coriolis_upwind(iele_fsmp_stack, n_int,      &
!!     &          k2, dt, vector_1, angular, vxe_up, ele, jac_3d, sk_v)
!
      module fem_skv_nonlinear_upwind
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
      subroutine fem_skv_scalar_inertia_upwind(iele_fsmp_stack, n_int,  &
     &          k2, dt, scalar_1, vxe, vxe_up, ele, jac_3d, sk_v)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: scalar_1(ele%numele)
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_inertia_upw                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, dt, jac_3d%ntot_int,            &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    scalar_1, vxe, vxe_up, sk_v)
!
      end subroutine fem_skv_scalar_inertia_upwind
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_inertia_upwind(iele_fsmp_stack, n_int,  &
     &          k2, dt, vector_1, vxe, vxe_up, ele, jac_3d, sk_v)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_inertia_upw                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, dt, jac_3d%ntot_int,            &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    vector_1, vxe, vxe_up, sk_v)
!
      end subroutine fem_skv_vector_inertia_upwind
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_inertia_upwind(iele_fsmp_stack, n_int,     &
     &          k2, dt, vector_1, wxe, vxe_up, ele, jac_3d, sk_v)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
      real(kind=kreal), intent(in) :: wxe(ele%numele,3)
      real(kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_inertia_rot_upw                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, dt, jac_3d%ntot_int,            &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%an,                &
     &    vector_1, wxe, vxe_up, sk_v)
!
      end subroutine fem_skv_rot_inertia_upwind
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_coriolis_upwind(iele_fsmp_stack, n_int,        &
     &          k2, dt, vector_1, angular, vxe_up, ele, jac_3d, sk_v)
!
      use fem_skv_inertia_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
      real(kind=kreal), intent(in) :: angular(3)
      real(kind=kreal), intent(in) :: vxe_up(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_coriolis_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, max_int_point, maxtot_int_3d,        &
     &    int_start3, owe3d, n_int, k2, dt, jac_3d%ntot_int,            &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%an,                &
     &    vector_1, angular, vxe_up, sk_v)
!
      end subroutine fem_skv_coriolis_upwind
!
!-----------------------------------------------------------------------
!
      end module fem_skv_nonlinear_upwind
