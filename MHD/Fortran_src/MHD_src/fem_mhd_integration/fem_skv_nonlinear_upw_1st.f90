!fem_skv_nonlinear_upw_1st.f90
!      module fem_skv_nonlinear_upw_1st
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_scalar_inertia_upw_1st(iele_fsmp_stack,       &
!     &           n_int, k2, scalar_1, vxe, vxe_up, sk_v)
!      subroutine fem_skv_vector_inertia_upw_1st(iele_fsmp_stack,       &
!     &          n_int, k2, vect_1, vxe, vxe_up, sk_v)
!      subroutine fem_skv_rot_inertia_upw_1st(iele_fsmp_stack,          &
!     &          n_int, k2, vect_1, vxe, vxe_up, sk_v)
!
!      subroutine fem_skv_coriolis_upw_1st(iele_fsmp_stack,             &
!     &          n_int, k2, vect_1, angular, vxe_up, sk_v)
!
      module fem_skv_nonlinear_upw_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_t_int_parameter
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
      subroutine fem_skv_scalar_inertia_upw_1st(iele_fsmp_stack,        &
     &           n_int, k2, scalar_1, vxe, vxe_up, sk_v)
!
      use fem_skv_inertia_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: scalar_1(ele1%numele)
      real(kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real (kind=kreal), intent(in) :: vxe_up(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
      call fem_skv_scalar_inertia_upw                                   &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac1_3d_q%ntot_int,   &
     &    jac1_3d_q%xjac, aw, dwx, dwx, scalar_1, vxe, vxe_up, sk_v)
!
      end subroutine fem_skv_scalar_inertia_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_inertia_upw_1st(iele_fsmp_stack,        &
     &          n_int, k2, vect_1, vxe, vxe_up, sk_v)
!
      use fem_skv_inertia_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real(kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real (kind=kreal), intent(in) :: vxe_up(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_vector_inertia_upw                                   &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac1_3d_q%ntot_int,   &
     &    jac1_3d_q%xjac, aw, dwx, dwx, vect_1, vxe, vxe_up, sk_v)
!
      end subroutine fem_skv_vector_inertia_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_inertia_upw_1st(iele_fsmp_stack,           &
     &          n_int, k2, vect_1, wxe, vxe_up, sk_v)
!
      use fem_skv_inertia_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real(kind=kreal), intent(in) :: wxe(ele1%numele,3)
      real (kind=kreal), intent(in) :: vxe_up(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_inertia_rot_upw                                      &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac1_3d_q%ntot_int,   &
     &    jac1_3d_q%xjac, aw, dwx, aw, vect_1, wxe, vxe_up, sk_v)
!
      end subroutine fem_skv_rot_inertia_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_coriolis_upw_1st(iele_fsmp_stack,              &
     &          n_int, k2, vect_1, angular, vxe_up, sk_v)
!
      use fem_skv_inertia_upw
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real(kind=kreal), intent(in) :: angular(3)
      real (kind=kreal), intent(in) :: vxe_up(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_coriolis_upw                                         &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt, jac1_3d_q%ntot_int,   &
     &    jac1_3d_q%xjac, aw, dwx, aw, vect_1, angular, vxe_up, sk_v)
!
      end subroutine fem_skv_coriolis_upw_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_nonlinear_upw_1st
