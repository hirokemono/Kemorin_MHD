!fem_skv_nonlinear_1st.f90
!      module fem_skv_nonlinear_1st
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_scalar_inertia_1st(iele_fsmp_stack, n_int, k2,&
!     &          scalar_1, vxe, sk_v)
!      subroutine fem_skv_vector_inertia_1st(iele_fsmp_stack, n_int, k2,&
!     &          vect_1, vxe, sk_v)
!      subroutine fem_skv_rot_inertia_1st(iele_fsmp_stack, n_int, k2,   &
!     &          vect_1, wxe, sk_v)
!
!      subroutine fem_skv_coriolis_1st(iele_fsmp_stack, n_int, k2,      &
!     &          vect_1, angular, sk_v)
!
      module fem_skv_nonlinear_1st
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
      subroutine fem_skv_scalar_inertia_1st(iele_fsmp_stack, n_int, k2, &
     &          scalar_1, vxe, sk_v)
!
      use fem_skv_inertia
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: scalar_1(ele1%numele)
      real(kind=kreal), intent(in) :: vxe(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
      call fem_skv_scalar_inertia(ele1%numele, nnod_4_ele, nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,              &
     &    xjac, aw, dwx, scalar_1, vxe, sk_v)
!
      end subroutine fem_skv_scalar_inertia_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_inertia_1st(iele_fsmp_stack, n_int, k2, &
     &          vect_1, vxe, sk_v)
!
      use fem_skv_inertia
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real(kind=kreal), intent(in) :: vxe(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_vector_inertia(ele1%numele, nnod_4_ele, nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,              &
     &    xjac, aw, dwx, vect_1, vxe, sk_v)
!
      end subroutine fem_skv_vector_inertia_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_inertia_1st(iele_fsmp_stack, n_int, k2,    &
     &          vect_1, wxe, sk_v)
!
      use fem_skv_inertia
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real(kind=kreal), intent(in) :: wxe(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_rot_inertia(ele1%numele, nnod_4_ele, nnod_4_ele,     &
     &    np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,              &
     &    xjac, aw, aw, vect_1, wxe, sk_v)
!
      end subroutine fem_skv_rot_inertia_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_coriolis_1st(iele_fsmp_stack, n_int, k2,       &
     &          vect_1, angular, sk_v)
!
      use fem_skv_inertia
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real(kind=kreal), intent(in) :: angular(3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_coriolis(ele1%numele, nnod_4_ele, nnod_4_ele,        &
     &    np_smp, iele_fsmp_stack, n_int, k2, ntot_int_3d,              &
     &    xjac, aw, aw, vect_1, angular, sk_v)
!
      end subroutine fem_skv_coriolis_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_nonlinear_1st
