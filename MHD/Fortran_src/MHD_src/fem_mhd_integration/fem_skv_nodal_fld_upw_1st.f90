!fem_skv_nodal_fld_upw_1st.f90
!      module fem_skv_nodal_fld_upw_1st
!
!     programmed by H.Matsui on May 2012
!
!      subroutine fem_skv_scalar_field_upw_1st(iele_fsmp_stack,         &
!     &          n_int, k2, vxe, scalar_e, sk_v) 
!      subroutine fem_skv_vector_field_upw_1st(iele_fsmp_stack,         &
!     &          n_int, k2, vxe, vector_e, sk_v)
!      subroutine fem_skv_tensor_field_upw_1st(iele_fsmp_stack,         &
!     &          n_int, k2, vxe, tensor_e, sk_v)
!
      module fem_skv_nodal_fld_upw_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_t_int_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_field_upw_1st(iele_fsmp_stack,          &
     &          n_int, k2, vxe, scalar_e, sk_v) 
!
      use fem_skv_nodal_field_upw
!
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real (kind=kreal), intent(in) :: scalar_e(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &           :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_field_upw                                     &
     &         (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,          &
     &          jac1_3d_q%ntot_int, iele_fsmp_stack, n_int, k2, xjac,   &
     &          aw, aw, dwx, dt, vxe, scalar_e, sk_v) 
!
      end subroutine fem_skv_scalar_field_upw_1st
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_vector_field_upw_1st(iele_fsmp_stack,          &
     &          n_int, k2, vxe, vector_e, sk_v) 
!
      use fem_skv_nodal_field_upw
!
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real (kind=kreal), intent(in) :: vector_e(ele1%numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &           :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_vector_field_upw                                     &
     &         (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,          &
     &          jac1_3d_q%ntot_int, iele_fsmp_stack, n_int, k2, xjac,   &
     &          aw, aw, dwx, dt, vxe, vector_e, sk_v) 
!
      end subroutine fem_skv_vector_field_upw_1st
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_field_upw_1st(iele_fsmp_stack,          &
     &          n_int, k2, vxe, tensor_e, sk_v) 
!
      use fem_skv_nodal_field_upw
!
      integer (kind=kint), intent(in) :: n_int, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real (kind=kreal), intent(in)                                     &
     &                   :: tensor_e(ele1%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &           :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_tensor_field_upw                                     &
     &         (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,          &
     &          jac1_3d_q%ntot_int, iele_fsmp_stack, n_int, k2, xjac,   &
     &          aw, aw, dwx, dt, vxe, tensor_e, sk_v) 
!
      end subroutine fem_skv_tensor_field_upw_1st
!
! ----------------------------------------------------------------------
!
      end module fem_skv_nodal_fld_upw_1st
