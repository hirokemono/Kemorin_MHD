!fem_skv_nodal_field_1st.f90
!      module fem_skv_nodal_field_1st
!
!     programmed by H.Matsui on May 2012
!
!      subroutine fem_skv_scalar_1st(iele_fsmp_stack, n_int, k2,        &
!     &          scalar_1, sk_v)
!      subroutine fem_skv_vector_1st(iele_fsmp_stack, n_int, k2,        &
!     &          vect_1, sk_v)
!      subroutine fem_skv_tensor_1st(iele_fsmp_stack, n_int, k2,        &
!     &          tensor_1, sk_v)
!
!      subroutine fem_skv_scalar_on_ele_1st(iele_fsmp_stack, n_int,     &
!     &          scalar_ele, sk_v)
!      subroutine fem_skv_vector_on_ele_1st(iele_fsmp_stack, n_int,     &
!     &          vector_ele, sk_v)
!      subroutine fem_skv_tensor_on_ele_1st(iele_fsmp_stack, n_int,     &
!     &          tensor_ele, sk_v)
!
!      subroutine fem_skv_scalar_on_ele_HRZ_1st(iele_fsmp_stack,        &
!     &          ml_ele_diag, scalar_ele, sk_v)
!      subroutine fem_skv_vector_on_ele_HRZ_1st(iele_fsmp_stack,        &
!     &          ml_ele_diag, vector_ele, sk_v)
!      subroutine fem_skv_tensor_on_ele_HRZ_1st(iele_fsmp_stack,        &
!     &          ml_ele_diag, tensor_ele, sk_v)
!
!      subroutine fem_skv_scalar_on_ele_grp_1st(iele_fsmp_stack,        &
!     &          nele_grp, iele_grp, n_int, scalar_ele, sk_v)
!      subroutine fem_skv_vector_on_ele_grp_1st(iele_fsmp_stack,        &
!     &          nele_grp, iele_grp, n_int, vector_ele, sk_v)
!      subroutine fem_skv_tensor_on_ele_grp_1st(iele_fsmp_stack,        &
!     &          nele_grp, iele_grp, n_int, tensor_ele, sk_v)
!
!      subroutine fem_skv_scalar_on_egrp_HRZ_1st(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, ml_ele_diag, scalar_ele, sk_v)
!      subroutine fem_skv_vector_on_egrp_HRZ_1st(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, ml_ele_diag, vector_ele, sk_v)
!      subroutine fem_skv_tensor_on_egrp_HRZ_1st(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, ml_ele_diag, tensor_ele, sk_v)
!
      module fem_skv_nodal_field_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
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
      subroutine fem_skv_scalar_1st(iele_fsmp_stack, n_int, k2,         &
     &          scalar_1, sk_v)
!
      use fem_skv_nodal_field
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal),   intent(in) :: scalar_1(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_field                                         &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    jac1_3d_q%ntot_int, iele_fsmp_stack, n_int, k2, xjac, aw, aw, &
     &    scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_1st(iele_fsmp_stack, n_int, k2,         &
     &          vect_1, sk_v)
!
      use fem_skv_nodal_field
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)    :: vect_1(ele1%numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_vector_field                                         &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    jac1_3d_q%ntot_int, iele_fsmp_stack, n_int, k2, xjac, aw, aw, &
     &    vect_1, sk_v)
!
      end subroutine fem_skv_vector_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_1st(iele_fsmp_stack, n_int, k2,         &
     &          tensor_1, sk_v)
!
      use fem_skv_nodal_field
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)                                      &
     &                   :: tensor_1(ele1%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_tensor_field                                         &
     &   (ele1%numele, ele1%nnod_4_ele, ele1%nnod_4_ele,                &
     &    jac1_3d_q%ntot_int, iele_fsmp_stack, n_int, k2, xjac, aw, aw, &
     &    tensor_1, sk_v)
!
      end subroutine fem_skv_tensor_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_1st(iele_fsmp_stack, n_int,      &
     &          scalar_ele, sk_v)
!
      use fem_skv_scalar_on_ele
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_ele(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_on_ele_m(ele1%numele, ele1%nnod_4_ele,        &
     &          iele_fsmp_stack, jac1_3d_q%ntot_int, n_int, xjac, aw,   &
     &          scalar_ele, sk_v)
!
      end subroutine fem_skv_scalar_on_ele_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_1st(iele_fsmp_stack, n_int,      &
     &          vector_ele, sk_v)
!
      use fem_skv_vector_on_ele
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_ele(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_m(ele1%numele, ele1%nnod_4_ele,        &
     &          iele_fsmp_stack, jac1_3d_q%ntot_int, n_int, xjac, aw,   &
     &          vector_ele, sk_v)
!
      end subroutine fem_skv_vector_on_ele_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_1st(iele_fsmp_stack, n_int,      &
     &          tensor_ele, sk_v)
!
      use fem_skv_tensor_on_ele
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_ele(ele1%numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_m(ele1%numele, ele1%nnod_4_ele,        &
     &          iele_fsmp_stack, jac1_3d_q%ntot_int, n_int, xjac, aw,   &
     &          tensor_ele, sk_v)
!
      end subroutine fem_skv_tensor_on_ele_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_HRZ_1st(iele_fsmp_stack,         &
     &          ml_ele_diag, scalar_ele, sk_v)
!
      use fem_skv_scalar_on_ele
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_ele(ele1%numele)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_on_ele_HRZ_m(ele1%numele, ele1%nnod_4_ele,    &
     &    iele_fsmp_stack, ele1%volume_ele, ml_ele_diag,                &
     &    scalar_ele, sk_v)
!
      end subroutine fem_skv_scalar_on_ele_HRZ_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_HRZ_1st(iele_fsmp_stack,         &
     &          ml_ele_diag, vector_ele, sk_v)
!
      use fem_skv_vector_on_ele
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_ele(ele1%numele,3)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_HRZ_m(ele1%numele, ele1%nnod_4_ele,    &
     &    iele_fsmp_stack, ele1%volume_ele, ml_ele_diag,                &
     &    vector_ele, sk_v)
!
      end subroutine fem_skv_vector_on_ele_HRZ_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_HRZ_1st(iele_fsmp_stack,         &
     &          ml_ele_diag, tensor_ele, sk_v)
!
      use m_geometry_data
      use fem_skv_tensor_on_ele
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_ele(ele1%numele,6)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_HRZ_m(ele1%numele, ele1%nnod_4_ele,    &
     &    iele_fsmp_stack, ele1%volume_ele, ml_ele_diag,                &
     &    tensor_ele, sk_v)
!
      end subroutine fem_skv_tensor_on_ele_HRZ_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_grp_1st(iele_fsmp_stack,         &
     &          nele_grp, iele_grp, n_int, scalar_ele, sk_v)
!
      use fem_skv_scalar_on_ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_ele(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_on_ele_grp_m(ele1%numele, ele1%nnod_4_ele,    &
     &    iele_fsmp_stack, nele_grp, iele_grp, jac1_3d_q%ntot_int,      &
     &    n_int, xjac, aw, scalar_ele, sk_v)
!
      end subroutine fem_skv_scalar_on_ele_grp_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_grp_1st(iele_fsmp_stack,         &
     &          nele_grp, iele_grp, n_int, vector_ele, sk_v)
!
      use fem_skv_vector_on_ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_ele(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_grp_m(ele1%numele, ele1%nnod_4_ele,    &
     &    iele_fsmp_stack, nele_grp, iele_grp, jac1_3d_q%ntot_int,      &
     &    n_int, xjac, aw, vector_ele, sk_v)
!
      end subroutine fem_skv_vector_on_ele_grp_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_grp_1st(iele_fsmp_stack,         &
     &          nele_grp, iele_grp, n_int, tensor_ele, sk_v)
!
      use fem_skv_tensor_on_ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_ele(ele1%numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_grp_m(ele1%numele, ele1%nnod_4_ele,    &
     &    iele_fsmp_stack, nele_grp, iele_grp, jac1_3d_q%ntot_int,      &
     &    n_int, xjac, aw, tensor_ele, sk_v)
!
      end subroutine fem_skv_tensor_on_ele_grp_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_egrp_HRZ_1st(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, ml_ele_diag, scalar_ele, sk_v)
!
      use fem_skv_scalar_on_ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_ele(ele1%numele)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_scalar_on_ele_grp_HRZ(ele1%numele, ele1%nnod_4_ele,  &
     &    iele_fsmp_stack, ele1%volume_ele, nele_grp, iele_grp,         &
     &    ml_ele_diag, scalar_ele, sk_v)
!
      end subroutine fem_skv_scalar_on_egrp_HRZ_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_egrp_HRZ_1st(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, ml_ele_diag, vector_ele, sk_v)
!
      use fem_skv_vector_on_ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_ele(ele1%numele,3)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_grp_HRZ(ele1%numele, ele1%nnod_4_ele,  &
     &    iele_fsmp_stack, ele1%volume_ele, nele_grp, iele_grp,         &
     &    ml_ele_diag, vector_ele, sk_v)
!
      end subroutine fem_skv_vector_on_egrp_HRZ_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_egrp_HRZ_1st(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, ml_ele_diag, tensor_ele, sk_v)
!
      use fem_skv_tensor_on_ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_ele(ele1%numele,6)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele1%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele1%numele,n_sym_tensor,ele1%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_grp_HRZ(ele1%numele, ele1%nnod_4_ele,  &
     &    iele_fsmp_stack, ele1%volume_ele, nele_grp, iele_grp,         &
     &    ml_ele_diag, tensor_ele, sk_v)
!
      end subroutine fem_skv_tensor_on_egrp_HRZ_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_nodal_field_1st
