!fem_skv_nodal_field_type.f90
!      module fem_skv_nodal_field_type
!
!     programmed by H.Matsui on May 2012
!
!!      subroutine fem_skv_scalar_type(iele_fsmp_stack, n_int, k2,      &
!!     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!!      subroutine fem_skv_vector_type(iele_fsmp_stack, n_int, k2,      &
!!     &          ele, g_FEM, jac_3d, vector_1, sk_v)
!!      subroutine fem_skv_tensor_type(iele_fsmp_stack, n_int, k2,      &
!!     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!!
!!      subroutine fem_skv_scalar_on_ele_type(iele_fsmp_stack, n_int,   &
!!     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!!      subroutine fem_skv_vector_on_ele_type(iele_fsmp_stack, n_int,   &
!!     &          ele, g_FEM, jac_3d, vector_1, sk_v)
!!      subroutine fem_skv_tensor_on_ele_type(iele_fsmp_stack, n_int,   &
!!     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!!
!!      subroutine fem_skv_scalar_on_ele_HRZ_type(iele_fsmp_stack,      &
!!     &          ml_ele_diag, ele, scalar_1, sk_v)
!!      subroutine fem_skv_vector_on_ele_HRZ_type(iele_fsmp_stack,      &
!!     &          ml_ele_diag, ele, vector_1, sk_v)
!!      subroutine fem_skv_tensor_on_ele_HRZ_type(iele_fsmp_stack,      &
!!     &          ml_ele_diag, ele, tensor_1, sk_v)
!!
!!      subroutine fem_skv_scalar_on_ele_grp_type                       &
!!     &         (iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!!      subroutine fem_skv_vector_on_ele_grp_type                       &
!!     &         (iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          ele, g_FEM, jac_3d, vector_1, sk_v)
!!      subroutine fem_skv_tensor_on_ele_grp_type                       &
!!     &         (iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!!
!!      subroutine fem_skv_scalar_on_egrp_HRZ_type(iele_fsmp_stack,     &
!!     &          nele_grp, iele_grp, ml_ele_diag, ele, scalar_1, sk_v)
!!      subroutine fem_skv_vector_on_egrp_HRZ_type(iele_fsmp_stack,     &
!!     &          nele_grp, iele_grp, ml_ele_diag, ele, vector_1, sk_v)
!!      subroutine fem_skv_tensor_on_egrp_HRZ_type(iele_fsmp_stack,     &
!!     &          nele_grp, iele_grp, ml_ele_diag, ele, tensor_1, sk_v)
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        integer(kind=kint), intent(in) :: n_int, k2
!!        integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!!        real(kind=kreal),   intent(in) :: scalar_1(ele%numele)
!!
!!        real (kind=kreal), intent(inout)                              &
!!     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
      module fem_skv_nodal_field_type
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_type(iele_fsmp_stack, n_int, k2,        &
     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!
      use fem_skv_nodal_field
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal),   intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_field                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, k2, jac_3d%xjac,         &
     &    jac_3d%an, jac_3d%an, scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_type(iele_fsmp_stack, n_int, k2,        &
     &          ele, g_FEM, jac_3d, vector_1, sk_v)
!
      use fem_skv_nodal_field
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)   :: vector_1(ele%numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_field                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, k2, jac_3d%xjac,         &
     &    jac_3d%an, jac_3d%an, vector_1, sk_v)
!
      end subroutine fem_skv_vector_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_type(iele_fsmp_stack, n_int, k2,        &
     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!
      use fem_skv_nodal_field
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in)                                      &
     &                   :: tensor_1(ele%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_tensor_field                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, k2, jac_3d%xjac,         &
     &    jac_3d%an, jac_3d%an, tensor_1, sk_v)
!
      end subroutine fem_skv_tensor_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_type(iele_fsmp_stack, n_int,     &
     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_on_ele_m                                      &
     &   (ele%numele, ele%nnod_4_ele, iele_fsmp_stack,                  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_on_ele_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_type(iele_fsmp_stack, n_int,     &
     &          ele, g_FEM, jac_3d, vector_1, sk_v)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_m                                      &
     &   (ele%numele, ele%nnod_4_ele, iele_fsmp_stack,                  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    vector_1, sk_v)
!
      end subroutine fem_skv_vector_on_ele_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_type(iele_fsmp_stack, n_int,     &
     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_1(ele%numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_m                                      &
     &   (ele%numele, ele%nnod_4_ele, iele_fsmp_stack,                  &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    tensor_1, sk_v)
!
      end subroutine fem_skv_tensor_on_ele_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_HRZ_type(iele_fsmp_stack,        &
     &          ml_ele_diag, ele, scalar_1, sk_v)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_1(ele%numele)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, ele%volume_ele, ml_ele_diag, scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_on_ele_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_HRZ_type(iele_fsmp_stack,        &
     &          ml_ele_diag, ele, vector_1, sk_v)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_1(ele%numele,3)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &               :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, ele%volume_ele, ml_ele_diag, vector_1, sk_v)
!
      end subroutine fem_skv_vector_on_ele_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_HRZ_type(iele_fsmp_stack,        &
     &          ml_ele_diag, ele, tensor_1, sk_v)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_1(ele%numele,6)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, ele%volume_ele, ml_ele_diag, tensor_1, sk_v)
!
      end subroutine fem_skv_tensor_on_ele_HRZ_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_grp_type                         &
     &         (iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          ele, g_FEM, jac_3d, scalar_1, sk_v)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_on_ele_grp_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, nele_grp, iele_grp,                          &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_on_ele_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_grp_type                         &
     &         (iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          ele, g_FEM, jac_3d, vector_1, sk_v)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_grp_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, nele_grp, iele_grp,                          &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    vector_1, sk_v)
!
      end subroutine fem_skv_vector_on_ele_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_grp_type                         &
     &         (iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          ele, g_FEM, jac_3d, tensor_1, sk_v)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_1(ele%numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_grp_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, nele_grp, iele_grp,                          &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    tensor_1, sk_v)
!
      end subroutine fem_skv_tensor_on_ele_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_egrp_HRZ_type(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, ml_ele_diag, ele, scalar_1, sk_v)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: scalar_1(ele%numele)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
!
      call fem_skv_scalar_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,          &
     &     ml_ele_diag, scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_on_egrp_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_egrp_HRZ_type(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, ml_ele_diag, ele, vector_1, sk_v)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: vector_1(ele%numele,3)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,          &
     &    ml_ele_diag, vector_1, sk_v)
!
      end subroutine fem_skv_vector_on_egrp_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_egrp_HRZ_type(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, ml_ele_diag, ele, tensor_1, sk_v)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: tensor_1(ele%numele,6)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &              :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_tensor_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,          &
     &    ml_ele_diag, tensor_1, sk_v)
!
      end subroutine fem_skv_tensor_on_egrp_HRZ_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_nodal_field_type
