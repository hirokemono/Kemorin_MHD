!fem_skv_nodal_field_type.f90
!      module fem_skv_nodal_field_type
!
!     programmed by H.Matsui on May 2012
!
!      subroutine fem_skv_scalar_type(iele_fsmp_stack, n_int, k2,       &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_vector_type(iele_fsmp_stack, n_int, k2,       &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_tensor_type(iele_fsmp_stack, n_int, k2,       &
!     &          ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_scalar_on_ele_type(iele_fsmp_stack, n_int,    &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_vector_on_ele_type(iele_fsmp_stack, n_int,    &
!     &          ele, jac_3d, fem_wk)
!      subroutine fem_skv_tensor_on_ele_type(iele_fsmp_stack, n_int,    &
!     &          ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_scalar_on_ele_HRZ_type(iele_fsmp_stack,       &
!     &          ml_ele_diag, ele, fem_wk)
!      subroutine fem_skv_vector_on_ele_HRZ_type(iele_fsmp_stack,       &
!     &          ml_ele_diag, ele, fem_wk)
!      subroutine fem_skv_tensor_on_ele_HRZ_type(iele_fsmp_stack,       &
!     &          ml_ele_diag, ele, fem_wk)
!
!      subroutine fem_skv_scalar_on_ele_grp_type(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, n_int, ele, jac_3d, fem_wk)
!      subroutine fem_skv_vector_on_ele_grp_type(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, n_int, ele, jac_3d, fem_wk)
!      subroutine fem_skv_tensor_on_ele_grp_type(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, n_int, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_scalar_on_egrp_HRZ_type(iele_fsmp_stack,      &
!     &          nele_grp, iele_grp, ml_ele_diag, ele, fem_wk)
!      subroutine fem_skv_vector_on_egrp_HRZ_type(iele_fsmp_stack,      &
!     &          nele_grp, iele_grp, ml_ele_diag, ele, fem_wk)
!      subroutine fem_skv_tensor_on_egrp_HRZ_type(iele_fsmp_stack,      &
!     &          nele_grp, iele_grp, ml_ele_diag, ele, fem_wk)
!
      module fem_skv_nodal_field_type
!
      use m_precision
!
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
      use m_machine_parameter
      use m_phys_constants
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
      subroutine fem_skv_scalar_type(iele_fsmp_stack, n_int, k2,        &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_nodal_field
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_field                                         &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &     jac_3d%ntot_int, iele_fsmp_stack, n_int, k2,                 &
     &     jac_3d%xjac, jac_3d%an, jac_3d%an,                           &
     &     fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_scalar_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_type(iele_fsmp_stack, n_int, k2,        &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_nodal_field
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_field                                         &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &     jac_3d%ntot_int, iele_fsmp_stack, n_int, k2,                 &
     &     jac_3d%xjac, jac_3d%an, jac_3d%an,                           &
     &     fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_vector_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_type(iele_fsmp_stack, n_int, k2,        &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_nodal_field
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_tensor_field                                         &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &     jac_3d%ntot_int, iele_fsmp_stack, n_int, k2,                 &
     &     jac_3d%xjac, jac_3d%an, jac_3d%an,                           &
     &     fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_tensor_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_type(iele_fsmp_stack, n_int,     &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_on_ele_m(ele%numele, ele%nnod_4_ele,          &
     &    iele_fsmp_stack, jac_3d%ntot_int, n_int,                      &
     &    jac_3d%xjac, jac_3d%an, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_scalar_on_ele_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_type(iele_fsmp_stack, n_int,     &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_on_ele_m(ele%numele, ele%nnod_4_ele,          &
     &    iele_fsmp_stack, jac_3d%ntot_int, n_int,                      &
     &    jac_3d%xjac, jac_3d%an, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_vector_on_ele_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_type(iele_fsmp_stack, n_int,     &
     &          ele, jac_3d, fem_wk)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_tensor_on_ele_m(ele%numele, ele%nnod_4_ele,          &
     &    iele_fsmp_stack, jac_3d%ntot_int, n_int,                      &
     &    jac_3d%xjac, jac_3d%an, fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_tensor_on_ele_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_HRZ_type(iele_fsmp_stack,        &
     &          ml_ele_diag, ele, fem_wk)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, ele%volume_ele, ml_ele_diag,                 &
     &    fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_scalar_on_ele_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_HRZ_type(iele_fsmp_stack,        &
     &          ml_ele_diag, ele, fem_wk)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, ele%volume_ele, ml_ele_diag,                 &
     &    fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_vector_on_ele_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_HRZ_type(iele_fsmp_stack,        &
     &          ml_ele_diag, ele, fem_wk)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_tensor_on_ele_HRZ_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, ele%volume_ele, ml_ele_diag,                 &
     &    fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_tensor_on_ele_HRZ_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_ele_grp_type(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, n_int, ele, jac_3d, fem_wk)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_on_ele_grp_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, nele_grp, iele_grp, jac_3d%ntot_int, n_int,  &
     &    jac_3d%xjac, jac_3d%an, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_scalar_on_ele_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_ele_grp_type(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, n_int, ele, jac_3d, fem_wk)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_on_ele_grp_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, nele_grp, iele_grp, jac_3d%ntot_int, n_int,  &
     &    jac_3d%xjac, jac_3d%an, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_vector_on_ele_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_ele_grp_type(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, n_int, ele, jac_3d, fem_wk)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_tensor_on_ele_grp_m(ele%numele, ele%nnod_4_ele,      &
     &    iele_fsmp_stack, nele_grp, iele_grp, jac_3d%ntot_int, n_int,  &
     &    jac_3d%xjac, jac_3d%an, fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_tensor_on_ele_grp_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scalar_on_egrp_HRZ_type(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, ml_ele_diag, ele, fem_wk)
!
      use fem_skv_scalar_on_ele
!
      type(element_data), intent(in) :: ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,          &
     &     ml_ele_diag,  fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_scalar_on_egrp_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_on_egrp_HRZ_type(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, ml_ele_diag, ele, fem_wk)
!
      use fem_skv_vector_on_ele
!
      type(element_data), intent(in) :: ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,          &
     &     ml_ele_diag,  fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_vector_on_egrp_HRZ_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_tensor_on_egrp_HRZ_type(iele_fsmp_stack,       &
     &          nele_grp, iele_grp, ml_ele_diag, ele, fem_wk)
!
      use fem_skv_tensor_on_ele
!
      type(element_data), intent(in) :: ele
!
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: ml_ele_diag(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_tensor_on_ele_grp_HRZ(ele%numele, ele%nnod_4_ele,    &
     &    iele_fsmp_stack, ele%volume_ele, nele_grp, iele_grp,          &
     &     ml_ele_diag,  fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_tensor_on_egrp_HRZ_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_nodal_field_type
