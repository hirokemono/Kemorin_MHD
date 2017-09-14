!
!     module cal_fields_on_element
!
!     Written by H. Matsui on Oct., 2005
!
!!      subroutine scalar_on_element(node, ele, g_FEM, jac_3d,          &
!!     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!!      subroutine vector_on_element(node, ele, g_FEM, jac_3d,          &
!!     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!!      subroutine sym_tensor_on_element(node, ele, g_FEM, jac_3d,      &
!!     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!!
!!      subroutine scalar_grp_on_element(node, ele, g_FEM, jac_3d,      &
!!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          d_nod, d_ele)
!!      subroutine vector_grp_on_element(node, ele, g_FEM, jac_3d,      &
!!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          d_nod, d_ele)
!!      subroutine sym_tensor_grp_on_element(node, ele, g_FEM, jac_3d,  &
!!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          d_nod, d_ele)
!!        type(node_data), intent(in) ::    node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!
      module cal_fields_on_element
!
      use m_precision
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
!
      implicit none
!
! ----------------------------------------------------------------------
!
     contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_on_element(node, ele, g_FEM, jac_3d,            &
     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!
      use fem_fields_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele)
!
!
      call fem_scalar_on_element(iele_fsmp_stack, node%numnod,          &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%an, jac_3d%xjac,  &
     &    d_ele, d_nod)
!
      end subroutine scalar_on_element
!
! ----------------------------------------------------------------------
!
      subroutine vector_on_element(node, ele, g_FEM, jac_3d,            &
     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!
      use fem_fields_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,3)
!
!
      call fem_vector_on_element(iele_fsmp_stack, node%numnod,          &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%an, jac_3d%xjac,  &
     &    d_ele, d_nod)
!
      end subroutine vector_on_element
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_on_element(node, ele, g_FEM, jac_3d,        &
     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!
      use fem_fields_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,6)
!
!
      call fem_sym_tensor_on_element(iele_fsmp_stack, node%numnod,      &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%an, jac_3d%xjac,  &
     &    d_ele, d_nod)
!
      end subroutine sym_tensor_on_element
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_grp_on_element(node, ele, g_FEM, jac_3d,        &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_nod, d_ele)
!
      use fem_fields_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele)
!
      call fem_scalar_grp_on_element                                    &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%an, jac_3d%xjac,  &
     &    d_ele, d_nod)
!
      end subroutine scalar_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine vector_grp_on_element(node, ele, g_FEM, jac_3d,        &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_nod, d_ele)
!
      use fem_fields_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,3)
!
!
      call fem_vector_grp_on_element                                    &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele,  nele_grp, iele_grp,                   &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%an, jac_3d%xjac,  &
     &    d_ele, d_nod)
!
      end subroutine vector_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_grp_on_element(node, ele, g_FEM, jac_3d,    &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_nod, d_ele)
!
      use fem_fields_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,6)
!
!
      call fem_sym_tensor_grp_on_element                                &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%an, jac_3d%xjac,  &
     &    d_ele, d_nod)
!
      end subroutine sym_tensor_grp_on_element
!
! ----------------------------------------------------------------------
!
      end module cal_fields_on_element
