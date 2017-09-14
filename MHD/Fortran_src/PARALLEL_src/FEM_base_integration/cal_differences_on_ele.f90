!cal_differences_on_ele.f90
!     module cal_differences_on_ele
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine difference_on_element(node, ele, g_FEM, jac_3d,      &
!!     &          iele_fsmp_stack, n_int, nd, d_nod, d_ele)
!!      subroutine gradient_on_element(node, ele, g_FEM, jac_3d,        &
!!     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!!      subroutine divergence_on_element(node, ele, g_FEM, jac_3d,      &
!!     &          iele_fsmp_stack, n_int,d_nod, d_ele)
!!      subroutine rotation_on_element(node, ele, g_FEM, jac_3d,        &
!!     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!!      subroutine div_sym_tensor_on_element(node, ele, g_FEM, jac_3d,  &
!!     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!!      subroutine div_asym_tensor_on_element(node, ele, g_FEM, jac_3d, &
!!     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!!
!!      subroutine difference_grp_on_element(node, ele, g_FEM, jac_3d,  &
!!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int, nd,       &
!!     &          d_nod, d_ele)
!!      subroutine gradient_grp_on_element(node, ele, g_FEM, jac_3d,    &
!!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          d_nod, d_ele)
!!      subroutine divergence_grp_on_element(node, ele, g_FEM, jac_3d,  &
!!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          d_nod, d_ele)
!!      subroutine rotation_grp_on_element(node, ele, g_FEM, jac_3d,    &
!!     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,           &
!!     &          d_nod, d_ele)
!!      subroutine div_sym_tensor_grp_on_element                        &
!!     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack,            &
!!     &          nele_grp, iele_grp, n_int, d_nod, d_ele)
!!      subroutine div_asym_tensor_grp_on_element                       &
!!     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack,            &
!!     &          nele_grp, iele_grp, n_int, d_nod, d_ele)
!!        type(node_data), intent(in) ::    node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!
!
      module cal_differences_on_ele
!
      use m_precision
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
      subroutine difference_on_element(node, ele, g_FEM, jac_3d,        &
     &          iele_fsmp_stack, n_int, nd, d_nod, d_ele)
!
      use cal_difference_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int, nd
      real(kind = kreal), intent(in) :: d_nod(node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele)
!
!
      call fem_difference_on_element(iele_fsmp_stack, node%numnod,      &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    nd, d_ele, d_nod)
!
      end subroutine difference_on_element
!
! ----------------------------------------------------------------------
!
      subroutine gradient_on_element(node, ele, g_FEM, jac_3d,          &
     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!
      use cal_gradient_on_element
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
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,3)
!
!
      call fem_gradient_on_element(iele_fsmp_stack, node%numnod,        &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine gradient_on_element
!
! ----------------------------------------------------------------------
!
      subroutine divergence_on_element(node, ele, g_FEM, jac_3d,        &
     &          iele_fsmp_stack, n_int,d_nod, d_ele)
!
      use cal_divergence_on_element
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
      real(kind = kreal), intent(inout) :: d_ele(ele%numele)
!
      call fem_divergence_on_element(iele_fsmp_stack, node%numnod,      &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine divergence_on_element
!
!-----------------------------------------------------------------------
!
      subroutine rotation_on_element(node, ele, g_FEM, jac_3d,          &
     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!
      use cal_rotation_on_element
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
      call fem_rotation_on_element(iele_fsmp_stack, node%numnod,        &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine rotation_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_sym_tensor_on_element(node, ele, g_FEM, jac_3d,    &
     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!
      use div_s_tensor_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) ::  n_int
      real(kind = kreal), intent(in) :: d_nod(node%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,3)
!
!
      call fem_div_sym_tensor_on_ele(iele_fsmp_stack, node%numnod,      &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine div_sym_tensor_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_asym_tensor_on_element(node, ele, g_FEM, jac_3d,   &
     &          iele_fsmp_stack, n_int, d_nod, d_ele)
!
      use div_as_tensor_on_element
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
      call fem_div_asym_tensor_on_ele(iele_fsmp_stack, node%numnod,     &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ele%a_vol_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine div_asym_tensor_on_element
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine difference_grp_on_element(node, ele, g_FEM, jac_3d,    &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int, nd,         &
     &          d_nod, d_ele)
!
      use cal_difference_on_element
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int, nd
      real(kind = kreal), intent(in) :: d_nod(node%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele%numele)
!
!
      call fem_difference_grp_on_element                                &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    nd, d_ele, d_nod)
!
      end subroutine difference_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine gradient_grp_on_element(node, ele, g_FEM, jac_3d,      &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_nod, d_ele)
!
      use cal_gradient_on_element
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
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,3)
!
!
      call fem_gradient_grp_on_element                                  &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine gradient_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine divergence_grp_on_element(node, ele, g_FEM, jac_3d,    &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_nod, d_ele)
!
      use cal_divergence_on_element
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
      real(kind = kreal), intent(inout) :: d_ele(ele%numele)
!
      call fem_divergence_grp_on_element                                &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine divergence_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine rotation_grp_on_element(node, ele, g_FEM, jac_3d,      &
     &          iele_fsmp_stack, nele_grp, iele_grp, n_int,             &
     &          d_nod, d_ele)
!
      use cal_rotation_on_element
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
      call fem_rotation_grp_on_element                                  &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine rotation_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_sym_tensor_grp_on_element                          &
     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack,              &
     &          nele_grp, iele_grp, n_int, d_nod, d_ele)
!
      use div_s_tensor_on_element
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
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,3)
!
!
      call fem_div_sym_tensor_grp_on_ele                                &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine div_sym_tensor_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_asym_tensor_grp_on_element                         &
     &         (node, ele, g_FEM, jac_3d, iele_fsmp_stack,              &
     &          nele_grp, iele_grp, n_int, d_nod, d_ele)
!
      use div_as_tensor_on_element
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
      call fem_div_asym_tensor_grp_on_ele                               &
     &   (iele_fsmp_stack, node%numnod, ele%numele, ele%nnod_4_ele,     &
     &    ele%ie, ele%a_vol_ele, nele_grp, iele_grp,                    &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%dnx, jac_3d%xjac, &
     &    d_ele, d_nod)
!
      end subroutine div_asym_tensor_grp_on_element
!
! ----------------------------------------------------------------------
!
      end module cal_differences_on_ele
