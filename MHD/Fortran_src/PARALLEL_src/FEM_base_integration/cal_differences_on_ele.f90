!cal_differences_on_ele.f90
!     module cal_differences_on_ele
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine difference_on_element(iele_fsmp_stack, n_int, nd,     &
!     &          d_ele, d_nod)
!      subroutine gradient_on_element(iele_fsmp_stack, n_int,           &
!     &          d_ele, d_nod)
!      subroutine divergence_on_element(iele_fsmp_stack, n_int,         &
!     &          d_ele, d_nod)
!      subroutine rotation_on_element(iele_fsmp_stack, n_int,           &
!     &          d_ele, d_nod)
!      subroutine div_sym_tensor_on_element(iele_fsmp_stack, n_int,     &
!     &          d_ele, d_nod)
!      subroutine div_asym_tensor_on_element(iele_fsmp_stack, n_int,    &
!     &          d_ele, d_nod)
!
!      subroutine difference_grp_on_element(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, n_int, nd, d_ele, d_nod)
!      subroutine gradient_grp_on_element(iele_fsmp_stack,              &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine divergence_grp_on_element(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine rotation_grp_on_element(iele_fsmp_stack,              &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine div_sym_tensor_grp_on_element(iele_fsmp_stack,        &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine div_asym_tensor_grp_on_element(iele_fsmp_stack,       &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      module cal_differences_on_ele
!
      use m_precision
!
      use m_geometry_data
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
      subroutine difference_on_element(iele_fsmp_stack, n_int, nd,      &
     &          d_ele, d_nod)
!
      use cal_difference_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int, nd
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele)
!
!
      call fem_difference_on_element(iele_fsmp_stack, node1%numnod,     &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%a_vol_ele,        &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    nd, d_ele, d_nod)
!
      end subroutine difference_on_element
!
! ----------------------------------------------------------------------
!
      subroutine gradient_on_element(iele_fsmp_stack, n_int,            &
     &          d_ele, d_nod)
!
      use cal_gradient_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
!
      call fem_gradient_on_element(iele_fsmp_stack, node1%numnod,       &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%a_vol_ele,        &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine gradient_on_element
!
! ----------------------------------------------------------------------
!
      subroutine divergence_on_element(iele_fsmp_stack, n_int,          &
     &          d_ele, d_nod)
!
      use cal_divergence_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele)
!
      call fem_divergence_on_element(iele_fsmp_stack, node1%numnod,     &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%a_vol_ele,        &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine divergence_on_element
!
!-----------------------------------------------------------------------
!
      subroutine rotation_on_element(iele_fsmp_stack, n_int,            &
     &          d_ele, d_nod)
!
      use cal_rotation_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
!
      call fem_rotation_on_element(iele_fsmp_stack, node1%numnod,       &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%a_vol_ele,        &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine rotation_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_sym_tensor_on_element(iele_fsmp_stack, n_int,      &
     &          d_ele, d_nod)
!
      use div_s_tensor_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) ::  n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
!
      call fem_div_sym_tensor_on_ele(iele_fsmp_stack, node1%numnod,     &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%a_vol_ele,        &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &     d_ele, d_nod)
!
      end subroutine div_sym_tensor_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_asym_tensor_on_element(iele_fsmp_stack, n_int,     &
     &          d_ele, d_nod)
!
      use div_as_tensor_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
      call fem_div_asym_tensor_on_ele(iele_fsmp_stack, node1%numnod,    &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%a_vol_ele,        &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine div_asym_tensor_on_element
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine difference_grp_on_element(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, n_int, nd, d_ele, d_nod)
!
      use cal_difference_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int, nd
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele)
!
!
      call fem_difference_grp_on_element                                &
     &   (iele_fsmp_stack, node1%numnod, ele1%numele, ele1%nnod_4_ele,  &
     &    ele1%ie, ele1%a_vol_ele, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac, nd,           &
     &    d_ele, d_nod)
!
      end subroutine difference_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine gradient_grp_on_element(iele_fsmp_stack,               &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use cal_gradient_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
!
      call fem_gradient_grp_on_element                                  &
     &   (iele_fsmp_stack, node1%numnod, ele1%numele, ele1%nnod_4_ele,  &
     &    ele1%ie, ele1%a_vol_ele, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine gradient_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine divergence_grp_on_element(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use cal_divergence_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele)
!
      call fem_divergence_grp_on_element                                &
     &   (iele_fsmp_stack, node1%numnod, ele1%numele, ele1%nnod_4_ele,  &
     &    ele1%ie, ele1%a_vol_ele, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine divergence_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine rotation_grp_on_element(iele_fsmp_stack,               &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use cal_rotation_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
!
      call fem_rotation_grp_on_element                                  &
     &   (iele_fsmp_stack, node1%numnod, ele1%numele, ele1%nnod_4_ele,  &
     &    ele1%ie, ele1%a_vol_ele, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine rotation_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_sym_tensor_grp_on_element(iele_fsmp_stack,         &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use div_s_tensor_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
!
      call fem_div_sym_tensor_grp_on_ele&
     &   (iele_fsmp_stack, node1%numnod, ele1%numele, ele1%nnod_4_ele,  &
     &    ele1%ie, ele1%a_vol_ele, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine div_sym_tensor_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine div_asym_tensor_grp_on_element(iele_fsmp_stack,        &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use div_as_tensor_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
      call fem_div_asym_tensor_grp_on_ele                               &
     &   (iele_fsmp_stack, node1%numnod, ele1%numele, ele1%nnod_4_ele,  &
     &    ele1%ie, ele1%a_vol_ele, nele_grp, iele_grp,                  &
     &    jac1_3d_q%ntot_int, n_int, dwx, jac1_3d_q%xjac,               &
     &    d_ele, d_nod)
!
      end subroutine div_asym_tensor_grp_on_element
!
! ----------------------------------------------------------------------
!
      end module cal_differences_on_ele
