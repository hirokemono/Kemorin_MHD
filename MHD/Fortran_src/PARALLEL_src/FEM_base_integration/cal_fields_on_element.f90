!
!     module cal_fields_on_element
!
!     Written by H. Matsui on Oct., 2005
!
!      subroutine scalar_on_element(iele_fsmp_stack, n_int,             &
!     &          d_ele, d_nod)
!      subroutine vector_on_element(iele_fsmp_stack, n_int,             &
!     &          d_ele, d_nod)
!      subroutine sym_tensor_on_element(iele_fsmp_stack, n_int,         &
!     &          d_ele, d_nod)
!
!      subroutine scalar_grp_on_element(iele_fsmp_stack,                &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine vector_grp_on_element(iele_fsmp_stack,                &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!      subroutine sym_tensor_grp_on_element(iele_fsmp_stack,            &
!     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      module cal_fields_on_element
!
      use m_precision
!
      use m_geometry_data
      use m_jacobians
!
      implicit none
!
! ----------------------------------------------------------------------
!
     contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_on_element(iele_fsmp_stack, n_int,              &
     &          d_ele, d_nod)
!
      use fem_fields_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele)
!
!
      call fem_scalar_on_element(iele_fsmp_stack,                       &
     &          node1%numnod, ele1%numele, nnod_4_ele, ie, a_vol_ele,   &
     &          ntot_int_3d, n_int, aw, xjac, d_ele, d_nod)
!
      end subroutine scalar_on_element
!
! ----------------------------------------------------------------------
!
      subroutine vector_on_element(iele_fsmp_stack, n_int,              &
     &          d_ele, d_nod)
!
      use fem_fields_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,3)
!
!
      call fem_vector_on_element(iele_fsmp_stack,                       &
     &          node1%numnod, ele1%numele, nnod_4_ele, ie, a_vol_ele,   &
     &          ntot_int_3d, n_int, aw, xjac, d_ele, d_nod)
!
      end subroutine vector_on_element
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_on_element(iele_fsmp_stack, n_int,          &
     &          d_ele, d_nod)
!
      use fem_fields_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,6)
!
!
      call fem_sym_tensor_on_element(iele_fsmp_stack,                   &
     &          node1%numnod, ele1%numele, nnod_4_ele, ie, a_vol_ele,   &
     &          ntot_int_3d, n_int, aw, xjac, d_ele, d_nod)
!
      end subroutine sym_tensor_on_element
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_grp_on_element(iele_fsmp_stack,                 &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use fem_fields_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele)
!
      call fem_scalar_grp_on_element(iele_fsmp_stack,                   &
     &          node1%numnod, ele1%numele, nnod_4_ele, ie, a_vol_ele,   &
     &          nele_grp, iele_grp, ntot_int_3d, n_int, aw, xjac,       &
     &          d_ele, d_nod)
!
      end subroutine scalar_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine vector_grp_on_element(iele_fsmp_stack,                 &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use fem_fields_on_element
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
      call fem_vector_grp_on_element(iele_fsmp_stack,                   &
     &          node1%numnod, ele1%numele, nnod_4_ele, ie, a_vol_ele,   &
     &          nele_grp, iele_grp, ntot_int_3d, n_int, aw, xjac,       &
     &          d_ele, d_nod)
!
      end subroutine vector_grp_on_element
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_grp_on_element(iele_fsmp_stack,             &
     &          nele_grp, iele_grp, n_int, d_ele, d_nod)
!
      use fem_fields_on_element
!
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: nele_grp
      integer (kind = kint), intent(in) :: iele_grp(nele_grp)
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: d_nod(node1%numnod,6)
!
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,6)
!
!
      call fem_sym_tensor_grp_on_element(iele_fsmp_stack,               &
     &          node1%numnod, ele1%numele, nnod_4_ele, ie, a_vol_ele,   &
     &          nele_grp, iele_grp, ntot_int_3d, n_int, aw, xjac,       &
     &          d_ele, d_nod)
!
      end subroutine sym_tensor_grp_on_element
!
! ----------------------------------------------------------------------
!
      end module cal_fields_on_element
