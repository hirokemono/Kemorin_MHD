!
!     module average_on_elements
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!
!      subroutine velocity_on_element
!      subroutine magnetic_on_element
!      subroutine filtered_magne_on_ele
!
!      subroutine vorticity_on_element
!      subroutine rot_magne_on_element
!      subroutine current_on_element
!      subroutine rot_filter_magne_on_element
!
      module average_on_elements
!
      use m_precision
      use m_machine_parameter
!
      use m_control_parameter
!      use m_geometry_data
!      use m_int_vol_data
!
      use t_geometry_data
      use t_jacobian_3d
!
      use cal_fields_on_element
      use cal_differences_on_ele
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine vector_on_element_1st(node, ele, jac_3d,               &
     &          iele_fsmp_stack, n_int, ncomp_nod, ifld_nod, d_nod,     &
     &          ncomp_ele, ifld_ele, iflag_update, d_ele)
!
!      use m_jacobians
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_nod, ifld_nod
      real(kind = kreal), intent(in) :: d_nod(node%numnod,ncomp_nod)
!
      integer(kind = kint), intent(in) :: ncomp_ele, ifld_ele
      integer(kind = kint), intent(inout) :: iflag_update(ncomp_ele)
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,ncomp_ele)
!
!
      call vector_on_element(node, ele, jac_3d, iele_fsmp_stack,        &
     &    n_int, d_nod(1,ifld_nod), d_ele(1,ifld_ele))
      iflag_update(ifld_ele:ifld_ele+2) = 1
!
      end subroutine vector_on_element_1st
!
! -----------------------------------------------------------------------
!
      subroutine rotation_on_element_1st(node, ele, jac_3d,             &
     &          iele_fsmp_stack, n_int, ncomp_nod, ifld_nod, d_nod,     &
     &          ncomp_ele, ifld_ele, iflag_update, d_ele)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_nod, ifld_nod
      real(kind = kreal), intent(in) :: d_nod(node%numnod,ncomp_nod)
!
      integer(kind = kint), intent(in) :: ncomp_ele, ifld_ele
      integer(kind = kint), intent(inout) :: iflag_update(ncomp_ele)
      real(kind = kreal), intent(inout) :: d_ele(ele%numele,ncomp_ele)
!
!
      call rotation_on_element(node, ele, jac_3d,                       &
     &    iele_fsmp_stack, n_int, d_nod(1,ifld_nod), d_ele(1,ifld_ele))
      iflag_update(ifld_ele:ifld_ele+2) = 1
!
      end subroutine rotation_on_element_1st
!
! -----------------------------------------------------------------------
!
      end module average_on_elements
