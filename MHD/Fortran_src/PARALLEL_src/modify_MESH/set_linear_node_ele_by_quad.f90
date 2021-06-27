!>@file   set_linear_node_ele_by_quad.f90
!!@brief  module set_linear_node_ele_by_quad
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief node list to construct tri-linear mesh from quad mesh
!!
!!@verbatim
!!      subroutine set_linear_node_by_quad(node, ele, surf, q_to_l,     &
!!     &                                   nnod_8, inod_gl8, xx8)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(quad_to_linear_list), intent(in) :: q_to_l
!!        integer(kind = kint), intent(in) :: nnod_8
!!        integer(kind = kint_gl), intent(inout) :: inod_gl8(nnod_8)
!!        real(kind=kreal), intent(inout) :: xx8(nnod_8,3)
!!      subroutine set_linear_ele_connect_by_quad                       &
!!     &         (ele_q, surf_q, q_to_l, numele_l, iele_gl_l, ie_l)
!!        type(element_data), intent(in) :: ele_q
!!        type(surface_data), intent(in) :: surf_q
!!        type(quad_to_linear_list), intent(in) :: q_to_l
!!        integer(kind = kint), intent(in) :: numele_l
!!        integer(kind = kint_gl), intent(inout) :: iele_gl_l(numele_l)
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: ie_l(numele_l,num_t_linear)
!!        integer(kind = kint) :: iele, k1
!!        integer(kind = kint) :: inod20(20), ie1_q27(27), ie1_l(8,8)
!!@endverbatim
!
      module set_linear_node_ele_by_quad
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_quad_to_linear_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_linear_node_by_quad(node, ele, surf, q_to_l,       &
     &                                   nnod_8, inod_gl8, xx8)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(quad_to_linear_list), intent(in) :: q_to_l
      integer(kind = kint), intent(in) :: nnod_8
!
      integer(kind = kint_gl), intent(inout) :: inod_gl8(nnod_8)
      real(kind=kreal), intent(inout) :: xx8(nnod_8,3)
!
      integer(kind = kint) :: inum, inod, iele, isurf
!
!
!$omp parallel do private(inum,inod)
        do inum = 1, node%numnod
          inod = q_to_l%inod_quad_to_linear(inum)
          inod_gl8(inod) = node%inod_global(inum)
          xx8(inod,1) = node%xx(inum,1)
          xx8(inod,2) = node%xx(inum,2)
          xx8(inod,3) = node%xx(inum,3)
        end do
!$omp end parallel do
!
!$omp parallel do private(isurf,inod)
      do isurf = 1, surf%numsurf
        inod = q_to_l%isurf_quad_to_linear(isurf)
        inod_gl8(inod) = surf%isurf_global(isurf)                       &
     &                       + q_to_l%numnod_gl_q2l
        xx8(inod,1) = surf%x_surf(isurf,1)
        xx8(inod,2) = surf%x_surf(isurf,2)
        xx8(inod,3) = surf%x_surf(isurf,3)
      end do
!$omp end parallel do
!
!$omp parallel do private(iele,inod)
      do iele = 1, ele%numele
        inod = q_to_l%iele_quad_to_linear(iele)
        inod_gl8(inod) = ele%iele_global(iele)                          &
     &                       + q_to_l%numnod_gl_q2l                     &
     &                       + q_to_l%numsurf_gl_q2l
        xx8(inod,1) = ele%x_ele(iele,1)
        xx8(inod,2) = ele%x_ele(iele,2)
        xx8(inod,3) = ele%x_ele(iele,3)
      end do
!$omp end parallel do
!
      end subroutine set_linear_node_by_quad
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_linear_ele_connect_by_quad                         &
     &         (ele_q, surf_q, q_to_l, numele_l, iele_gl_l, ie_l)
!
      use m_27quad_2_8x8linear
!
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
      type(quad_to_linear_list), intent(in) :: q_to_l
      integer(kind = kint), intent(in) :: numele_l
!
      integer(kind = kint_gl), intent(inout) :: iele_gl_l(numele_l)
      integer(kind = kint), intent(inout)                               &
     &                     :: ie_l(numele_l,num_t_linear)
!
      integer(kind = kint) :: iele, k1
      integer(kind = kint) :: inod20(20), ie1_q27(27), ie1_l(8,8)
!
!
!$omp parallel do private(iele,inod20,ie1_q27,ie1_l,k1)
      do iele = 1, ele_q%numele
        inod20(1:20) = ele_q%ie(iele, 1:20)
        ie1_q27(1:20) = q_to_l%inod_quad_to_linear(inod20(1:20))
!
        inod20(1:6) =    abs(surf_q%isf_4_ele(iele,1:6))
        ie1_q27(21:26) = q_to_l%isurf_quad_to_linear(inod20(1:6))
!
        ie1_q27(27) =    q_to_l%iele_quad_to_linear(iele)
!
        call set_27quad_2_8x8linear_1ele(ie1_q27, ie1_l)
!
        do k1 = 1, 8
          iele_gl_l(8*iele-7:8*iele)                                    &
     &        = 8 * (ele_q%iele_global(iele)-1) + k1
        end do
        do k1 = 1, num_t_linear
          ie_l(8*iele-7:8*iele,k1) = ie1_l(k1,1:8)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_linear_ele_connect_by_quad
!
!-----------------------------------------------------------------------
!
      end module set_linear_node_ele_by_quad
