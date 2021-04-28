!>@file   set_quad_node_ele_by_linear.f90
!!@brief  module set_quad_node_ele_by_linear
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief node list to construct tri-linear mesh from quad mesh
!!
!!@verbatim
!!      subroutine set_quad_node_by_linear                              &
!!     &         (node_l, edge_l, l_to_q, nnod_20, inod_gl20, xx20)
!!        type(node_data), intent(in) :: node_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_quad_list), intent(in) :: l_to_q
!!        integer(kind = kint), intent(in) :: nnod_20
!!        integer(kind = kint_gl), intent(inout) :: inod_gl20(nnod_20)
!!        real(kind=kreal), intent(inout) :: xx20(nnod_20,3)
!!      subroutine set_quad_ele_connect_by_linear                       &
!!     &         (ele_l, edge_l, l_to_q, numele_q, ie_q)
!!        type(element_data), intent(in) :: ele_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_quad_list), intent(in) :: l_to_q
!!        integer(kind = kint), intent(in) :: numele_q
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: ie_q(numele_q,num_t_quad)
!!      subroutine set_quad_surf_connect_by_linear                      &
!!     &         (surf_l, edge_l, l_to_q, numsurf_q, ie_sf_q)
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_quad_list), intent(in) :: l_to_q
!!        integer(kind = kint), intent(in) :: numsurf_q
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: ie_sf_q(numsurf_q,num_quad_sf)
!!      subroutine set_quad_edge_connect_by_linear                      &
!!     &         (edge_l, l_to_q, numedge_q, ie_ed_q)
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_quad_list), intent(in) :: l_to_q
!!        integer(kind = kint), intent(in) :: numedge_q
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: ie_ed_q(numedge_q,num_quad_edge)
!!@endverbatim
!
      module set_quad_node_ele_by_linear
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_linear_to_quad_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_quad_node_by_linear                                &
     &         (node_l, edge_l, l_to_q, nnod_20, inod_gl20, xx20)
!
      type(node_data), intent(in) :: node_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_quad_list), intent(in) :: l_to_q
      integer(kind = kint), intent(in) :: nnod_20
!
      integer(kind = kint_gl), intent(inout) :: inod_gl20(nnod_20)
      real(kind=kreal), intent(inout) :: xx20(nnod_20,3)
!
      integer(kind = kint) :: inum, inod, iedge
!
!
!$omp parallel do private(inum,inod)
        do inum = 1, node_l%numnod
          inod = l_to_q%inod_linear_to_quad(inum)
          inod_gl20(inod) = node_l%inod_global(inum)
          xx20(inod,1) = node_l%xx(inum,1)
          xx20(inod,2) = node_l%xx(inum,2)
          xx20(inod,3) = node_l%xx(inum,3)
        end do
!$omp end parallel do
!
!$omp parallel do private(iedge,inod)
      do iedge = 1, edge_l%numedge
        inod = l_to_q%iedge_linear_to_quad(iedge)
        inod_gl20(inod) = edge_l%iedge_global(iedge)                    &
     &                       + l_to_q%numnod_gl_l2q
        xx20(inod,1) = edge_l%x_edge(iedge,1)
        xx20(inod,2) = edge_l%x_edge(iedge,2)
        xx20(inod,3) = edge_l%x_edge(iedge,3)
      end do
!$omp end parallel do
!
      end subroutine set_quad_node_by_linear
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_quad_ele_connect_by_linear                         &
     &         (ele_l, edge_l, l_to_q, numele_q, ie_q)
!
      type(element_data), intent(in) :: ele_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_quad_list), intent(in) :: l_to_q
!
      integer(kind = kint), intent(in) :: numele_q
      integer(kind = kint), intent(inout)                               &
     &                     :: ie_q(numele_q,num_t_quad)
!
      integer(kind = kint) :: inod, iele, iedge, k1, is
!
!
!$omp parallel private(k1)
      do k1 = 1, ele_l%nnod_4_ele
!$omp do private(iele,inod)
        do iele = 1, numele_q
          inod = ele_l%ie(iele,k1)
          ie_q(iele,k1) = l_to_q%inod_linear_to_quad(inod)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(is)
      do is = 1, nedge_4_ele
!$omp do private(iele,iedge)
        do iele = 1, numele_q
          iedge = abs(edge_l%iedge_4_ele(iele,is))
          ie_q(iele,is+8) = l_to_q%iedge_linear_to_quad(iedge)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_quad_ele_connect_by_linear
!
!-----------------------------------------------------------------------
!
      subroutine set_quad_surf_connect_by_linear                        &
     &         (surf_l, edge_l, l_to_q, numsurf_q, ie_sf_q)
!
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_quad_list), intent(in) :: l_to_q
!
      integer(kind = kint), intent(in) :: numsurf_q
      integer(kind = kint), intent(inout)                               &
     &                     :: ie_sf_q(numsurf_q,num_quad_sf)
!
      integer(kind = kint) :: inod, iedge, isurf, k1, is
!
!
!$omp parallel private(k1)
      do k1 = 1, surf_l%nnod_4_surf
!$omp do private(isurf,inod)
        do isurf = 1, numsurf_q
          inod = surf_l%ie_surf(isurf,k1)
          ie_sf_q(isurf,k1) = l_to_q%inod_linear_to_quad(inod)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(is)
      do is = 1, nedge_4_surf
!$omp do private(isurf,iedge)
        do isurf = 1, numsurf_q
          iedge = abs(edge_l%iedge_4_sf(isurf,is))
          ie_sf_q(isurf,is+4) = l_to_q%iedge_linear_to_quad(iedge)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_quad_surf_connect_by_linear
!
!-----------------------------------------------------------------------
!
      subroutine set_quad_edge_connect_by_linear                        &
     &         (edge_l, l_to_q, numedge_q, ie_ed_q)
!
      type(edge_data), intent(in) :: edge_l
      type(linear_to_quad_list), intent(in) :: l_to_q
!
      integer(kind = kint), intent(in) :: numedge_q
      integer(kind = kint), intent(inout)                               &
     &                     :: ie_ed_q(numedge_q,num_quad_edge)
!
      integer(kind = kint) :: inod, iedge, k1
!
!
!$omp parallel private(k1)
      do k1 = 1, edge_l%nnod_4_edge
!$omp do private(iedge,inod)
        do iedge = 1, numedge_q
          inod = edge_l%ie_edge(iedge,k1)
          ie_ed_q(iedge,k1) = l_to_q%inod_linear_to_quad(inod)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel do private(iedge)
      do iedge = 1, numedge_q
        ie_ed_q(iedge,3) = l_to_q%iedge_linear_to_quad(iedge)
      end do
!$omp end parallel do
!
      end subroutine set_quad_edge_connect_by_linear
!
!-----------------------------------------------------------------------
!
      end module set_quad_node_ele_by_linear
