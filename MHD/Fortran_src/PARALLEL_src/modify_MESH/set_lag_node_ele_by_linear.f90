!>@file   set_lag_node_ele_by_linear.f90
!!@brief  module set_lag_node_ele_by_linear
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief node list to construct tri-linear mesh from quad mesh
!!
!!@verbatim
!!      subroutine set_lag_node_by_linear                               &
!!     &         (node_l, ele_l, surf_l, edge_l, l_to_lag,              &
!!     &          nnod_27, inod_gl27, xx27)
!!        type(node_data), intent(in) :: node_l
!!        type(element_data), intent(in) :: ele_l
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        integer(kind = kint), intent(in) :: nnod_27
!!        integer(kind = kint_gl), intent(inout) :: inod_gl27(nnod_27)
!!        real(kind=kreal), intent(inout) :: xx27(nnod_27,3)
!!      subroutine set_lag_ele_connect_by_linear                        &
!!     &         (ele_l, surf_l, edge_l, l_to_lag, numele_q, ie_q)
!!        type(element_data), intent(in) :: ele_l
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        integer(kind = kint), intent(in) :: numele_q
!!        integer(kind = kint), intent(inout) :: ie_q(numele_q,num_t_lag)
!!      subroutine set_lag_surf_connect_by_linear                       &
!!     &         (surf_l, edge_l, l_to_lag, numsurf_q, ie_sf_q)
!!        type(surface_data), intent(in) :: surf_l
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        integer(kind = kint), intent(in) :: numsurf_q
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: ie_sf_q(numsurf_q,num_lag_sf)
!!      subroutine set_lag_edge_connect_by_linear                       &
!!     &         (edge_l, l_to_lag, numedge_q, ie_ed_q)
!!        type(edge_data), intent(in) :: edge_l
!!        type(linear_to_lag_list), intent(in) :: l_to_lag
!!        integer(kind = kint), intent(in) :: numedge_q
!!        integer(kind = kint), intent(inout)                           &
!!       &                     :: ie_ed_q(numedge_q,num_quad_edge)
!!@endverbatim
!
      module set_lag_node_ele_by_linear
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_linear_to_lag_list
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_lag_node_by_linear                                 &
     &         (node_l, ele_l, surf_l, edge_l, l_to_lag,                &
     &          nnod_27, inod_gl27, xx27)
!
      type(node_data), intent(in) :: node_l
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
      integer(kind = kint), intent(in) :: nnod_27
!
      integer(kind = kint_gl), intent(inout) :: inod_gl27(nnod_27)
      real(kind=kreal), intent(inout) :: xx27(nnod_27,3)
!
      integer(kind = kint) :: inum, inod, iele, isurf, iedge
!
!
!$omp parallel do private(inum,inod)
        do inum = 1, node_l%numnod
          inod = l_to_lag%inod_linear_to_lag(inum)
          inod_gl27(inod) = node_l%inod_global(inum)
          xx27(inod,1) = node_l%xx(inum,1)
          xx27(inod,2) = node_l%xx(inum,2)
          xx27(inod,3) = node_l%xx(inum,3)
        end do
!$omp end parallel do
!
!$omp parallel do private(iedge,inod)
      do iedge = 1, edge_l%numedge
        inod = l_to_lag%iedge_linear_to_lag(iedge)
        inod_gl27(inod) = edge_l%iedge_global(iedge)                    &
     &                       + l_to_lag%numnod_gl_l2lag
        xx27(inod,1) = edge_l%x_edge(iedge,1)
        xx27(inod,2) = edge_l%x_edge(iedge,2)
        xx27(inod,3) = edge_l%x_edge(iedge,3)
      end do
!$omp end parallel do
!
!$omp parallel do private(isurf,inod)
      do isurf = 1, surf_l%numsurf
        inod = l_to_lag%isurf_linear_to_lag(isurf)
        inod_gl27(inod) = surf_l%isurf_global(isurf)                    &
     &                       + l_to_lag%numnod_gl_l2lag                 &
     &                       + l_to_lag%numedge_gl_l2lag
        xx27(inod,1) = surf_l%x_surf(isurf,1)
        xx27(inod,2) = surf_l%x_surf(isurf,2)
        xx27(inod,3) = surf_l%x_surf(isurf,3)
      end do
!$omp end parallel do
!
!$omp parallel do private(iele,inod)
      do iele = 1, ele_l%numele
        inod = l_to_lag%iele_linear_to_lag(iele)
        inod_gl27(inod) = ele_l%iele_global(iele)                       &
     &                       + l_to_lag%numnod_gl_l2lag                 &
     &                       + l_to_lag%numedge_gl_l2lag                &
     &                       + l_to_lag%numsurf_gl_l2lag
        xx27(inod,1) = ele_l%x_ele(iele,1)
        xx27(inod,2) = ele_l%x_ele(iele,2)
        xx27(inod,3) = ele_l%x_ele(iele,3)
      end do
!$omp end parallel do
!
      end subroutine set_lag_node_by_linear
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_lag_ele_connect_by_linear                          &
     &         (ele_l, surf_l, edge_l, l_to_lag, numele_q, ie_q)
!
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      integer(kind = kint), intent(in) :: numele_q
      integer(kind = kint), intent(inout) :: ie_q(numele_q,num_t_lag)
!
      integer(kind = kint) :: inod, iele, iedge, isurf, k1, is
!
!
!$omp parallel private(k1)
      do k1 = 1, ele_l%nnod_4_ele
!$omp do private(iele,inod)
        do iele = 1, numele_q
          inod = ele_l%ie(iele,k1)
          ie_q(iele,k1) = l_to_lag%inod_linear_to_lag(inod)
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
          ie_q(iele,is+8) = l_to_lag%iedge_linear_to_lag(iedge)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel private(is)
      do is = 1, nsurf_4_ele
!$omp do private(iele,isurf)
        do iele = 1, numele_q
          isurf = abs(surf_l%isf_4_ele(iele,is))
          ie_q(iele,is+20) = l_to_lag%isurf_linear_to_lag(isurf)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel do private(iele)
      do iele = 1, numele_q
        ie_q(iele,27) = l_to_lag%iele_linear_to_lag(iele)
      end do
!$omp end parallel do
!
      end subroutine set_lag_ele_connect_by_linear
!
!-----------------------------------------------------------------------
!
      subroutine set_lag_surf_connect_by_linear                         &
     &         (surf_l, edge_l, l_to_lag, numsurf_q, ie_sf_q)
!
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      integer(kind = kint), intent(in) :: numsurf_q
      integer(kind = kint), intent(inout)                               &
     &                     :: ie_sf_q(numsurf_q,num_lag_sf)
!
      integer(kind = kint) :: inod, iedge, isurf, k1, is
!
!
!$omp parallel private(k1)
      do k1 = 1, surf_l%nnod_4_surf
!$omp do private(isurf,inod)
        do isurf = 1, numsurf_q
          inod = surf_l%ie_surf(isurf,k1)
          ie_sf_q(isurf,k1) = l_to_lag%inod_linear_to_lag(inod)
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
          ie_sf_q(isurf,is+4) = l_to_lag%iedge_linear_to_lag(iedge)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel do private(isurf)
      do isurf = 1, numsurf_q
        ie_sf_q(isurf,9) = l_to_lag%isurf_linear_to_lag(isurf)
      end do
!$omp end parallel do
!
      end subroutine set_lag_surf_connect_by_linear
!
!-----------------------------------------------------------------------
!
      subroutine set_lag_edge_connect_by_linear                         &
     &         (edge_l, l_to_lag, numedge_q, ie_ed_q)
!
      type(edge_data), intent(in) :: edge_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
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
          ie_ed_q(iedge,k1) = l_to_lag%inod_linear_to_lag(inod)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!$omp parallel do private(iedge)
      do iedge = 1, numedge_q
        ie_ed_q(iedge,3) = l_to_lag%iedge_linear_to_lag(iedge)
      end do
!$omp end parallel do
!
      end subroutine set_lag_edge_connect_by_linear
!
!-----------------------------------------------------------------------
!
      end module set_lag_node_ele_by_linear
