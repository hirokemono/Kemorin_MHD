!>@file   find_belonged_process.f90
!!@brief  module find_belonged_process
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2021
!
!>@brief Find belonged pe for each node, element, surface, and edge
!!
!!@verbatim
!!      subroutine find_belonged_pe_4_node                              &
!!     &         (my_rank, node, nod_comm, ip_node)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        integer(kind = kint), intent(inout) :: ip_node(node%numnod)
!!      subroutine find_belonged_pe_4_ele(my_rank, node, ip_node,       &
!!     &                                  ele, ip_ref_ele, k_ref_ele)
!!        integer, intent(in) :: my_rank
!!        type(node_data), intent(in) :: node
!!        integer(kind = kint), intent(in) :: ip_node(node%numnod)
!!        type(element_data), intent(inout) :: ele
!!        integer(kind = kint), intent(inout) :: ip_ref_ele(ele%numele)
!!        integer(kind = kint), intent(inout) :: k_ref_ele(ele%numele)
!!      subroutine find_belonged_pe_4_surf(my_rank, node, ip_node,      &
!!     &                                   surf, ip_ref_sf, k_ref_sf)
!!        integer, intent(in) :: my_rank
!!        type(node_data), intent(in) :: node
!!        integer(kind = kint), intent(in) :: ip_node(node%numnod)
!!        type(surface_data), intent(inout) :: surf
!!        integer(kind = kint), intent(inout) :: ip_ref_sf(surf%numsurf)
!!        integer(kind = kint), intent(inout) :: k_ref_sf(surf%numsurf)
!!      subroutine find_belonged_pe_4_edge(my_rank, node, ip_node,      &
!!     &                                   edge, ip_ref_ed, k_ref_ed)
!!        integer, intent(in) :: my_rank
!!        type(node_data), intent(in) :: node
!!        integer(kind = kint), intent(in) :: ip_node(node%numnod)
!!        type(edge_data), intent(inout) :: edge
!!        integer(kind = kint), intent(inout) :: ip_ref_ed(edge%numedge)
!!        integer(kind = kint), intent(inout) :: k_ref_ed(edge%numedge)
!!@endverbatim
      module find_belonged_process
!
      use m_geometry_constants
      use t_geometry_data
!
      implicit none
!
      private :: find_belonged_pe_each_ele, find_belonged_pe_each_surf
      private :: find_belonged_pe_each_edge, set_each_interior_flag
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_node                                &
     &         (my_rank, node, nod_comm, ip_node)
!
      use t_comm_table
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(inout) :: ip_node(node%numnod)
!
      integer(kind = kint) :: inod, ist, ied, i, ip
!
!
!$omp parallel do
      do inod = 1, node%internal_node
        ip_node(inod) = my_rank
      end do
!$omp end parallel do
!
      do ip = 1, nod_comm%num_neib
        ist = nod_comm%istack_import(ip-1) + 1
        ied = nod_comm%istack_import(ip)
!$omp parallel do private(i,inod)
        do i = ist, ied
          inod = nod_comm%item_import(i)
          ip_node(inod) = nod_comm%id_neib(ip)
        end do
!$omp end parallel do
      end do
!
      end subroutine find_belonged_pe_4_node
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_ele(my_rank, node, ip_node,         &
     &                                  ele, ip_ref_ele, k_ref_ele)
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: ip_node(node%numnod)
!
      type(element_data), intent(inout) :: ele
      integer(kind = kint), intent(inout) :: ip_ref_ele(ele%numele)
      integer(kind = kint), intent(inout) :: k_ref_ele(ele%numele)
!
      integer(kind = kint) :: iele
      integer(kind = kint) :: ie_one
!
!
!$omp parallel workshare
      ip_ref_ele(1:ele%numele) =   -1
      k_ref_ele(1:ele%numele) =     0
!$omp end parallel workshare
!
!%omp parallel do private(iele,ie_one)
      do iele = 1, ele%numele
        ie_one = ele%ie(iele,1)
        call find_belonged_pe_each_ele(node%numnod, ip_node,            &
     &      ie_one, ip_ref_ele(iele), k_ref_ele(iele))
!
        ele%interior_ele(iele)                                          &
     &     = set_each_interior_flag(my_rank, ip_ref_ele(iele))
      end do
!%omp end parallel do
!
      end subroutine find_belonged_pe_4_ele
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_surf(my_rank, node, ip_node,        &
     &                                   surf, ip_ref_sf, k_ref_sf)
!
      use t_surface_data
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: ip_node(node%numnod)
!
      type(surface_data), intent(inout) :: surf
      integer(kind = kint), intent(inout) :: ip_ref_sf(surf%numsurf)
      integer(kind = kint), intent(inout) :: k_ref_sf(surf%numsurf)
!
      integer(kind = kint) :: isurf, nnod_same
      integer(kind = kint) :: ie_surf_one(num_linear_sf)
!
!
!$omp parallel workshare
      ip_ref_sf(1:surf%numsurf) =   -1
      k_ref_sf(1:surf%numsurf) =     0
!$omp end parallel workshare
!
!%omp parallel do private(isurf,ie_surf_one,nnod_same)
      do isurf = 1, surf%numsurf
        ie_surf_one(1:num_linear_sf)                                    &
     &         = surf%ie_surf(isurf,1:num_linear_sf)
        call find_belonged_pe_each_surf                                 &
     &     (node%numnod, ip_node, ie_surf_one, nnod_same,               &
     &      ip_ref_sf(isurf), k_ref_sf(isurf))
!
        surf%interior_surf(isurf)                                       &
     &     = set_each_interior_flag(my_rank, ip_ref_sf(isurf))
      end do
!%omp end parallel do
!
      end subroutine find_belonged_pe_4_surf
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_edge(my_rank, node, ip_node,        &
     &                                   edge, ip_ref_ed, k_ref_ed)
!
      use t_edge_data
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: ip_node(node%numnod)
!
      type(edge_data), intent(inout) :: edge
      integer(kind = kint), intent(inout) :: ip_ref_ed(edge%numedge)
      integer(kind = kint), intent(inout) :: k_ref_ed(edge%numedge)
!
      integer(kind = kint) :: iedge, nnod_same
      integer(kind = kint) :: ie_edge_one(num_linear_edge)
!
!$omp parallel workshare
      ip_ref_ed(1:edge%numedge) =   -1
      k_ref_ed(1:edge%numedge) =     0
!$omp end parallel workshare
!
!%omp parallel do private(iedge,ie_edge_one,nnod_same)
      do iedge = 1, edge%numedge
        ie_edge_one(1) = edge%ie_edge(iedge,1)
        ie_edge_one(2) = edge%ie_edge(iedge,2)
        call find_belonged_pe_each_edge                                 &
     &     (node%numnod, ip_node, ie_edge_one, nnod_same,               &
     &      ip_ref_ed(iedge), k_ref_ed(iedge))
!
        edge%interior_edge(iedge)                                       &
     &     = set_each_interior_flag(my_rank, ip_ref_ed(iedge))
      end do
!%omp end parallel do
!
      end subroutine find_belonged_pe_4_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    set_each_interior_flag(my_rank, ip_ref)
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: ip_ref
!
      set_each_interior_flag = 0
      if(ip_ref .eq. my_rank) set_each_interior_flag = 1
!
      end function set_each_interior_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_each_ele(numnod, ip_node,             &
     &          ie_one, ip_ref, k_ref)
!
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
       integer(kind = kint) :: ie_one
!
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
!
       ip_ref = ip_node(ie_one)
       k_ref = 1
 !
       end subroutine find_belonged_pe_each_ele
!
! ----------------------------------------------------------------------
!
       subroutine find_belonged_pe_each_surf(numnod, ip_node,           &
      &          ie_surf_one, nnod_same, ip_ref, k_ref)
!
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
       integer(kind = kint) :: ie_surf_one(num_linear_sf)
!
       integer(kind = kint), intent(inout) :: nnod_same
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
       integer(kind = kint) :: ip1, ip2, ip3, ip4
!
!
       ip1 = ip_node(ie_surf_one(1))
       ip2 = ip_node(ie_surf_one(2))
       ip3 = ip_node(ie_surf_one(3))
       ip4 = ip_node(ie_surf_one(4))
!
       if(ip1.eq.ip2 .and. ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 4
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip3 .and. ip2.eq.ip4) then
         nnod_same = 3
         ip_ref = ip2
         k_ref = 2
       else if(ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip2 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip2 .and. ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
!
       else if(ip1.eq.ip2 .and. ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = min(ip1, ip3)
         k_ref = 1
         if(ip_ref .eq. ip4) k_ref = 3
       else if(ip2.eq.ip3 .and. ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = min(ip1, ip2)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
       else if(ip1.eq.ip2) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip3) then
         nnod_same = 2
         ip_ref = ip2
         k_ref = 2
       else if(ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = ip3
         k_ref = 3
       else if(ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip4) then
         nnod_same = 2
         ip_ref = ip2
         k_ref = 2
       else
         nnod_same = 1
         ip_ref = min(ip1, ip2)
         ip_ref = min(ip3, ip_ref)
         ip_ref = min(ip4, ip_ref)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
         if(ip_ref .eq. ip3) k_ref = 3
         if(ip_ref .eq. ip4) k_ref = 4
       end if
!
       end subroutine find_belonged_pe_each_surf
!
! ----------------------------------------------------------------------
!
       subroutine find_belonged_pe_each_edge(numnod, ip_node,           &
      &          ie_edge_one, nnod_same, ip_ref, k_ref)
!
       integer(kind = kint) :: ie_edge_one(num_linear_edge)
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
!
       integer(kind = kint), intent(inout) :: nnod_same
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
       integer(kind = kint) :: ip1, ip2
!
       ip1 = ip_node(ie_edge_one(1))
       ip2 = ip_node(ie_edge_one(2))
!
       if(ip1.eq.ip2) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else
         nnod_same = 1
         ip_ref = min(ip1, ip2)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
       end if
!
       end subroutine find_belonged_pe_each_edge
!
! ----------------------------------------------------------------------
!
      end module find_belonged_process