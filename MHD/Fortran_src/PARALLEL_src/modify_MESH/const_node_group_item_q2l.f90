!>@file   const_node_group_item_q2l.f90
!!@brief  module const_node_group_item_q2l
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief Construct node group data on liear mesh from quad mesh
!!
!!@verbatim
!!      subroutine s_const_node_group_item_q2l                          &
!!     &         (mesh_q, nod_grp_q, q_to_l, nod_grp_l)
!!        type(mesh_geometry), intent(in) :: mesh_q
!!        type(group_data), intent(in) :: nod_grp_q
!!        type(quad_to_linear_list), intent(in) :: q_to_l
!!        type(group_data), intent(inout) :: nod_grp_l
!!@endverbatim
!
      module const_node_group_item_q2l
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_quad_to_linear_list
      use t_linear_to_quad_list
      use t_linear_to_lag_list
!
      implicit none
!
      private :: count_node_group_item_q2l, set_node_group_item_q2l
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_const_node_group_item_q2l                            &
     &         (mesh_q, nod_grp_q, q_to_l, nod_grp_l)
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(group_data), intent(in) :: nod_grp_q
      type(quad_to_linear_list), intent(in) :: q_to_l
!
      type(group_data), intent(inout) :: nod_grp_l
!
      integer(kind = kint), allocatable :: istack_new_sf_smp(:,:)
      integer(kind = kint), allocatable :: istack_new_ele_smp(:,:)
      integer(kind = kint), allocatable :: iflag_nod(:)
!
!
      nod_grp_l%num_grp = nod_grp_q%num_grp
      call alloc_group_num(nod_grp_l)
      allocate(istack_new_sf_smp(0:np_smp,nod_grp_l%num_grp))
      allocate(istack_new_ele_smp(0:np_smp,nod_grp_l%num_grp))
      allocate(iflag_nod(mesh_q%node%numnod))
!
!$omp parallel workshare
      nod_grp_l%grp_name(1:nod_grp_l%num_grp)                           &
     &      = nod_grp_q%grp_name(1:nod_grp_l%num_grp)
!$omp end parallel workshare
!
      call count_node_group_item_q2l                                    &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf, nod_grp_q,              &
     &    nod_grp_l%num_grp, nod_grp_l%nitem_grp,                       &
     &    istack_new_sf_smp, istack_new_ele_smp,                        &
     &    nod_grp_l%istack_grp, nod_grp_l%num_item, iflag_nod)
!
      call alloc_group_item(nod_grp_l)
      call set_node_group_item_q2l                                      &
     &   (mesh_q%node, mesh_q%ele, mesh_q%surf, nod_grp_q, q_to_l,      &
     &    nod_grp_l%num_grp, istack_new_sf_smp, istack_new_ele_smp,     &
     &    nod_grp_l%istack_grp, nod_grp_l%num_item,                     &
     &    nod_grp_l%item_grp, iflag_nod)
!
      deallocate(iflag_nod)
      deallocate(istack_new_ele_smp, istack_new_sf_smp)
!
      end subroutine s_const_node_group_item_q2l
!
!-----------------------------------------------------------------------
!
      subroutine s_const_node_group_item_l2q                            &
     &         (mesh_l, nod_grp_l, l_to_q, nod_grp_q)
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(group_data), intent(in) :: nod_grp_l
      type(linear_to_quad_list), intent(in) :: l_to_q
!
      type(group_data), intent(inout) :: nod_grp_q
!
      integer(kind = kint), allocatable :: istack_new_ed_smp(:,:)
      integer(kind = kint), allocatable :: iflag_nod(:)
!
!
      nod_grp_q%num_grp = nod_grp_l%num_grp
      call alloc_group_num(nod_grp_q)
      allocate(istack_new_ed_smp(0:np_smp,nod_grp_q%num_grp))
      allocate(iflag_nod(mesh_l%node%numnod))
!
!$omp parallel workshare
      nod_grp_q%grp_name(1:nod_grp_q%num_grp)                           &
     &      = nod_grp_l%grp_name(1:nod_grp_q%num_grp)
!$omp end parallel workshare
!
      call count_node_group_item_l2q                                    &
     &   (mesh_l%node, mesh_l%edge, nod_grp_l,                          &
     &    nod_grp_q%num_grp, nod_grp_q%nitem_grp, istack_new_ed_smp,    &
     &    nod_grp_q%istack_grp, nod_grp_q%num_item, iflag_nod)
!
      call alloc_group_item(nod_grp_q)
      call set_node_group_item_l2q                                      &
     &   (mesh_l%node, mesh_l%edge, nod_grp_l, l_to_q,                  &
     &    nod_grp_q%num_grp, istack_new_ed_smp, nod_grp_q%istack_grp,   &
     &    nod_grp_q%num_item, nod_grp_q%item_grp, iflag_nod)
!
      deallocate(iflag_nod)
      deallocate(istack_new_ed_smp)
!
      end subroutine s_const_node_group_item_l2q
!
!-----------------------------------------------------------------------
!
      subroutine s_const_node_group_item_l2lag                          &
     &         (mesh_l, nod_grp_l, l_to_lag, nod_grp_q)
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(group_data), intent(in) :: nod_grp_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      type(group_data), intent(inout) :: nod_grp_q
!
      integer(kind = kint), allocatable :: istack_new_ed_smp(:,:)
      integer(kind = kint), allocatable :: istack_new_sf_smp(:,:)
      integer(kind = kint), allocatable :: istack_new_ele_smp(:,:)
      integer(kind = kint), allocatable :: iflag_nod(:)
!
!
      nod_grp_q%num_grp = nod_grp_l%num_grp
      call alloc_group_num(nod_grp_q)
      allocate(istack_new_ed_smp(0:np_smp,nod_grp_q%num_grp))
      allocate(istack_new_sf_smp(0:np_smp,nod_grp_q%num_grp))
      allocate(istack_new_ele_smp(0:np_smp,nod_grp_q%num_grp))
      allocate(iflag_nod(mesh_l%node%numnod))
!
!$omp parallel workshare
      nod_grp_q%grp_name(1:nod_grp_q%num_grp)                           &
     &      = nod_grp_l%grp_name(1:nod_grp_q%num_grp)
!$omp end parallel workshare
!
      call count_node_group_item_l2lag                                  &
     &   (mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge, nod_grp_l, &
     &    nod_grp_q%num_grp, nod_grp_q%nitem_grp,                       &
     &    istack_new_ed_smp, istack_new_sf_smp, istack_new_ele_smp,     &
     &    nod_grp_q%istack_grp, nod_grp_q%num_item, iflag_nod)
!
      call alloc_group_item(nod_grp_q)
      call set_node_group_item_l2lag                                    &
     &   (mesh_l%node, mesh_l%ele, mesh_l%surf, mesh_l%edge, nod_grp_l, &
     &    l_to_lag, nod_grp_q%num_grp, istack_new_ed_smp,               &
     &    istack_new_sf_smp, istack_new_ele_smp, nod_grp_q%istack_grp,  &
     &    nod_grp_q%num_item, nod_grp_q%item_grp, iflag_nod)
!
      deallocate(iflag_nod)
      deallocate(istack_new_ele_smp, istack_new_sf_smp)
      deallocate(istack_new_ed_smp)
!
      end subroutine s_const_node_group_item_l2lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_node_group_item_q2l                              &
     &         (node_q, ele_q, surf_q, nod_grp_q, num_nod_grp,          &
     &          nitem_nod_grp, istack_new_sf_smp, istack_new_ele_smp,   &
     &          istack_nod_grp, ntot_nod_grp, iflag_nod)
!
      type(node_data), intent(in) :: node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
      type(group_data), intent(in) :: nod_grp_q
!
      integer(kind = kint), intent(in) :: num_nod_grp
!
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_new_sf_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_new_ele_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(inout) :: ntot_nod_grp
      integer(kind = kint), intent(inout) :: nitem_nod_grp(num_nod_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_nod_grp(0:num_nod_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node_q%numnod)
!
      integer(kind = kint) :: ip, igrp
!
!
      do igrp = 1, num_nod_grp
        call mark_node_group_for_ele                                    &
     &     (igrp, nod_grp_q, node_q%numnod, iflag_nod)
!
        call each_node_group_num_from_ele                               &
     &     (node_q%numnod, surf_q%numsurf, surf_q%nnod_4_surf,          &
     &      surf_q%istack_surf_smp, surf_q%ie_surf, iflag_nod,          &
     &      istack_new_sf_smp(0,igrp))
!
        call each_node_group_num_from_ele                               &
     &     (node_q%numnod, ele_q%numele, ele_q%nnod_4_ele,              &
     &      ele_q%istack_ele_smp, ele_q%ie, iflag_nod,                  &
     &      istack_new_ele_smp(0,igrp))
      end do
!
      istack_nod_grp(0) = 0
      do igrp = 1, num_nod_grp
        istack_new_sf_smp(0,igrp) = istack_nod_grp(igrp-1)              &
     &                             + nitem_nod_grp(igrp)
        do ip = 1, np_smp
          istack_new_sf_smp(ip,igrp) = istack_new_sf_smp(ip,igrp)       &
     &                              + istack_new_sf_smp(ip-1,igrp)
        end do
        istack_new_ele_smp(0,igrp) = istack_new_sf_smp(np_smp,igrp)
        do ip = 1, np_smp
          istack_new_ele_smp(ip,igrp) = istack_new_ele_smp(ip,igrp)     &
     &                              + istack_new_ele_smp(ip-1,igrp)
        end do
        istack_nod_grp(igrp) = istack_new_ele_smp(np_smp,igrp)
      end do
      ntot_nod_grp = istack_nod_grp(num_nod_grp)
!
      end subroutine count_node_group_item_q2l
!
!-----------------------------------------------------------------------
!
      subroutine set_node_group_item_q2l                                &
     &         (node_q, ele_q, surf_q, nod_grp_q, q_to_l,               &
     &          num_nod_grp, istack_new_sf_smp, istack_new_ele_smp,     &
     &          istack_nod_grp, ntot_nod_grp, item_nod_grp, iflag_nod)
!
      type(node_data), intent(in) :: node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
      type(group_data), intent(in) :: nod_grp_q
      type(quad_to_linear_list), intent(in) :: q_to_l
!
      integer(kind = kint), intent(in) :: num_nod_grp
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: istack_nod_grp(0:num_nod_grp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_new_sf_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_new_ele_smp(0:np_smp,num_nod_grp)
!
      integer(kind = kint), intent(inout) :: item_nod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node_q%numnod)
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, num_nod_grp
        call mark_node_group_item_for_ele(igrp, nod_grp_q,              &
     &      node_q%numnod, q_to_l%inod_quad_to_linear, ntot_nod_grp,    &
     &      istack_nod_grp(igrp-1), item_nod_grp, iflag_nod)
!
        call each_node_group_item_from_ele                              &
     &     (node_q%numnod, surf_q%numsurf, surf_q%nnod_4_surf,          &
     &      surf_q%istack_surf_smp, surf_q%ie_surf,                     &
     &      q_to_l%isurf_quad_to_linear, istack_new_sf_smp(0,igrp),     &
     &      iflag_nod, ntot_nod_grp, item_nod_grp)
!
        call each_node_group_item_from_ele                              &
     &     (node_q%numnod, ele_q%numele, ele_q%nnod_4_ele,              &
     &      ele_q%istack_ele_smp, ele_q%ie,                             &
     &      q_to_l%iele_quad_to_linear, istack_new_ele_smp(0,igrp),     &
     &      iflag_nod, ntot_nod_grp, item_nod_grp)
      end do
!
      end subroutine set_node_group_item_q2l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_node_group_item_l2q(node_l, edge_l, nod_grp_l,   &
     &          num_nod_grp, nitem_nod_grp, istack_new_ed_smp,          &
     &          istack_nod_grp, ntot_nod_grp, iflag_nod)
!
      type(node_data), intent(in) ::  node_l
      type(edge_data), intent(in) ::  edge_l
      type(group_data), intent(in) :: nod_grp_l
!
      integer(kind = kint), intent(in) :: num_nod_grp
!
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_new_ed_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(inout) :: ntot_nod_grp
      integer(kind = kint), intent(inout) :: nitem_nod_grp(num_nod_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_nod_grp(0:num_nod_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node_l%numnod)
!
      integer(kind = kint) :: ip, igrp
!
!
      do igrp = 1, num_nod_grp
        call mark_node_group_for_ele                                    &
     &     (igrp, nod_grp_l, node_l%numnod, iflag_nod)
        call each_node_group_num_from_ele                               &
     &     (node_l%numnod, edge_l%numedge, edge_l%nnod_4_edge,          &
     &      edge_l%istack_edge_smp, edge_l%ie_edge, iflag_nod,          &
     &      istack_new_ed_smp(0,igrp))
      end do
!
      istack_nod_grp(0) = 0
      do igrp = 1, num_nod_grp
        istack_new_ed_smp(0,igrp) = istack_nod_grp(igrp-1)              &
     &                             + nitem_nod_grp(igrp)
        do ip = 1, np_smp
          istack_new_ed_smp(ip,igrp) = istack_new_ed_smp(ip,igrp)       &
     &                              + istack_new_ed_smp(ip-1,igrp)
        end do
        istack_nod_grp(igrp) = istack_new_ed_smp(np_smp,igrp)
      end do
      ntot_nod_grp = istack_nod_grp(num_nod_grp)
!
      end subroutine count_node_group_item_l2q
!
!-----------------------------------------------------------------------
!
      subroutine set_node_group_item_l2q                                &
     &         (node_l, edge_l, nod_grp_l, l_to_q,                      &
     &          num_nod_grp, istack_new_ed_smp, istack_nod_grp,         &
     &          ntot_nod_grp, item_nod_grp, iflag_nod)
!
      use t_linear_to_quad_list
!
      type(node_data), intent(in) :: node_l
      type(edge_data), intent(in) ::    edge_l
      type(group_data), intent(in) :: nod_grp_l
      type(linear_to_quad_list), intent(in) :: l_to_q
!
      integer(kind = kint), intent(in) :: num_nod_grp
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: istack_nod_grp(0:num_nod_grp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_new_ed_smp(0:np_smp,num_nod_grp)
!
      integer(kind = kint), intent(inout) :: item_nod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node_l%numnod)
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, num_nod_grp
        call mark_node_group_item_for_ele(igrp, nod_grp_l,              &
     &      node_l%numnod, l_to_q%inod_linear_to_quad, ntot_nod_grp,    &
     &      istack_nod_grp(igrp-1), item_nod_grp, iflag_nod)
!
        call each_node_group_item_from_ele                              &
     &     (node_l%numnod, edge_l%numedge, edge_l%nnod_4_edge,          &
     &      edge_l%istack_edge_smp, edge_l%ie_edge,                     &
     &      l_to_q%iedge_linear_to_quad, istack_new_ed_smp(0,igrp),     &
     &      iflag_nod, ntot_nod_grp, item_nod_grp)
      end do
!
      end subroutine set_node_group_item_l2q
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_node_group_item_l2lag                            &
     &         (node_l, ele_l, surf_l, edge_l, nod_grp_l, num_nod_grp,  &
     &          nitem_nod_grp, istack_new_ed_smp, istack_new_sf_smp,    &
     &          istack_new_ele_smp, istack_nod_grp, ntot_nod_grp,       &
     &          iflag_nod)
!
      type(node_data), intent(in) :: node_l
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) ::    edge_l
      type(group_data), intent(in) :: nod_grp_l
!
      integer(kind = kint), intent(in) :: num_nod_grp
!
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_new_ed_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_new_sf_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_new_ele_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(inout) :: ntot_nod_grp
      integer(kind = kint), intent(inout) :: nitem_nod_grp(num_nod_grp)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_nod_grp(0:num_nod_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node_l%numnod)
!
      integer(kind = kint) :: ip, igrp
!
!
      do igrp = 1, num_nod_grp
        call mark_node_group_for_ele                                    &
     &     (igrp, nod_grp_l, node_l%numnod, iflag_nod)
!
        call each_node_group_num_from_ele                               &
     &     (node_l%numnod, edge_l%numedge, edge_l%nnod_4_edge,          &
     &      edge_l%istack_edge_smp, edge_l%ie_edge, iflag_nod,          &
     &      istack_new_ed_smp(0,igrp))
        call each_node_group_num_from_ele                               &
     &     (node_l%numnod, surf_l%numsurf, surf_l%nnod_4_surf,          &
     &      surf_l%istack_surf_smp, surf_l%ie_surf, iflag_nod,          &
     &      istack_new_sf_smp(0,igrp))
        call each_node_group_num_from_ele                               &
     &     (node_l%numnod, ele_l%numele, ele_l%nnod_4_ele,              &
     &      ele_l%istack_ele_smp, ele_l%ie, iflag_nod,                  &
     &      istack_new_ele_smp(0,igrp))
      end do
!
      istack_nod_grp(0) = 0
      do igrp = 1, num_nod_grp
        istack_new_ed_smp(0,igrp) = istack_nod_grp(igrp-1)              &
     &                             + nitem_nod_grp(igrp)
        do ip = 1, np_smp
          istack_new_ed_smp(ip,igrp) = istack_new_ed_smp(ip,igrp)       &
     &                              + istack_new_ed_smp(ip-1,igrp)
        end do
        istack_new_sf_smp(0,igrp) = istack_new_ed_smp(np_smp,igrp)
        do ip = 1, np_smp
          istack_new_sf_smp(ip,igrp) = istack_new_sf_smp(ip,igrp)       &
     &                              + istack_new_sf_smp(ip-1,igrp)
        end do
        istack_new_ele_smp(0,igrp) = istack_new_sf_smp(np_smp,igrp)
        do ip = 1, np_smp
          istack_new_ele_smp(ip,igrp) = istack_new_ele_smp(ip,igrp)     &
     &                              + istack_new_ele_smp(ip-1,igrp)
        end do
        istack_nod_grp(igrp) = istack_new_ele_smp(np_smp,igrp)
      end do
      ntot_nod_grp = istack_nod_grp(num_nod_grp)
!
      end subroutine count_node_group_item_l2lag
!
!-----------------------------------------------------------------------
!
      subroutine set_node_group_item_l2lag                              &
     &         (node_l, ele_l, surf_l, edge_l, nod_grp_l, l_to_lag,     &
     &          num_nod_grp, istack_new_ed_smp, istack_new_sf_smp,      &
     &          istack_new_ele_smp, istack_nod_grp, ntot_nod_grp,       &
     &          item_nod_grp, iflag_nod)
!
      use t_linear_to_lag_list
!
      type(node_data), intent(in) :: node_l
      type(element_data), intent(in) :: ele_l
      type(surface_data), intent(in) :: surf_l
      type(edge_data), intent(in) ::    edge_l
      type(group_data), intent(in) :: nod_grp_l
      type(linear_to_lag_list), intent(in) :: l_to_lag
!
      integer(kind = kint), intent(in) :: num_nod_grp
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: istack_nod_grp(0:num_nod_grp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_new_ed_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_new_sf_smp(0:np_smp,num_nod_grp)
      integer(kind = kint), intent(in)                                  &
     &              :: istack_new_ele_smp(0:np_smp,num_nod_grp)
!
      integer(kind = kint), intent(inout) :: item_nod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(node_l%numnod)
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, num_nod_grp
        call mark_node_group_item_for_ele(igrp, nod_grp_l,              &
     &      node_l%numnod, l_to_lag%inod_linear_to_lag, ntot_nod_grp,   &
     &      istack_nod_grp(igrp-1), item_nod_grp, iflag_nod)
!
        call each_node_group_item_from_ele                              &
     &     (node_l%numnod, edge_l%numedge, edge_l%nnod_4_edge,          &
     &      edge_l%istack_edge_smp, edge_l%ie_edge,                     &
     &      l_to_lag%iedge_linear_to_lag, istack_new_ed_smp(0,igrp),    &
     &      iflag_nod, ntot_nod_grp, item_nod_grp)
!
        call each_node_group_item_from_ele                              &
     &     (node_l%numnod, surf_l%numsurf, surf_l%nnod_4_surf,          &
     &      surf_l%istack_surf_smp, surf_l%ie_surf,                     &
     &      l_to_lag%isurf_linear_to_lag, istack_new_sf_smp(0,igrp),    &
     &      iflag_nod, ntot_nod_grp, item_nod_grp)
!
        call each_node_group_item_from_ele                              &
     &     (node_l%numnod, ele_l%numele, ele_l%nnod_4_ele,              &
     &      ele_l%istack_ele_smp, ele_l%ie,                             &
     &      l_to_lag%iele_linear_to_lag, istack_new_ele_smp(0,igrp),    &
     &      iflag_nod, ntot_nod_grp, item_nod_grp)
      end do
!
      end subroutine set_node_group_item_l2lag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mark_node_group_for_ele                                &
     &         (igrp, nod_grp_l, numnod, iflag_nod)
!
      type(group_data), intent(in) :: nod_grp_l
      integer(kind = kint), intent(in) :: igrp, numnod
!
      integer(kind = kint), intent(inout) :: iflag_nod(numnod)
!
      integer(kind = kint) :: ist, num, inum, inod
!
!
!$omp parallel workshare
        iflag_nod(1:numnod) = 0
!$omp end parallel workshare
!
        ist = nod_grp_l%istack_grp(igrp-1)
        num = nod_grp_l%istack_grp(igrp) - ist
!$omp parallel do private(inum,inod)
        do inum = 1, num
          inod = nod_grp_l%item_grp(inum+ist)
          iflag_nod(inod) = 1
        end do
!$omp end parallel do
!
      end subroutine mark_node_group_for_ele
!
!-----------------------------------------------------------------------
!
      subroutine mark_node_group_item_for_ele                           &
     &         (igrp, nod_grp_l, numnod, idx_old_to_newnod,             &
     &          ntot_nod_grp, istart_nod_grp, item_nod_grp, iflag_nod)
!
      type(group_data), intent(in) :: nod_grp_l
      integer(kind = kint), intent(in) :: igrp, numnod
      integer(kind = kint), intent(in) :: idx_old_to_newnod(numnod)
!
      integer(kind = kint), intent(in) :: ntot_nod_grp, istart_nod_grp
!
      integer(kind = kint), intent(inout) :: item_nod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iflag_nod(numnod)
!
      integer(kind = kint) :: ist, num, inum, inod
!
!
!$omp parallel workshare
        iflag_nod(1:numnod) = 0
!$omp end parallel workshare
!
        ist = nod_grp_l%istack_grp(igrp-1)
        num = nod_grp_l%istack_grp(igrp) - ist
!$omp parallel do private(inum,inod)
        do inum = 1, num
          inod = nod_grp_l%item_grp(inum+ist)
          iflag_nod(inod) = 1
          item_nod_grp(inum+istart_nod_grp) = idx_old_to_newnod(inod)
        end do
!$omp end parallel do
!
      end subroutine mark_node_group_item_for_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine each_node_group_num_from_ele                           &
     &         (numnod, numele, nnod_4_ele, istack_ele_smp, ie,         &
     &          iflag_nod, istack_new_ele_smp)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: istack_ele_smp(0:np_smp)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: iflag_nod(numnod)
!
      integer(kind = kint), intent(inout) :: istack_new_ele_smp(0:np_smp)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: inod, iele, k1, iflag, icou
!
!
        istack_new_ele_smp(0) = 0
!$omp parallel do private(ip,ist,ied,iele,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = 0
          ist = istack_ele_smp(ip-1)
          ied = istack_ele_smp(ip) + 1
          do iele = 1, numele
            iflag = 0
            do k1 = 1, nnod_4_ele
              inod = ie(iele,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. nnod_4_ele) icou = icou + 1
          end do
          istack_new_ele_smp(ip) = icou
        end do
!$omp end parallel do
!
      end subroutine each_node_group_num_from_ele
!
!-----------------------------------------------------------------------
!
      subroutine each_node_group_item_from_ele                          &
     &         (numnod, numele, nnod_4_ele, istack_ele_smp, ie,         &
     &          idx_old_to_newnod, istack_new_ele_smp, iflag_nod,       &
     &          ntot_nod_grp,  item_nod_grp)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: istack_ele_smp(0:np_smp)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: idx_old_to_newnod(numele)
!
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: istack_new_ele_smp(0:np_smp)
      integer(kind = kint), intent(in) :: iflag_nod(numnod)
!
      integer(kind = kint), intent(inout) :: item_nod_grp(ntot_nod_grp)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: inod, iele, k1, iflag, icou
!
!
!$omp parallel do private(ip,ist,ied,iele,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = istack_new_ele_smp(ip-1)
          ist = istack_ele_smp(ip-1)
          ied = istack_ele_smp(ip) + 1
          do iele = 1, numele
            iflag = 0
            do k1 = 1, nnod_4_ele
              inod = ie(iele,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. nnod_4_ele) then
              icou = icou + 1
              item_nod_grp(icou) = idx_old_to_newnod(iele)
            end if
          end do
        end do
!$omp end parallel do
!
      end subroutine each_node_group_item_from_ele
!
!-----------------------------------------------------------------------
!
      end module const_node_group_item_q2l
