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
      use t_linear_to_lag_list
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
      integer(kind = kint) :: ip, igrp, ist, ied, inum
      integer(kind = kint) :: inod, iele, isurf, k1
      integer(kind = kint) :: iflag, icou
!
!
      do igrp = 1, num_nod_grp
!$omp parallel workshare
        iflag_nod(1:node_q%numnod) = 0
!$omp end parallel workshare
!
        jst = istack_nod_grp(igrp-1)
        ist = nod_grp_q%istack_grp(igrp-1)
        nitem_nod_grp(igrp) = nod_grp_q%istack_grp(igrp) - ist
!$omp parallel do
        do inum = 1, nitem_nod_grp(igrp)
          inod = nod_grp_q%item_grp(inum+ist)
          iflag_nod(inod) = 1
        end do
!$omp end parallel do
!
        istack_new_sf_smp(0,igrp) = 0
!$omp parallel do private(ip,ist,ied,isurf,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = 0
          ist = surf_q%istack_surf_smp(ip-1)
          ied = surf_q%istack_surf_smp(ip) + 1
          do isurf = ist, ied
            iflag = 0
            do k1 = 1, surf_q%nnod_4_surf
              inod = surf_q%ie_surf(isurf,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. surf_q%nnod_4_surf) icou = icou + 1
          end do
          istack_new_sf_smp(ip,igrp) = icou
        end do
!$omp end parallel do
!
        istack_new_ele_smp(0,igrp) = 0
!$omp parallel do private(ip,ist,ied,iele,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = 0
          ist = ele_q%istack_ele_smp(ip-1)
          ied = ele_q%istack_ele_smp(ip) + 1
          do iele = 1, ele_q%numele
            iflag = 0
            do k1 = 1, ele_q%nnod_4_ele
              inod = ele_q%ie(iele,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. ele_q%nnod_4_ele) icou = icou + 1
          end do
          istack_new_ele_smp(ip,igrp) = icou
        end do
!$omp end parallel do
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
      integer(kind = kint) :: ip, igrp, ist, ied, num, inum
      integer(kind = kint) :: inod, iele, isurf, k1
      integer(kind = kint) :: iflag, icou
!
!
      do igrp = 1, num_nod_grp
!$omp parallel workshare
        iflag_nod(1:node_q%numnod) = 0
!$omp end parallel workshare
!
        jst = istack_nod_grp(igrp-1)
        ist = nod_grp_q%istack_grp(igrp-1)
        num = nod_grp_q%istack_grp(igrp) - ist
!$omp parallel do
        do inum = 1, num
          inod = nod_grp_q%item_grp(inum+ist)
          iflag_nod(inod) = 1
          item_nod_grp(inum+jst) = q_to_l%inod_quad_to_linear(inod)
        end do
!$omp end parallel do
!
!$omp parallel do private(ip,ist,ied,isurf,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = istack_new_sf_smp(ip-1,igrp)
          ist = surf_q%istack_surf_smp(ip-1)
          ied = surf_q%istack_surf_smp(ip) + 1
          do isurf = ist, ied
            iflag = 0
            do k1 = 1, surf_q%nnod_4_surf
              inod = surf_q%ie_surf(isurf,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. surf_q%nnod_4_surf) then
              icou = icou + 1
              item_nod_grp(icou) = q_to_l%isurf_quad_to_linear(isurf)
            end if
          end do
        end do
!$omp end parallel do
!
!$omp parallel do private(ip,ist,ied,iele,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = istack_new_ele_smp(ip-1,igrp)
          ist = ele_q%istack_ele_smp(ip-1)
          ied = ele_q%istack_ele_smp(ip) + 1
          do iele = 1, ele_q%numele
            iflag = 0
            do k1 = 1, ele_q%nnod_4_ele
              inod = ele_q%ie(iele,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. ele_q%nnod_4_ele) then
              icou = icou + 1
              item_nod_grp(icou) = q_to_l%iele_quad_to_linear(iele)
            end if
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine set_node_group_item_q2l
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
      integer(kind = kint) :: ip, igrp, ist, ied, inum
      integer(kind = kint) :: inod, iele, isurf, iedge, k1
      integer(kind = kint) :: iflag, icou
!
!
      do igrp = 1, num_nod_grp
!$omp parallel workshare
        iflag_nod(1:node_l%numnod) = 0
!$omp end parallel workshare
!
        jst = istack_nod_grp(igrp-1)
        ist = nod_grp_l%istack_grp(igrp-1)
        nitem_nod_grp(igrp) = nod_grp_l%istack_grp(igrp) - ist
!$omp parallel do
        do inum = 1, nitem_nod_grp(igrp)
          inod = nod_grp_l%item_grp(inum+ist)
          iflag_nod(inod) = 1
        end do
!$omp end parallel do
!
        istack_new_ed_smp(0,igrp) = 0
!$omp parallel do private(ip,ist,ied,iedge,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = 0
          ist = edge_l%istack_edge_smp(ip-1)
          ied = edge_l%istack_edge_smp(ip) + 1
          do iedge = ist, ied
            iflag = 0
            do k1 = 1, edge_l%nnod_4_edge
              inod = edge_l%ie_edge(iedge,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. edge_l%nnod_4_edge) icou = icou + 1
          end do
          istack_new_ed_smp(ip,igrp) = icou
        end do
!$omp end parallel do
!
        istack_new_sf_smp(0,igrp) = 0
!$omp parallel do private(ip,ist,ied,isurf,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = 0
          ist = surf_l%istack_surf_smp(ip-1)
          ied = surf_l%istack_surf_smp(ip) + 1
          do isurf = ist, ied
            iflag = 0
            do k1 = 1, surf_l%nnod_4_surf
              inod = surf_l%ie_surf(isurf,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. surf_l%nnod_4_surf) icou = icou + 1
          end do
          istack_new_sf_smp(ip,igrp) = icou
        end do
!$omp end parallel do
!
        istack_new_ele_smp(0,igrp) = 0
!$omp parallel do private(ip,ist,ied,iele,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = 0
          ist = ele_l%istack_ele_smp(ip-1)
          ied = ele_l%istack_ele_smp(ip) + 1
          do iele = 1, ele_l%numele
            iflag = 0
            do k1 = 1, ele_l%nnod_4_ele
              inod = ele_l%ie(iele,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. ele_l%nnod_4_ele) icou = icou + 1
          end do
          istack_new_ele_smp(ip,igrp) = icou
        end do
!$omp end parallel do
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
      integer(kind = kint) :: ip, igrp, ist, ied, num, inum
      integer(kind = kint) :: inod, iele, isurf, iedge, k1
      integer(kind = kint) :: iflag, icou
!
!
      do igrp = 1, num_nod_grp
!$omp parallel workshare
        iflag_nod(1:node_l%numnod) = 0
!$omp end parallel workshare
!
        jst = istack_nod_grp(igrp-1)
        ist = nod_grp_l%istack_grp(igrp-1)
        num = nod_grp_l%istack_grp(igrp) - ist
!$omp parallel do
        do inum = 1, num
          inod = nod_grp_l%item_grp(inum+ist)
          iflag_nod(inod) = 1
          item_nod_grp(inum+jst) = l_to_lag%inod_linear_to_lag(inod)
        end do
!$omp end parallel do
!
!$omp parallel do private(ip,ist,ied,iedge,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = istack_new_ed_smp(ip-1,igrp)
          ist = edge_l%istack_edge_smp(ip-1)
          ied = edge_l%istack_edge_smp(ip) + 1
          do iedge = ist, ied
            iflag = 0
            do k1 = 1, edge_l%nnod_4_edge
              inod = edge_l%ie_edge(iedge,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. edge_l%nnod_4_edge) then
              icou = icou + 1
              item_nod_grp(icou) = l_to_lag%iedge_linear_to_lag(iedge)
            end if
          end do
        end do
!$omp end parallel do
!
!$omp parallel do private(ip,ist,ied,isurf,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = istack_new_sf_smp(ip-1,igrp)
          ist = surf_l%istack_surf_smp(ip-1)
          ied = surf_l%istack_surf_smp(ip) + 1
          do isurf = ist, ied
            iflag = 0
            do k1 = 1, surf_l%nnod_4_surf
              inod = surf_l%ie_surf(isurf,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. surf_l%nnod_4_surf) then
              icou = icou + 1
              item_nod_grp(icou) = l_to_lag%isurf_linear_to_lag(isurf)
            end if
          end do
        end do
!$omp end parallel do
!
!$omp parallel do private(ip,ist,ied,iele,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = istack_new_ele_smp(ip-1,igrp)
          ist = ele_l%istack_ele_smp(ip-1)
          ied = ele_l%istack_ele_smp(ip) + 1
          do iele = 1, ele_l%numele
            iflag = 0
            do k1 = 1, ele_l%nnod_4_ele
              inod = ele_l%ie(iele,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. ele_l%nnod_4_ele) then
              icou = icou + 1
              item_nod_grp(icou) = l_to_lag%iele_linear_to_lag(iele)
            end if
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine set_node_group_item_l2lag
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
      integer(kind = kint) :: ip, igrp, ist, ied, inum
      integer(kind = kint) :: inod, iedge, k1
      integer(kind = kint) :: iflag, icou
!
!
      do igrp = 1, num_nod_grp
!$omp parallel workshare
        iflag_nod(1:node_l%numnod) = 0
!$omp end parallel workshare
!
        jst = istack_nod_grp(igrp-1)
        ist = nod_grp_l%istack_grp(igrp-1)
        nitem_nod_grp(igrp) = nod_grp_l%istack_grp(igrp) - ist
!$omp parallel do
        do inum = 1, nitem_nod_grp(igrp)
          inod = nod_grp_l%item_grp(inum+ist)
          iflag_nod(inod) = 1
        end do
!$omp end parallel do
!
        istack_new_ed_smp(0,igrp) = 0
!$omp parallel do private(ip,ist,ied,iedge,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = 0
          ist = edge_l%istack_edge_smp(ip-1)
          ied = edge_l%istack_edge_smp(ip) + 1
          do iedge = ist, ied
            iflag = 0
            do k1 = 1, edge_l%nnod_4_edge
              inod = edge_l%ie_edge(iedge,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. edge_l%nnod_4_edge) icou = icou + 1
          end do
          istack_new_ed_smp(ip,igrp) = icou
        end do
!$omp end parallel do
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
      integer(kind = kint) :: ip, igrp, ist, ied, num, inum
      integer(kind = kint) :: inod, iedge, k1
      integer(kind = kint) :: iflag, icou
!
!
      do igrp = 1, num_nod_grp
!$omp parallel workshare
        iflag_nod(1:node_l%numnod) = 0
!$omp end parallel workshare
!
        jst = istack_nod_grp(igrp-1)
        ist = nod_grp_l%istack_grp(igrp-1)
        num = nod_grp_l%istack_grp(igrp) - ist
!$omp parallel do
        do inum = 1, num
          inod = nod_grp_l%item_grp(inum+ist)
          iflag_nod(inod) = 1
          item_nod_grp(inum+jst) = l_to_q%inod_linear_to_quad(inod)
        end do
!$omp end parallel do
!
!$omp parallel do private(ip,ist,ied,iedge,iflag,k1,inod,icou)
        do ip = 1, np_smp
          icou = istack_new_ed_smp(ip-1,igrp)
          ist = edge_l%istack_edge_smp(ip-1)
          ied = edge_l%istack_edge_smp(ip) + 1
          do iedge = ist, ied
            iflag = 0
            do k1 = 1, edge_l%nnod_4_edge
              inod = edge_l%ie_edge(iedge,k1)
              if(iflag_nod(inod) .eq. 0) exit
              iflag = iflag + iflag_nod(inod)
            end do
            if(iflag .eq. edge_l%nnod_4_edge) then
              icou = icou + 1
              item_nod_grp(icou) = l_to_q%iedge_linear_to_quad(iedge)
            end if
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine set_node_group_item_l2q
!
!-----------------------------------------------------------------------
!
      end module const_node_group_item_q2l
