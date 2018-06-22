!>@file   set_internal_mesh_data.f90
!!       module set_internal_mesh_data
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in June, 2018
!
!> @brief Construct mesh data with internal node and element only
!!
!!@verbatim
!!      subroutine find_internal_element(internal_node, numele, ie,     &
!!     &          nele_new, iele_to_new, iele_to_org)
!!
!!      subroutine copy_internal_node_position(my_rank, node, new_node)
!!        type(node_data), intent(in) :: node
!!        type(node_data), intent(inout) :: new_node
!!      subroutine set_internal_element_connent                         &
!!     &         (my_rank, node, ele, dbl_nod, iele_to_org, new_ele)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(parallel_double_numbering), intent(in) :: dbl_nod
!!        type(element_data), intent(inout) :: new_ele
!!
!!      subroutine count_internal_node_grp                              &
!!     &         (internal_node, nod_grp, new_nod_grp)
!!      subroutine set_internal_node_grp                                &
!!     &         (nshift, internal_node, nod_grp, new_nod_grp)
!!        type(group_data), intent(in) :: nod_grp
!!        type(group_data), intent(inout) :: new_nod_grp
!!      subroutine count_internal_element_grp                           &
!!     &         (nele, iele_to_new, ele_grp, new_ele_grp)
!!      subroutine set_internal_element_grp                             &
!!     &          (nshift, nele, iele_to_new, ele_grp, new_ele_grp)
!!        type(group_data), intent(in) :: ele_grp
!!        type(group_data), intent(inout) :: new_ele_grp
!!      subroutine count_internal_surface_grp                           &
!!     &         (nele, iele_to_new, sf_grp, new_sf_grp)
!!      subroutine set_internal_surface_grp                             &
!!     &         (nele, iele_to_new, sf_grp, new_sf_grp)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_group_data), intent(inout) :: new_sf_grp
!!@endverbatim
!
      module set_internal_mesh_data
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_group_data
      use t_para_double_numbering
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine find_internal_element(internal_node, numele, ie,       &
     &          nele_new, iele_to_new, iele_to_org)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: numele
      integer(kind=kint), intent(in)  :: ie(numele,1)
!
!
      integer(kind=kint), intent(inout) :: nele_new
      integer(kind=kint), intent(inout)  :: iele_to_new(numele)
      integer(kind=kint), intent(inout)  :: iele_to_org(numele)
!
      integer(kind = kint) :: iele, icou
!
!
!$omp parallel workshare
      iele_to_new(1:numele) = 0
      iele_to_org(1:numele) = 0
!$omp end parallel workshare
!
      icou = 0
      do iele = 1, numele
        if(ie(iele,1) .le. internal_node) then
          icou = icou + 1
          iele_to_new(iele) = icou
          iele_to_org(icou) = iele
        end if
      end do
      nele_new = icou
!
      end subroutine find_internal_element
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_internal_node_position(my_rank, node, new_node)
!
      integer(kind=kint), intent(in)  :: my_rank
      type(node_data), intent(in) :: node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private(inod)
      do inod = 1, node%internal_node
        new_node%inod_global(inod)                                     &
     &        = inod + node%istack_internod(my_rank)
        new_node%xx(inod,1) = node%xx(inod,1)
        new_node%xx(inod,2) = node%xx(inod,2)
        new_node%xx(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_internal_node_position
!
!-----------------------------------------------------------------------
!
      subroutine set_internal_element_connent                           &
     &         (my_rank, node, ele, dbl_nod, iele_to_org, new_ele)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(parallel_double_numbering), intent(in) :: dbl_nod
      integer(kind=kint), intent(in)  :: my_rank
      integer(kind=kint), intent(in)  :: iele_to_org(ele%numele)
!
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: inum, iele, k1, inod, irank
!
!
!$omp parallel do private(inum,iele)
      do inum = 1, new_ele%numele
        iele = iele_to_org(inum)
        new_ele%iele_global(inum) = inum + ele%istack_interele(my_rank)
        new_ele%elmtyp(inum) =      ele%elmtyp(iele)
        new_ele%nodelm(inum) =      ele%nodelm(iele)
      end do
!$omp end parallel do
!
      do k1 = 1, ele%nnod_4_ele
!$omp parallel do private(inum,iele,inod,irank)
        do inum = 1, new_ele%numele
          iele = iele_to_org(inum)
          inod = ele%ie(iele,k1)
          irank = dbl_nod%irank_home(inod)
          new_ele%ie(inum,k1) = dbl_nod%inod_local(inod)                &
     &                         + int(node%istack_internod(irank))
        end do
!$omp end parallel do
      end do
!
      end subroutine set_internal_element_connent
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_internal_node_grp                                &
     &         (internal_node, nod_grp, new_nod_grp)
!
      integer(kind=kint), intent(in) :: internal_node
      type(group_data), intent(in) :: nod_grp
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: icou, inum, igrp, ist, ied
!
!
      do igrp = 1, nod_grp%num_grp
        ist = nod_grp%istack_grp(igrp-1) + 1
        ied = nod_grp%istack_grp(igrp)
        icou = 0
        do inum = ist, ied
          if(nod_grp%item_grp(inum) .le. internal_node) icou = icou + 1
        end do
        new_nod_grp%istack_grp(igrp)                                    &
     &      = new_nod_grp%istack_grp(igrp-1) + icou
        new_nod_grp%grp_name(igrp) = nod_grp%grp_name(igrp)
      end do
      new_nod_grp%num_item = new_nod_grp%istack_grp(nod_grp%num_grp)
!
      end subroutine count_internal_node_grp
!
!-----------------------------------------------------------------------
!
      subroutine set_internal_node_grp                                  &
     &         (nshift, internal_node, nod_grp, new_nod_grp)
!
      integer(kind=kint), intent(in) :: internal_node
      integer(kind=kint), intent(in) :: nshift
      type(group_data), intent(in) :: nod_grp
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: icou, inum, igrp, ist, ied
!
!
      do igrp = 1, nod_grp%num_grp
        ist = nod_grp%istack_grp(igrp-1) + 1
        ied = nod_grp%istack_grp(igrp)
        icou = new_nod_grp%istack_grp(igrp-1)
        do inum = ist, ied
          if(nod_grp%item_grp(inum) .le. internal_node) then
            icou = icou + 1
            new_nod_grp%item_grp(icou)                                  &
     &           = nod_grp%item_grp(inum) + nshift
          end if
        end do
      end do
!
      end subroutine set_internal_node_grp
!
!-----------------------------------------------------------------------
!
      subroutine count_internal_element_grp                             &
     &         (nele, iele_to_new, ele_grp, new_ele_grp)
!
      integer(kind=kint), intent(in) :: nele
      integer(kind=kint), intent(in)  :: iele_to_new(nele)
      type(group_data), intent(in) :: ele_grp
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: iele, icou, inum, igrp, ist, ied
!
!
      do igrp = 1, ele_grp%num_grp
        ist = ele_grp%istack_grp(igrp-1) + 1
        ied = ele_grp%istack_grp(igrp)
        icou = 0
        do inum = ist, ied
          iele = ele_grp%item_grp(inum)
          if(iele_to_new(iele) .gt. 0) icou = icou + 1
        end do
        new_ele_grp%istack_grp(igrp)                                    &
     &          = new_ele_grp%istack_grp(igrp-1) + icou
        new_ele_grp%grp_name(igrp) = ele_grp%grp_name(igrp)
      end do
      new_ele_grp%num_item = new_ele_grp%istack_grp(ele_grp%num_grp)
!
      end subroutine count_internal_element_grp
!
!-----------------------------------------------------------------------
!
      subroutine set_internal_element_grp                               &
     &          (nshift, nele, iele_to_new, ele_grp, new_ele_grp)
!
      integer(kind=kint), intent(in) :: nshift, nele
      integer(kind=kint), intent(in)  :: iele_to_new(nele)
      type(group_data), intent(in) :: ele_grp
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: iele, icou, inum, igrp, ist, ied
!
!
      do igrp = 1, ele_grp%num_grp
        ist = ele_grp%istack_grp(igrp-1) + 1
        ied = ele_grp%istack_grp(igrp)
        icou = new_ele_grp%istack_grp(igrp-1)
        do inum = ist, ied
          iele = ele_grp%item_grp(inum)
          if(iele_to_new(iele) .gt. 0) then
            icou = icou + 1
            new_ele_grp%item_grp(icou) = iele_to_new(iele) + nshift
          end if
        end do
      end do
!
      end subroutine set_internal_element_grp
!
!-----------------------------------------------------------------------
!
      subroutine count_internal_surface_grp                             &
     &         (nele, iele_to_new, sf_grp, new_sf_grp)
!
      integer(kind=kint), intent(in) :: nele
      integer(kind=kint), intent(in)  :: iele_to_new(nele)
      type(surface_group_data), intent(in) :: sf_grp
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: iele, icou, inum, igrp, ist, ied
!
!
      do igrp = 1, sf_grp%num_grp
        ist = sf_grp%istack_grp(igrp-1) + 1
        ied = sf_grp%istack_grp(igrp)
        icou = 0
        do inum = ist, ied
          iele = sf_grp%item_sf_grp(1,inum)
          if(iele_to_new(iele) .gt. 0) icou = icou + 1
        end do
        new_sf_grp%istack_grp(igrp)                                     &
     &        = new_sf_grp%istack_grp(igrp-1) + icou
        new_sf_grp%grp_name(igrp) = sf_grp%grp_name(igrp)
      end do
      new_sf_grp%num_item = new_sf_grp%istack_grp(sf_grp%num_grp)
!
      end subroutine count_internal_surface_grp
!
!-----------------------------------------------------------------------
!
      subroutine set_internal_surface_grp                               &
     &         (nele, iele_to_new, sf_grp, new_sf_grp)
!
      integer(kind=kint), intent(in) :: nele
      integer(kind=kint), intent(in)  :: iele_to_new(nele)
      type(surface_group_data), intent(in) :: sf_grp
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: iele, icou, inum, igrp, ist, ied
!
!
      do igrp = 1, sf_grp%num_grp
        ist = sf_grp%istack_grp(igrp-1) + 1
        ied = sf_grp%istack_grp(igrp)
        icou = new_sf_grp%istack_grp(igrp-1)
        do inum = ist, ied
          iele = sf_grp%item_sf_grp(1,inum)
          if(iele_to_new(iele) .gt. 0) then
            icou = icou + 1
            new_sf_grp%item_sf_grp(1,icou) = iele_to_new(iele)
            new_sf_grp%item_sf_grp(2,icou) = sf_grp%item_sf_grp(2,inum)
          end if
        end do
      end do
!
      end subroutine set_internal_surface_grp
!
!-----------------------------------------------------------------------
!
      end module set_internal_mesh_data
