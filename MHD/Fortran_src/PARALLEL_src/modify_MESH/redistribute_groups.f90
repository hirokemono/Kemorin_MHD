!>@file   redistribute_groups.f90
!!@brief  module redistribute_groups
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Re-distribute group data
!!
!!@verbatim
!!      subroutine s_redistribute_groups(org_mesh, org_groups,          &
!!     &          new_mesh, new_ele_comm, part_tbl, ele_tbl, new_groups)
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(mesh_groups), intent(in) :: org_groups
!!        type(mesh_geometry), intent(in) :: new_mesh
!!        type(communication_table), intent(in) :: new_ele_comm
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(mesh_groups), intent(inout) :: new_groups
!!@endverbatim
!
      module redistribute_groups
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_calypso_comm_table
!
      implicit none
!
      integer(kind = kint), allocatable :: iflag_org(:)
      integer(kind = kint), allocatable :: iflag_new(:)
      private :: iflag_org, iflag_new
!
      private :: allocate_group_flags, deallocate_group_flags
      private :: repartition_node_group, repartition_element_group
      private :: repartition_surface_group
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_redistribute_groups(org_mesh, org_groups,            &
     &          new_mesh, new_ele_comm, part_tbl, ele_tbl, new_groups)
!
      type(mesh_geometry), intent(in) :: org_mesh
      type(mesh_groups), intent(in) :: org_groups
      type(mesh_geometry), intent(in) :: new_mesh
      type(communication_table), intent(in) :: new_ele_comm
!
      type(calypso_comm_table), intent(in) :: part_tbl
      type(calypso_comm_table), intent(in) :: ele_tbl
!
      type(mesh_groups), intent(inout) :: new_groups
!
!
      call repartition_node_group                                       &
     &   (org_mesh%node, org_groups%nod_grp, part_tbl,                  &
     &    new_mesh%node, new_mesh%nod_comm, new_groups%nod_grp)
      call repartition_element_group                                    &
     &   (org_mesh%ele, org_groups%ele_grp, ele_tbl, new_ele_comm,      &
     &    new_mesh%ele, new_groups%ele_grp)
      call repartition_surface_group                                    &
     &   (org_mesh%ele, org_groups%surf_grp, ele_tbl, new_ele_comm,     &
     &    new_mesh%ele, new_groups%surf_grp)
!
      end subroutine s_redistribute_groups
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_group_flags(n_point, new_n_point)
!
      integer(kind = kint), intent(in) :: n_point, new_n_point
!
      allocate(iflag_org(n_point))
!$omp parallel workshare
      iflag_org(1:n_point) = 0
!$omp end parallel workshare
!
      allocate(iflag_new(new_n_point))
!$omp parallel workshare
      iflag_new(1:new_n_point) = 0
!$omp end parallel workshare
!
      end subroutine allocate_group_flags
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_group_flags
!
      deallocate(iflag_org, iflag_new)
!
      end subroutine deallocate_group_flags
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine repartition_node_group(node, nod_grp, part_tbl,        &
     &          new_node, new_comm, new_nod_grp)
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
      type(calypso_comm_table), intent(in) :: part_tbl
!
      type(node_data), intent(in) :: new_node
      type(communication_table), intent(in) :: new_comm
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: igrp, icou
!
!
      call allocate_group_flags(node%numnod, new_node%numnod)
!
      new_nod_grp%num_grp = nod_grp%num_grp
      call alloc_group_num(new_nod_grp)
!
!$omp parallel do
      do igrp = 1, nod_grp%num_grp
        new_nod_grp%grp_name(igrp) = nod_grp%grp_name(igrp)
      end do
!$omp end parallel do
!
      do igrp = 1, nod_grp%num_grp
!$omp parallel workshare
        iflag_new(1:new_node%numnod) = 0
!$omp end parallel workshare
!
        call mark_org_group_repart                                      &
     &     (igrp, node%numnod, nod_grp, iflag_org)
!
        call calypso_SR_type_int(iflag_import_item, part_tbl,           &
     &      node%numnod, new_node%internal_node,                        &
     &      iflag_org(1), iflag_new(1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, iflag_new)
!
        new_nod_grp%nitem_grp(igrp) = sum(iflag_new)
      end do
!
      call s_cal_total_and_stacks                                       &
     &   (new_nod_grp%num_grp, new_nod_grp%nitem_grp, izero,            &
     &    new_nod_grp%istack_grp, new_nod_grp%num_item)
      call alloc_group_item(new_nod_grp)
      do igrp = 1, nod_grp%num_grp
        icou = new_nod_grp%istack_grp(igrp-1)
!$omp parallel workshare
        iflag_new(1:new_node%numnod) = 0
!$omp end parallel workshare
!
        call mark_org_group_repart                                      &
     &     (igrp, node%numnod, nod_grp, iflag_org)
!
        call calypso_SR_type_int(iflag_import_item, part_tbl,           &
     &      node%numnod, new_node%internal_node,                        &
     &      iflag_org(1), iflag_new(1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, iflag_new)
!
        call set_group_item_repart                                      &
     &     (new_node%numnod, iflag_new(1), new_nod_grp, icou)
      end do
      call deallocate_group_flags
!
      end subroutine repartition_node_group
!
! ----------------------------------------------------------------------
!
      subroutine repartition_element_group(ele, ele_grp,                &
     &          ele_tbl, new_ele_comm, new_ele, new_ele_grp)
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(calypso_comm_table), intent(in) :: ele_tbl
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: new_ele_comm
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: igrp, icou
!
!
      call allocate_group_flags(ele%numele, ele_tbl%ntot_import)
!
      new_ele_grp%num_grp = ele_grp%num_grp
      call alloc_group_num(new_ele_grp)
!
!$omp parallel do
      do igrp = 1, ele_grp%num_grp
        new_ele_grp%grp_name(igrp) = ele_grp%grp_name(igrp)
      end do
!$omp end parallel do
!
      do igrp = 1, ele_grp%num_grp
!$omp parallel workshare
        iflag_new(1:new_ele%numele) = 0
!$omp end parallel workshare
!
        call mark_org_group_repart                                      &
     &     (igrp, ele%numele, ele_grp, iflag_org)
!
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import,                            &
     &      iflag_org(1), iflag_new(1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, iflag_new(1))
!
          new_ele_grp%nitem_grp(igrp) = sum(iflag_new)
      end do
!
      call s_cal_total_and_stacks                                       &
     &   (new_ele_grp%num_grp, new_ele_grp%nitem_grp, izero,            &
     &    new_ele_grp%istack_grp, new_ele_grp%num_item)
      call alloc_group_item(new_ele_grp)
!
      do igrp = 1, ele_grp%num_grp
        icou = new_ele_grp%istack_grp(igrp-1)
!$omp parallel workshare
        iflag_new(1:new_ele%numele) = 0
!$omp end parallel workshare
!
        call mark_org_group_repart                                      &
     &     (igrp, ele%numele, ele_grp, iflag_org)
!
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import,                            &
     &      iflag_org(1), iflag_new(1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, iflag_new(1))
!
        call set_group_item_repart                                      &
     &     (new_ele%numele, iflag_new(1), new_ele_grp, icou)
      end do
      call deallocate_group_flags
!
      end subroutine repartition_element_group
!
! ----------------------------------------------------------------------
!
      subroutine repartition_surface_group(ele, surf_grp,               &
     &          ele_tbl, new_ele_comm, new_ele, new_surf_grp)
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(calypso_comm_table), intent(in) :: ele_tbl
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: new_ele_comm
!
      type(surface_group_data), intent(inout) :: new_surf_grp
!
      integer(kind = kint) :: igrp, k1, icou
!
!
      call allocate_group_flags(ele%numele, ele_tbl%ntot_import)
!
      new_surf_grp%num_grp = surf_grp%num_grp
      call alloc_sf_group_num(new_surf_grp)
!
!$omp parallel workshare
      new_surf_grp%grp_name(1:surf_grp%num_grp)                         &
     &   = surf_grp%grp_name(1:surf_grp%num_grp)
!$omp end parallel workshare
!
      do igrp = 1, surf_grp%num_grp
        do k1 = 1, ele%nnod_4_ele
!$omp parallel workshare
          iflag_new(1:new_ele%numele) = 0
!$omp end parallel workshare
!
          call mark_org_surf_group_repart                               &
     &       (igrp, k1, ele%numele, surf_grp, iflag_org)
          call calypso_SR_type_int(iflag_import_item, ele_tbl,          &
     &        ele%numele, ele_tbl%ntot_import,                          &
     &        iflag_org(1), iflag_new(1))
          call SOLVER_SEND_RECV_int_type                                &
     &       (new_ele%numele, new_ele_comm, iflag_new)
          new_surf_grp%nitem_grp(igrp) = new_surf_grp%nitem_grp(igrp)   &
     &                                  + sum(iflag_new)
        end do
      end do
!
      call s_cal_total_and_stacks                                       &
     &   (new_surf_grp%num_grp, new_surf_grp%nitem_grp, izero,          &
     &    new_surf_grp%istack_grp, new_surf_grp%num_item)
      call alloc_sf_group_item(new_surf_grp)

      do igrp = 1, surf_grp%num_grp
        icou = new_surf_grp%istack_grp(igrp-1)
        do k1 = 1, ele%nnod_4_ele
!$omp parallel workshare
          iflag_new(1:new_ele%numele) = 0
!$omp end parallel workshare
!
          call mark_org_surf_group_repart                               &
     &       (igrp, k1, ele%numele, surf_grp, iflag_org)
          call calypso_SR_type_int(iflag_import_item, ele_tbl,          &
     &        ele%numele, ele_tbl%ntot_import,                          &
     &        iflag_org(1), iflag_new(1))
          call SOLVER_SEND_RECV_int_type                                &
     &       (new_ele%numele, new_ele_comm, iflag_new)
!
          call set_surf_group_item_repart                               &
     &      (k1, new_ele%numele, iflag_new(1), new_surf_grp, icou)
        end do
      end do
      call deallocate_group_flags
!
      end subroutine repartition_surface_group
!
! ----------------------------------------------------------------------
!
      end module redistribute_groups
