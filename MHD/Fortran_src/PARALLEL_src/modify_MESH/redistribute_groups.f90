!>@file   redistribute_groups.f90
!!@brief  module redistribute_groups
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Re-distribute group data
!!
!!@verbatim
!!      subroutine s_redistribute_groups(flag_new_element_comm,         &
!!     &         org_mesh, org_groups, ele_comm, new_mesh, new_ele_comm,&
!!     &         part_tbl, ele_tbl, new_groups, SR_sig, SR_i)
!!        logical, intent(in) :: flag_new_element_comm
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(mesh_groups), intent(in) :: org_groups
!!        type(communication_table), intent(in) :: ele_comm
!!        type(mesh_geometry), intent(in) :: new_mesh
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(calypso_comm_table), intent(in) :: ele_tbl
!!        type(mesh_groups), intent(inout) :: new_groups
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
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
      use t_solver_SR
      use t_solver_SR_int
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
      subroutine s_redistribute_groups(flag_new_element_comm,           &
     &         org_mesh, org_groups, ele_comm, new_mesh, new_ele_comm,  &
     &         part_tbl, ele_tbl, new_groups, SR_sig, SR_i)
!
      logical, intent(in) :: flag_new_element_comm
      type(mesh_geometry), intent(in) :: org_mesh
      type(mesh_groups), intent(in) :: org_groups
      type(communication_table), intent(in) :: ele_comm, new_ele_comm
      type(mesh_geometry), intent(in) :: new_mesh
!
      type(calypso_comm_table), intent(in) :: part_tbl
      type(calypso_comm_table), intent(in) :: ele_tbl
!
      type(mesh_groups), intent(inout) :: new_groups
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call repartition_node_group                                       &
     &   (org_mesh%node, org_groups%nod_grp, part_tbl,                  &
     &    new_mesh%node, new_mesh%nod_comm, new_groups%nod_grp,         &
     &    SR_sig, SR_i)
      call repartition_element_group(flag_new_element_comm,             &
     &    org_mesh%ele, org_groups%ele_grp, ele_comm, ele_tbl,          &
     &    new_mesh%ele, new_ele_comm, new_groups%ele_grp, SR_sig, SR_i)
      call repartition_surface_group(flag_new_element_comm,             &
     &    org_mesh%ele, org_groups%surf_grp, ele_comm, ele_tbl,         &
     &    new_mesh%ele, new_ele_comm, new_groups%surf_grp, SR_sig, SR_i)
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
     &          new_node, new_comm, new_nod_grp, SR_sig, SR_i)
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
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
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
     &      iflag_org(1), iflag_new(1), SR_sig, SR_i)
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, SR_sig, SR_i, iflag_new)
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
     &      iflag_org(1), iflag_new(1), SR_sig, SR_i)
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, SR_sig, SR_i, iflag_new)
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
      subroutine repartition_element_group(flag_new_element_comm,       &
     &          ele, ele_grp, ele_comm, ele_tbl, new_ele,               &
     &          new_ele_comm, new_ele_grp, SR_sig, SR_i)
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      logical, intent(in) :: flag_new_element_comm
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(calypso_comm_table), intent(in) :: ele_tbl
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: ele_comm, new_ele_comm
!
      type(group_data), intent(inout) :: new_ele_grp
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: igrp, icou, i, num_import_flag
!
!
      num_import_flag = max(ele_tbl%ntot_import, new_ele%numele)
      call allocate_group_flags(ele%numele, num_import_flag)
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
        iflag_new(1:num_import_flag) = 0
!$omp end parallel workshare
!
        call mark_org_group_repart                                      &
     &     (igrp, ele%numele, ele_grp, iflag_org)
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, ele_comm, SR_sig, SR_i, iflag_org(1))
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import,                            &
     &      iflag_org(1), iflag_new(1), SR_sig, SR_i)
        if(flag_new_element_comm) then
          call SOLVER_SEND_RECV_int_type                                &
     &       (ele%numele, new_ele_comm, SR_sig, SR_i, iflag_new(1))
        end if
!
        icou = 0
!$omp parallel do reduction(+:icou)
        do i = 1, new_ele%numele
          icou = icou + iflag_new(i)
        end do
!$omp end parallel do
        new_ele_grp%nitem_grp(igrp) = icou
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
        iflag_new(1:num_import_flag) = 0
!$omp end parallel workshare
!
        call mark_org_group_repart                                      &
     &     (igrp, ele%numele, ele_grp, iflag_org)
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (ele%numele, ele_comm, SR_sig, SR_i, iflag_org(1))
        call calypso_SR_type_int(iflag_import_item, ele_tbl,            &
     &      ele%numele, ele_tbl%ntot_import,                            &
     &      iflag_org(1), iflag_new(1), SR_sig, SR_i)
        if(flag_new_element_comm) then
          call SOLVER_SEND_RECV_int_type                                &
     &       (ele%numele, new_ele_comm, SR_sig, SR_i, iflag_new(1))
        end if
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
      subroutine repartition_surface_group(flag_new_element_comm,       &
     &          ele, surf_grp, ele_comm, ele_tbl, new_ele,              &
     &          new_ele_comm, new_surf_grp, SR_sig, SR_i)
!
      use calypso_SR_type
      use solver_SR_type
      use select_copy_from_recv
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      logical, intent(in) :: flag_new_element_comm
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
      type(calypso_comm_table), intent(in) :: ele_tbl
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: ele_comm, new_ele_comm
!
      type(surface_group_data), intent(inout) :: new_surf_grp
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: igrp, k1, icou, i, num_import_flag
!
!
      num_import_flag = max(ele_tbl%ntot_import, new_ele%numele)
      call allocate_group_flags(ele%numele, num_import_flag)
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
        new_surf_grp%nitem_grp(igrp) = 0
        do k1 = 1, ele%nnod_4_ele
!$omp parallel workshare
          iflag_new(1:num_import_flag) = 0
!$omp end parallel workshare
!
          call mark_org_surf_group_repart                               &
     &       (igrp, k1, ele%numele, surf_grp, iflag_org)
!
          call SOLVER_SEND_RECV_int_type                                &
     &       (ele%numele, ele_comm, SR_sig, SR_i, iflag_org)
          call calypso_SR_type_int(iflag_import_item, ele_tbl,          &
     &        ele%numele, ele_tbl%ntot_import,                          &
     &        iflag_org(1), iflag_new(1), SR_sig, SR_i)
          if(flag_new_element_comm) then
            call SOLVER_SEND_RECV_int_type                              &
     &         (ele%numele, new_ele_comm, SR_sig, SR_i, iflag_new(1))
          end if
!
          icou = 0
!$omp parallel do reduction(+:icou)
          do i = 1, new_ele%numele
            icou = icou + iflag_new(i)
          end do
!$omp end parallel do
          new_surf_grp%nitem_grp(igrp)                                  &
     &          = new_surf_grp%nitem_grp(igrp) + icou
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
          iflag_new(1:num_import_flag) = 0
!$omp end parallel workshare
!
          call mark_org_surf_group_repart                               &
     &       (igrp, k1, ele%numele, surf_grp, iflag_org)
!
          call SOLVER_SEND_RECV_int_type                                &
     &       (ele%numele, ele_comm, SR_sig, SR_i, iflag_org)
          call calypso_SR_type_int(iflag_import_item, ele_tbl,          &
     &        ele%numele, ele_tbl%ntot_import,                          &
     &        iflag_org(1), iflag_new(1), SR_sig, SR_i)
          if(flag_new_element_comm) then
            call SOLVER_SEND_RECV_int_type                              &
     &         (ele%numele, new_ele_comm, SR_sig, SR_i, iflag_new(1))
          end if
!
          call set_surf_group_item_repart                               &
     &       (k1, new_ele%numele, iflag_new(1), new_surf_grp, icou)
        end do
      end do
      call deallocate_group_flags
!
      end subroutine repartition_surface_group
!
! ----------------------------------------------------------------------
!
      end module redistribute_groups
