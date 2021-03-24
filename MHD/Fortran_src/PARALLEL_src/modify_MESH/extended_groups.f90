!>@file   extended_groups.f90
!!@brief  module extended_groups
!!
!!@author H. Matsui
!!@date Programmed on Mar., 2021
!
!>@brief  Re-distribute group data
!!
!!@verbatim
!!      subroutine s_extended_groups(org_mesh, org_groups,              &
!!     &                             new_mesh, new_ele_comm, new_groups)
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(mesh_groups), intent(in) :: org_groups
!!        type(mesh_geometry), intent(in) :: new_mesh
!!        type(communication_table), intent(in) :: new_ele_comm
!!        type(mesh_groups), intent(inout) :: new_groups
!!@endverbatim
!
      module extended_groups
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
!
      implicit none
!
      integer(kind = kint), allocatable, private :: iflag_new(:)
!
      private :: allocate_group_flag, deallocate_group_flag
      private :: extended_node_group, extended_element_group
      private :: extended_surface_group
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_extended_groups(org_mesh, org_groups,                &
     &                             new_mesh, new_ele_comm, new_groups)
!
      type(mesh_geometry), intent(in) :: org_mesh
      type(mesh_groups), intent(in) :: org_groups
      type(mesh_geometry), intent(in) :: new_mesh
      type(communication_table), intent(in) :: new_ele_comm
!
      type(mesh_groups), intent(inout) :: new_groups
!
!
      call extended_node_group                                          &
     &   (org_mesh%node, org_groups%nod_grp,                            &
     &    new_mesh%node, new_mesh%nod_comm, new_groups%nod_grp)
      call extended_element_group(org_groups%ele_grp,                   &
     &    new_mesh%ele, new_ele_comm, new_groups%ele_grp)
      call extended_surface_group(org_groups%surf_grp,                  &
     &    new_mesh%ele, new_ele_comm, new_groups%surf_grp)
!
      end subroutine s_extended_groups
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_group_flag(new_n_point)
!
      integer(kind = kint), intent(in) :: new_n_point
!
      allocate(iflag_new(new_n_point))
!$omp parallel workshare
      iflag_new(1:new_n_point) = 0
!$omp end parallel workshare
!
      end subroutine allocate_group_flag
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_group_flag
!
      deallocate(iflag_new)
!
      end subroutine deallocate_group_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine extended_node_group(node, nod_grp,                     &
     &          new_node, new_comm, new_nod_grp)
!
      use solver_SR_type
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      type(node_data), intent(in) :: new_node
      type(communication_table), intent(in) :: new_comm
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: igrp, icou
!
!
      call allocate_group_flag(new_node%numnod)
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
        call mark_org_group_repart                                      &
     &     (igrp, node%numnod, nod_grp, iflag_new(1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, iflag_new)
        new_nod_grp%nitem_grp(igrp) = sum(iflag_new)
      end do
!
      call s_cal_total_and_stacks                                       &
     &   (new_nod_grp%num_grp, new_nod_grp%nitem_grp, izero,            &
     &    new_nod_grp%istack_grp, new_nod_grp%num_item)
      call alloc_group_item(new_nod_grp)
!
      do igrp = 1, nod_grp%num_grp
        icou = new_nod_grp%istack_grp(igrp-1)
        call mark_org_group_repart                                      &
     &     (igrp, node%numnod, nod_grp, iflag_new(1))
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, iflag_new)
        call set_group_item_repart                                      &
     &     (new_node%numnod, iflag_new(1), new_nod_grp, icou)
      end do
      call deallocate_group_flag
!
      end subroutine extended_node_group
!
! ----------------------------------------------------------------------
!
      subroutine extended_element_group                                 &
     &         (ele_grp, new_ele, new_ele_comm, new_ele_grp)
!
      use solver_SR_type
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      type(group_data), intent(in) :: ele_grp
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: new_ele_comm
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: igrp, icou
!
!
      call allocate_group_flag(new_ele%numele)
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
        call mark_org_group_repart                                      &
     &     (igrp, new_ele%numele, ele_grp, iflag_new)
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, iflag_new(1))
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
        call mark_org_group_repart                                      &
     &     (igrp, new_ele%numele, ele_grp, iflag_new)
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, iflag_new(1))
!
        call set_group_item_repart                                      &
     &     (new_ele%numele, iflag_new(1), new_ele_grp, icou)
      end do
      call deallocate_group_flag
!
      end subroutine extended_element_group
!
! ----------------------------------------------------------------------
!
      subroutine extended_surface_group                                 &
     &         (surf_grp, new_ele, new_ele_comm, new_surf_grp)
!
      use solver_SR_type
      use redistribute_group_data
      use cal_minmax_and_stacks
!
      type(surface_group_data), intent(in) :: surf_grp
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: new_ele_comm
!
      type(surface_group_data), intent(inout) :: new_surf_grp
!
      integer(kind = kint) :: igrp, k1, icou
!
!
      call allocate_group_flag(new_ele%numele)
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
        do k1 = 1, new_ele%nnod_4_ele
          call mark_org_surf_group_repart                               &
     &       (igrp, k1, new_ele%numele, surf_grp, iflag_new)
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
        do k1 = 1, new_ele%nnod_4_ele
          call mark_org_surf_group_repart                               &
     &       (igrp, k1, new_ele%numele, surf_grp, iflag_new)
          call SOLVER_SEND_RECV_int_type                                &
     &       (new_ele%numele, new_ele_comm, iflag_new)
          call set_surf_group_item_repart                               &
     &      (k1, new_ele%numele, iflag_new(1), new_surf_grp, icou)
        end do
      end do
      call deallocate_group_flag
!
      end subroutine extended_surface_group
!
! ----------------------------------------------------------------------
!
      end module extended_groups
