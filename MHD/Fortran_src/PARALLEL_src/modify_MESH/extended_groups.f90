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
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      type(node_data), intent(in) :: new_node
      type(communication_table), intent(in) :: new_comm
!
      type(group_data), intent(inout) :: new_nod_grp
!
      integer(kind = kint) :: igrp
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
      new_nod_grp%istack_grp(0:new_nod_grp%num_grp) = 0
      call alloc_group_item(new_nod_grp)
      do igrp = 1, nod_grp%num_grp
        call mark_org_group_repart                                      &
     &     (igrp, node%numnod, nod_grp, iflag_new(1))
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, iflag_new)
!
        call count_group_item_repart                                    &
     &     (igrp, new_node%numnod, iflag_new(1), new_nod_grp)
        call append_group_item_repart                                   &
     &     (igrp, new_node%numnod, iflag_new(1), new_nod_grp)
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
!
      type(group_data), intent(in) :: ele_grp
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: new_ele_comm
!
      type(group_data), intent(inout) :: new_ele_grp
!
      integer(kind = kint) :: igrp
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
      new_ele_grp%istack_grp(0:new_ele_grp%num_grp) = 0
      call alloc_group_item(new_ele_grp)
      do igrp = 1, ele_grp%num_grp
        call mark_org_group_repart                                      &
     &     (igrp, new_ele%numele, ele_grp, iflag_new)
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, iflag_new(1))
!
        call count_group_item_repart                                    &
     &     (igrp, new_ele%numele, iflag_new(1), new_ele_grp)
        call append_group_item_repart                                   &
     &     (igrp, new_ele%numele, iflag_new(1), new_ele_grp)
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
!
      type(surface_group_data), intent(in) :: surf_grp
!
      type(element_data), intent(in) :: new_ele
      type(communication_table), intent(in) :: new_ele_comm
!
      type(surface_group_data), intent(inout) :: new_surf_grp
!
      integer(kind = kint) :: igrp
!
!
      call allocate_group_flag(new_ele%numele)
!
      new_surf_grp%num_grp = surf_grp%num_grp
      call alloc_sf_group_num(new_surf_grp)
!
!$omp parallel do
      do igrp = 1, surf_grp%num_grp
        new_surf_grp%grp_name(igrp) = surf_grp%grp_name(igrp)
      end do
!$omp end parallel do
!
      new_surf_grp%istack_grp(0:new_surf_grp%num_grp) = 0
      call alloc_sf_group_item(new_surf_grp)
!
      do igrp = 1, surf_grp%num_grp
        call mark_org_surf_group_repart                                 &
     &     (igrp, new_ele%numele, surf_grp, iflag_new)
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_ele%numele, new_ele_comm, iflag_new)
!
!        write(100+my_rank,*) igrp, new_ele%numele, 'iflag',            &
!     &        sum(iflag_new), sum(iflag_new)

        call count_surf_group_item_repart                               &
     &    (igrp, new_ele%numele, iflag_new(1), new_surf_grp)
        call append_surf_group_item_repart                              &
     &    (igrp, new_ele%numele, iflag_new(1), new_surf_grp)
      end do
      call deallocate_group_flag
!
      end subroutine extended_surface_group
!
! ----------------------------------------------------------------------
!
      end module extended_groups
