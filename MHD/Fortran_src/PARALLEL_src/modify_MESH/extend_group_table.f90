!> @file  extend_group_table.f90
!!      module extend_group_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Update group data for sleeve extender
!!
!!@verbatim
!!      subroutine s_extend_group_table                                 &
!!     &        (nprocs, new_comm, new_ele_comm, new_node, new_ele,     &
!!     &         org_group, new_group)
!!        type(communication_table), intent(in) :: new_comm, new_ele_comm
!!        type(node_data), intent(in) :: new_node
!!        type(element_data), intent(inout) :: new_ele
!!        type(mesh_groups), intent(in) :: org_group
!!        type(mesh_groups), intent(inout) :: new_group
!!@endverbatim
!
      module extend_group_table
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_comm_table
!
      implicit none
!
      private :: extend_node_group, extend_surf_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_extend_group_table                                   &
     &        (nprocs, new_comm, new_ele_comm, new_node, new_ele,       &
     &         org_group, new_group)
!
      use copy_mesh_structures
      use add_comm_table_in_node_grp
!
      integer(kind = kint), intent(in) :: nprocs
      type(communication_table), intent(in) :: new_comm, new_ele_comm
      type(node_data), intent(in) :: new_node
      type(element_data), intent(inout) :: new_ele
      type(mesh_groups), intent(in) :: org_group
!
      type(mesh_groups), intent(inout) :: new_group
!
!
      call extend_node_group(new_node%numnod, new_comm,                 &
     &    org_group%nod_grp, new_group%nod_grp)
!
!      call add_comm_table_in_node_group                                &
!     &   (nprocs, new_comm, org_group%nod_grp, new_group%nod_grp)
!
      call extend_node_group(new_ele%numele, new_ele_comm,              &
     &    org_group%ele_grp, new_group%ele_grp)
      call extend_surf_group(new_ele%numele, new_ele_comm,              &
     &    org_group%surf_grp, new_group%surf_grp)
!
      end subroutine s_extend_group_table
!
!  ---------------------------------------------------------------------
!
      subroutine extend_node_group                                      &
     &         (nnod, new_comm, old_grp, new_grp)
!
      use solver_SR_type
!
      integer(kind = kint) :: nnod
      type(communication_table), intent(in) :: new_comm
      type(group_data), intent(in) :: old_grp
      type(group_data), intent(inout) :: new_grp
!
      integer(kind = kint) :: iflag_node(nnod)
!
      integer(kind = kint) :: igrp, inum, inod
      integer(kind = kint) :: ist, ied, jst, num, icou
!
!
      new_grp%num_grp = old_grp%num_grp
      call allocate_grp_type_num(new_grp)
!
      if (new_grp%num_grp .gt. 0) then
        new_grp%grp_name(1:old_grp%num_grp)                             &
     &          = old_grp%grp_name(1:old_grp%num_grp)
      end if
!
      do igrp = 1, old_grp%num_grp
!$omp parallel workshare
        iflag_node(1:nnod) = 0
!$omp end parallel workshare
!
        ist = old_grp%istack_grp(igrp-1) + 1
        ied = old_grp%istack_grp(igrp)
        do inum = ist, ied
          inod = old_grp%item_grp(inum)
          iflag_node(inod) = 1
        end do
!
        call SOLVER_SEND_RECV_int_type(nnod, new_comm, iflag_node)
!
        do inum = ist, ied
          inod = old_grp%item_grp(inum)
          iflag_node(inod) = 0
        end do
!
        new_grp%istack_grp(igrp) = new_grp%istack_grp(igrp-1)           &
     &        + old_grp%istack_grp(igrp) - old_grp%istack_grp(igrp-1)
        do inum = 1, new_comm%ntot_import
          inod = new_comm%item_import(inum)
          new_grp%istack_grp(igrp) = new_grp%istack_grp(igrp)           &
     &                              + iflag_node(inod)
        end do
      end do
!
      new_grp%num_item = new_grp%istack_grp(new_grp%num_grp)
      call allocate_grp_type_item(new_grp)
!
      do igrp = 1, old_grp%num_grp
        ist = old_grp%istack_grp(igrp-1)
        num = old_grp%istack_grp(igrp) - old_grp%istack_grp(igrp-1)
!
        jst = new_grp%istack_grp(igrp-1)
        do inum = 1, num
          new_grp%item_grp(inum+jst) = old_grp%item_grp(inum+ist)
        end do
      end do
!
      do igrp = 1, old_grp%num_grp
!$omp parallel workshare
        iflag_node(1:nnod) = 0
!$omp end parallel workshare
!
        ist = old_grp%istack_grp(igrp-1) + 1
        ied = old_grp%istack_grp(igrp)
        do inum = ist, ied
          inod = old_grp%item_grp(inum)
          iflag_node(inod) = 1
        end do
!
        call SOLVER_SEND_RECV_int_type(nnod, new_comm, iflag_node)
!
        do inum = ist, ied
          inod = old_grp%item_grp(inum)
          iflag_node(inod) = 0
        end do
!
        icou = new_grp%istack_grp(igrp-1)                               &
     &        + old_grp%istack_grp(igrp) - old_grp%istack_grp(igrp-1)
        do inum = 1, new_comm%ntot_import
          inod = new_comm%item_import(inum)
          if(iflag_node(inod) .gt. 0) then
            icou = icou + 1
            new_grp%item_grp(icou) = inod
          end if
        end do
      end do
!
      end subroutine extend_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine extend_surf_group                                      &
     &         (nnod, new_comm, old_sf_grp, new_sf_grp)
!
      use solver_SR_type
!
      integer(kind = kint) :: nnod
      type(communication_table), intent(in) :: new_comm
      type(surface_group_data), intent(in) :: old_sf_grp
      type(surface_group_data), intent(inout) :: new_sf_grp
!
      integer(kind = kint) :: iflag_node(nnod)
!
      integer(kind = kint) :: igrp, inum, inod
      integer(kind = kint) :: ist, ied, jst, num, icou
!
!
      new_sf_grp%num_grp = old_sf_grp%num_grp
      call allocate_sf_grp_type_num(new_sf_grp)
!
      if (new_sf_grp%num_grp .gt. 0) then
        new_sf_grp%grp_name(1:old_sf_grp%num_grp)                       &
     &          = old_sf_grp%grp_name(1:old_sf_grp%num_grp)
      end if
!
      do igrp = 1, old_sf_grp%num_grp
!$omp parallel workshare
        iflag_node(1:nnod) = 0
!$omp end parallel workshare
!
        ist = old_sf_grp%istack_grp(igrp-1) + 1
        ied = old_sf_grp%istack_grp(igrp)
        do inum = ist, ied
          inod = old_sf_grp%item_sf_grp(1,inum)
          iflag_node(inod) = old_sf_grp%item_sf_grp(2,inum)
        end do
!
        call SOLVER_SEND_RECV_int_type(nnod, new_comm, iflag_node)
!
        do inum = ist, ied
          inod = old_sf_grp%item_sf_grp(1,inum)
          iflag_node(inod) = 0
        end do
!
        new_sf_grp%istack_grp(igrp) = new_sf_grp%istack_grp(igrp-1)     &
     &    + old_sf_grp%istack_grp(igrp) - old_sf_grp%istack_grp(igrp-1)
        do inum = 1, new_comm%ntot_import
          inod = new_comm%item_import(inum)
          if(iflag_node(inod) .gt. 0) then
            new_sf_grp%istack_grp(igrp)                                 &
     &           = new_sf_grp%istack_grp(igrp) + 1
          end if
        end do
      end do
!
      new_sf_grp%num_item = new_sf_grp%istack_grp(new_sf_grp%num_grp)
      call allocate_sf_grp_type_item(new_sf_grp)
!
      do igrp = 1, old_sf_grp%num_grp
        ist = old_sf_grp%istack_grp(igrp-1)
        num = old_sf_grp%istack_grp(igrp)                               &
     &       - old_sf_grp%istack_grp(igrp-1)
!
        jst = new_sf_grp%istack_grp(igrp-1)
        do inum = 1, num
          new_sf_grp%item_sf_grp(1,inum+jst)                            &
     &           = old_sf_grp%item_sf_grp(1,inum+ist)
          new_sf_grp%item_sf_grp(2,inum+jst)                            &
     &           = old_sf_grp%item_sf_grp(2,inum+ist)
        end do
      end do
!
      do igrp = 1, old_sf_grp%num_grp
!$omp parallel workshare
        iflag_node(1:nnod) = 0
!$omp end parallel workshare
!
        ist = old_sf_grp%istack_grp(igrp-1) + 1
        ied = old_sf_grp%istack_grp(igrp)
        do inum = ist, ied
          inod = old_sf_grp%item_sf_grp(1,inum)
          iflag_node(inod) = old_sf_grp%item_sf_grp(2,inum)
        end do
!
        call SOLVER_SEND_RECV_int_type(nnod, new_comm, iflag_node)
!
        do inum = ist, ied
          inod = old_sf_grp%item_sf_grp(1,inum)
          iflag_node(inod) = 0
        end do
!
        icou = new_sf_grp%istack_grp(igrp-1)                            &
     &    + old_sf_grp%istack_grp(igrp) - old_sf_grp%istack_grp(igrp-1)
        do inum = 1, new_comm%ntot_import
          inod = new_comm%item_import(inum)
          if(iflag_node(inod) .gt. 0) then
            icou = icou + 1
            new_sf_grp%item_sf_grp(1,icou) = inod
            new_sf_grp%item_sf_grp(2,icou) = iflag_node(inod)
          end if
        end do
      end do
!
      end subroutine extend_surf_group
!
!  ---------------------------------------------------------------------
!
      end module extend_group_table
