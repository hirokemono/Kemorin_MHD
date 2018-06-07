!> @file  extend_group_table.f90
!!      module extend_group_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine s_extend_group_table                                 &
!!     &        (new_comm, new_node, org_group, new_group)
!!@endverbatim
!
      module extend_group_table
!
      use m_precision
      use m_constants
      use m_phys_constants
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_comm_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_extend_group_table                                   &
     &        (new_comm, new_node, org_group, new_group)
!
      use copy_mesh_structures
!
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(mesh_groups), intent(in) :: org_group
      type(mesh_groups), intent(inout) :: new_group
!
!
      call extend_node_group                                            &
     &   (new_comm, new_node, org_group%nod_grp, new_group%nod_grp)
!
!      call add_comm_table_in_node_group                                &
!     &   (new_comm, org_group%nod_grp, new_group%nod_grp)
!
      call copy_group_data(org_group%ele_grp, new_group%ele_grp)
      call copy_surface_group(org_group%surf_grp, new_group%surf_grp)
!
      end subroutine s_extend_group_table
!
!  ---------------------------------------------------------------------
!
      subroutine extend_node_group                                      &
     &        (new_comm, new_node, old_grp, new_grp)
!
      use solver_SR_type
!
      type(communication_table), intent(in) :: new_comm
      type(node_data), intent(in) :: new_node
      type(group_data), intent(in) :: old_grp
      type(group_data), intent(inout) :: new_grp
!
      character(len=kchara), parameter :: import_head = 'import'
      character(len=kchara), parameter :: export_head = 'export'
      integer(kind = kint) :: iflag_node(new_node%numnod)
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
        iflag_node(1:new_node%numnod) = 0
!$omp end parallel workshare
!
        ist = old_grp%istack_grp(igrp-1) + 1
        ied = old_grp%istack_grp(igrp)
        do inum = ist, ied
          inod = old_grp%item_grp(inum)
          iflag_node(inod) = 1
        end do
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, iflag_node)
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
        iflag_node(1:new_node%numnod) = 0
!$omp end parallel workshare
!
        ist = old_grp%istack_grp(igrp-1) + 1
        ied = old_grp%istack_grp(igrp)
        do inum = ist, ied
          inod = old_grp%item_grp(inum)
          iflag_node(inod) = 1
        end do
!
        call SOLVER_SEND_RECV_int_type                                  &
     &     (new_node%numnod, new_comm, iflag_node)
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
      subroutine add_comm_table_in_node_group                           &
     &         (new_comm, old_nod_grp, new_nod_grp)
!
      use set_parallel_file_name
!
      type(communication_table), intent(in) :: new_comm
      type(group_data), intent(in) :: old_nod_grp
      type(group_data), intent(inout) :: new_nod_grp
!
      character(len=kchara), parameter :: import_head = 'import'
      character(len=kchara), parameter :: export_head = 'export'
      integer(kind = kint) :: n_import(nprocs)
      integer(kind = kint) :: n_export(nprocs)
!
      integer(kind = kint) :: i, ip, igrp, inum, ist, jst
!
!
      new_nod_grp%num_grp = old_nod_grp%num_grp + 2*nprocs
      new_nod_grp%num_item = old_nod_grp%num_item                       &
     &                    + new_comm%ntot_import + new_comm%ntot_export
!
      call allocate_grp_type_num(new_nod_grp)
!
!
      if (new_nod_grp%num_grp .gt. 0) then
        new_nod_grp%grp_name(1:old_nod_grp%num_grp)                     &
     &          = old_nod_grp%grp_name(1:old_nod_grp%num_grp)
        new_nod_grp%istack_grp(0:old_nod_grp%num_grp)                   &
     &          = old_nod_grp%istack_grp(0:old_nod_grp%num_grp)
      end if
!
      do ip = 1, nprocs
        igrp = old_nod_grp%num_grp + ip
        call add_int_suffix                                             &
     &     ((ip-1), import_head, new_nod_grp%grp_name(igrp))
        igrp = old_nod_grp%num_grp + nprocs + ip
        call add_int_suffix                                             &
     &     ((ip-1), export_head, new_nod_grp%grp_name(igrp))
      end do
      n_import = 0
      n_export = 0
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i) + 1
        n_import(ip) = new_comm%istack_import(i)                        &
     &                - new_comm%istack_import(i-1)
        n_export(ip) = new_comm%istack_export(i)                        &
     &                - new_comm%istack_export(i-1)
      end do
      do ip = 1, nprocs
        igrp = old_nod_grp%num_grp + ip
        new_nod_grp%istack_grp(igrp) = new_nod_grp%istack_grp(igrp-1)   &
     &                                + n_import(ip)
      end do
      do ip = 1, nprocs
        igrp = old_nod_grp%num_grp + nprocs + ip
        new_nod_grp%istack_grp(igrp) = new_nod_grp%istack_grp(igrp-1)   &
     &                                + n_export(ip)
      end do
!
      call allocate_grp_type_item(new_nod_grp)
!
!
      if (old_nod_grp%num_item .gt. 0) then
        new_nod_grp%item_grp(1:old_nod_grp%num_item)                    &
     &          = old_nod_grp%item_grp(1:old_nod_grp%num_item)
      end if
!
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i) + 1
        ist = new_comm%istack_import(i-1)
        jst = new_nod_grp%istack_grp(old_nod_grp%num_grp+ip-1)
        do inum = 1, n_import(ip)
          new_nod_grp%item_grp(jst+inum) = new_comm%item_import(ist+inum)
        end do
!
        ist = new_comm%istack_export(i-1)
        jst = new_nod_grp%istack_grp(old_nod_grp%num_grp+nprocs+ip-1)
        do inum = 1, n_export(ip)
          new_nod_grp%item_grp(jst+inum) = new_comm%item_export(ist+inum)
        end do
      end do
!
      end subroutine add_comm_table_in_node_group
!
!  ---------------------------------------------------------------------
!
      end module extend_group_table
