!>@file   const_same_domain_grouping.f90
!!@brief  module const_same_domain_grouping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping without changing
!!
!!@verbatim
!!      subroutine const_trans_tbl_to_same_mesh(node, part_tbl)
!!        type(node_data), intent(inout) :: node
!!        type(calypso_comm_table), intent(inout) :: part_tbl
!!      subroutine const_samedomain_grp_data(my_rank, nprocs,           &
!!     &                                     node, part_grp)
!!        integer, intent(in) :: my_rank, nprocs
!!        type(node_data), intent(in) :: node
!!        type(group_data), intent(inout) :: part_grp
!!@endverbatim
!
      module const_same_domain_grouping
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_group_data
      use t_calypso_comm_table
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: base_name = 'same_domain_'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_trans_tbl_to_same_mesh(node, part_tbl)
!
      type(node_data), intent(inout) :: node
      type(calypso_comm_table), intent(inout) :: part_tbl
!
      integer(kind = kint) :: i
!
!
      part_tbl%iflag_self_copy = 1
      part_tbl%nrank_import = 1
!
      call alloc_calypso_import_num(part_tbl)
      part_tbl%irank_import(1) = my_rank
      part_tbl%istack_import(0) = 0
      part_tbl%istack_import(1) = node%internal_node
      part_tbl%num_import(1) =    node%internal_node
      part_tbl%ntot_import =      node%internal_node
!
      part_tbl%nrank_export = 1
      part_tbl%nrank_import = 1
!
      call alloc_calypso_export_num(part_tbl)
      part_tbl%irank_export(1) = my_rank
      part_tbl%istack_export(0) = 0
      part_tbl%istack_export(1) = node%internal_node
      part_tbl%num_export(1) =    node%internal_node
      part_tbl%ntot_export =      node%internal_node
!
      call alloc_calypso_import_item(node%numnod, part_tbl)
      call alloc_calypso_export_item(part_tbl)
!
!$omp parallel do private
      do i = 1, internal_node
        part_tbl%irev_import(i) = i
        part_tbl%item_import(i) = i
        part_tbl%item_export(i) = i
      end do
!$omp parallel do private
!
      end subroutine const_trans_tbl_to_same_mesh
!
! ----------------------------------------------------------------------
!
      subroutine const_samedomain_grp_data(my_rank, nprocs,             &
     &                                     node, part_grp)
!
      use set_parallel_file_name
!
      integer, intent(in) :: my_rank, nprocs
      type(node_data), intent(in) :: node
      type(group_data), intent(inout) :: part_grp
!
      integer(kind = kint) :: i
!
      part_grp%num_grp = nprocs
      call alloc_group_num(part_grp)
!
!$omp parallel do
      do i = 0, part_grp%num_grp-1
        call add_index_after_name(i, base_name, part_grp%grp_name(i))
      end do
!$omp end parallel do
!
!$omp parallel workshare
      part_grp%istack_grp(0:my_rank) = 0
!$omp end parallel workshare
!
!$omp parallel workshare
      part_grp%istack_grp(my_rank+1:nprocs) = node%internal_node
!$omp end parallel workshare
      part_grp%num_item = part_grp%istack_grp(part_grp%num_grp)
      call alloc_group_item(part_grp)
!
!$omp parallel do
      do i = 1, node%internal_node
        part_grp%item_grp(i) = i
      end do
!$omp end parallel do
!
      end subroutine const_samedomain_grp_data
!
! ----------------------------------------------------------------------
!
      end module const_same_domain_grouping
