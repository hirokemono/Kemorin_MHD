!> @file  const_extend_nod_comm_table.f90
!!      module const_extend_nod_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine const_extended_node_position                         &
!!     &         (nod_comm, expand_nod_comm, exp_import_xx,             &
!!     &          add_nod_comm, ext_nod_trim, trim_nod_to_ext)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        type(communication_table), intent(in) :: add_nod_comm
!!        type(data_for_trim_import), intent(inout) :: ext_nod_trim
!!        type(import_extend_to_trim), intent(inout) :: trim_nod_to_ext
!!      subroutine const_extended_nod_comm_table                        &
!!     &         (org_node, expand_nod_comm, ext_nod_trim,              &
!!     &          exp_import_xx, trim_import_xx, trim_nod_to_ext,       &
!!     &          dist_4_comm, add_nod_comm)
!!        type(node_data), intent(in) :: org_node
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(data_for_trim_import), intent(in) :: ext_nod_trim
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        type(node_data_for_sleeve_ext), intent(inout) :: trim_import_xx
!!        type(import_extend_to_trim), intent(inout) :: trim_nod_to_ext
!!        type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
!!        type(communication_table), intent(inout) :: add_nod_comm
!!@endverbatim
!
      module const_extend_nod_comm_table
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_comm_table_for_each_pe
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
      use t_trim_overlapped_import
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_extended_node_position                           &
     &         (nod_comm, expand_nod_comm, exp_import_xx,               &
     &          add_nod_comm, ext_nod_trim, trim_nod_to_ext)
!
      use calypso_mpi_int
      use set_expanded_comm_table
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
      type(communication_table), intent(in) :: add_nod_comm
!
      type(data_for_trim_import), intent(inout) :: ext_nod_trim
      type(import_extend_to_trim), intent(inout) :: trim_nod_to_ext
!
      type(sort_data_for_sleeve_trim), save :: sort_nod_import
      integer(kind = kint) :: num, icou, ntot_failed_gl
!
!
      call alloc_sort_data_sleeve_ext                                   &
     &   (nprocs, expand_nod_comm%ntot_import, sort_nod_import)
      call sort_import_by_pe_and_local_id(nprocs, nod_comm,             &
     &    expand_nod_comm, exp_import_xx%irank_comm, sort_nod_import)
!
      call trim_overlapped_sleeve_ext                                   &
     &   (expand_nod_comm%ntot_import, exp_import_xx%irank_comm,        &
     &    sort_nod_import, ext_nod_trim)
      if(i_debug .gt. 0) then
        call check_overlapped_sleeve_ext                                &
     &     (nod_comm, add_nod_comm, sort_nod_import, ext_nod_trim)
      end if
!
      num = expand_nod_comm%ntot_import
      allocate(trim_nod_to_ext%idx_extend_to_trim(num))
      call find_home_import_item_by_trim                                &
     &   (nprocs, expand_nod_comm%ntot_import, sort_nod_import,         &
     &    ext_nod_trim, trim_nod_to_ext%idx_extend_to_trim, icou)
      call calypso_mpi_reduce_one_int(icou, ntot_failed_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'Missing import item in trimmed:', ntot_failed_gl
      call dealloc_sort_data_sleeve_ext(sort_nod_import)

      end subroutine const_extended_node_position
!
!  ---------------------------------------------------------------------
!
      subroutine const_extended_nod_comm_table                          &
     &         (org_node, expand_nod_comm, ext_nod_trim,                &
     &          exp_import_xx, trim_import_xx, trim_nod_to_ext,         &
     &          dist_4_comm, add_nod_comm)
!
      use calypso_mpi_int
      use reverse_SR_int
      use reverse_SR_real
!
      use cal_minmax_and_stacks
      use set_expanded_comm_table
      use trim_mesh_for_sleeve_extend
!
      type(node_data), intent(in) :: org_node
      type(communication_table), intent(in) :: expand_nod_comm
      type(data_for_trim_import), intent(in) :: ext_nod_trim
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!
      type(node_data_for_sleeve_ext), intent(inout) :: trim_import_xx
      type(import_extend_to_trim), intent(inout) :: trim_nod_to_ext
      type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
      type(communication_table), intent(inout) :: add_nod_comm
!
      integer(kind = kint) :: num
!
!
      call alloc_import_num(add_nod_comm)
      call calypso_mpi_barrier
      write(*,*) my_rank, 'count_import_item_for_extend'
      call count_import_item_for_extend                                 &
     &   (nprocs, ext_nod_trim%istack_trimmed_pe,                       &
     &    add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%num_import)
      call calypso_mpi_barrier
      write(*,*) my_rank, 's_cal_total_and_stacks'
      call s_cal_total_and_stacks                                       &
     &   (add_nod_comm%num_neib, add_nod_comm%num_import, izero,        &
     &    add_nod_comm%istack_import, add_nod_comm%ntot_import)
      call alloc_import_item(add_nod_comm)
!
      call alloc_node_data_sleeve_ext(add_nod_comm%ntot_import,         &
     &                                trim_import_xx)
      num = add_nod_comm%ntot_import
      allocate(trim_nod_to_ext%import_lc_trimmed(num))
!
      call calypso_mpi_barrier
      write(*,*) my_rank, 'set_import_item_for_extend'
      call set_import_item_for_extend                                   &
     &   (org_node, expand_nod_comm, ext_nod_trim,                      &
     &    add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%ntot_import,         &
     &    trim_nod_to_ext%import_lc_trimmed, add_nod_comm%item_import)
      call trim_imported_expand_node(add_nod_comm, ext_nod_trim,        &
     &                               exp_import_xx, trim_import_xx)
!
      call alloc_export_num(add_nod_comm)
      call calypso_mpi_barrier
      write(*,*) my_rank, 'num_items_send_recv'
      if(my_rank .eq. 5 .or. my_rank .eq. 11 .or. my_rank .eq. 1        &
     &  .or. my_rank .eq. 15 .or. my_rank .eq. 17 .eq. my_rank .eq. 21) &
     &  write(*,*) my_rank, 'add_nod_comm%id_neib', add_nod_comm%id_neib
!
      call num_items_send_recv                                          &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%num_import, add_nod_comm%num_export,             &
     &    add_nod_comm%istack_export, add_nod_comm%ntot_export)
      call alloc_export_item(add_nod_comm)
!
!
      dist_4_comm%ntot = add_nod_comm%ntot_export
      allocate(dist_4_comm%distance_in_export(dist_4_comm%ntot))
!
      call calypso_mpi_barrier
      write(*,*) my_rank, 'comm_items_send_recv'
      call comm_items_send_recv                                         &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%istack_export,       &
     &    trim_nod_to_ext%import_lc_trimmed, add_nod_comm%item_export)
      call calypso_mpi_barrier
      write(*,*) my_rank, 'real_items_send_recv'
      call real_items_send_recv                                         &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%istack_export,       &
     &    trim_import_xx%distance, dist_4_comm%distance_in_export)
!
      end subroutine const_extended_nod_comm_table
!
!  ---------------------------------------------------------------------
!
      end module const_extend_nod_comm_table
