!> @file  const_extend_nod_comm_table.f90
!!      module const_extend_nod_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine const_trimmed_expand_import                          &
!!     &         (inod_dbl, nod_comm, expand_nod_comm, exp_import_xx,   &
!!     &          add_nod_comm, ext_nod_trim, trim_nod_to_ext)
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        type(calypso_comm_table), intent(in) :: add_nod_comm
!!        type(data_for_trim_import), intent(inout) :: ext_nod_trim
!!        type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!!        type(sort_data_for_sleeve_trim), save :: sort_nod_import
!!      subroutine const_extended_nod_comm_table(org_node, nod_comm,    &
!!     &          expand_nod_comm, ext_nod_trim, exp_import_xx,         &
!!     &          trim_import_xx, trim_nod_to_ext, add_nod_comm)
!!        type(node_data), intent(in) :: org_node
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(data_for_trim_import), intent(in) :: ext_nod_trim
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        type(node_data_for_sleeve_ext), intent(inout) :: trim_import_xx
!!        type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!!        type(dist_from_wall_in_export), intent(inout) :: dist_add
!!@endverbatim
!
      module const_extend_nod_comm_table
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_geometry_data
      use t_comm_table
      use t_calypso_comm_table
      use t_para_double_numbering
      use t_comm_table_for_each_pe
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
      use t_trim_overlapped_import
      use t_work_nod_import_extend
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_trimmed_expand_import                            &
     &         (inod_dbl, nod_comm, expand_nod_comm, exp_import_xx,     &
     &          add_nod_comm, ext_nod_trim, trim_nod_to_ext)
!
      use t_para_double_numbering
      use calypso_mpi_int
      use set_expanded_comm_table
!
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
      type(calypso_comm_table), intent(in) :: add_nod_comm
!
      type(data_for_trim_import), intent(inout) :: ext_nod_trim
      type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
!
      type(sort_data_for_sleeve_trim), save :: sort_nod_import
!
      integer(kind = kint) :: ntot_mix, icou, ntot_failed_gl
!
!
      ntot_mix = nod_comm%ntot_import + expand_nod_comm%ntot_import
      call alloc_sort_data_sleeve_ext                                   &
     &   (nprocs, ntot_mix, sort_nod_import)
      call sort_mix_import_by_pe_inod_lc(inod_dbl, nod_comm,            &
     &    expand_nod_comm, exp_import_xx%irank_comm, sort_nod_import)
!
      call trim_overlap_expanded_import                                 &
     &   (expand_nod_comm%ntot_import, exp_import_xx%irank_comm,        &
     &    sort_nod_import, ext_nod_trim)
      if(i_debug .gt. 0) then
        call check_overlapped_sleeve_ext                                &
     &     (nod_comm, add_nod_comm, sort_nod_import, ext_nod_trim)
      end if
!
      call find_home_import_item_by_trim                                &
     &   (nprocs, expand_nod_comm%ntot_import, sort_nod_import,         &
     &    ext_nod_trim, trim_nod_to_ext%idx_extend_to_trim, icou)
      call calypso_mpi_reduce_one_int(icou, ntot_failed_gl, MPI_SUM, 0)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'Missing import item in trimmed:', ntot_failed_gl
      call dealloc_sort_data_sleeve_ext(sort_nod_import)

      end subroutine const_trimmed_expand_import
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_extended_nod_comm_table(org_node, nod_comm,      &
     &          expand_nod_comm, ext_nod_trim, exp_import_xx,           &
     &          trim_import_xx, trim_nod_to_ext, add_nod_comm)
!
      use calypso_mpi_int
      use reverse_SR_int
!
      use cal_minmax_and_stacks
      use set_expanded_comm_table
      use trim_mesh_for_sleeve_extend
!
      type(node_data), intent(in) :: org_node
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(data_for_trim_import), intent(in) :: ext_nod_trim
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!
      type(node_data_for_sleeve_ext), intent(inout) :: trim_import_xx
      type(work_nod_import_extend), intent(inout) :: trim_nod_to_ext
      type(calypso_comm_table), intent(inout) :: add_nod_comm
!
      integer(kind = kint) :: num
!
!
      call count_import_item_for_extend(nod_comm, ext_nod_trim,         &
     &    add_nod_comm%nrank_import, add_nod_comm%irank_import,         &
     &    add_nod_comm%num_import)
      call s_cal_total_and_stacks                                       &
     &   (add_nod_comm%nrank_import, add_nod_comm%num_import, izero,    &
     &    add_nod_comm%istack_import, add_nod_comm%ntot_import)
      call alloc_calypso_import_item                                    &
     &   (add_nod_comm%ntot_import, add_nod_comm)
!
      call alloc_node_data_sleeve_ext(add_nod_comm%ntot_import,         &
     &                                trim_import_xx)
      call alloc_import_lc_trimmed(add_nod_comm%ntot_import,            &
     &                             trim_nod_to_ext)
!
      call set_import_item_for_extend                                   &
     &   (org_node, nod_comm, expand_nod_comm, ext_nod_trim,            &
     &    add_nod_comm%nrank_import, add_nod_comm%irank_import,         &
     &    add_nod_comm%istack_import, add_nod_comm%ntot_import,         &
     &    trim_nod_to_ext%idx_extend_to_trim,                           &
     &    trim_nod_to_ext%import_lc_trimmed, add_nod_comm%item_import,  &
     &    trim_nod_to_ext%inod_added_import)
      call trim_imported_expand_node(add_nod_comm, ext_nod_trim,        &
     &                               exp_import_xx, trim_import_xx)
!
      call num_items_send_recv                                          &
     &   (add_nod_comm%nrank_import, add_nod_comm%irank_import,         &
     &    add_nod_comm%num_import,                                      &
     &    add_nod_comm%nrank_export, add_nod_comm%irank_export, izero,  &
     &    add_nod_comm%num_export, add_nod_comm%istack_export,          &
     &    add_nod_comm%ntot_export)
      call alloc_calypso_export_item(add_nod_comm)
!
!
      call comm_items_send_recv                                         &
     &  (add_nod_comm%nrank_import, add_nod_comm%irank_import,          &
     &   add_nod_comm%istack_import, trim_nod_to_ext%import_lc_trimmed, &
     &   add_nod_comm%nrank_export, add_nod_comm%irank_export,          &
     &   add_nod_comm%istack_export, add_nod_comm%iflag_self_copy,      &
     &   add_nod_comm%item_export)
!
      end subroutine const_extended_nod_comm_table
!
!  ---------------------------------------------------------------------
!
      end module const_extend_nod_comm_table
