!> @file  const_extend_ele_comm_table.f90
!!      module const_extend_ele_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine const_extended_element_connect                       &
!!     &         (nod_comm, ele, inod_new_dbl,                          &
!!     &          expand_nod_comm, exp_import_xx, inod_added_import,    &
!!     &          expand_ele_comm, exp_import_ie)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(element_data), intent(in) :: ele
!!        type(node_ele_double_number), intent(in) :: inod_new_dbl
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(calypso_comm_table), intent(in) :: add_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        integer(kind = kint), intent(in)                              &
!!     &      :: idx_nod_extend_to_trimmed(expand_nod_comm%ntot_import)
!!        type(communication_table), intent(inout) :: expand_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!!      subroutine const_extended_ele_comm_table(ele, iele_dbl,         &
!!     &          nod_comm, ele_comm, add_nod_comm, expand_ele_comm,    &
!!     &          exp_import_ie, trim_import_ie, add_ele_comm, SR_sig)
!!        type(element_data), intent(in) :: ele
!!        type(node_ele_double_number), intent(in) :: iele_dbl
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: ele_comm
!!        type(calypso_comm_table), intent(in) :: add_nod_comm
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!!        type(ele_data_for_sleeve_ext), intent(inout) :: trim_import_ie
!!        type(calypso_comm_table), intent(inout) :: add_ele_comm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module const_extend_ele_comm_table
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
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
      use t_trim_overlapped_import
      use t_solver_SR
!
      implicit none
!
      integer(kind = kint), allocatable :: iele_lc_import_trim(:)
      private :: iele_lc_import_trim
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_extended_element_connect                         &
     &         (nod_comm, ele, inod_new_dbl,                            &
     &          expand_nod_comm, exp_import_xx, inod_added_import,      &
     &          expand_ele_comm, exp_import_ie)
!
      use trim_mesh_for_sleeve_extend
      use set_mesh_for_sleeve_extend
      use checks_for_sleeve_extend
!
      type(communication_table), intent(in) :: nod_comm
      type(element_data), intent(in) :: ele
      type(node_ele_double_number), intent(in) :: inod_new_dbl
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx

      integer(kind = kint), intent(in)                                  &
     &      :: inod_added_import(expand_nod_comm%ntot_import)
!
      type(communication_table), intent(inout) :: expand_ele_comm
      type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!
!
      if(i_debug .gt. 0) then
        call check_expanded_import_node(inod_new_dbl, expand_nod_comm,  &
     &      exp_import_xx, inod_added_import)
      end if
!
      if(i_debug .gt. 0) then
        call check_expanded_import_ele                                  &
     &    (ele, expand_ele_comm, exp_import_ie)
      end if
!
      call renumber_extended_ele_import(my_rank, ele, nod_comm,         &
     &    expand_nod_comm, expand_ele_comm, inod_added_import,          &
     &    exp_import_ie%itype_comm, exp_import_ie%ie_comm)
!
      if(i_debug .gt. 0) then
        call check_expanded_import_ele                                  &
     &    (ele, expand_ele_comm, exp_import_ie)
      end if
!
      end subroutine const_extended_element_connect
!
!  ---------------------------------------------------------------------
!
      subroutine const_extended_ele_comm_table(ele, iele_dbl,           &
     &          nod_comm, ele_comm, add_nod_comm, expand_ele_comm,      &
     &          exp_import_ie, trim_import_ie, add_ele_comm, SR_sig)
!
      use calypso_mpi_int
      use reverse_SR_int
!
      use cal_minmax_and_stacks
      use set_expanded_comm_table
      use trim_mesh_for_sleeve_extend
      use checks_for_sleeve_extend
!
      type(element_data), intent(in) :: ele
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(calypso_comm_table), intent(in) :: add_nod_comm
      type(communication_table), intent(in) :: expand_ele_comm
      type(ele_data_for_sleeve_ext), intent(in) :: exp_import_ie
!
      type(ele_data_for_sleeve_ext), intent(inout) :: trim_import_ie
      type(calypso_comm_table), intent(inout) :: add_ele_comm
      type(send_recv_status), intent(inout) :: SR_sig
!
      type(sort_data_for_sleeve_trim), save :: sort_ele_import
      type(data_for_trim_import), save :: ext_ele_trim
!
      integer(kind = kint) :: ntot_mix
!
!
      add_ele_comm%iflag_self_copy = add_nod_comm%iflag_self_copy
      add_ele_comm%nrank_import =    add_nod_comm%nrank_import
      add_ele_comm%nrank_export =    add_nod_comm%nrank_export
!
      call alloc_calypso_import_num(add_ele_comm)
      call alloc_calypso_export_num(add_ele_comm)
!
!$omp parallel workshare
      add_ele_comm%irank_import(1:add_ele_comm%nrank_import)            &
     &         = add_nod_comm%irank_import(1:add_ele_comm%nrank_import)
!$omp end parallel workshare
!$omp parallel workshare
      add_ele_comm%irank_export(1:add_ele_comm%nrank_export)            &
     &         = add_nod_comm%irank_export(1:add_ele_comm%nrank_export)
!$omp end parallel workshare
!
!
      ntot_mix = ele_comm%ntot_import + expand_ele_comm%ntot_import
      call alloc_sort_data_sleeve_ext                                   &
     &   (nprocs, ntot_mix, sort_ele_import)
      call sort_mix_import_by_pe_inod_lc(iele_dbl, ele_comm,            &
     &    expand_ele_comm, exp_import_ie%irank_comm, sort_ele_import)
!
      call trim_overlap_expanded_import                                 &
     &   (expand_ele_comm%ntot_import, exp_import_ie%irank_comm,        &
     &    sort_ele_import, ext_ele_trim)
      if(i_debug .gt. 0) then
        call check_overlapped_sleeve_ext                                &
     &    (nod_comm, add_ele_comm, sort_ele_import, ext_ele_trim)
      end if
      call dealloc_sort_data_sleeve_ext(sort_ele_import)
!
      call count_import_item_for_extend(ele_comm, ext_ele_trim,         &
     &    add_ele_comm%nrank_import, add_ele_comm%irank_import,         &
     &    add_ele_comm%num_import)
      call s_cal_total_and_stacks                                       &
     &   (add_ele_comm%nrank_import, add_ele_comm%num_import, izero,    &
     &    add_ele_comm%istack_import, add_ele_comm%ntot_import)
      call alloc_calypso_import_item                                    &
     &   (add_ele_comm%ntot_import, add_ele_comm)
!
      allocate(iele_lc_import_trim(add_ele_comm%ntot_import))
      call alloc_ele_data_sleeve_ext(add_ele_comm%ntot_import,          &
     &    ele%nnod_4_ele, trim_import_ie)
!
      call set_trimmed_import_items                                     &
     &   (ele, expand_ele_comm, ext_ele_trim, exp_import_ie,            &
     &    iele_lc_import_trim, add_ele_comm, trim_import_ie)
!
      call dealloc_stack_to_trim_extend(ext_ele_trim)
      call dealloc_idx_trimed_to_sorted(ext_ele_trim)
!
      call check_trim_import_ele_connect(ele, add_ele_comm,             &
     &                                   trim_import_ie%ie_comm)
!
!
      call num_items_send_recv                                          &
     &   (add_ele_comm%nrank_import, add_ele_comm%irank_import,         &
     &    add_ele_comm%num_import,                                      &
     &    add_ele_comm%nrank_export, add_ele_comm%irank_export, izero,  &
     &    add_ele_comm%num_export, add_ele_comm%istack_export,          &
     &    add_ele_comm%ntot_export, SR_sig)
      call alloc_calypso_export_item(add_ele_comm)
!
      call comm_items_send_recv                                         &
     &   (add_ele_comm%nrank_import, add_ele_comm%irank_import,         &
     &    add_ele_comm%istack_import, iele_lc_import_trim,              &
     &    add_ele_comm%nrank_export, add_ele_comm%irank_export,         &
     &    add_ele_comm%istack_export, add_ele_comm%iflag_self_copy,     &
     &    add_ele_comm%item_export, SR_sig)
      deallocate(iele_lc_import_trim)
!
      end subroutine const_extended_ele_comm_table
!
!  ---------------------------------------------------------------------
!
      end module const_extend_ele_comm_table
