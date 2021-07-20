!> @file  const_nod_ele_to_extend.f90
!!      module const_nod_ele_to_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine const_sleeve_expand_list                             &
!!     &         (sleeve_exp_p, nod_comm, ele_comm, node, ele, neib_ele,&
!!     &          sleeve_exp_WK, mark_saved, mark_nod, mark_ele,        &
!!     &          SR_sig, SR_r, SR_i)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm, ele_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_around_node), intent(in) :: neib_ele
!!        type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!!        type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
!!        type(mark_for_each_comm), intent(inout)                       &
!!     &                         :: mark_nod(nod_comm%num_neib)
!!        type(mark_for_each_comm), intent(inout)                       &
!!     &                         :: mark_ele(nod_comm%num_neib)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!      subroutine comm_extended_import_nod_ele                         &
!!     &         (nod_comm, node, inod_dbl, ele, iele_dbl,              &
!!     &          mark_nod, mark_ele, expand_nod_comm, expand_ele_comm, &
!!     &          exp_import_xx, exp_import_ie, SR_sig)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(node_ele_double_number), intent(in) :: iele_dbl
!!        type(communication_table), intent(inout) :: expand_nod_comm
!!        type(communication_table), intent(inout) :: expand_ele_comm
!!        type(node_data_for_sleeve_ext), intent(inout) :: exp_import_xx
!!        type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module const_nod_ele_to_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_next_node_ele_4_node
      use t_ctl_param_sleeve_extend
      use t_mark_node_ele_to_extend
      use t_mesh_for_sleeve_extend
      use t_solver_SR
!
      use m_work_time
      use m_work_time_4_sleeve_extend
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_sleeve_expand_list                               &
     &         (sleeve_exp_p, nod_comm, ele_comm, node, ele, neib_ele,  &
     &          sleeve_exp_WK, mark_saved, mark_nod, mark_ele,          &
     &          SR_sig, SR_r, SR_i)
!
      use t_solver_SR
      use t_solver_SR_int
      use t_comm_table_for_each_pe
      use t_flags_each_comm_extend
!
      use calypso_mpi_int
      use solver_SR_type
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm, ele_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: neib_ele
      type(sleeve_extension_work), intent(in) :: sleeve_exp_WK
!
      type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
      type(mark_for_each_comm), intent(inout)                           &
     &                         :: mark_nod(nod_comm%num_neib)
      type(mark_for_each_comm), intent(inout)                           &
     &                         :: mark_ele(nod_comm%num_neib)
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      type(comm_table_for_each_pe), save :: each_comm
      type(flags_each_comm_extend), save :: each_exp_flags
      integer(kind = kint) :: i, ip, icou, jcou
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
!
      call alloc_flags_each_comm_extend                                 &
     &   (node%numnod, ele%numele, each_exp_flags)
      call alloc_comm_table_for_each(node, each_comm)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+9)
      do ip = 1, nprocs
        if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+15)
        call reset_flags_each_comm_extend                               &
     &     (node%numnod, ele%numele, each_exp_flags)
        call set_distance_from_mark_list                                &
     &     (-1, mark_saved(ip), each_exp_flags)
        call dealloc_mark_for_each_comm(mark_saved(ip))
        if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+15)
!
        if(iflag_SLEX_time)                                             &
       &                  call start_elapsed_time(ist_elapsed_SLEX+16)
        call SOLVER_SEND_RECV_type(node%numnod, nod_comm,               &
     &      SR_sig, SR_r, each_exp_flags%distance)
        if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+16)
!
        if(iflag_SLEX_time)                                             &
       &                  call start_elapsed_time(ist_elapsed_SLEX+17)
        icou = count_num_marked_by_dist(node%numnod,                    &
     &                                  each_exp_flags%distance)
        call alloc_mark_for_each_comm(icou, mark_saved(ip))
        call set_distance_to_mark_by_dist                               &
     &     (node%numnod, each_exp_flags%distance, mark_saved(ip))
        if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+17)
      end do
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+9)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+10)
      icou = 0
      jcou = 0
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i) + 1
        call s_mark_node_ele_to_extend(i, sleeve_exp_p, nod_comm,       &
     &      ele_comm, node, ele, neib_ele, sleeve_exp_WK, each_comm,    &
     &      mark_saved(ip), mark_nod(i), mark_ele(i), each_exp_flags)
!
        call check_missing_connect_to_extend(node, ele,                 &
    &       mark_ele(i), each_exp_flags%iflag_node, icou, jcou)
      end do
!
      call dealloc_flags_each_comm_extend(each_exp_flags)
      call dealloc_comm_table_for_each(each_comm)
!
!
      call calypso_mpi_reduce_one_int(icou, ntot_failed_gl, MPI_SUM, 0)
      call calypso_mpi_reduce_one_int(jcou, nele_failed_gl, MPI_SUM, 0)
      if(iflag_debug .gt. 0) write(*,*) 'Failed element list:',         &
     &                             ntot_failed_gl, nele_failed_gl
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+10)
!
      if(i_debug .eq. 0) return
      write(*,*) my_rank, 'mark_nod%num_marked',                        &
     &    mark_nod(1:nod_comm%num_neib)%num_marked, ' of ', node%numnod
      write(*,*) my_rank, 'mark_ele%num_marked',                        &
     &    mark_ele(1:nod_comm%num_neib)%num_marked, ' of ', ele%numele
!
      end subroutine const_sleeve_expand_list
!
!  ---------------------------------------------------------------------
!
      subroutine comm_extended_import_nod_ele                           &
     &         (nod_comm, node, inod_dbl, ele, iele_dbl,                &
     &          mark_nod, mark_ele, expand_nod_comm, expand_ele_comm,   &
     &          exp_import_xx, exp_import_ie, SR_sig)
!
      use calypso_mpi_int
      use reverse_SR_int
      use cal_minmax_and_stacks
      use set_mesh_for_sleeve_extend
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(mark_for_each_comm), intent(in)                              &
     &                         :: mark_nod(nod_comm%num_neib)
      type(mark_for_each_comm), intent(in)                              &
     &                         :: mark_ele(nod_comm%num_neib)
!
      type(communication_table), intent(inout) :: expand_nod_comm
      type(communication_table), intent(inout) :: expand_ele_comm
      type(node_data_for_sleeve_ext), intent(inout) :: exp_import_xx
      type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
      type(send_recv_status), intent(inout) :: SR_sig
!
      type(node_data_for_sleeve_ext), save :: exp_export_xx
      type(ele_data_for_sleeve_ext), save :: exp_export_ie
!
!
      expand_nod_comm%num_neib = nod_comm%num_neib
      expand_ele_comm%num_neib = nod_comm%num_neib
!
      call alloc_neighbouring_id(expand_nod_comm)
      call alloc_import_num(expand_nod_comm)
      call alloc_export_num(expand_nod_comm)
!
      call alloc_neighbouring_id(expand_ele_comm)
      call alloc_import_num(expand_ele_comm)
      call alloc_export_num(expand_ele_comm)
!
!$omp parallel workshare
      expand_nod_comm%id_neib(1:nod_comm%num_neib)                      &
     &        = nod_comm%id_neib(1:nod_comm%num_neib)
      expand_ele_comm%id_neib(1:nod_comm%num_neib)                      &
     &        = nod_comm%id_neib(1:nod_comm%num_neib)
!$omp end parallel workshare
!
      call count_export_4_expanded_mesh                                 &
     &   (nod_comm, node, mark_nod, mark_ele,                           &
     &    expand_nod_comm%num_export, expand_ele_comm%num_export)
      call s_cal_total_and_stacks                                       &
     &   (nod_comm%num_neib, expand_nod_comm%num_export, izero,         &
     &    expand_nod_comm%istack_export, expand_nod_comm%ntot_export)
      call s_cal_total_and_stacks                                       &
     &   (nod_comm%num_neib, expand_ele_comm%num_export, izero,         &
     &    expand_ele_comm%istack_export, expand_ele_comm%ntot_export)

      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_nod_comm%num_export,                                   &
     &    nod_comm%num_neib, nod_comm%id_neib, izero,                   &
     &    expand_nod_comm%num_import, expand_nod_comm%istack_import,    &
     &    expand_nod_comm%ntot_import, SR_sig)
      call num_items_send_recv                                          &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_ele_comm%num_export,                                   &
     &    nod_comm%num_neib, nod_comm%id_neib, izero,                   &
     &    expand_ele_comm%num_import, expand_ele_comm%istack_import,    &
     &    expand_ele_comm%ntot_import, SR_sig)
!
!
      call alloc_export_item(expand_nod_comm)
      call alloc_node_data_sleeve_ext                                   &
     &   (expand_nod_comm%ntot_export, exp_export_xx)
      call alloc_export_item(expand_ele_comm)
      call alloc_ele_data_sleeve_ext                                    &
     &   (expand_ele_comm%ntot_export, ele%nnod_4_ele, exp_export_ie)
!
      call set_export_4_expanded_mesh                                   &
     &   (nod_comm, node, ele, inod_dbl, iele_dbl, mark_nod, mark_ele,  &
     &    expand_nod_comm%ntot_export, expand_nod_comm%istack_export,   &
     &    expand_ele_comm%ntot_export, expand_ele_comm%istack_export,   &
     &    expand_nod_comm%item_export, exp_export_xx,                   &
     &    expand_ele_comm%item_export, exp_export_ie)
!
!
      call alloc_import_item(expand_nod_comm)
      call comm_items_send_recv                                         &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_nod_comm%istack_export, expand_nod_comm%item_export,   &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_nod_comm%istack_import, izero,                         &
     &    expand_nod_comm%item_import, SR_sig)
!
      call alloc_node_data_sleeve_ext(expand_nod_comm%ntot_import,      &
     &                                exp_import_xx)
      call send_extended_node_position(expand_nod_comm,                 &
     &    exp_export_xx, exp_import_xx, SR_sig)
      call dealloc_node_data_sleeve_ext(exp_export_xx)
!
!
      call alloc_import_item(expand_ele_comm)
      call alloc_ele_data_sleeve_ext                                    &
     &   (expand_ele_comm%ntot_import, ele%nnod_4_ele, exp_import_ie)
!
      call comm_items_send_recv                                         &
     &   (nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_ele_comm%istack_export, expand_ele_comm%item_export,   &
     &    nod_comm%num_neib, nod_comm%id_neib,                          &
     &    expand_ele_comm%istack_import, izero,                         &
     &    expand_ele_comm%item_import, SR_sig)
      call send_extended_element_connect(ele, expand_ele_comm,          &
     &    exp_export_ie, exp_import_ie, SR_sig)
      call dealloc_ele_data_sleeve_ext(exp_export_ie)
!
      end subroutine comm_extended_import_nod_ele
!
!  ---------------------------------------------------------------------
!
      end module const_nod_ele_to_extend
