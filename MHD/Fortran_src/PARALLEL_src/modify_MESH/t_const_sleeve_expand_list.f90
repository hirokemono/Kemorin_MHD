!> @file  t_const_sleeve_expand_list.f90
!!      module t_const_sleeve_expand_list
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
!!     &          SR_sig, SR_r)
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
!!@endverbatim
!
      module t_const_sleeve_expand_list
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
      use t_pe_list_for_marks_extend
      use t_flags_each_comm_extend
      use t_export_grp_list_extend
      use t_solver_SR
!
      use m_work_time
      use m_work_time_4_sleeve_extend
!
      implicit none
!
      type work_sleeve_extend_marking
        type(comm_table_for_each_pe) :: each_comm
        type(flags_each_comm_extend) :: each_exp_flags
        integer(kind = kint), allocatable :: iflag_exp_ele(:)
!
        integer(kind = kint) :: maxpe_dist_send
        type(mark_in_export), allocatable :: marked_export(:)
!
        type(pe_list_for_marks_extend) :: pe_list_extend
        type(export_grp_list_extend) :: grp_list_export
      end type work_sleeve_extend_marking
!
      private :: alloc_marked_export_extend
      private :: dealloc_marked_export_extend
      private :: alloc_iflag_expand_ele, dealloc_iflag_expand_ele
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
     &          SR_sig, SR_r)
!
      use t_solver_SR
      use t_solver_SR_int
      use t_comm_table_for_each_pe
!
      use calypso_mpi_int
      use solver_SR_type
      use reverse_SR_int
      use load_distance_and_mark_list
      use mark_node_ele_to_extend
      use append_mark_export_extend
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
!
      type(work_sleeve_extend_marking), save :: marking_WK
!
      integer(kind = kint) :: i, ip, icou, jcou
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
!
      call alloc_flags_each_comm_extend                                 &
     &   (node%numnod, marking_WK%each_exp_flags)
      call alloc_comm_table_for_each(node, marking_WK%each_comm)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+9)
      call const_pe_list_for_marks_extend(nod_comm, mark_saved,         &
     &    marking_WK%maxpe_dist_send, marking_WK%pe_list_extend,        &
     &    SR_sig)
      call const_export_grp_list_extend(nod_comm,                       &
     &    marking_WK%pe_list_extend, marking_WK%grp_list_export)
!
      call alloc_marked_export_extend(marking_WK)
      call comm_marked_export_for_extend(nod_comm, node,                &
     &    mark_saved, marking_WK%pe_list_extend,                        &
     &    marking_WK%maxpe_dist_send, marking_WK%marked_export,         &
     &    marking_WK%each_exp_flags, SR_sig, SR_r)
      call dealloc_extend_pe_list_send(marking_WK%pe_list_extend)
      call dealloc_extend_pe_list_recv(marking_WK%pe_list_extend)
!
      call append_mark_export_for_extend                                &
     &   (node, marking_WK%maxpe_dist_send, marking_WK%marked_export,   &
     &    marking_WK%grp_list_export, mark_saved,                       &
     &    marking_WK%each_exp_flags)
      call dealloc_marked_export_extend(marking_WK)
      call dealloc_iset_import_recv(marking_WK%grp_list_export)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+9)
!
!
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+10)
      icou = 0
      jcou = 0
      call alloc_iflag_expand_ele(ele%numele, marking_WK)
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i) + 1
        call s_mark_node_ele_to_extend                                  &
     &     (i, sleeve_exp_p, nod_comm, ele_comm, node, ele,             &
     &      neib_ele, sleeve_exp_WK, marking_WK%each_comm,              &
     &      mark_saved(ip), mark_nod(i), mark_ele(i),                   &
     &      marking_WK%each_exp_flags, marking_WK%iflag_exp_ele)
!
        call check_missing_connect_to_extend(node, ele,                 &
    &       mark_ele(i), marking_WK%each_exp_flags%iflag_node,          &
    &       icou, jcou)
      end do
      call dealloc_iflag_expand_ele(marking_WK)
      call dealloc_flags_each_comm_extend(marking_WK%each_exp_flags)
      call dealloc_comm_table_for_each(marking_WK%each_comm)
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
!  ---------------------------------------------------------------------
!
      subroutine alloc_marked_export_extend(marking_WK)
!
      type(work_sleeve_extend_marking), intent(inout) :: marking_WK
!
      allocate(marking_WK%marked_export(marking_WK%maxpe_dist_send))
!
      end subroutine alloc_marked_export_extend
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_marked_export_extend(marking_WK)
!
      type(work_sleeve_extend_marking), intent(inout) :: marking_WK
!
      integer(kind = kint) :: icou
!
      do icou = 1, marking_WK%maxpe_dist_send
        call dealloc_mark_in_export(marking_WK%marked_export(icou))
      end do
      deallocate(marking_WK%marked_export)
!
      end subroutine dealloc_marked_export_extend
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iflag_expand_ele(numele, marking_WK)
!
      integer(kind = kint), intent(in) :: numele
      type(work_sleeve_extend_marking), intent(inout) :: marking_WK
!
      allocate(marking_WK%iflag_exp_ele(numele))
!
!$omp parallel workshare
      marking_WK%iflag_exp_ele(1:numele) = 0
!$omp end parallel workshare
!
      end subroutine alloc_iflag_expand_ele
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iflag_expand_ele(marking_WK)
!
      type(work_sleeve_extend_marking), intent(inout) :: marking_WK
!
      deallocate(marking_WK%iflag_exp_ele)
!
      end subroutine dealloc_iflag_expand_ele
!
!  ---------------------------------------------------------------------
!
      end module t_const_sleeve_expand_list
