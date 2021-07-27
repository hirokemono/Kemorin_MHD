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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine comm_marked_export_for_extend(nod_comm, node,          &
     &          mark_saved, pe_list_extend, maxpe_dist_send,            &
     &          marked_export, each_exp_flags, SR_sig, SR_r)
!
      use calypso_mpi
      use solver_SR_type
      use load_distance_and_mark_list
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(mark_for_each_comm), intent(in) :: mark_saved(nprocs)
      type(pe_list_for_marks_extend), intent(in) :: pe_list_extend
!
      integer(kind = kint), intent(in) :: maxpe_dist_send
      type(mark_in_export), intent(inout)                               &
     &                     :: marked_export(maxpe_dist_send)
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: icou, ip
!
!
      do icou = 1, maxpe_dist_send
        call reset_flags_each_comm_extend(node%numnod, each_exp_flags)
        if(icou .le. pe_list_extend%npe_dist_send(1)) then
          ip = pe_list_extend%irank_dist_send(icou,1) + 1
          call set_distance_from_mark_list                              &
     &       (-1, mark_saved(ip), each_exp_flags)
        end if
!
        call SOLVER_SEND_RECV_type(node%numnod, nod_comm,               &
     &      SR_sig, SR_r, each_exp_flags%distance)
!
        call alloc_istack_marked_export(nod_comm, marked_export(icou))
        call count_num_marked_in_export(nod_comm, each_exp_flags,       &
     &      marked_export(icou)%istack_marked_export(0),                &
     &      marked_export(icou)%ntot_marked_export)
!
        call alloc_items_marked_export(marked_export(icou))
        call set_marked_distance_in_export(nod_comm, each_exp_flags,    &
     &      marked_export(icou)%ntot_marked_export,                     &
     &      marked_export(icou)%istack_marked_export(0),                &
     &      marked_export(icou)%item_marked_export,                     &
     &      marked_export(icou)%dist_marked_export)
      end do
!
      end subroutine comm_marked_export_for_extend
!
!  ---------------------------------------------------------------------
!
      subroutine append_mark_export_for_extend                          &
     &         (node, maxpe_dist_send, marked_export, grp_list_export,  &
     &          mark_saved, each_exp_flags)
!
      use load_distance_and_mark_list
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: maxpe_dist_send
      type(mark_in_export), intent(in)                                  &
     &                     :: marked_export(maxpe_dist_send)
      type(export_grp_list_extend), intent(in) :: grp_list_export
!
      type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
      type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!
      integer(kind = kint) :: ist, ip
!
!
      do ip = 1, nprocs
        call reset_flags_each_comm_extend(node%numnod, each_exp_flags)
        call set_distance_from_intenal_mark                             &
     &     (node%internal_node, mark_saved(ip), each_exp_flags)
        call dealloc_mark_for_each_comm(mark_saved(ip))
!
        ist = grp_list_export%istack_set_import_recv(ip  )
        call set_dist_from_marke_in_export(node%numnod,                 &
     &      maxpe_dist_send, marked_export,                             &
     &      grp_list_export%nset_import_recv(ip),                       &
     &      grp_list_export%iset_import_recv(ist+1,1),                  &
     &      grp_list_export%iset_import_recv(ist+1,2),                  &
     &      each_exp_flags%distance)
!
        if(iflag_SLEX_time)                                             &
     &                  call start_elapsed_time(ist_elapsed_SLEX+17)
        call count_num_marked_by_dist                                   &
     &     (node, each_exp_flags, mark_saved(ip)%num_marked,            &
     &      mark_saved(ip)%istack_marked_smp)
        call alloc_mark_for_each_comm(mark_saved(ip))
        call set_distance_to_mark_by_dist(node, each_exp_flags,         &
     &     mark_saved(ip)%num_marked, mark_saved(ip)%istack_marked_smp, &
     &     mark_saved(ip)%idx_marked, mark_saved(ip)%dist_marked)
        if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+17)
      end do
!
      end subroutine append_mark_export_for_extend
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
!
      use calypso_mpi_int
      use solver_SR_type
      use reverse_SR_int
      use load_distance_and_mark_list
      use mark_node_ele_to_extend
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
      type(flags_each_comm_extend) :: each_exp_flags
      integer(kind = kint), allocatable :: iflag_exp_ele(:)
!
      integer(kind = kint) :: maxpe_dist_send
      type(mark_in_export), allocatable :: marked_export(:)
!
      type(pe_list_for_marks_extend) :: pe_list_extend
      type(export_grp_list_extend) :: grp_list_export
!
      integer(kind = kint) :: i, ip, icou, jcou, ist
      integer(kind = kint) :: ntot_failed_gl, nele_failed_gl
!
!
      call alloc_flags_each_comm_extend(node%numnod, each_exp_flags)
      call alloc_comm_table_for_each(node, each_comm)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+9)
      call const_pe_list_for_marks_extend(nod_comm, mark_saved,         &
     &    maxpe_dist_send, pe_list_extend, SR_sig)
      call const_export_grp_list_extend(nod_comm, pe_list_extend,       &
     &                                  grp_list_export)
!
      allocate(marked_export(maxpe_dist_send))
      call comm_marked_export_for_extend(nod_comm, node,                &
     &    mark_saved, pe_list_extend, maxpe_dist_send,                  &
     &    marked_export, each_exp_flags, SR_sig, SR_r)
      call dealloc_extend_pe_list_send(pe_list_extend)
      call dealloc_extend_pe_list_recv(pe_list_extend)
!
      call append_mark_export_for_extend                                &
     &   (node, maxpe_dist_send, marked_export,                         &
     &    grp_list_export, mark_saved, each_exp_flags)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+9)
!
      call dealloc_iset_import_recv(grp_list_export)
      do icou = 1, maxpe_dist_send
        call dealloc_mark_in_export(marked_export(icou))
      end do
      deallocate(marked_export)
!
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+10)
      icou = 0
      jcou = 0
      allocate(iflag_exp_ele(ele%numele))
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i) + 1
        call s_mark_node_ele_to_extend(i, sleeve_exp_p, nod_comm,       &
     &      ele_comm, node, ele, neib_ele, sleeve_exp_WK, each_comm,    &
     &      mark_saved(ip), mark_nod(i), mark_ele(i),                   &
     &      each_exp_flags, iflag_exp_ele)
!
        call check_missing_connect_to_extend(node, ele,                 &
    &       mark_ele(i), each_exp_flags%iflag_node, icou, jcou)
      end do
!
      deallocate(iflag_exp_ele)
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
!  ---------------------------------------------------------------------
!
      subroutine count_num_marked_in_export(nod_comm, each_exp_flags,   &
     &          istack_marked, ntot_marked)
!
      type(communication_table), intent(in) :: nod_comm
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
!
      integer(kind = kint), intent(inout)                               &
     &                  :: istack_marked(0:nod_comm%num_neib)
      integer(kind = kint), intent(inout) :: ntot_marked
!
      integer(kind = kint) :: icou, inum, inod, ip, ist, ied
!
!
!$omp parallel do private(ip,icou,ist,ied,inum,inod)
      do ip = 1, nod_comm%num_neib
        icou = 0
        ist = nod_comm%istack_import(ip-1) + 1
        ied = nod_comm%istack_import(ip  )
        do inum = ist, ied
          inod = nod_comm%item_import(inum)
          if(each_exp_flags%distance(inod) .gt. 0.0d0) icou = icou + 1
        end do
        istack_marked(ip) = icou
      end do
!$omp end parallel do
!
      istack_marked(0) = 0
      do ip = 1, nod_comm%num_neib
        istack_marked(ip) = istack_marked(ip-1) + istack_marked(ip)
      end do
      ntot_marked = istack_marked(nod_comm%num_neib)
!
      end subroutine count_num_marked_in_export
!
!  ---------------------------------------------------------------------
!
      subroutine set_marked_distance_in_export                          &
     &         (nod_comm, each_exp_flags, ntot_marked,                  &
     &          istack_marked, item_marked, dist_marked)
!
      type(communication_table), intent(in) :: nod_comm
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
      integer(kind = kint), intent(in) :: ntot_marked
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_marked(0:nod_comm%num_neib)
!
      integer(kind = kint), intent(inout) :: item_marked(ntot_marked)
      real(kind = kreal), intent(inout) :: dist_marked(ntot_marked)
!
      integer(kind = kint) :: icou, inod, ip, ist, ied, inum
!
!
      if(ntot_marked .le. 0) return
!
!$omp parallel do private(ip,icou,ist,ied,inum,inod)
      do ip = 1, nod_comm%num_neib
        icou = istack_marked(ip-1)
        ist = nod_comm%istack_import(ip-1) + 1
        ied = nod_comm%istack_import(ip  )
        do inum = ist, ied
          inod = nod_comm%item_import(inum)
          if(each_exp_flags%distance(inod) .gt. 0.0d0) then
            icou = icou + 1
            item_marked(icou) =  inod
            dist_marked(icou) = each_exp_flags%distance(inod)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_marked_distance_in_export
!
!  ---------------------------------------------------------------------
!
      subroutine set_dist_from_marke_in_export                          &
     &         (numnod, maxpe_dist_send, marked_export,                 &
     &          nset_import_recv, ineib_import_recv,                    &
     &          itarget_import_recv, dist_marked)
!
      use t_flags_each_comm_extend
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: maxpe_dist_send
      type(mark_in_export), intent(in)                                  &
     &                      :: marked_export(maxpe_dist_send)
!
      integer(kind = kint), intent(in) :: nset_import_recv
      integer(kind = kint), intent(in)                                  &
     &                     :: ineib_import_recv(nset_import_recv)
      integer(kind = kint), intent(in)                                  &
     &                     :: itarget_import_recv(nset_import_recv)
!
      real(kind = kreal), intent(inout) :: dist_marked(numnod)
!
      integer(kind = kint) :: igrp, inod, jst, jed, jnum, inum, icou
!
!
      do inum = 1, nset_import_recv
        igrp = ineib_import_recv(inum)
        icou = itarget_import_recv(inum)
        jst = marked_export(icou)%istack_marked_export(igrp-1) + 1
        jed = marked_export(icou)%istack_marked_export(igrp  )
        do jnum = jst, jed
          inod = marked_export(icou)%item_marked_export(jnum)
          dist_marked(inod)                                             &
     &         = marked_export(icou)%dist_marked_export(jnum)
        end do
      end do
!
      end subroutine set_dist_from_marke_in_export
!
!  ---------------------------------------------------------------------
!
      end module const_nod_ele_to_extend
