!> @file  append_mark_export_extend.f90
!!      module append_mark_export_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine comm_marked_export_for_extend(nod_comm, node,        &
!!     &          mark_saved, pe_list_extend, maxpe_dist_send,          &
!!     &          marked_export, each_exp_flags, SR_sig, SR_r)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(mark_for_each_comm), intent(in) :: mark_saved(nprocs)
!!        type(pe_list_for_marks_extend), intent(in) :: pe_list_extend
!!        integer(kind = kint), intent(in) :: maxpe_dist_send
!!        type(mark_in_export), intent(inout)                           &
!!     &                     :: marked_export(maxpe_dist_send)
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine append_mark_export_for_extend                        &
!!     &         (node, maxpe_dist_send, marked_export, grp_list_export,&
!!     &          mark_saved, each_exp_flags)
!!        type(node_data), intent(in) :: node
!!        integer(kind = kint), intent(in) :: maxpe_dist_send
!!        type(mark_in_export), intent(in)                              &
!!     &                     :: marked_export(maxpe_dist_send)
!!        type(export_grp_list_extend), intent(in) :: grp_list_export
!!        type(mark_for_each_comm), intent(inout) :: mark_saved(nprocs)
!!        type(flags_each_comm_extend), intent(inout) :: each_exp_flags
!!@endverbatim
!
      module append_mark_export_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_mark_node_ele_to_extend
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
!
      private :: set_marked_distance_in_export
      private :: set_dist_from_marke_in_export
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
        call set_marked_distance_in_export(nod_comm, each_exp_flags,    &
     &                                     marked_export(icou))
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
        ist = grp_list_export%istack_set_import_recv(ip-1)
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
!  ---------------------------------------------------------------------
!
      subroutine set_marked_distance_in_export                          &
     &         (nod_comm, each_exp_flags, marked_ex)
!
      type(communication_table), intent(in) :: nod_comm
      type(flags_each_comm_extend), intent(in) :: each_exp_flags
!
      type(mark_in_export), intent(inout) :: marked_ex
!
      integer(kind = kint), allocatable :: item_tmp(:)
      real(kind = kreal), allocatable :: dist_tmp(:)
      integer(kind = kint) :: icou, inum, inod, ip, ist, ied
!
!
      allocate(item_tmp(nod_comm%ntot_import))
      allocate(dist_tmp(nod_comm%ntot_import))

      item_tmp(1:nod_comm%ntot_import) = 0
      dist_tmp(1:nod_comm%ntot_import) = 0.0d0
!
      call alloc_istack_marked_export(nod_comm, marked_ex)
!
      icou = 0
      marked_ex%istack_marked_export(0) = 0
      do ip = 1, nod_comm%num_neib
        ist = nod_comm%istack_import(ip-1) + 1
        ied = nod_comm%istack_import(ip  )
        do inum = ist, ied
          inod = nod_comm%item_import(inum)
          if(each_exp_flags%distance(inod) .gt. 0.0d0) then
            icou = icou + 1
            item_tmp(icou) = inod
            dist_tmp(icou) = each_exp_flags%distance(inod)
          end if
        end do
        marked_ex%istack_marked_export(ip) = icou
      end do
      marked_ex%ntot_marked_export                                      &
     &      = marked_ex%istack_marked_export(nod_comm%num_neib)
!
      call alloc_items_marked_export(marked_ex)
!
      if(marked_ex%ntot_marked_export .gt. 0) then
!$omp parallel workshare
        marked_ex%item_marked_export(1:marked_ex%ntot_marked_export)    &
     &    =  item_tmp(1:marked_ex%ntot_marked_export)
        marked_ex%dist_marked_export(1:marked_ex%ntot_marked_export)    &
     &      = dist_tmp(1:marked_ex%ntot_marked_export)
!$omp end parallel workshare
      end if
!
      deallocate(item_tmp, dist_tmp)
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
      end module append_mark_export_extend
