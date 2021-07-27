!> @file  t_pe_list_for_marks_extend.f90
!!      module t_pe_list_for_marks_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine const_pe_list_for_marks_extend(nod_comm, mark_saved, &
!!     &          maxpe_dist_send, pe_list_extend, SR_sig)
!!      subroutine dealloc_extend_pe_list_send(pe_list_extend)
!!      subroutine dealloc_extend_pe_list_recv(pe_list_extend)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(mark_for_each_comm), intent(in) :: mark_saved(nprocs)
!!        integer(kind = kint), intent(inout) :: maxpe_dist_send
!!        type(pe_list_for_marks_extend), intent(inout) :: pe_list_extend
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module t_pe_list_for_marks_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_mark_node_ele_to_extend
!
      implicit none
!
      type pe_list_for_marks_extend
        integer(kind = kint), allocatable :: npe_dist_send(:)
        integer(kind = kint), allocatable :: istack_num_dist(:)
        integer(kind = kint), allocatable :: irank_dist_send(:,:)
!
        integer(kind = kint), allocatable :: npe_dist_recv(:)
        integer(kind = kint), allocatable :: istack_pe_dist_recv(:)
        integer(kind = kint), allocatable :: irank_dist_recv(:,:)
      end type pe_list_for_marks_extend
!
      private :: init_extend_pe_list_send, init_extend_pe_list_recv
      private :: set_irank_with_distance_send
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_pe_list_for_marks_extend(nod_comm, mark_saved,   &
     &          maxpe_dist_send, pe_list_extend, SR_sig)
!
      use t_comm_table
      use t_solver_SR
      use calypso_mpi_int
      use reverse_SR_int
!
      type(communication_table), intent(in) :: nod_comm
      type(mark_for_each_comm), intent(in) :: mark_saved(nprocs)
!
      integer(kind = kint), intent(inout) :: maxpe_dist_send
      type(pe_list_for_marks_extend), intent(inout) :: pe_list_extend
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: npe_dist, ntot_pe_dist_recv
      integer(kind = kint) :: jcou
!
!
      npe_dist = num_rank_with_distance_send(nprocs, mark_saved)
      call calypso_mpi_allreduce_one_int                                &
        (npe_dist, maxpe_dist_send, MPI_MAX)
      call calypso_mpi_allreduce_one_int(npe_dist, jcou, MPI_SUM)
      if(my_rank .eq. 0) write(*,*) 'max pe for distance',              &
     &                     jcou, maxpe_dist_send, ' of ', nprocs
!
      call init_extend_pe_list_send                                     &
     &   (nod_comm%num_neib, npe_dist, maxpe_dist_send, pe_list_extend)
      call set_irank_with_distance_send                                 &
     &   (nprocs, mark_saved, nod_comm%num_neib,                        &
     &    maxpe_dist_send, pe_list_extend%irank_dist_send)
!
      call init_extend_pe_list_recv(nod_comm%num_neib, maxpe_dist_send, &
     &                              pe_list_extend)
      call num_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,     &
     &                         pe_list_extend%npe_dist_send,            &
     &                         nod_comm%num_neib, nod_comm%id_neib,     &
     &                         izero,  pe_list_extend%npe_dist_recv,    &
     &                         pe_list_extend%istack_pe_dist_recv,      &
     &                         ntot_pe_dist_recv, SR_sig)
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &                          pe_list_extend%istack_num_dist,         &
     &                          pe_list_extend%irank_dist_send,         &
     &                          nod_comm%num_neib, nod_comm%id_neib,    &
     &                          pe_list_extend%istack_num_dist, izero,  &
     &                          pe_list_extend%irank_dist_recv, SR_sig)
!
      end subroutine const_pe_list_for_marks_extend
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_extend_pe_list_send(pe_list_extend)
!
      type(pe_list_for_marks_extend), intent(inout) :: pe_list_extend
!
      deallocate(pe_list_extend%npe_dist_send)
      deallocate(pe_list_extend%istack_num_dist)
      deallocate(pe_list_extend%irank_dist_send)
!
      end subroutine dealloc_extend_pe_list_send
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_extend_pe_list_recv(pe_list_extend)
!
      type(pe_list_for_marks_extend), intent(inout) :: pe_list_extend
!
      deallocate(pe_list_extend%npe_dist_recv)
      deallocate(pe_list_extend%istack_pe_dist_recv)
      deallocate(pe_list_extend%irank_dist_recv)
!
      end subroutine dealloc_extend_pe_list_recv
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_extend_pe_list_send                               &
     &         (num_neib, npe_dist, maxpe_send, pe_list_extend)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: npe_dist, maxpe_send
      type(pe_list_for_marks_extend), intent(inout) :: pe_list_extend
!
      integer(kind = kint) :: ip
!
!
      allocate(pe_list_extend%npe_dist_send(num_neib))
      allocate(pe_list_extend%istack_num_dist(0:num_neib))
      allocate(pe_list_extend%irank_dist_send(maxpe_send,num_neib))
!
      pe_list_extend%istack_num_dist(0) = 0
!$omp parallel do
      do ip = 1, num_neib
        pe_list_extend%istack_num_dist(ip) = ip * maxpe_send
        pe_list_extend%npe_dist_send(ip) = npe_dist
        pe_list_extend%irank_dist_send(1:maxpe_send,ip) = -1
      end do
!$omp end parallel do
!
      end subroutine init_extend_pe_list_send
!
!  ---------------------------------------------------------------------
!
      subroutine init_extend_pe_list_recv(num_neib, maxpe_send,         &
     &                                    pe_list_extend)
!
      integer(kind = kint), intent(in) :: num_neib, maxpe_send
      type(pe_list_for_marks_extend), intent(inout) :: pe_list_extend
!
!
      allocate(pe_list_extend%npe_dist_recv(num_neib))
      allocate(pe_list_extend%istack_pe_dist_recv(0:num_neib))
      allocate(pe_list_extend%irank_dist_recv(maxpe_send,num_neib))
!
      pe_list_extend%istack_pe_dist_recv(0) = 0
      if(num_neib .le. 0) return
!$omp parallel workshare
      pe_list_extend%npe_dist_recv(1:num_neib) = 0
      pe_list_extend%istack_pe_dist_recv(1:num_neib) = 0
      pe_list_extend%irank_dist_recv(1:maxpe_send,1:num_neib) = -1
!$omp end parallel workshare
!
      end subroutine init_extend_pe_list_recv
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_rank_with_distance_send         &
     &                            (nprocs, mark_saved)
!
      integer, intent(in) :: nprocs
      type(mark_for_each_comm), intent(in) :: mark_saved(nprocs)
!
      integer(kind = kint) :: ip, npe_dist
!
      npe_dist = 0
      do ip = 1, nprocs
        if(mark_saved(ip)%num_marked .gt. 0) npe_dist = npe_dist + 1
      end do
      num_rank_with_distance_send = npe_dist
!
      end function num_rank_with_distance_send
!
!  ---------------------------------------------------------------------
!
      subroutine set_irank_with_distance_send(nprocs, mark_saved,       &
     &          num_neib, maxpe_send, irank_dist_send)
!
      integer, intent(in) :: nprocs
      type(mark_for_each_comm), intent(in) :: mark_saved(nprocs)
      integer(kind = kint), intent(in) :: num_neib, maxpe_send
      integer(kind = kint), intent(inout)                               &
     &                     :: irank_dist_send(maxpe_send,num_neib)
!
      integer(kind = kint) :: ip, icou
!
      if(num_neib .le. 0) return
!
      icou = 0
      do ip = 1, nprocs
        if(mark_saved(ip)%num_marked .gt. 0) then
          icou = icou + 1
          irank_dist_send(icou,1) = ip-1
        end if
      end do
!$omp parallel do
      do ip = 2, num_neib
        irank_dist_send(1:maxpe_send,ip)                                &
     &      = irank_dist_send(1:maxpe_send,1)
      end do
!$omp end parallel do
!
      end subroutine set_irank_with_distance_send
!
!  ---------------------------------------------------------------------
!
      end module t_pe_list_for_marks_extend
