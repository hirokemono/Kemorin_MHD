!>@file   const_global_element_ids.f90
!!@brief  module const_global_element_ids
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Construct global element IDs by number of internal elements
!!
!!@verbatim
!!      subroutine count_number_of_node_stack(nnod, istack_nod_list)
!!      subroutine set_global_ele_id(txt, nele, istack_internal_e,      &
!!     &         internal_flag, e_comm, iele_global)
!!      subroutine check_element_position(txt, nele, x_ele, e_comm)
!!@endverbatim
!!
      module const_global_element_ids
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_number_of_node_stack(nnod, istack_nod_list)
!
      integer(kind = kint) :: nnod
      integer(kind = kint_gl), intent(inout)                            &
     &            :: istack_nod_list(0:nprocs)
!
      integer(kind = kint), allocatable :: nnod_list_gl(:)
!
      integer(kind = kint) :: ip
!
!
      allocate(nnod_list_gl(nprocs))
      nnod_list_gl = 0
!
      call MPI_Allgather(nnod, ione, CALYPSO_INTEGER,                   &
     &    nnod_list_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
!
      istack_nod_list(0) = 0
      do ip = 1, nprocs
        istack_nod_list(ip) = istack_nod_list(ip-1) + nnod_list_gl(ip)
      end do
!
      deallocate(nnod_list_gl)
!
      end subroutine count_number_of_node_stack
!
!-----------------------------------------------------------------------
!
      subroutine set_global_ele_id(txt, nele, istack_internal_e,        &
     &          internal_flag, e_comm, iele_global)
!
      use t_comm_table
      use solver_SR_type
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: internal_flag(nele)
      integer(kind = kint_gl), intent(in)                               &
     &        :: istack_internal_e(0:nprocs)
!
      type(communication_table), intent(in) :: e_comm
!
      integer(kind = kint_gl), intent(inout)  :: iele_global(nele)
!
      integer(kind = kint) :: iele, icou
!
!
      icou = 0
      do iele = 1, nele
        if(internal_flag(iele) .gt. 0) then
          icou = icou + 1
          iele_global(iele) = icou + istack_internal_e(my_rank)
        else
          iele_global(iele) = 0
        end if
      end do
!
      call SOLVER_SEND_RECV_int8_type(nele, e_comm, iele_global)
!
      do iele = 1, nele
        if(iele_global(iele) .eq. 0)  write(*,*)                        &
     &        'Missing communication for ', trim(txt), ': ', iele
      end do
!
      end subroutine set_global_ele_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_element_position(txt, nele, x_ele, e_comm)
!
      use t_comm_table
      use solver_SR_type
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in)  :: x_ele(nele,3)
!
      type(communication_table), intent(in) :: e_comm
!
!
      real(kind = kreal), parameter :: tiny = 1.0d-14
      real(kind = kreal) :: dx, dy, dz
      real(kind = kreal), allocatable :: x_test(:)
      integer(kind = kint) :: iele, inum
!
!
      write(*,*) 'Number of  ', trim(txt), ' for ', my_rank, ': ',      &
     &            nele, size(x_ele,1)
      allocate(x_test(3*nele))
!
!$omp parallel do
      do iele = 1, nele
        x_test(3*iele-2) = x_ele(iele,1)
        x_test(3*iele-1) = x_ele(iele,2)
        x_test(3*iele  ) = x_ele(iele,3)
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,iele)
      do inum = 1, e_comm%ntot_import
        iele = e_comm%item_import(inum)
        x_test(3*iele-2) = 1.e30
        x_test(3*iele-1) = 1.e30
        x_test(3*iele  ) = 1.e30
      end do
!$omp end parallel do
!
      open(100+my_rank,position='append')
      write(100+my_rank,*) 'num_neib', my_rank, e_comm%num_neib
      do inum = 1, e_comm%num_neib
        write(100+my_rank,*) 'id_neib', my_rank, e_comm%id_neib(inum), &
     &                                     e_comm%istack_import(inum), &
     &                                     e_comm%istack_export(inum)
      end do
      close(100+my_rank)
      call calypso_mpi_barrier
!
      call SOLVER_SEND_RECV_3_ttest(nele, e_comm, x_test(1))
      write(*,*) 'send_recv end', my_rank
!
      do iele = 1, nele
        dx = x_test(3*iele-2) - x_ele(iele,1)
        dy = x_test(3*iele-1) - x_ele(iele,2)
        dz = x_test(3*iele  ) - x_ele(iele,3)
        if(     (abs(dx) .ge. tiny)  .or. (abs(dy) .ge. tiny)           &
     &     .or. (abs(dz) .ge. tiny)) then
          write(*,*) 'wrong ', trim(txt), ' position at: ',             &
     &         my_rank, iele, x_ele(iele,1:3), dx, dy, dz
        end if
      end do
!
      deallocate(x_test)
!
      end subroutine check_element_position
!
!-----------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_3_ttest(NP, comm_tbl, X)
!
      use t_comm_table
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(3*NP)
!
!
!      if (comm_tbl%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_3_test                                    &
     &     (NP, comm_tbl%num_neib, comm_tbl%id_neib,                    &
     &      comm_tbl%istack_import, comm_tbl%item_import,               &
     &      comm_tbl%istack_export, comm_tbl%item_export, X(1) )
!      end if
!
      end subroutine SOLVER_SEND_RECV_3_ttest
!
!-----------------------------------------------------------------------
!
      subroutine  SOLVER_SEND_RECV_3_test                               &
     &         ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
     &                                 STACK_EXPORT, NOD_EXPORT, X)

      use calypso_mpi
      use m_solver_SR

!>       number of nodes
      integer(kind=kint )                , intent(in)   ::  N
!>       total neighboring pe count
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
!>       neighboring pe id                        (i-th pe)
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE
!>       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
!>       imported node                            (i-th dof)
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &        :: NOD_IMPORT
!>       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
!>       exported node                            (i-th dof)
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &        :: NOD_EXPORT
!>       communicated result vector
      real   (kind=kreal), dimension(3*N), intent(inout):: X
!
      integer (kind = kint) :: neib, istart, inum, iend, k, ii
      integer (kind = kint) :: icou_s, icou_r
!
!
      call resize_work_4_SR(ithree, NEIBPETOT,                          &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
      icou_s = 0
      icou_r = 0
!
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        istart= 3 * STACK_EXPORT(neib-1) + 1
        inum  = 3 * ( STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
!
        if(inum .gt. 0) then
          do k= istart, iend
             ii   = 3*NOD_EXPORT(k)
             WS(3*k-2)= X(ii-2)
             WS(3*k-1)= X(ii-1)
             WS(3*k  )= X(ii  )
          end do
!
          icou_s = icou_s + 1
          call MPI_ISEND(WS(istart), inum, CALYPSO_REAL,                &
     &                 NEIBPE(neib), 0, CALYPSO_COMM, req1(icou_s),     &
     &                 ierr_MPI)
        end if
      end  do

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= 3 * STACK_IMPORT(neib-1) + 1
        inum  = 3 * ( STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        if(inum .gt. 0) then
          icou_r = icou_r + 1
          call MPI_IRECV(WR(istart), inum, CALYPSO_REAL,                &
     &                 NEIBPE(neib), 0, CALYPSO_COMM, req2(icou_r),     &
     &                 ierr_MPI)
        end if
      end do

      call MPI_WAITALL (icou_r, req2(1), sta2(1,1), ierr_MPI)
   
      do neib = 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do k= istart, iend
          ii   = 3*NOD_IMPORT(k)
          X(ii-2)= WR(3*k-2)
          X(ii-1)= WR(3*k-1)
          X(ii  )= WR(3*k  )
        end do
      end do

      call MPI_WAITALL (icou_s, req1(1), sta1(1,1), ierr_MPI)

      end subroutine SOLVER_SEND_RECV_3_test
!
!  ---------------------------------------------------------------------
!
      end module const_global_element_ids
