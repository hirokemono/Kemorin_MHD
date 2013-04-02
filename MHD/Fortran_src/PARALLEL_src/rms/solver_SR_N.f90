!
!     module solver_SR_N
!
      module solver_SR_N
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by H. Matsui on Sep. 2002 (ver 1.0)
!     Modified by H. Matsui on May. 2007 (ver 1.1)
!
      use calypso_mpi
      use m_precision
!
      use m_RMA_SR
!
      implicit none
!
!      subroutine  SOLVER_SEND_RECV_N                                   &
!     &            (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!     &                                        STACK_EXPORT, NOD_EXPORT,&
!     &             X, SOLVER_COMM,my_rank)
!      subroutine  solver_send_recv_Nx3                                 &
!     &            (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!     &                                        STACK_EXPORT, NOD_EXPORT,&
!     &             X1, X2, X3, SOLVER_COMM,my_rank)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!C
!C*** SOLVER_SEND_RECV
!C
      subroutine  SOLVER_SEND_RECV_N                                    &
     &            (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &             X, SOLVER_COMM,my_rank)
!
! ......................................................................

      integer(kind=kint ), intent(in)   ::  NP, NB
! \beginARG       number of nodes
      integer(kind=kint ), intent(in)   ::  NEIBPETOT
! \beginARG       total neighboring pe count
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
! \beginARG       neighboring pe id                        (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
! \beginARG       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
! \beginARG       imported node                            (i-th dof)
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
! \beginARG       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
! \beginARG       exported node                            (i-th dof)
      real(kind=kreal), intent(inout):: X(NB*NP)
! \beginARG       communicated result vector
      integer, intent(in)   ::SOLVER_COMM
! \beginARG       communicator for mpi
      integer, intent(in)   :: my_rank

      integer(kind = kint) :: neib, istart, inum, ierr, k, ii, ix, nd
      integer(kind = kint) :: import_NB
!
!C    Check array size
!C
      if (iflag_init .eq. 0) call init_work_4_SR                        &
     &       ( NEIBPETOT, NEIBPE, STACK_IMPORT, SOLVER_COMM, my_rank )
      if (iflag_win .lt. (NB*STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_SR
        call init_window_4_SR(NB, NEIBPETOT, STACK_IMPORT)
      end if
!
      call resize_wsend_RMA(NB, STACK_EXPORT(NEIBPETOT))
!
!C
!C-- SEND
      
      do nd = 1, NB
        do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
            ii   = NB * (NOD_EXPORT(k)-1) + nd
            ix   = NB * (k-1) + nd
           WS(ix)= X(ii)
         end do
       end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE,win,ierr)
!      call MPI_WIN_POST(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!      call MPI_WIN_START(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!
      do neib= 1, NEIBPETOT
        istart= NB*STACK_EXPORT(neib-1) + 1
        inum  = NB*(STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        import_NB = NB*import_a(neib) + 1
        call MPI_PUT (WS(istart), inum, MPI_DOUBLE_PRECISION,           &
     &                NEIBPE(neib), import_NB, inum,                    &
     &                MPI_DOUBLE_PRECISION, win, ierr)
!
      enddo
!
!      call MPI_WIN_COMPLETE (win, ierr)
!      call MPI_WIN_wait (win, ierr)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED,win,ierr)
!
      do nd = 1, NB
        do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
          ii   = NB*(NOD_IMPORT(k)-1) + nd
          ix   = NB * (k-1) + nd
          X(ii)= WRecieve(ix)
        end do
      end do

      end subroutine solver_send_recv_N
!
!-----------------------------------------------------------------------
!C
!C*** SOLVER_SEND_RECV
!C
      subroutine  solver_send_recv_Nx3                                  &
     &            (NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &             X1, X2, X3, SOLVER_COMM,my_rank)

! ......................................................................

      integer(kind=kint ), intent(in)   ::  NP, NB
      integer(kind=kint ), intent(in)   ::  NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      real   (kind=kreal), intent(inout):: X1(NB*NP)
      real   (kind=kreal), intent(inout):: X2(NB*NP)
      real   (kind=kreal), intent(inout):: X3(NB*NP)
! \beginARG       communicated result vector
      integer(kind=kint ), intent(in)   ::SOLVER_COMM
! \beginARG       communicator for mpi
      integer(kind=kint ), intent(in)   :: my_rank

      integer (kind = kint) :: neib, istart, inum, ierr, k, ii, ix, nd
      integer(kind = kint) :: import_NB, NB3
!
!C    Check array size
!C
      NB3 = 3*NB
!
      if (iflag_init .eq. 0) call init_work_4_SR                        &
     &       ( NEIBPETOT, NEIBPE, STACK_IMPORT, SOLVER_COMM, my_rank )
      if (iflag_win .lt. (NB3*STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_SR
        call init_window_4_SR(NB3, NEIBPETOT, STACK_IMPORT)
      end if
!
      call resize_wsend_RMA(NB3, STACK_EXPORT(NEIBPETOT))
!C
!C-- SEND
      
      do nd = 1, NB
        do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
            ii   = NB * (NOD_EXPORT(k)-1) + nd
            ix   = NB3 * (k-1) + 3*nd
           WS(ix-2)= X1(ii)
           WS(ix-1)= X2(ii)
           WS(ix  )= X3(ii)
         end do
       end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE,win,ierr)
!      call MPI_WIN_POST(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!      call MPI_WIN_START(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!
      do neib= 1, NEIBPETOT
        istart= NB3*STACK_EXPORT(neib-1)
        inum  = NB3*STACK_EXPORT(neib  ) - istart
        import_NB = NB3*import_a(neib) + 1
        call MPI_PUT (WS(istart+1), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), import_NB, inum,                  &
     &                  MPI_DOUBLE_PRECISION, win, ierr)
!
      enddo
!
!      call MPI_WIN_COMPLETE (win, ierr)
!      call MPI_WIN_wait (win, ierr)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED,win,ierr)
!
      do nd = 1, NB
        do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
          ii   = NB*(NOD_IMPORT(k)-1) + nd
          ix   = NB3 * (k-1) + 3*nd
          X1(ii)= WRecieve(ix-2)
          X2(ii)= WRecieve(ix-1)
          X3(ii)= WRecieve(ix  )
        end do
      end do

      end subroutine solver_send_recv_Nx3
!
!-----------------------------------------------------------------------
!
      end module solver_SR_N

