!
!     module solver_SR_6
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by H. Matsui on Sep. 2002 (ver 1.0)
!     Modified by H. Matsui on May. 2007 (ver 1.1)
!
!      subroutine  SOLVER_SEND_RECV_6                                   &
!     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,    &
!     &                                    STACK_EXPORT, NOD_EXPORT, X)
!
      module solver_SR_6
!
      use calypso_mpi
      use m_precision
      use m_constants
!
      use m_RMA_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!C
!C*** SOLVER_SEND_RECV_6
!C
      subroutine  SOLVER_SEND_RECV_6                                    &
     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
     &                                    STACK_EXPORT, NOD_EXPORT, X)

! ......................................................................

      integer(kind=kint ), intent(in)   ::  NP
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
      real   (kind=kreal), intent(inout):: X(isix*NP)
! \beginARG       communicated result vector

      integer(kind = kint) :: neib, istart, inum, ierr, k, ii, ix
      integer(kind = kint) :: import_NB
!
!C    Check array size
!C
      if (iflag_init .eq. 0) call init_work_4_SR                        &
     &       ( NEIBPETOT, NEIBPE, STACK_IMPORT )
      if (iflag_win .lt. (isix*STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_SR
        call init_window_4_SR(isix, NEIBPETOT, STACK_IMPORT)
      end if
!
      call resize_wsend_RMA(isix, STACK_EXPORT(NEIBPETOT))
!
!C
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
         ii   = isix * NOD_EXPORT(k)
         ix   = isix * k
        WS(ix-5)= X(ii-5)
        WS(ix-4)= X(ii-4)
        WS(ix-3)= X(ii-3)
        WS(ix-2)= X(ii-2)
        WS(ix-1)= X(ii-1)
        WS(ix  )= X(ii  )
      end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE,win,ierr)
!      call MPI_WIN_POST(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!      call MPI_WIN_START(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!
      do neib= 1, NEIBPETOT
        istart= isix*STACK_EXPORT(neib-1) + 1
        inum  = isix*(STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        import_NB = isix*import_a(neib) + 1
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
      do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
        ii   = isix * NOD_IMPORT(k)
        ix   = isix * k
        X(ii-5)= WRecieve(ix-5)
        X(ii-4)= WRecieve(ix-4)
        X(ii-3)= WRecieve(ix-3)
        X(ii-2)= WRecieve(ix-2)
        X(ii-1)= WRecieve(ix-1)
        X(ii  )= WRecieve(ix  )
      end do

      end subroutine SOLVER_SEND_RECV_6
!
!-----------------------------------------------------------------------
!
      end module solver_SR_6

