!
!     module solver_SR_3
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by H. Matsui on Sep. 2002 (ver 1.0)
!     Modified by H. Matsui on May. 2007 (ver 1.1)
!
!      subroutine  SOLVER_SEND_RECV_3                                   &
!     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,    &
!     &                                    STACK_EXPORT, NOD_EXPORT, X)
!      subroutine  solver_send_recv_3x3                                 &
!     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,    &
!     &                                    STACK_EXPORT, NOD_EXPORT,    &
!     &             X1, X2, X3)
!
      module solver_SR_3
!
      use calypso_mpi
      use m_precision
!
      use m_constants
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
!C*** SOLVER_SEND_RECV_3
!C
      subroutine  SOLVER_SEND_RECV_3                                    &
     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
     &                                    STACK_EXPORT, NOD_EXPORT, X)
!
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
      real   (kind=kreal), intent(inout):: X(ithree*NP)
! \beginARG       communicated result vector

      integer(kind = kint) :: neib, istart, inum, ierr, k, ii, ix
      integer(kind = kint) :: import_NB
!
!C    Check array size
!C
      if (iflag_init .eq. 0) call init_work_4_SR                        &
     &       ( NEIBPETOT, NEIBPE, STACK_IMPORT)
      if (iflag_win .lt. (ithree*STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_SR
        call init_window_4_SR(ithree, NEIBPETOT, STACK_IMPORT)
      end if
!
      call resize_wsend_RMA(ithree, STACK_EXPORT(NEIBPETOT))
!
!C
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
        ii   = ithree * NOD_EXPORT(k)
        ix   = ithree * k
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
        istart= ithree*STACK_EXPORT(neib-1) + 1
        inum  = ithree*(STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        import_NB = ithree*import_a(neib) + 1
        call MPI_PUT (WS(istart), inum, CALYPSO_REAL,                   &
     &                NEIBPE(neib), import_NB, inum,                    &
     &                CALYPSO_REAL, win, ierr)
!
      enddo
!
!      call MPI_WIN_COMPLETE (win, ierr)
!      call MPI_WIN_wait (win, ierr)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED,win,ierr)
!
      do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
        ii   = ithree * NOD_IMPORT(k)
        ix   = ithree * k
        X(ii-2)= WRecieve(ix-2)
        X(ii-1)= WRecieve(ix-1)
        X(ii  )= WRecieve(ix  )
      end do

      end subroutine SOLVER_SEND_RECV_3
!
!-----------------------------------------------------------------------
!
      subroutine  solver_send_recv_3x3                                  &
     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
     &                                    STACK_EXPORT, NOD_EXPORT,     &
     &             X1, X2, X3)

! ......................................................................

      integer(kind=kint ), intent(in)   ::  NP
      integer(kind=kint ), intent(in)   ::  NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      real   (kind=kreal), intent(inout):: X1(ithree*NP)
      real   (kind=kreal), intent(inout):: X2(ithree*NP)
      real   (kind=kreal), intent(inout):: X3(ithree*NP)
! \beginARG       communicated result vector

      integer (kind = kint) :: neib, istart, inum, ierr, k, ii, ix
      integer(kind = kint) :: import_NB
!
!C    Check array size
!C
      if (iflag_init .eq. 0) call init_work_4_SR                        &
     &       ( NEIBPETOT, NEIBPE, STACK_IMPORT )
      if (iflag_win .lt. (inine*STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_SR
        call init_window_4_SR(inine, NEIBPETOT, STACK_IMPORT)
      end if
!C
      call resize_wsend_RMA(ithree,STACK_EXPORT(NEIBPETOT))
!
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
        ii   = ithree * NOD_EXPORT(k)
        ix   = inine * k
         WS(ix-8)= X1(ii-2)
         WS(ix-7)= X2(ii-2)
         WS(ix-6)= X3(ii-2)
         WS(ix-5)= X1(ii-1)
         WS(ix-4)= X2(ii-1)
         WS(ix-3)= X3(ii-1)
         WS(ix-2)= X1(ii  )
         WS(ix-1)= X2(ii  )
         WS(ix  )= X3(ii  )
      end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE,win,ierr)
!      call MPI_WIN_POST(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!      call MPI_WIN_START(nbr_group ,MPI_MODE_NOCHECK , win, ierr)
!
      do neib= 1, NEIBPETOT
        istart= inine*STACK_EXPORT(neib-1)
        inum  = inine*STACK_EXPORT(neib  ) - istart
        import_NB = inine*import_a(neib) + 1
        call MPI_PUT (WS(istart+1), inum, CALYPSO_REAL,                 &
     &                  NEIBPE(neib), import_NB, inum,                  &
     &                  CALYPSO_REAL, win, ierr)
!
      enddo
!
!      call MPI_WIN_COMPLETE (win, ierr)
!      call MPI_WIN_wait (win, ierr)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED,win,ierr)
!
      do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
        ii   = ithree * NOD_IMPORT(k)
        ix   = inine * k
        X1(ii-2)= WRecieve(ix-8)
        X2(ii-2)= WRecieve(ix-7)
        X3(ii-2)= WRecieve(ix-6)
        X1(ii-1)= WRecieve(ix-5)
        X2(ii-1)= WRecieve(ix-4)
        X3(ii-1)= WRecieve(ix-3)
        X1(ii  )= WRecieve(ix-2)
        X2(ii  )= WRecieve(ix-1)
        X3(ii  )= WRecieve(ix  )
      end do

      end subroutine solver_send_recv_3x3
!
!-----------------------------------------------------------------------
!
      end module solver_SR_3

