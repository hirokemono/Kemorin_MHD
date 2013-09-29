!
!     module solver_SR_int
!
      module solver_SR_int
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by H. Matsui on Sep., 2002 (ver 1.0)
!     Modified by H. Matsui on May, 2007 (ver 1.1)
!     Modified by H. Matsui on July, 2007 (ver 1.2)
!
      use calypso_mpi
      use m_precision
      use m_RMA_SR
!
      implicit none
!
!
!      subroutine  solver_send_recv_i                                   &
!     &           (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
!     &                                   STACK_EXPORT, NOD_EXPORT, iX)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!C
!C*** SOLVER_SEND_RECV_i
!C
      subroutine  solver_send_recv_i                                    &
     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,       &
     &                                  STACK_EXPORT, NOD_EXPORT, iX)
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
      integer(kind=kint ), intent(inout):: iX(NP)
! \beginARG       communicated result vector

      integer(kind = kint) :: neib, istart, inum, k, ii
      integer(kind = kint) :: import_NB
!
!C    Check array size
!C
      if (iflag_init .eq. 0) call init_work_4_SR                        &
     &       ( NEIBPETOT, NEIBPE, STACK_IMPORT )
      if (iflag_win .lt. (STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_SR_int
        call init_window_4_SR_int(ione, NEIBPETOT, STACK_IMPORT)
      end if
!
      call resize_isend_RMA( STACK_EXPORT(NEIBPETOT) )
!
!C
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
        ii   = NOD_EXPORT(k)
        iWS(k  )= iX(ii  )
      end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE,iwin,ierr_MPI)
!      call MPI_WIN_POST(inbr_group ,MPI_MODE_NOCHECK , iwin, ierr_MPI)
!      call MPI_WIN_START(inbr_group ,MPI_MODE_NOCHECK , iwin, ierr_MPI)
!
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        inum  = (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        import_NB = import_a(neib) + 1
        call MPI_PUT (iWS(istart), inum, CALYPSO_INTEGER,               &
     &                NEIBPE(neib), import_NB, inum,                    &
     &                CALYPSO_INTEGER, iwin, ierr_MPI)
!
      enddo
!
!      call MPI_WIN_COMPLETE (iwin, ierr_MPI)
!      call MPI_WIN_wait (iwin, ierr_MPI)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED,iwin,ierr_MPI)
!
      do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
        ii   = NOD_IMPORT(k)
        iX(ii  )= iWRecieve(k  )
      end do

      end subroutine solver_send_recv_i
!
!-----------------------------------------------------------------------
!
      end module solver_SR_int

