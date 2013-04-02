!
!C*** 
!C*** module solver_SR_int
!C***
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!     modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!
!      subroutine  solver_send_recv_i                                   &
!     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!     &                                        STACK_EXPORT, NOD_EXPORT,&
!     &                  ix, SOLVER_COMM,my_rank)
!
      module solver_SR_int
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine  solver_send_recv_i                                    &
     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &                  ix, SOLVER_COMM,my_rank)
!
      use calypso_mpi
!
      use m_solver_SR
!
! ......................................................................

      integer(kind=kint )                , intent(in)   ::  N
!<       number of nodes
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
!<       total neighboring pe count
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE
!<       neighboring pe id                        (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
!<       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &        :: NOD_IMPORT
!<       imported node                            (i-th dof)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
!<       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &        :: NOD_EXPORT
!<       exported node                            (i-th dof)
      integer (kind=kint), dimension(N)  , intent(inout):: iX
!<       communicated result vector
      integer                            , intent(in)   ::SOLVER_COMM
!<       communicator for mpi
      integer                            , intent(in)   :: my_rank
!C
!
      integer (kind = kint) :: neib, istart, inum, ierr, k
!
!
      call resize_iwork_4_SR(NEIBPETOT,                                 &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1)
        inum  = STACK_EXPORT(neib  ) - istart
        
        do k= istart+1, istart+inum
           iWS(k)= iX(NOD_EXPORT(k))
        enddo
        call MPI_ISEND (iWS(istart+1), inum, MPI_INTEGER,               &
     &                  NEIBPE(neib), 0, SOLVER_COMM,                   &
     &                  req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
        call MPI_IRECV (iWR(istart+1), inum, MPI_INTEGER,               &
     &                  NEIBPE(neib), 0, SOLVER_COMM,                   &
     &                  req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
      do k= istart+1, istart+inum
        iX(NOD_IMPORT(k))= iWR(k)
      enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv_i
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_int
