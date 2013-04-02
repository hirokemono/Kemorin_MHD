!C*** 
!C*** module solver_SR_3
!C***
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!     modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!
!      subroutine  SOLVER_SEND_RECV_3                                   &
!     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!     &                                        STACK_EXPORT, NOD_EXPORT,&
!     &                  X, SOLVER_COMM,my_rank)
!      subroutine  solver_send_recv_3x3                                 &
!     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!     &                                        STACK_EXPORT, NOD_EXPORT,&
!     &                  X1, X2, X3, SOLVER_COMM,my_rank)
!
      module solver_SR_3
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
!C
!C*** SOLVER_SEND_RECV
!C
      subroutine  SOLVER_SEND_RECV_3                                    &
     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &                  X, SOLVER_COMM,my_rank)

      use calypso_mpi
!
      use m_solver_SR

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
      real   (kind=kreal), dimension(3*N), intent(inout):: X
!<       communicated result vector
      integer                            , intent(in)   ::SOLVER_COMM
!<       communicator for mpi
      integer                            , intent(in)   :: my_rank
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr, k, ii
!
!
      call resize_work_4_SR(ithree, NEIBPETOT,                          &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        
        do k= istart, iend
               ii   = 3*NOD_EXPORT(k)
           WS(3*k-2)= X(ii-2)
           WS(3*k-1)= X(ii-1)
           WS(3*k  )= X(ii  )
        enddo
        istart= 3 * STACK_EXPORT(neib-1) + 1
        inum  = 3 * ( STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
        call MPI_ISEND (WS(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= 3 * STACK_IMPORT(neib-1) + 1
        inum  = 3 * ( STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV (WR(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
      do k= istart, iend
          ii   = 3*NOD_IMPORT(k)
        X(ii-2)= WR(3*k-2)
        X(ii-1)= WR(3*k-1)
        X(ii  )= WR(3*k  )
      enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv_3
!
!  ---------------------------------------------------------------------
!
      subroutine  solver_send_recv_3x3                                  &
     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &                  X1, X2, X3, SOLVER_COMM,my_rank)

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
      real   (kind=kreal), dimension(3*N), intent(inout):: X1
      real   (kind=kreal), dimension(3*N), intent(inout):: X2
      real   (kind=kreal), dimension(3*N), intent(inout):: X3
!<       communicated result vector
      integer                            , intent(in)   ::SOLVER_COMM
!<       communicator for mpi
      integer                            , intent(in)   :: my_rank
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr, k, ii
!
!
      call resize_work_4_SR(inine, NEIBPETOT,                           &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        
        do k= istart, iend
               ii   = 3*NOD_EXPORT(k)
           WS(9*k-8)= X1(ii-2)
           WS(9*k-7)= X1(ii-1)
           WS(9*k-6)= X1(ii  )
           WS(9*k-5)= X2(ii-2)
           WS(9*k-4)= X2(ii-1)
           WS(9*k-3)= X2(ii  )
           WS(9*k-2)= X3(ii-2)
           WS(9*k-1)= X3(ii-1)
           WS(9*k  )= X3(ii  )
        enddo
        istart= 9 * STACK_EXPORT(neib-1) + 1
        inum  = 9 * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        call MPI_ISEND (WS(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= 9 * STACK_IMPORT(neib-1) + 1
        inum  = 9 * ( STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV (WR(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do k= istart, iend
            ii   = 3*NOD_IMPORT(k)
          X1(ii-2)= WR(9*k-8)
          X1(ii-1)= WR(9*k-7)
          X1(ii  )= WR(9*k-6)
          X2(ii-2)= WR(9*k-5)
          X2(ii-1)= WR(9*k-4)
          X2(ii  )= WR(9*k-3)
          X3(ii-2)= WR(9*k-2)
          X3(ii-1)= WR(9*k-1)
          X3(ii  )= WR(9*k  )
        enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)
!
      end subroutine solver_send_recv_3x3
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_3
