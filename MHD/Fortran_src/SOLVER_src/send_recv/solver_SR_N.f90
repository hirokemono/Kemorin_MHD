!C*** 
!C*** module solver_SR_N
!C***
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!     modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!
!      subroutine  SOLVER_SEND_RECV_N                                   &
!     &            ( N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!     &                                        STACK_EXPORT, NOD_EXPORT,&
!     &              X, SOLVER_COMM,my_rank)
!      subroutine  SOLVER_SEND_RECV_Nx3                                 &
!     &            ( N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!     &                                        STACK_EXPORT, NOD_EXPORT,&
!     &              X1, X2, X3, SOLVER_COMM,my_rank)
!
      module solver_SR_N
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
!C
!C*** SOLVER_SEND_RECV
!C
      subroutine  SOLVER_SEND_RECV_N                                    &
     &            ( N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &              X, SOLVER_COMM, my_rank)
!
      use calypso_mpi
!
      use m_solver_SR
!
! ......................................................................

      integer(kind=kint )                , intent(in)   ::  N, NB
!<       number of nodes
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
!<       total neighboring pe count
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
!<       neighboring pe id                        (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!<       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
!<       imported node                            (i-th dof)
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
!<       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!<       exported node                            (i-th dof)
      real   (kind=kreal), intent(inout):: X(NB*N)
!<       communicated result vector
      integer                            , intent(in)   ::SOLVER_COMM
!<       communicator for mpi
      integer                            , intent(in)   :: my_rank
!
      integer (kind = kint) :: neib, istart, inum, iend
      integer (kind = kint) :: ierr, k, ii, ix, nd
!
!
      call resize_work_4_SR(NB, NEIBPETOT,                              &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
                 ii   = NB * (NOD_EXPORT(k)-1) + nd
                 ix   = NB * (k-1) + nd
             WS(ix)= X(ii)
           end do
         end do
!
        istart= NB *  STACK_EXPORT(neib-1) + 1
        inum  = NB * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
        call MPI_ISEND(WS(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= NB *  STACK_IMPORT(neib-1) + 1
        inum  = NB * (STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                 NEIBPE(neib), 0, SOLVER_COMM, req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
            ii   = NB * (NOD_IMPORT(k)-1) + nd
            ix   = NB * (k-1) + nd
            X(ii)= WR(ix)
          end do
        enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine  SOLVER_SEND_RECV_Nx3                                  &
     &            ( N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &              X1, X2, X3, SOLVER_COMM, my_rank)

      use calypso_mpi
!
      use m_solver_SR
!
! ......................................................................

      integer(kind=kint )                , intent(in)   ::  N, NB
!<       number of nodes
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
!<       total neighboring pe count
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
!<       neighboring pe id                        (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!<       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
!<       imported node                            (i-th dof)
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
!<       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!<       exported node                            (i-th dof)
      real   (kind=kreal), intent(inout):: X1(NB*N)
      real   (kind=kreal), intent(inout):: X2(NB*N)
      real   (kind=kreal), intent(inout):: X3(NB*N)
!<       communicated result vector
      integer                            , intent(in)   ::SOLVER_COMM
!<       communicator for mpi
      integer                            , intent(in)   :: my_rank
!
      integer (kind = kint) :: neib, istart, inum, iend
      integer (kind = kint) :: ierr, k, ii, ix, nd, NB3
!
!
      NB3 = 3 * NB
      call resize_work_4_SR(NB3, NEIBPETOT,                             &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
                 ii   = NB * (NOD_EXPORT(k)-1) + nd
                 ix   = 3*NB * (k-1) + 3*nd
             WS(ix-2)= X1(ii)
             WS(ix-1)= X2(ii)
             WS(ix  )= X3(ii)
           end do
         end do
!
        istart= 3*NB *  STACK_EXPORT(neib-1) + 1
        inum  = 3*NB * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
        call MPI_ISEND(WS(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= 3*NB *  STACK_IMPORT(neib-1) + 1
        inum  = 3*NB * (STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                 NEIBPE(neib), 0, SOLVER_COMM, req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
            ii   = NB * (NOD_IMPORT(k)-1) + nd
            ix   = 3*NB * (k-1) + 3*nd
            X1(ii)= WR(ix-2)
            X2(ii)= WR(ix-1)
            X3(ii)= WR(ix  )
          end do
        enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv_Nx3
!
! ----------------------------------------------------------------------
!
      end module     solver_SR_N
