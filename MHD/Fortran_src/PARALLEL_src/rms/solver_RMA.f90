!
!     module solver_RMA
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by H. Matsui on Sep. 2002 (ver 1.0)
!     Modified by H. Matsui on May. 2007 (ver 1.1)
!
!      subroutine  SOLVER_REMOTE_ACCESS                                 &
!     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,    &
!     &                                    STACK_EXPORT, NOD_EXPORT, X)
!      subroutine  solver_remote_access_x3                              &
!     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,    &
!     &                                    STACK_EXPORT, NOD_EXPORT,    &
!     &             X1, X2, X3)
!
      module solver_RMA
!
      use calypso_mpi
      use m_precision
!
      use m_constants
      use t_solver_RMA
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
!C*** SOLVER_REMOTE_ACCESS
!C
      subroutine  SOLVER_REMOTE_ACCESS                                  &
     &            (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
     &                                    STACK_EXPORT, NOD_EXPORT,     &
     &             X)
!
! ......................................................................

!>       number of nodes
      integer(kind=kint ), intent(in)   ::  NP
!>       total neighboring pe count
      integer(kind=kint ), intent(in)   ::  NEIBPETOT
!>       neighboring pe id                        (i-th pe)
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
!>       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!>       imported node                            (i-th dof)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
!>       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
!>       exported node                            (i-th dof)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!>       communicated result vector
      real   (kind=kreal), intent(inout):: X(NP)
!>      Structure for RMA of real data
      type(RMA_real_buffer), intent(inout) :: RMA_r

      integer(kind = kint) :: neib, istart, inum, k, ii
      integer(kind = MPI_ADDRESS_KIND) :: import_NB
!
!C    Check array size
!C
      if (RMA_r%iflag_win .lt. (STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_RMA(RMA_r)
        call init_window_4_RMA(ione, NEIBPETOT, STACK_IMPORT, RMA_r)
        call init_work_4_RMA                                            &
     &     (NEIBPETOT, NEIBPE, STACK_IMPORT, RMA_r%import_a)
      end if
!
      call resize_wsend_RMA(ione, STACK_EXPORT(NEIBPETOT), RMA_r)
!
!C
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
        ii   = NOD_EXPORT(k)
        RMA_r%WS_RMA(k  )= X(ii  )
      end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE, RMA_r%win, ierr_MPI)
!      call MPI_WIN_POST(RMA_r%nbr_group, MPI_MODE_NOCHECK,             &
!     &                  RMA_r%win, ierr_MPI)
!      call MPI_WIN_START(RMA_r%nbr_group, MPI_MODE_NOCHECK,            &
!     &                   RMA_r%win, ierr_MPI)
!
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        inum  = (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        import_NB = RMA_i%import_a(neib) + 1
        call MPI_PUT (RMA_r%WS_RMA(istart), inum, CALYPSO_REAL,         &
     &                NEIBPE(neib), import_NB, inum,                    &
     &                CALYPSO_REAL, RMA_r%win, ierr_MPI)
!
      enddo
!
!      call MPI_WIN_COMPLETE (RMA_r%win, ierr_MPI)
!      call MPI_WIN_wait (RMA_r%win, ierr_MPI)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED, RMA_r%win, ierr_MPI)
!
      do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
        ii   = NOD_IMPORT(k)
        X(ii  ) = RMA_r%WRecieve(k  )
      end do

      end subroutine SOLVER_REMOTE_ACCESS
!
!-----------------------------------------------------------------------
!C
!C*** solver_remote_access_x3
!C
      subroutine  solver_remote_access_x3                               &
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
      real   (kind=kreal), intent(inout):: X1(NP)
      real   (kind=kreal), intent(inout):: X2(NP)
      real   (kind=kreal), intent(inout):: X3(NP)
!
      type(RMA_real_buffer), intent(inout) :: RMA_r

      integer (kind = kint) :: neib, istart, inum, k, ii, ix
      integer(kind = MPI_ADDRESS_KIND) :: import_NB
!
!C    Check array size
!C
      if (RMA_r%iflag_win .lt. (ithree*STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_RMA(RMA_r)
        call init_window_4_RMA(ione, NEIBPETOT, STACK_IMPORT, RMA_r)
        call init_work_4_RMA                                            &
     &     (NEIBPETOT, NEIBPE, STACK_IMPORT, RMA_r%import_a)
      end if
!
      call resize_wsend_RMA(ithree, STACK_EXPORT(NEIBPETOT), RMA_r)
!C
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
        ii   = NOD_EXPORT(k)
        ix   = ithree * k
        RMA_r%WS_RMA(ix-2)= X1(ii  )
        RMA_r%WS_RMA(ix-1)= X2(ii  )
        RMA_r%WS_RMA(ix  )= X3(ii  )
      end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE, RMA_r%win, ierr_MPI)
!      call MPI_WIN_POST(RMA_r%nbr_group, MPI_MODE_NOCHECK,             &
!     &                  RMA_r%win, ierr_MPI)
!      call MPI_WIN_START(RMA_r%nbr_group, MPI_MODE_NOCHECK,            &
!     &                   RMA_r%win, ierr_MPI)
!
      do neib= 1, NEIBPETOT
        istart= ithree*STACK_EXPORT(neib-1)
        inum  = ithree*STACK_EXPORT(neib  ) - istart
        import_NB = ithree*RMA_i%import_a(neib) + 1
        call MPI_PUT (RMA_r%WS_RMA(istart+1), inum, CALYPSO_REAL,       &
     &                  NEIBPE(neib), import_NB, inum,                  &
     &                  CALYPSO_REAL, RMA_r%win, ierr_MPI)
!
      enddo
!
!      call MPI_WIN_COMPLETE (RMA_r%win, ierr_MPI)
!      call MPI_WIN_wait (RMA_r%win, ierr_MPI)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED, RMA_r%win, ierr_MPI)
!
      do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
        ii   = NOD_IMPORT(k)
        ix   = ithree * k
        X1(ii  ) = RMA_r%WRecieve(ix-2)
        X2(ii  ) = RMA_r%WRecieve(ix-1)
        X3(ii  ) = RMA_r%WRecieve(ix  )
      end do

      end subroutine solver_remote_access_x3
!
!-----------------------------------------------------------------------
!
      end module solver_RMA

