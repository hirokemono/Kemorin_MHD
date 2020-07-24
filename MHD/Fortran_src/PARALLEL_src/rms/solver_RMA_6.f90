!
!     module solver_RMA_6
!
!    MPI SEND and RECEIVE routine for overlapped partitioning
!     coded by H. Matsui on Sep. 2002 (ver 1.0)
!     Modified by H. Matsui on May. 2007 (ver 1.1)
!
!!      subroutine  SOLVER_REMOTE_ACCESS_6(NP, NEIBPETOT, NEIBPE,       &
!!     &                                   STACK_IMPORT, NOD_IMPORT,    &
!!     &                                   STACK_EXPORT, NOD_EXPORT,    &
!!     &                                   RMA_r, X)
!
      module solver_RMA_6
!
      use calypso_mpi
      use m_precision
      use m_constants
!
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
!C*** SOLVER_REMOTE_ACCESS_6
!C
      subroutine  SOLVER_REMOTE_ACCESS_6(NP, NEIBPETOT, NEIBPE,         &
     &                                   STACK_IMPORT, NOD_IMPORT,      &
     &                                   STACK_EXPORT, NOD_EXPORT,      &
     &                                   RMA_r, X)

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
      real   (kind=kreal), intent(inout):: X(isix*NP)
!>      Structure for RMA of real data
      type(RMA_real_buffer), intent(inout) :: RMA_r

      integer(kind = kint) :: neib, istart, inum, k, ii, ix
      integer(kind = MPI_ADDRESS_KIND) :: import_NB
!
!C    Check array size
!C
      if (RMA_r%iflag_win .lt. (isix*STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_RMA(RMA_r)
        call init_window_4_RMA(isix, NEIBPETOT, STACK_IMPORT, RMA_r)
        call init_work_4_RMA                                            &
     &     (NEIBPETOT, NEIBPE, STACK_IMPORT, RMA_r%import_a)
      end if
!
      call resize_wsend_RMA(isix, STACK_EXPORT(NEIBPETOT), RMA_r)
!
!C
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
         ii   = isix * NOD_EXPORT(k)
         ix   = isix * k
        RMA_r%WS_RMA(ix-5)= X(ii-5)
        RMA_r%WS_RMA(ix-4)= X(ii-4)
        RMA_r%WS_RMA(ix-3)= X(ii-3)
        RMA_r%WS_RMA(ix-2)= X(ii-2)
        RMA_r%WS_RMA(ix-1)= X(ii-1)
        RMA_r%WS_RMA(ix  )= X(ii  )
      end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE, RMA_r%win, ierr_MPI)
!      call MPI_WIN_POST(RMA_r%nbr_group, MPI_MODE_NOCHECK,             &
!     &                  RMA_r%win, ierr_MPI)
!      call MPI_WIN_START(RMA_r%nbr_group, MPI_MODE_NOCHECK,            &
!     &                   RMA_r%win, ierr_MPI)
!
      do neib= 1, NEIBPETOT
        istart= isix*STACK_EXPORT(neib-1) + 1
        inum  = isix*(STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        import_NB = isix*RMA_i%import_a(neib) + 1
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
        ii   = isix * NOD_IMPORT(k)
        ix   = isix * k
        X(ii-5) = RMA_r%WRecieve(ix-5)
        X(ii-4) = RMA_r%WRecieve(ix-4)
        X(ii-3) = RMA_r%WRecieve(ix-3)
        X(ii-2) = RMA_r%WRecieve(ix-2)
        X(ii-1) = RMA_r%WRecieve(ix-1)
        X(ii  ) = RMA_r%WRecieve(ix  )
      end do

      end subroutine SOLVER_REMOTE_ACCESS_6
!
!-----------------------------------------------------------------------
!
      end module solver_RMA_6

