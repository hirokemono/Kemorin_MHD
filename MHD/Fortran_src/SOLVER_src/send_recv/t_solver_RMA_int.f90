!>@file   t_solver_RMA_int.f90
!!@brief  module t_solver_RMA_int
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Work area for data communications by RMA
!!
!!@verbatim
!!      subroutine  solver_remote_access_i(NP, NEIBPETOT, NEIBPE,       &
!!     &                               STACK_IMPORT, NOD_IMPORT,        &
!!     &                               STACK_EXPORT, NOD_EXPORT,        &
!!     &                               RMA_i, iX)
!!
!!      subroutine init_window_4_RMA_int(NEIBPETOT, STACK_IMPORT, RMA_i)
!!        type(RMA_int_buffer), intent(inout) :: RMA_i
!!
!!      subroutine delete_window_4_RMA_int(RMA_i)
!!        type(RMA_int_buffer), intent(inout) :: RMA_i
!!
!!      subroutine resize_isend_RMA(ntot_send, RMA_i)
!!        type(RMA_int_buffer), intent(inout) :: RMA_i
!!@endverbatim
!
!
      module t_solver_RMA_int
!
      use calypso_mpi
      use m_precision
!
      implicit none
!
!>      Structure for RMA of integer data
      type RMA_int_buffer
!>        Import table for RMA
        integer(kind=MPI_ADDRESS_KIND), allocatable :: import_a(:)
!
!>        Integer flag to initialized send buffer
        integer(kind = kint) :: iflag_iws =  -1
!>        work array for communication (send)
        integer(kind=kint ), allocatable:: iWS_RMA(:)
!>        Integer flag to initialized recieve buffer
        integer(kind = kint) :: iflag_iwin =  0
!>        work array for communication (receive)
        integer(kind=kint ), allocatable:: iWRecieve(:)
!
!>        Window flag
        integer :: iwin
!>        Window flag
        integer :: inbr_group
      end type RMA_int_buffer
!
      private :: allocate_isend_RMA
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine  solver_remote_access_i(NP, NEIBPETOT, NEIBPE,         &
     &                               STACK_IMPORT, NOD_IMPORT,          &
     &                               STACK_EXPORT, NOD_EXPORT,          &
     &                               RMA_i, iX)
!
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
      integer(kind=kint ), intent(inout):: iX(NP)

!>      Structure for integer RMA
      type(RMA_int_buffer), intent(inout) :: RMA_i

      integer(kind = kint) :: neib, istart, inum, k, ii
      integer(kind = MPI_ADDRESS_KIND) :: import_NB
!
!C    Check array size
!C
      if (RMA_i%iflag_iwin .lt. (STACK_IMPORT(NEIBPETOT)) ) then
        call delete_window_4_RMA_int(RMA_i)
        call init_window_4_RMA_int(NEIBPETOT, STACK_IMPORT, RMA_i)
        call init_work_4_RMA                                            &
     &     (NEIBPETOT, NEIBPE, STACK_IMPORT, RMA_i%import_a)
      end if
!
      call resize_isend_RMA(STACK_EXPORT(NEIBPETOT), RMA_i)
!
!C
!C-- SEND
      
      do k= STACK_EXPORT(0)+1, STACK_EXPORT(NEIBPETOT)
        ii   = NOD_EXPORT(k)
        RMA_i%iWS_RMA(k  )= iX(ii  )
      end do
!
      call MPI_WIN_FENCE(MPI_MODE_NOPRECEDE, RMA_i%iwin,ierr_MPI)
!      call MPI_WIN_POST                                                &
!     &   (RMA_i%inbr_group, MPI_MODE_NOCHECK, RMA_i%iwin, ierr_MPI)
!      call MPI_WIN_START                                               &
!     &   (RMA_i%inbr_group, MPI_MODE_NOCHECK, RMA_i%iwin, ierr_MPI)
!
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        inum  = (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        import_NB = RMA_i%import_a(neib) + 1
        call MPI_PUT (RMA_i%iWS_RMA(istart), inum, CALYPSO_INTEGER,     &
     &                NEIBPE(neib), import_NB, inum,                    &
     &                CALYPSO_INTEGER, RMA_i%iwin, ierr_MPI)
!
      enddo
!
!      call MPI_WIN_COMPLETE(RMA_i%iwin, ierr_MPI)
!      call MPI_WIN_wait(RMA_i%iwin, ierr_MPI)
      call MPI_WIN_FENCE(MPI_MODE_NOSUCCEED, RMA_i%iwin, ierr_MPI)
!
      do k= STACK_IMPORT(0)+1, STACK_IMPORT(NEIBPETOT)
        ii   = NOD_IMPORT(k)
        iX(ii  ) = RMA_i%iWRecieve(k  )
      end do

      end subroutine solver_remote_access_i
!
!-----------------------------------------------------------------------
!
      subroutine init_window_4_RMA_int(NEIBPETOT, STACK_IMPORT, RMA_i)
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!
      type(RMA_int_buffer), intent(inout) :: RMA_i
!
      integer(kind=MPI_ADDRESS_KIND) :: size_window
!
!
      allocate(RMA_i%import_a(NEIBPETOT))
      allocate(RMA_i%iWRecieve(STACK_IMPORT(NEIBPETOT)))
      size_window = STACK_IMPORT(NEIBPETOT) * kint
!
      call MPI_WIN_CREATE(RMA_i%iWRecieve, size_window, kint,           &
     &    MPI_INFO_NULL, CALYPSO_COMM, RMA_i%iwin, ierr_MPI)
!
      RMA_i%iflag_iwin = STACK_IMPORT(NEIBPETOT)
!
      end subroutine init_window_4_RMA_int
!
! ----------------------------------------------------------------------
!
      subroutine delete_window_4_RMA_int(RMA_i)
!
      type(RMA_int_buffer), intent(inout) :: RMA_i
!
      call MPI_WIN_FREE(RMA_i%iwin, ierr_MPI)
      deallocate(RMA_i%iWRecieve, RMA_i%import_a)
!
      RMA_i%iflag_iwin = 0
!
      end subroutine delete_window_4_RMA_int
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_isend_RMA(ntot_send, RMA_i)
!
      integer(kind=kint), intent(in) :: ntot_send
      type(RMA_int_buffer), intent(inout) :: RMA_i
!
      if (RMA_i%iflag_iws .lt. 0) then
        call allocate_isend_RMA(ntot_send, RMA_i)
!
      else if (RMA_i%iflag_iws .ge. 0                                   &
     &       .and. RMA_i%iflag_iws .lt. ntot_send ) then
        call dealloc_isend_RMA(RMA_i)
        call allocate_isend_RMA(ntot_send, RMA_i)
      end if
!
      end subroutine resize_isend_RMA
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_isend_RMA(ntot_send, RMA_i)
!
      integer(kind=kint), intent(in) :: ntot_send
      type(RMA_int_buffer), intent(inout) :: RMA_i
!
      RMA_i%iflag_iws = ntot_send
      allocate(RMA_i%iWS_RMA(RMA_i%iflag_iws))
!
      end subroutine allocate_isend_RMA
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_isend_RMA(RMA_i)
!
      type(RMA_int_buffer), intent(inout) :: RMA_i
!
!
      deallocate(RMA_i%iWS_RMA)
      RMA_i%iflag_iws = -1
!
      end subroutine dealloc_isend_RMA
!
! ----------------------------------------------------------------------
!
      end module t_solver_RMA_int
