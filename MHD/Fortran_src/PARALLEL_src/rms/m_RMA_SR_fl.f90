!
!C*** 
!C*** module m_RMA_SR_fl
!C***
!
!      Written by H. Matsui on May, 2007
!
!      subroutine init_work_4_SR_fl                                     &
!     &       ( NEIBPETOT, NEIBPE, STACK_IMPORT, CALYPSO_COMM )
!      subroutine init_window_4_SR_fl(NB, NEIBPETOT, STACK_IMPORT)
!      subroutine init_window_4_SR_int_fl(NB, NEIBPETOT, STACK_IMPORT)
!
!      subroutine delete_window_4_SR_fl
!      subroutine delete_window_4_SR_int_fl
!      subroutine deallocate_import_table_fl
!
!
      module m_RMA_SR_fl
!
      use calypso_mpi
      use m_precision
!
      implicit none
!
      integer(kind=MPI_ADDRESS_KIND), save, allocatable :: import_a(:)
! \beginARG       work array for communication (wait)
      integer(kind=MPI_ADDRESS_KIND)  :: size_window
!
      real(kind = kreal), allocatable:: WRecieve(:)
! \beginARG       work array for communication (receive)
      integer(kind=kint ), allocatable:: iWRecieve(:)
! \beginARG       work array for communication (receive)
!
      integer, save :: win, iwin
      integer, save :: group, igroup
      integer, save :: nbr_group, inbr_group
!        window name
!
      integer(kind = kint) :: iflag_init = 0
      integer(kind = kint) :: iflag_win =  0
      integer(kind = kint) :: iflag_iwin =  0
!
      private :: iflag_init
      private :: size_window
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine init_work_4_SR_fl(NEIBPETOT, NEIBPE, STACK_IMPORT)
!
      integer(kind=kint ), intent(in) ::  NEIBPETOT
!        total neighboring pe count
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
!        neighboring pe id                        (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!        imported node count for each neighbor pe (i-th pe)
!
!
      integer, allocatable :: tmp_stack(:)
! \beginARG       work array
      integer, allocatable :: sta1(:,:), sta2(:,:)
! \beginARG       work array for communication (wait)
      integer, allocatable :: req1(:  ), req2(:  )  
! \beginARG       work array for communication (wait)
!C
!C
!C-- INIT...... only for th first time
      allocate (sta1(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (sta2(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (req1(NEIBPETOT))
      allocate (req2(NEIBPETOT))
      allocate (tmp_stack(NEIBPETOT))
      allocate (import_a(NEIBPETOT))
!
!  send address for put
!
      do neib= 1, NEIBPETOT
        call MPI_ISEND(STACK_IMPORT(neib-1), 1, CALYPSO_INTEGER,        &
     &      int(NEIBPE(neib)), 0, CALYPSO_COMM, req1(neib), ierr_MPI)
      enddo
!C
!C-- RECEIVE
!
      do neib= 1, NEIBPETOT
       call MPI_IRECV (tmp_stack(neib), 1, CALYPSO_INTEGER,             &
     &     int(NEIBPE(neib)), 0, CALYPSO_COMM, req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL(int(NEIBPETOT), req2, sta2, ierr_MPI)
      call MPI_WAITALL(int(NEIBPETOT), req1, sta1, ierr_MPI)
!
      do neib= 1, NEIBPETOT
       import_a(neib) = tmp_stack(neib)
      enddo
!
!        call MPI_COMM_GROUP(CALYPSO_COMM, group, ierr_MPI)
!        call MPI_GROUP_INCL(group, NEIBPETOT, NEIBPE, nbr_group, ierr_MPI)
!        call MPI_GROUP_free(group, ierr_MPI)
!
      deallocate( tmp_stack )
      deallocate( sta1, sta2, req1, req2 )
!
      iflag_init = 1
!
      end subroutine init_work_4_SR_fl
!
! ----------------------------------------------------------------------
!
      subroutine init_window_4_SR_fl(NB, NEIBPETOT, STACK_IMPORT)
!
      integer(kind=kint ), intent(in) :: NB
      integer(kind=kint ), intent(in) :: NEIBPETOT
!        total neighboring pe count
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!        imported node count for each neighbor pe (i-th pe)
!
!
      allocate (WRecieve(STACK_IMPORT(NEIBPETOT)))
      size_window = NB*STACK_IMPORT(NEIBPETOT) * kreal
!
      call MPI_WIN_CREATE(WRecieve, size_window, kreal,                 &
     &    MPI_INFO_NULL, CALYPSO_COMM, win, ierr_MPI)
!
      iflag_win = NB*STACK_IMPORT(NEIBPETOT)
!
      end subroutine init_window_4_SR_fl
!
! ----------------------------------------------------------------------
!
      subroutine init_window_4_SR_int_fl(NB, NEIBPETOT, STACK_IMPORT)
!
      integer(kind=kint ), intent(in) :: NB
      integer(kind=kint ), intent(in) :: NEIBPETOT
!        total neighboring pe count
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!        imported node count for each neighbor pe (i-th pe)
!
!
      allocate (iWRecieve(STACK_IMPORT(NEIBPETOT)))
      size_window = NB*STACK_IMPORT(NEIBPETOT) * kint
!
      call MPI_WIN_CREATE(WRecieve, size_window, kint,                  &
     &    MPI_INFO_NULL, CALYPSO_COMM, iwin, ierr_MPI)
!
      iflag_iwin = NB*STACK_IMPORT(NEIBPETOT)
!
      end subroutine init_window_4_SR_int_fl
!
! ----------------------------------------------------------------------
!
      subroutine delete_window_4_SR_fl
!
!
      call MPI_WIN_FREE(win)
      deallocate (WRecieve)
!
      iflag_win = 0
!
      end subroutine delete_window_4_SR_fl
!
! ----------------------------------------------------------------------
!
      subroutine delete_window_4_SR_int_fl
!
!
      call MPI_WIN_FREE(iwin)
      deallocate (iWRecieve)
!
      iflag_iwin = 0
!
      end subroutine delete_window_4_SR_int_fl
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_import_table_fl
!
!
      deallocate( import_a )
      iflag_init = 0
!
      end subroutine deallocate_import_table_fl
!
! ----------------------------------------------------------------------
!
      end module m_RMA_SR_fl
