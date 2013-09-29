!
!C*** 
!C*** module m_RMA_SR
!C***
!
!      Written by H. Matsui on May, 2007
!
!      subroutine init_work_4_SR( NEIBPETOT, NEIBPE, STACK_IMPORT)
!      subroutine init_window_4_SR(NB, NEIBPETOT, STACK_IMPORT)
!      subroutine init_window_4_SR_int(NB, NEIBPETOT, STACK_IMPORT)
!
!      subroutine delete_window_4_SR
!      subroutine delete_window_4_SR_int
!      subroutine deallocate_import_table
!
!      subroutine resize_wsend_RMA(nb, ntot_send)
!      subroutine resize_isend_RMA(ntot_send)
!
!
      module m_RMA_SR
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
      real(kind = kreal), allocatable:: WS(:)
! \beginARG       work array for communication (send)
      real(kind = kreal), allocatable:: WRecieve(:)
! \beginARG       work array for communication (receive)
      integer(kind=kint ), allocatable:: iWS(:)
! \beginARG       work array for communication (receive)
      integer(kind=kint ), allocatable:: iWRecieve(send)
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
      integer(kind = kint) :: iflag_ws =  -1
      integer(kind = kint) :: iflag_iws = -1
!
      private :: iflag_init
      private :: size_window
      private :: iflag_ws, iflag_iws
      private :: allocate_wsend_RMA, deallocate_wsend_RMA
      private :: allocate_isend_RMA, deallocate_isend_RMA
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine init_work_4_SR( NEIBPETOT, NEIBPE, STACK_IMPORT )
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
        call MPI_ISEND (STACK_IMPORT(neib-1), 1, CALYPSO_INTEGER,       &
     &      NEIBPE(neib), 0, CALYPSO_COMM, req1(neib), ierr_MPI)
      enddo
!C
!C-- RECEIVE
!
      do neib= 1, NEIBPETOT
       call MPI_IRECV (tmp_stack(neib), 1, CALYPSO_INTEGER,             &
     &     NEIBPE(neib), 0, CALYPSO_COMM, req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2, sta2, ierr_MPI)
      call MPI_WAITALL (NEIBPETOT, req1, sta1, ierr_MPI)
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
      end subroutine init_work_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine init_window_4_SR(NB, NEIBPETOT, STACK_IMPORT)
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
      end subroutine init_window_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine init_window_4_SR_int(NB, NEIBPETOT, STACK_IMPORT)
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
      end subroutine init_window_4_SR_int
!
! ----------------------------------------------------------------------
!
      subroutine delete_window_4_SR
!
!
      call MPI_WIN_FREE(win)
      deallocate (WRecieve)
!
      iflag_win = 0
!
      end subroutine delete_window_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine delete_window_4_SR_int
!
!
      call MPI_WIN_FREE(iwin)
      deallocate (iWRecieve)
!
      iflag_iwin = 0
!
      end subroutine delete_window_4_SR_int
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_import_table
!
!
      deallocate( import_a )
      iflag_init = 0
!
      end subroutine deallocate_import_table
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_wsend_RMA(nb, ntot_send)
!
      integer(kind=kint), intent(in)   ::  nb, ntot_send
!
      if (iflag_ws .lt. 0) then
        call allocate_wsend_RMA(nb, ntot_send)
!
      else if (iflag_ws .ge. 0                                          &
     &       .and. iflag_ws .lt. (nb*ntot_send) ) then
        call deallocate_wsend_RMA
        call allocate_wsend_RMA(nb, ntot_send)
!
      end if
!
      end subroutine resize_wsend_RMA
!
! ----------------------------------------------------------------------
!
      subroutine resize_isend_RMA(ntot_send)
!
      integer(kind=kint), intent(in) :: ntot_send
!
      if (iflag_iws .lt. 0) then
        call allocate_isend_RMA(ntot_send)
!
      else if (iflag_iws .ge. 0                                         &
     &       .and. iflag_iws .lt. ntot_send ) then
        call deallocate_isend_RMA
        call allocate_isend_RMA(ntot_send)
!
      end if
!
      end subroutine resize_isend_RMA
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_wsend_RMA(nb, ntot_send)
!
      integer(kind=kint), intent(in)   ::  nb, ntot_send
!
      iflag_ws = nb * ntot_send
      allocate (WS(iflag_ws))
!
      end subroutine allocate_wsend_RMA
!
! ----------------------------------------------------------------------
!
      subroutine allocate_isend_RMA(ntot_send)
!
      integer(kind=kint), intent(in) :: ntot_send
!
      iflag_iws = ntot_send
      allocate (iWS(iflag_iws))
!
      end subroutine allocate_isend_RMA
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_wsend_RMA
!
      deallocate (WS)
      iflag_ws = -1
!
      end subroutine deallocate_wsend_RMA
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_isend_RMA
!
      deallocate (iWS)
      iflag_iws = -1
!
      end subroutine deallocate_isend_RMA
!
! ----------------------------------------------------------------------
!
      end module m_RMA_SR
