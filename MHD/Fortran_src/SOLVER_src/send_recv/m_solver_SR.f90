!C*** 
!C*** module m_solver_SR
!C***
!
!      Modified by H. Matsui on July, 2007
!      Modified by H. Matsui on Aug., 2007
!
!      subroutine resize_work_4_SR(nb, NEIBPETOT, ntot_send, ntot_recv)
!      subroutine resize_iwork_4_SR(NEIBPETOT, ntot_send, ntot_recv)
!
!      subroutine resize_work_itp_SR(nb, npe_send, npe_recv, ntot_recv)
!      subroutine resize_iwork_itp_SR(npe_send, npe_recv, ntot_recv)
!
!      subroutine resize_work_sph_SR(nb, npe_send, npe_recv,            &
!     &          ntot_send, ntot_recv)
!      subroutine resize_iwork_sph_SR(npe_send, npe_recv,               &
!     &          ntot_send, ntot_recv)
!
      module m_solver_SR
!
      use m_precision
!
      implicit none
!
      integer, save, allocatable :: sta1(:,:)
!       status flag for sending
      integer, save, allocatable :: sta2(:,:)
!       status flag for recieving
      integer, save, allocatable :: req1(:  )
!       status flag for sending
      integer, save, allocatable :: req2(:  )
!       status flag for recieving
!
!
      real(kind = kreal), allocatable :: WS(:)
!       work array for sending
      real(kind = kreal), allocatable :: WR(:)
!       work array for recieving
!
      integer(kind = kint), allocatable :: iWS(:)
!       work array for integer sending
      integer(kind = kint), allocatable :: iWR(:)
!       work array for integer recieving
!
!
      integer(kind = kint) :: iflag_snd_flags = -1
      integer(kind = kint) :: iflag_rcv_flags = -1
!
      integer(kind = kint) :: iflag_ws =  -1
      integer(kind = kint) :: iflag_wr =  -1
      integer(kind = kint) :: iflag_iws = -1
      integer(kind = kint) :: iflag_iwr = -1
!
      private :: iflag_snd_flags, iflag_rcv_flags
      private :: iflag_ws, iflag_wr, iflag_iws, iflag_iwr
!
      private :: resize_flag_4_SR
      private :: resize_wsend_SR, resize_wrecv_SR
      private :: resize_isend_SR, resize_irecv_SR
!
      private :: allocate_sendflag_4_SR, deallocate_sendflag_4_SR
      private :: allocate_recvflag_4_SR, deallocate_recvflag_4_SR
      private :: allocate_wsend_SR,  deallocate_wsend_SR
      private :: allocate_wrecv_SR,  deallocate_wrecv_SR
      private :: allocate_isend_SR,  deallocate_isend_SR
      private :: allocate_irecv_SR,  deallocate_irecv_SR
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_work_4_SR(nb, NEIBPETOT, ntot_send, ntot_recv)
!
      integer(kind = kint), intent(in) ::  NEIBPETOT
      integer(kind = kint), intent(in) ::  nb, ntot_send, ntot_recv
!
!
      call resize_flag_4_SR(NEIBPETOT, NEIBPETOT)
      call resize_wsend_SR(nb, ntot_send)
      call resize_wrecv_SR(nb, ntot_recv)
!
      end subroutine resize_work_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_iwork_4_SR(NEIBPETOT, ntot_send, ntot_recv)
!
      integer(kind = kint), intent(in) ::  NEIBPETOT
      integer(kind = kint), intent(in) ::  ntot_send, ntot_recv
!
!
      call resize_flag_4_SR( NEIBPETOT, NEIBPETOT )
      call resize_isend_SR(ntot_send)
      call resize_irecv_SR(ntot_recv)
!
      end subroutine resize_iwork_4_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_work_itp_SR(nb, npe_send, npe_recv, ntot_recv)
!
      integer(kind = kint), intent(in) ::  npe_send, npe_recv
      integer(kind = kint), intent(in) ::  nb, ntot_recv
!
!
      call resize_flag_4_SR(npe_send, npe_recv)
      call resize_wrecv_SR(nb, ntot_recv)
!
      end subroutine resize_work_itp_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_iwork_itp_SR(npe_send, npe_recv, ntot_recv)
!
      integer(kind = kint), intent(in) ::  npe_send, npe_recv
      integer(kind = kint), intent(in) ::  ntot_recv
!
!
      call resize_flag_4_SR(npe_send, npe_recv)
      call resize_irecv_SR(ntot_recv)
!
      end subroutine resize_iwork_itp_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_work_sph_SR(nb, npe_send, npe_recv,             &
     &          ntot_send, ntot_recv)
!
      integer(kind = kint), intent(in) ::  npe_send, npe_recv
      integer(kind = kint), intent(in) ::  nb, ntot_send, ntot_recv
!
!
      call resize_flag_4_SR(npe_send, npe_recv)
      call resize_wsend_SR(nb, ntot_send  )
      call resize_wrecv_SR(nb, ntot_recv+1)
!
      end subroutine resize_work_sph_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_iwork_sph_SR(npe_send, npe_recv,                &
     &          ntot_send, ntot_recv)
!
      integer(kind = kint), intent(in) ::  npe_send, npe_recv
      integer(kind = kint), intent(in) ::  ntot_send, ntot_recv
!
!
      call resize_flag_4_SR(npe_send, npe_recv)
      call resize_isend_SR(ntot_send  )
      call resize_irecv_SR(ntot_recv+1)
!
      end subroutine resize_iwork_sph_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_flag_4_SR( npe_send, npe_recv )
!
      integer(kind = kint) , intent(in)   ::  npe_send, npe_recv
!
!
      if (iflag_snd_flags .lt. 0) then
        call allocate_sendflag_4_SR(npe_send)
      else if (iflag_snd_flags .ge. 0                                   &
     &       .and. iflag_snd_flags .lt. npe_send) then
        call deallocate_sendflag_4_SR
        call allocate_sendflag_4_SR(npe_send)
      end if
!
      if (iflag_rcv_flags .lt. 0) then
        call allocate_recvflag_4_SR(npe_recv)
      else if (iflag_rcv_flags .ge. 0                                   &
     &       .and. iflag_rcv_flags .lt. npe_recv) then
        call deallocate_recvflag_4_SR
        call allocate_recvflag_4_SR(npe_recv)
      end if
!
      end subroutine resize_flag_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_wsend_SR(nb, ntot_send)
!
      integer(kind=kint), intent(in)   ::  nb, ntot_send
!
      if (iflag_ws .lt. 0) then
        call allocate_wsend_SR(nb, ntot_send)
!
      else if (iflag_ws .ge. 0                                          &
     &       .and. iflag_ws .lt. (nb*ntot_send) ) then
        call deallocate_wsend_SR
        call allocate_wsend_SR(nb, ntot_send)
!
      end if
!
      end subroutine resize_wsend_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_wrecv_SR(nb, ntot_recv)
!
      integer(kind=kint), intent(in) ::  nb, ntot_recv
!
      if (iflag_wr .lt. 0) then
        call allocate_wrecv_SR(nb, ntot_recv)
!
      else if (iflag_wr .ge. 0                                          &
     &       .and. iflag_wr .lt. (nb*ntot_recv) ) then
        call deallocate_wrecv_SR
        call allocate_wrecv_SR(nb, ntot_recv)
!
      end if
!
      end subroutine resize_wrecv_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_isend_SR(ntot_send)
!
      integer(kind=kint), intent(in) :: ntot_send
!
      if (iflag_iws .lt. 0) then
        call allocate_isend_SR(ntot_send)
!
      else if (iflag_iws .ge. 0                                         &
     &       .and. iflag_iws .lt. ntot_send ) then
        call deallocate_isend_SR
        call allocate_isend_SR(ntot_send)
!
      end if
!
      end subroutine resize_isend_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_irecv_SR(ntot_recv)
!
      integer(kind=kint), intent(in) :: ntot_recv
!
      if (iflag_iwr .lt. 0) then
        call allocate_irecv_SR(ntot_recv)
!
      else if (iflag_iwr .ge. 0                                         &
     &       .and. iflag_iwr .lt. ntot_recv ) then
        call deallocate_irecv_SR
        call allocate_irecv_SR(ntot_recv)
!
      end if
!
      end subroutine resize_irecv_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine allocate_sendflag_4_SR( npe_send )
!
      use calypso_mpi
!
!
      integer(kind=kint ) , intent(in)   ::  npe_send
!      num. total neighboring domains
!C
!C-- INIT.
        allocate (sta1(MPI_STATUS_SIZE,npe_send))
        allocate (req1(npe_send))
!
        iflag_snd_flags = npe_send
!
       end subroutine allocate_sendflag_4_SR
!
! ----------------------------------------------------------------------
!
       subroutine allocate_recvflag_4_SR( npe_recv )
!
      use calypso_mpi
!
!
      integer(kind=kint ) , intent(in)   ::  npe_recv
!      num. total neighboring domains
!C
!C-- INIT.
        allocate (sta2(MPI_STATUS_SIZE,npe_recv))
        allocate (req2(npe_recv))
!
        iflag_rcv_flags = npe_recv
!
       end subroutine allocate_recvflag_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine allocate_wsend_SR(nb, ntot_send)
!
      integer(kind=kint), intent(in)   ::  nb, ntot_send
!
      iflag_ws = nb * ntot_send
      allocate (WS(iflag_ws))
!
      end subroutine allocate_wsend_SR
!
! ----------------------------------------------------------------------
!
      subroutine allocate_wrecv_SR(nb, ntot_recv)
!
      integer(kind=kint), intent(in) ::  nb, ntot_recv
!
      iflag_wr = nb * ntot_recv
      allocate (WR(iflag_wr))
!
      end subroutine allocate_wrecv_SR
!
! ----------------------------------------------------------------------
!
      subroutine allocate_isend_SR(ntot_send)
!
      integer(kind=kint), intent(in) :: ntot_send
!
      iflag_iws = ntot_send
      allocate (iWS(iflag_iws))
!
      end subroutine allocate_isend_SR
!
! ----------------------------------------------------------------------
!
      subroutine allocate_irecv_SR(ntot_recv)
!
      integer(kind=kint), intent(in) :: ntot_recv
!
      iflag_iwr = ntot_recv
      allocate (iWR(iflag_iwr))
!
      end subroutine allocate_irecv_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine deallocate_sendflag_4_SR
!
        deallocate (sta1, req1)
!
       end subroutine deallocate_sendflag_4_SR
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_recvflag_4_SR
!
        deallocate (sta2, req2)
!
       end subroutine deallocate_recvflag_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_wsend_SR
!
      deallocate (WS)
      iflag_ws = -1
!
      end subroutine deallocate_wsend_SR
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_wrecv_SR
!
      deallocate (WR)
      iflag_wr = -1
!
      end subroutine deallocate_wrecv_SR
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_isend_SR
!
      deallocate (iWS)
      iflag_iws = -1
!
      end subroutine deallocate_isend_SR
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_irecv_SR
!
      deallocate (iWR)
      iflag_iwr = -1
!
      end subroutine deallocate_irecv_SR
!
! ----------------------------------------------------------------------
!
      end module m_solver_SR
