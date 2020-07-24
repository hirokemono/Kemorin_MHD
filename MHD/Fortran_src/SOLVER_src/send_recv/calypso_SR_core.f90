!>@file   calypso_SR_core.f90
!!@brief  module calypso_SR_core
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine calypso_send_recv_core                               &
!!     &         (NB, npe_send, isend_self, id_pe_send, istack_send,    &
!!     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!!
!!      subroutine calypso_send_recv_check                              &
!!     &         (NB, npe_send, isend_self, istack_send,                &
!!     &              npe_recv, irecv_self, istack_recv)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(istack_send(npe_send))
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!
      module calypso_SR_core
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_send_recv_core                                 &
     &         (NB, npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer (kind = kint) :: ist
      integer :: ncomm_send, ncomm_recv, neib
      integer :: num, i
      integer (kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = int(npe_send - isend_self)
      ncomm_recv = int(npe_recv - irecv_self)
!
      do neib = 1, ncomm_send
        ist = NB * istack_send(neib-1) + 1
        num = int(NB * (istack_send(neib  ) - istack_send(neib-1)))
        call MPI_ISEND                                                  &
     &      (SR_r1%WS(ist), num, CALYPSO_REAL, int(id_pe_send(neib)),   &
     &       0, CALYPSO_COMM, SR_sig1%req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= NB * istack_recv(neib-1) + 1
          num  = int(NB * (istack_recv(neib  ) - istack_recv(neib-1)))
          call MPI_IRECV                                                &
     &       (SR_r1%WR(ist), num, CALYPSO_REAL, int(id_pe_recv(neib)),  &
     &        0, CALYPSO_COMM, SR_sig1%req2(neib), ierr_MPI)
        end do
      end if
!
      if(ncomm_recv .gt. 0) then
        call MPI_WAITALL                                                &
     &     (ncomm_recv, SR_sig1%req2, SR_sig1%sta2, ierr_MPI)
      end if
!
      if (isend_self .eq. 0) return
      ist_send= NB * istack_send(npe_send-1)
      ist_recv= NB * istack_recv(npe_recv-1)
      num = int(NB * (istack_send(npe_send) - istack_send(npe_send-1)))
!$omp parallel do
      do i = 1, num
        SR_r1%WR(ist_recv+i) = SR_r1%WS(ist_send+i)
      end do
!$omp end parallel do
!
      end subroutine calypso_send_recv_core
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_check                                &
     &         (NB, npe_send, isend_self, istack_send,                  &
     &              npe_recv, irecv_self, istack_recv)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer (kind = kint) :: ist, num
      integer :: ncomm_send, ncomm_recv, neib
      integer (kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = int(npe_send - isend_self)
      ncomm_recv = int(npe_recv - irecv_self)
!
      do neib = 1, ncomm_send
        ist = NB * istack_send(neib-1) + 1
        num = NB * (istack_send(neib  ) - istack_send(neib-1))
        if(ist .lt. 0) write(*,*) 'wrong istack_send(0)', my_rank
        if(ist .gt. size(SR_r1%WS))                                     &
     &       write(*,*) 'wrong istack_send(neib)',                      &
     &       my_rank, neib, ist, size(SR_r1%WS)
        if((ist+num-1) .le. 0)                                          &
     &       write(*,*) 'negative num_send(0)',                         &
     &       my_rank, neib, ist, num, size(SR_r1%WS)
        if((ist+num-1) .gt. size(SR_r1%WS))                             &
     &       write(*,*) 'large num_send(neib)',                         &
     &       my_rank, neib, ist, num, size(SR_r1%WS)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= NB * istack_recv(neib-1) + 1
          num  = NB * (istack_recv(neib  ) - istack_recv(neib-1))
          if(ist .lt. 0) write(*,*) 'wrong istack_recv(0)', my_rank
          if(ist .gt. size(SR_r1%WR))                                   &
     &       write(*,*) 'wrong istack_recv(neib)',                      &
     &       my_rank, neib, ist, size(SR_r1%WR)
          if((ist+num-1) .le. 0) write(*,*) 'negative num_recv(0)',     &
     &       my_rank, neib, ist, num, size(SR_r1%WR)
          if((ist+num-1) .gt. size(SR_r1%WR))                           &
     &       write(*,*) 'large num_recv(neib)' ,                        &
     &       my_rank, neib, ist, num, size(SR_r1%WR)
        end do
      end if
!
      if (isend_self .eq. 0) return
      ist_send= NB * istack_send(npe_send-1)
      ist_recv= NB * istack_recv(npe_recv-1)
      num = NB * (istack_send(npe_send  ) - istack_send(npe_send-1))
        if(ist_send .lt. 0) write(*,*) 'wrong istack_send(0)', my_rank
        if(ist_send .gt. size(SR_r1%WS)) write(*,*)                     &
     &      'wrong istack_send(npe_send)',                              &
     &       my_rank, npe_send, ist_send, size(SR_r1%WS)
        if((ist_send+num-1) .le. 0) write(*,*) 'negative num_send(0)',  &
     &       my_rank, npe_send, ist_send, num, size(SR_r1%WS)
        if((ist_send+num-1) .gt. size(SR_r1%WS)) write(*,*)             &
     &      'large num_send(npe_send)',                                 &
     &       my_rank, npe_send, ist_send, num, size(SR_r1%WS)
!
        if(ist_recv .lt. 0) write(*,*) 'wrong istack_recv(0)', my_rank
        if(ist_recv .gt. size(SR_r1%WR)) write(*,*)                     &
     &      'wrong istack_recv(npe_recv)',                              &
     &       my_rank, npe_recv, ist_recv, size(SR_r1%WR)
        if((ist_recv+num-1) .le. 0) write(*,*) 'negative num_send(0)',  &
     &       my_rank, npe_recv, ist_recv, num, size(SR_r1%WR)
        if((ist_recv+num-1) .gt. size(SR_r1%WR)) write(*,*)             &
     &       'large num_send(npe_recv)',                                &
     &      my_rank, npe_recv, ist_recv, num, size(SR_r1%WR)
!
      end subroutine calypso_send_recv_check
!
! ----------------------------------------------------------------------
!
      end module calypso_SR_core
