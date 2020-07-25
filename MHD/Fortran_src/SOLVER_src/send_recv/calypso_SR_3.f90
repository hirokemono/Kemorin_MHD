!>@file   calypso_SR_3.f90
!!@brief  module calypso_SR_3
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  vector data communication
!!
!!@verbatim
!!      subroutine calypso_send_recv_3(nnod_org, nnod_new,              &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, SR_sig, SR_r,             &
!!     &                         X_org, X_new)
!!      subroutine calypso_send_recv_3x3(nnod_org, nnod_new,            &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, SR_sig, SR_r,             &
!!     &                         X1_org, X2_org, X3_org,                &
!!     &                         X1_new, X2_new, X3_new)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!!
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
!!@n @param  inod_import(istack_recv(npe_recv))
!!                    local node ID to copy from receive buffer
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_org(3*nnod_org)   Send data
!!@n @param  X_new(3*nnod_new)   Received data
!
      module calypso_SR_3
!
      use m_precision
      use m_constants
      use t_solver_SR
      use calypso_mpi

      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_send_recv_3(nnod_org, nnod_new,                &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, SR_sig, SR_r,               &
     &                         X_org, X_new)
!
      use calypso_SR_core
      use set_to_send_buffer
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(3*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(3*nnod_new)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(ithree, npe_send, npe_recv,                   &
     &    istack_send(npe_send), istack_recv(npe_recv), SR_sig, SR_r)
!
!C-- SEND
      call set_to_send_buf_3(nnod_org,                                  &
     &    istack_send(npe_send), inod_export, X_org, SR_r%WS)
!C
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (ithree, npe_send, isend_self, id_pe_send, istack_send,        &
     &            npe_recv, irecv_self, id_pe_recv, istack_recv,        &
     &            SR_sig, SR_r)
!C
!C-- RECEIVE
      call sel_cppy_from_recv_buf_3(SR_sig%iflag_recv, nnod_new,        &
     &    istack_recv(npe_recv), inod_import, irev_import,              &
     &    SR_r%WR(1), X_new)
!
!C-- WAIT
      call calypso_send_recv_fin(npe_send, isend_self, SR_sig)
!
      end subroutine calypso_send_recv_3
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_3x3(nnod_org, nnod_new,              &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, SR_sig, SR_r,               &
     &                         X1_org, X2_org, X3_org,                  &
     &                         X1_new, X2_new, X3_new)
!
      use calypso_SR_core
      use set_to_send_buf_tri
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X1_org(3*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(3*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(3*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(3*nnod_new)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR((ithree*ithree), npe_send, npe_recv,          &
     &    istack_send(npe_send), istack_recv(npe_recv), SR_sig, SR_r)
!
!C-- SEND
      call set_to_send_buf_3x3(nnod_org, istack_send(npe_send),         &
     &    inod_export, X1_org, X2_org, X3_org, SR_r%WS)
!C
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (inine, npe_send, isend_self, id_pe_send, istack_send,         &
     &           npe_recv, irecv_self, id_pe_recv, istack_recv,         &
     &           SR_sig, SR_r)
!C
!C-- RECEIVE
      call sel_cppy_from_recv_buf_3x3(SR_sig%iflag_recv, nnod_new,      &
     &    istack_recv(npe_recv), inod_import, irev_import,              &
     &    SR_r%WR(1), X1_new, X2_new, X3_new)
!
!C-- WAIT
      call calypso_send_recv_fin(npe_send, isend_self, SR_sig)
!
      end subroutine calypso_send_recv_3x3
!
! ----------------------------------------------------------------------
!
      end module calypso_SR_3
