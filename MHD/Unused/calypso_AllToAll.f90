!>@file   calypso_AllToAll.f90
!!@brief  module calypso_AllToAll
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine calypso_AllToAllV_N                                  &
!!     &                          (iflag_recv, NB, nnod_org, nnod_new,  &
!!     &                           npe_send, istack_send, inod_export,  &
!!     &                           npe_recv, istack_recv, inod_import,  &
!!     &                           X_org, X_new, SR_sig, SR_r,          &
!!     &                           CALYPSO_SUB_COMM)
!!
!!      subroutine calypso_AllToAll_N(NB, nnod_org, nnod_new, nitem_SR, &
!!     &                           npe_send, istack_send, inod_export,  &
!!     &                           npe_recv, istack_recv, inod_import,  &
!!     &                           X_org, X_new, SR_sig, SR_r,          &
!!     &                           CALYPSO_SUB_COMM)
!!      subroutine calypso_AllToAll_rev_N(NB, nnod_org, nnod_new,       &
!!     &                 nitem_SR, npe_send, istack_send, inod_export,  &
!!     &                           npe_recv, irev_import,               &
!!     &                           X_org, X_new, CALYPSO_SUB_COMM, SR_r)
!!
!!      subroutine calypso_AllToAllV_int(iflag_recv, nnod_org, nnod_new,&
!!     &                       npe_send, istack_send, inod_export,      &
!!     &                       npe_recv, istack_recv, inod_import,      &
!!     &                       irev_import, iX_org, iX_new,             &
!!     &                       SR_i, CALYPSO_SUB_COMM)
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!!
!!@n @param  CALYPSO_SUB_COMM
!!                    Sub MPI communicator
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(istack_send(npe_send))
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  inod_import(istack_recv(npe_recv))
!!                    local node ID to copy from receive buffer
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_org(NB*nnod_org)   Send data
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module calypso_AllToAll
!
      use m_precision
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_AllToAllV_N                                    &
     &                          (iflag_recv, NB, nnod_org, nnod_new,    &
     &                           npe_send, istack_send, inod_export,    &
     &                           npe_recv, istack_recv, inod_import,    &
     &                           X_org, X_new, SR_sig, SR_r,            &
     &                           CALYPSO_SUB_COMM)
!
      use calypso_mpi
      use calypso_AlltoAll_core
      use set_to_send_buffer
      use set_from_recv_buffer
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(NB, npe_send, npe_recv,                       &
     &    istack_send(npe_send), istack_recv(npe_recv), SR_sig, SR_r)
!
       call sel_cppy_to_send_buf_N(iflag_recv, NB, nnod_org,            &
      &    npe_send, istack_send(npe_send), istack_send, inod_export,   &
      &    X_org, SR_r%WS)
!C
      call calypso_AllToAllv_Ncore                                      &
     &   (NB, npe_send, istack_send, istack_recv, CALYPSO_SUB_COMM)
!
       call sel_cppy_from_recv_buf_N                                    &
      &   (iflag_recv, NB, nnod_new, npe_recv,                          &
      &    istack_recv(npe_recv), istack_recv, inod_import,             &
      &    irev_import, SR_r%WR(1), X_new)
!
      end subroutine calypso_AllToAllV_N
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine calypso_AllToAll_N(NB, nnod_org, nnod_new, nitem_SR,   &
     &                           npe_send, istack_send, inod_export,    &
     &                           npe_recv, istack_recv, inod_import,    &
     &                           X_org, X_new, SR_sig, SR_r,            &
     &                           CALYPSO_SUB_COMM)
!
      use calypso_mpi
      use calypso_AlltoAll_core
      use set_all2all_buffer
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: nitem_SR
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!
!C-- SEND
      call set_to_all2all_buf_N(NB, nnod_org, nitem_SR, npe_send,       &
     &    istack_send, inod_export, X_org, SR_r1%WS)
!C-- COMM
      call calypso_AllToAll_Ncore(NB, nitem_SR, CALYPSO_SUB_COMM)
!
!C-- RECV
      call set_from_all2all_buf_N(NB, nnod_new, nitem_SR,               &
     &    npe_recv, istack_recv, inod_import, SR_r1%WR, X_new)
!
      end subroutine calypso_AllToAll_N
!
! ----------------------------------------------------------------------
!
      subroutine calypso_AllToAll_rev_N(NB, nnod_org, nnod_new,         &
     &                 nitem_SR, npe_send, istack_send, inod_export,    &
     &                           npe_recv, irev_import,                 &
     &                           X_org, X_new, CALYPSO_SUB_COMM, SR_r)
!
      use calypso_mpi
      use calypso_AlltoAll_core
      use set_all2all_buffer
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: nitem_SR
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!
!C-- SEND
      call set_to_all2all_buf_N(NB, nnod_org, nitem_SR, npe_send,       &
     &    istack_send, inod_export, X_org, SR_r%WS)
!C-- COMM
      call calypso_AllToAll_Ncore(NB, nitem_SR, CALYPSO_SUB_COMM)
!
!C-- RECV
      call set_from_all2all_rev_N(NB, nnod_new, nitem_SR,               &
     &    npe_recv, irev_import, SR_r%WR, X_new)
!
      end subroutine calypso_AllToAll_rev_N
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine calypso_AllToAllV_int(iflag_recv, nnod_org, nnod_new,  &
     &                       npe_send, istack_send, inod_export,        &
     &                       npe_recv, istack_recv, inod_import,        &
     &                       irev_import, iX_org, iX_new,               &
     &                       SR_i, CALYPSO_SUB_COMM)
!
      use calypso_mpi
      use t_solver_SR_int
      use solver_SR_int
      use set_to_send_buffer
      use select_copy_from_recv
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
!
      integer (kind=kint), intent(in):: iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!>      Structure of communication buffer for integer
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call resize_iwork_SR_t(npe_send, npe_recv,                        &
     &    istack_send(npe_send), istack_recv(npe_recv), SR_i)
!
!C-- SEND
      call set_to_send_buf_int(nnod_org,                                &
     &    istack_send(npe_send), inod_export, iX_org, SR_i%iWS)
!C
!C-- COMM
      call calypso_AllToAllv_intcore                                    &
     &   (npe_send, istack_send, istack_recv, CALYPSO_SUB_COMM, SR_i)
!
!C-- RECV
      call sel_cppy_from_recv_buf_int(iflag_recv, nnod_new,             &
     &    istack_recv(npe_recv), inod_import, irev_import,              &
     &    SR_i%iWR(1), iX_new)
!
      end subroutine calypso_AllToAllV_int
!
! ----------------------------------------------------------------------
!
      end module calypso_AllToAll
