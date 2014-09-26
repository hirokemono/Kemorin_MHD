!>@file   select_calypso_AllToAll.f90
!!@brief  module select_calypso_AllToAll
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine sel_calypso_AllToAllv(iflag_SR,                      &
!!     &                 NB, nnod_org, nnod_new,                        &
!!     &                 npe_send, istack_send, inod_export,            &
!!     &                 npe_recv, istack_recv, inod_import,            &
!!     &                 irev_import, X_org, X_new, CALYPSO_SUB_COMM)
!!      subroutine sel_calypso_AllToAll                                 &
!!     &                (nitem_SR, NB, nnod_org, nnod_new, nitem_SR,    &
!!     &                 npe_send, istack_send, inod_export,            &
!!     &                 npe_recv, istack_recv, inod_import,            &
!!     &                 irev_import, X_org, X_new, CALYPSO_SUB_COMM)
!!@endverbatim
!!
!!@n @param  iflag_SR Integer flag for data transfer
!!@n            iflag_SR(1):: copy mode from recieve buffer
!!@n            iflag_SR(2):: communication routine selector
!!@n @param  CALYPSO_SUB_COMM
!!                    Sub MPI communicator
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(istack_send(npe_send))
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  inod_import(istack_recv(npe_recv))
!!                    local node ID to copy from receive buffer
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_org(NB*nnod_org)   Arbitrary components of send data
!!@n @param  X_new(NB*nnod_new)   Arbitrary components of received data
!!@n
!!@n @param  X_org(6*nnod_org)   Six components of send data
!!@n @param  X_new(6*nnod_new)   Six components of received data
!!@n
!!@n @param  X_org(3*nnod_org)   Three components of send data
!!@n @param  X_new(3*nnod_new)   Three components of received data
!!@n
!!@n @param  X_org(2*nnod_org)   Two components of send data
!!@n @param  X_new(2*nnod_new)   Two components of received data
!!@n
!!@n @param  X_org(nnod_org)   Scalar send data
!!@n @param  X_new(nnod_new)   Scalar received data
!!@n
!!@n @param  iX_org(nnod_org)   Integer send data
!!@n @param  iX_new(nnod_new)   Integer received data
!
      module select_calypso_AllToAll
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_AllToAllv(iflag_SR,                        &
     &                 NB, nnod_org, nnod_new,                          &
     &                 npe_send, istack_send, inod_export,              &
     &                 npe_recv, istack_recv, inod_import,              &
     &                 irev_import, X_org, X_new, CALYPSO_SUB_COMM)
!
      use m_solver_SR
      use calypso_AllToAll
      use select_copy_from_recv
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: iflag_SR
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
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
!
      call resize_work_sph_SR(NB, npe_send, npe_recv,                   &
     &    istack_send(npe_send), istack_recv(npe_recv))
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_AllToAllV_rev_N(NB, nnod_org, nnod_new,            &
     &                  npe_send, istack_send, inod_export,             &
     &                  npe_recv, istack_recv, irev_import,             &
     &                  X_org, X_new, CALYPSO_SUB_COMM)
      else
        call calypso_AllToAllV_N(NB, nnod_org, nnod_new,                &
     &                  npe_send, istack_send, inod_export,             &
     &                  npe_recv, istack_recv, inod_import,             &
     &                  X_org, X_new, CALYPSO_SUB_COMM)
      end if
!
      end subroutine sel_calypso_AllToAllv
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_AllToAll                                   &
     &                (NB, nnod_org, nnod_new, nitem_SR,                &
     &                 npe_send, istack_send, inod_export,              &
     &                 npe_recv, istack_recv, inod_import,              &
     &                 irev_import, X_org, X_new, CALYPSO_SUB_COMM)
!
      use m_solver_SR
      use calypso_AllToAll
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
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      integer(kind = kint) :: nitem
!
!
      nitem = npe_send*nitem_SR
      call resize_work_sph_SR(NB, npe_send, npe_recv, nitem, nitem)
!
      call calypso_AllToAll_N(NB, nnod_org, nnod_new, nitem_SR,         &
     &                  npe_send, istack_send, inod_export,             &
     &                  npe_recv, istack_recv, inod_import,             &
     &                  X_org, X_new, CALYPSO_SUB_COMM)
!
      end subroutine sel_calypso_AllToAll
!
!-----------------------------------------------------------------------
!
      end module select_calypso_AllToAll
