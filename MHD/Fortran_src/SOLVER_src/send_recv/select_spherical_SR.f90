!
!     module select_spherical_SR
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine sel_sph_send_recv_N(NB, nnod_org, nnod_new,           &
!     &                         npe_send, isend_self, nnod_send,        &
!     &                         id_pe_send, istack_send, inod_export,   &
!     &                         npe_recv, irecv_self, nnod_recv,        &
!     &                         id_pe_recv, istack_recv, inod_import,   &
!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
!      subroutine sel_sph_send_recv_6(nnod_org, nnod_new,               &
!     &                         npe_send, isend_self, nnod_send,        &
!     &                         id_pe_send, istack_send, inod_export,   &
!     &                         npe_recv, irecv_self, nnod_recv,        &
!     &                         id_pe_recv, istack_recv, inod_import,   &
!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
!      subroutine sel_sph_send_recv_3(nnod_org, nnod_new,               &
!     &                         npe_send, isend_self, nnod_send,        &
!     &                         id_pe_send, istack_send, inod_export,   &
!     &                         npe_recv, irecv_self, nnod_recv,        &
!     &                         id_pe_recv, istack_recv, inod_import,   &
!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
!      subroutine sel_sph_send_recv_2(nnod_org, nnod_new,               &
!     &                         npe_send, isend_self, nnod_send,        &
!     &                         id_pe_send, istack_send, inod_export,   &
!     &                         npe_recv, irecv_self, nnod_recv,        &
!     &                         id_pe_recv, istack_recv, inod_import,   &
!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
!      subroutine sel_sph_send_recv(nnod_org, nnod_new,                 &
!     &                         npe_send, isend_self, nnod_send,        &
!     &                         id_pe_send, istack_send, inod_export,   &
!     &                         npe_recv, irecv_self, nnod_recv,        &
!     &                         id_pe_recv, istack_recv, inod_import,   &
!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
!
!      subroutine sel_sph_send_recv_int(nnod_org, nnod_new,             &
!     &                       npe_send, isend_self, nnod_send,          &
!     &                       id_pe_send, istack_send, inod_export,     &
!     &                       npe_recv, irecv_self, nnod_recv,          &
!     &                       id_pe_recv, istack_recv, inod_import,     &
!     &                       irev_import, iX_org, iX_new)
!
!
      module select_spherical_SR
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: iflag_import_item = 0
      integer(kind = kint), parameter :: iflag_import_rev =  1
!
      integer(kind = kint) :: iflag_sph_SRN = iflag_import_item
      integer(kind = kint) :: iflag_sph_SR6 = iflag_import_item
      integer(kind = kint) :: iflag_sph_SR3 = iflag_import_item
      integer(kind = kint) :: iflag_sph_SR2 = iflag_import_item
      integer(kind = kint) :: iflag_sph_SR =  iflag_import_item
!
      integer(kind = kint) :: iflag_sph_SR_int = iflag_import_item
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_N(NB, nnod_org, nnod_new,            &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_N
      use spherical_SR_rev_N
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(isix*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(isix*nnod_new)
!
!
      if(iflag_sph_SRN .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_N(NB, nnod_org, nnod_new,             &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_N(NB, nnod_org, nnod_new,                    &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_6(nnod_org, nnod_new,                &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_6
      use spherical_SR_rev_6
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(isix*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(isix*nnod_new)
!
!
      if(iflag_sph_SR6 .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_6(nnod_org, nnod_new,                 &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_6(nnod_org, nnod_new,                        &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_6
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_3(nnod_org, nnod_new,                &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_3
      use spherical_SR_rev_3
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(ithree*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(ithree*nnod_new)
!
!
      if(iflag_sph_SR3 .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_3(nnod_org, nnod_new,                 &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_3(nnod_org, nnod_new,                        &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_3
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_2(nnod_org, nnod_new,                &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_2
      use spherical_SR_rev_2
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(itwo*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(itwo*nnod_new)
!
!
      if(iflag_sph_SR2 .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_2(nnod_org, nnod_new,                 &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_2(nnod_org, nnod_new,                        &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_2
!
! ----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv(nnod_org, nnod_new,                  &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR
      use spherical_SR_rev
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(nnod_new)
!
!
      if(iflag_sph_SR .eq. iflag_import_rev) then
        call sph_send_recv_by_rev(nnod_org, nnod_new,                   &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv(nnod_org, nnod_new,                          &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_int(nnod_org, nnod_new,              &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       irev_import, iX_org, iX_new, SOLVER_COMM)
!
      use spherical_SR_int
      use spherical_SR_rev_int
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      integer (kind=kint), intent(in):: iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
!
      if(iflag_sph_SR_int .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_int(nnod_org, nnod_new,               &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       iX_org, iX_new, SOLVER_COMM)
      else
        call sph_send_recv_int(nnod_org, nnod_new,                      &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       iX_org, iX_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_int
!
! ----------------------------------------------------------------------
!
      end module select_spherical_SR
