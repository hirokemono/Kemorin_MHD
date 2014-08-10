!>@file   m_sel_spherical_SRs.f90
!!@brief  module m_sel_spherical_SRs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2014
!
!>@brief  Data communication selector for spherical transform
!!
!!@verbatim
!!      subroutine set_import_table_ctl(import_ctl)
!!      subroutine set_sph_comm_routine_ctl(send_recv_ctl)
!!
!!      subroutine sel_calypso_sph_send_recv_N                          &
!!     &                   (NB, nnod_org, nnod_new, nmax_sr,            &
!!     &                    npe_send, isend_self,                       &
!!     &                    id_pe_send, istack_send, inod_export,       &
!!     &                    npe_recv, irecv_self,                       &
!!     &                    id_pe_recv, istack_recv, inod_import,       &
!!     &                    irev_import, X_org, X_new, CALYPSO_SUB_COMM)
!!      subroutine finish_sph_send_recv(npe_send, isend_self)
!!@endverbatim
!
      module m_sel_spherical_SRs
!
      use m_precision
      use m_solver_SR
!
!
!>      Character flag to use import table
      character(len = kchara), parameter                                &
     &                       :: hd_import_item = 'regular_table'
!>      Character flag to use reverse import table
      character(len = kchara), parameter                                &
     &                       :: hd_import_rev =  'reversed_table'
!
!>      Character flag to use FFTW3
      character(len = kchara), parameter                                &
     &                       :: hd_sendrecv = 'SEND_RECV'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter                                &
     &                       :: hd_all2allv =  'AllToAllv'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter                                &
     &                       :: hd_all2all =  'AllToAll'
!
!
!>      Undefined flag
      integer(kind = kint), parameter :: iflag_SR_UNDEFINED = -1
!>      Integer flag to use MPI_Isend and MPI_IRecv
      integer(kind = kint), parameter :: iflag_send_recv = 0
!>      Integer flag to use MPI_AllToAllv
      integer(kind = kint), parameter :: iflag_alltoallv = 1
!>      Integer flag to use MPI_AllToAll
      integer(kind = kint), parameter :: iflag_alltoall =  2
!
!
!>      Data communication mode for arbitrary size data
      integer(kind = kint) :: iflag_sph_SRN =   iflag_import_UNDEFINED
!
!>      Data communication mode for arbitrary size data
      integer(kind = kint) :: iflag_sph_commN = iflag_SR_UNDEFINED
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_import_table_ctl(import_ctl)
!
      use skip_comment_f
      use calypso_solver_SR
!
      character(len = kchara), intent(in) :: import_ctl
!
!
      if(cmp_no_case(import_ctl, hd_import_item) .gt. 0) then
        iflag_sph_SRN = iflag_import_item
      else if(cmp_no_case(import_ctl, hd_import_rev) .gt. 0) then
        iflag_sph_SRN = iflag_import_rev
      else
        iflag_sph_SRN = iflag_import_UNDEFINED
      end if
!
      end subroutine set_import_table_ctl
!
! ------------------------------------------------------------------
!
      subroutine set_sph_comm_routine_ctl(send_recv_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: send_recv_ctl
!
!
      if(cmp_no_case(send_recv_ctl, hd_sendrecv) .gt. 0) then
        iflag_sph_commN = iflag_send_recv
      else if(cmp_no_case(send_recv_ctl, hd_all2allv) .gt. 0) then
        iflag_sph_commN = iflag_alltoallv
      else if(cmp_no_case(send_recv_ctl, hd_all2all) .gt. 0) then
        iflag_sph_commN = iflag_alltoall
      else
        iflag_sph_commN = iflag_SR_UNDEFINED
      end if
!
      end subroutine set_sph_comm_routine_ctl
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_sph_send_recv_N                            &
     &                   (NB, nnod_org, nnod_new, nmax_sr,              &
     &                    npe_send, isend_self,                         &
     &                    id_pe_send, istack_send, inod_export,         &
     &                    npe_recv, irecv_self,                         &
     &                    id_pe_recv, istack_recv, inod_import,         &
     &                    irev_import, X_org, X_new, CALYPSO_SUB_COMM)
!
      use select_calypso_SR
      use select_calypso_AllToAll
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      integer(kind = kint), intent(in) :: nmax_sr
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
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
!
      if     (iflag_sph_commN .eq. iflag_alltoall) then
        call sel_calypso_AllToAll                                       &
     &    (NB, nnod_org, nnod_new, nmax_sr,                             &
     &     npe_send, istack_send, inod_export,                          &
     &     npe_recv,  istack_recv,  inod_import, irev_import,           &
     &     X_org, X_new, CALYPSO_SUB_COMM)
      else if(iflag_sph_commN .eq. iflag_alltoallv) then
        call sel_calypso_AllToAllv                                      &
     &    (iflag_sph_SRN, NB, nnod_org, nnod_new,                       &
     &     npe_send, istack_send, inod_export,                          &
     &     npe_recv, istack_recv, inod_import, irev_import,             &
     &     X_org, X_new, CALYPSO_SUB_COMM)
      else
        call sel_calypso_send_recv_N                                    &
     &     (iflag_sph_SRN, NB, nnod_org, nnod_new,                      &
     &      npe_send, isend_self, id_pe_send, istack_send, inod_export, &
     &      npe_recv, irecv_self, id_pe_recv, istack_recv, inod_import, &
     &      irev_import, X_org, X_new)
      end if
!
      end subroutine sel_calypso_sph_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine finish_sph_send_recv(npe_send, isend_self)
!
      use select_calypso_SR
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
!
!
      if(iflag_sph_commN .ne. iflag_send_recv) return
      call finish_calypso_send_recv(npe_send, isend_self)
!
      end subroutine finish_sph_send_recv
!
! ----------------------------------------------------------------------
!
      end module m_sel_spherical_SRs
