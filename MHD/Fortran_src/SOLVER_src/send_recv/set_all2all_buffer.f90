!>@file   set_all2all_buffer.f90
!!@brief  module set_all2all_buffer
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine set_rev_all2all_import_tbl(nnod_new, nitem_SR,       &
!!     &          npe_recv, istack_recv, inod_import, irev_import)
!!
!!      subroutine set_to_all2all_buf_N(NB, nnod_org, nitem_SR,         &
!!     &          npe_send, istack_send, inod_export, X_org, WS)
!!      subroutine set_from_all2all_buf_N(NB, nnod_new, nitem_SR,       &
!!     &          npe_recv, istack_recv, inod_import, WR, X_new)
!!      subroutine set_from_all2all_rev_N(NB, nnod_new, nitem_SR,       &
!!     &          npe_recv, irev_import, WR, X_new)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  nitem_SR    Number of data for each process
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  inod_import(nnod_recv)
!!                    local node ID to copy from receive buffer
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module set_all2all_buffer
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
      subroutine set_rev_all2all_import_tbl(nnod_new, nitem_SR,         &
     &          npe_recv, istack_recv, inod_import, irev_import)
!
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nitem_SR
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_import(istack_recv(npe_recv))
!
      integer(kind = kint), intent(inout):: irev_import(nnod_new)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, jj, kk
!
!
!$omp parallel do
      do jj = 1, nnod_new
        irev_import(jj) = npe_recv*nitem_SR + 1
      end do
!$omp end parallel do
!
!$omp parallel private(neib,ist,num)
      do neib = 1, npe_recv
        ist = istack_recv(neib-1)
        num = istack_recv(neib  ) - istack_recv(neib-1)
!$omp do private(k,jj,kk)
        do k = 1, num
          jj = inod_import(k+ist)
          kk = k + (neib-1)*nitem_SR
          irev_import(jj) = kk
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_rev_all2all_import_tbl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_to_all2all_buf_N(NB, nnod_org, nitem_SR,           &
     &          npe_send, istack_send, inod_export, X_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nitem_SR
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_export(istack_send(npe_send))
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(NB*npe_send*nitem_SR)
!
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, nd, jj, kk
!
!
!$omp parallel private(nd,neib,ist,num)
      do neib = 1, npe_send
        ist = istack_send(neib-1)
        num = istack_send(neib  ) - istack_send(neib-1)
        do nd = 1, NB
!$omp do private(k,jj,kk)
          do k = 1, num
            jj = nd + (inod_export(k+ist) - 1) * NB
            kk = nd + (k-1)*NB + (neib-1)*nitem_SR*NB
            WS(kk) = X_org(jj)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine set_to_all2all_buf_N
!
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_buf_N(NB, nnod_new, nitem_SR,         &
     &          npe_recv, istack_recv, inod_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nitem_SR
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_import(istack_recv(npe_recv))
!
      real (kind=kreal), intent(in):: WR(NB*npe_recv*nitem_SR)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, nd, jj, kk
!
!
!$omp parallel do
      do jj = 1, NB*nnod_new
        X_new(jj) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel private(nd,neib,ist,num)
      do neib = 1, npe_recv
        ist = istack_recv(neib-1)
        num = istack_recv(neib  ) - istack_recv(neib-1)
        do nd = 1, NB
!$omp do private(k,jj,kk)
          do k = 1, num
            jj = nd + (inod_import(k+ist) - 1) * NB
            kk = nd + (k-1)*NB + (neib-1)*nitem_SR*NB
            X_new(jj) = WR(kk)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine set_from_all2all_buf_N
!
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_rev_N(NB, nnod_new, nitem_SR,         &
     &          npe_recv, irev_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new, nitem_SR, npe_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(NB*(npe_recv*nitem_SR+1))
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      integer (kind = kint) :: k, kk, jj, nd
!
!
!$omp parallel do
      do k = 1, NB
        WR(NB*npe_recv*nitem_SR+k) = 0.0d0
      end do
!$omp end parallel do
!
!$omp parallel do private(k,jj,kk,nd)
      do kk = 1, NB*nnod_new
        nd = 1 + mod(kk-1,NB)
        k =  1 + (kk-nd) / NB
        jj = NB*(irev_import(k)-1) + nd
        X_new(kk) = WR(jj)
      end do
!$omp end parallel do
!
      end subroutine set_from_all2all_rev_N
!
! ----------------------------------------------------------------------
!
      end module set_all2all_buffer
