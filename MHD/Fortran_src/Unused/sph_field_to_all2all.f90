!>@file   sph_field_to_all2all.f90
!!@brief  module sph_field_to_all2all
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Data transfer to all2all buffer
!!
!!@verbatim
!!      subroutine set_to_all2all_buf_vector(NB, nnod_org, nitem_SR,    &
!!     &          npe_send, istack_send, inod_export,                   &
!!     &          ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!      subroutine set_to_all2all_buf_scalar(NB, nnod_org, nitem_SR,    &
!!     &          npe_send, istack_send, inod_export,                   &
!!     &          ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!      subroutine set_to_all2all_buf_tensor(NB, nnod_org, nitem_SR,    &
!!     &          npe_send, istack_send, inod_export,                   &
!!     &          ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
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
!!@n @param  d_org(nnod_org,ncomp_X)  field data to send
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module sph_field_to_all2all
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
      subroutine set_to_all2all_buf_vector(NB, nnod_org, nitem_SR,      &
     &          npe_send, istack_send, inod_export,                     &
     &          ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
      integer(kind = kint), intent(in) :: npe_send, nitem_SR
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_export(istack_send(npe_send))
!
      real (kind=kreal), intent(in)::    d_org(nnod_org,ncomp_X)
!
      real (kind=kreal), intent(inout):: WS(NB*npe_send*nitem_SR)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, jj, kk
!
!
!$omp parallel private(neib,ist,num)
      do neib = 1, npe_send
        ist = istack_send(neib-1)
        num = istack_send(neib  ) - istack_send(neib-1)
!$omp do private(k,jj,kk)
        do k = 1, num
          jj = inod_export(k+ist)
          kk = i_fld_WS + (k-1)*NB + (neib-1)*nitem_SR*NB
          WS(kk  ) = d_org(jj,i_fld_X  )
          WS(kk+1) = d_org(jj,i_fld_X+1)
          WS(kk+2) = d_org(jj,i_fld_X+2)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_to_all2all_buf_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_to_all2all_buf_scalar(NB, nnod_org, nitem_SR,      &
     &          npe_send, istack_send, inod_export,                     &
     &          ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
      integer(kind = kint), intent(in) :: npe_send, nitem_SR
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_export(istack_send(npe_send))
!
      real (kind=kreal), intent(in)::    d_org(nnod_org,ncomp_X)
!
      real (kind=kreal), intent(inout):: WS(NB*npe_send*nitem_SR)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, jj, kk
!
!
!$omp parallel private(neib,ist,num)
      do neib = 1, npe_send
        ist = istack_send(neib-1)
        num = istack_send(neib  ) - istack_send(neib-1)
!$omp do private(k,jj,kk)
        do k = 1, num
          jj = inod_export(k+ist)
          kk = i_fld_WS + (k-1)*NB + (neib-1)*nitem_SR*NB
          WS(kk  ) = d_org(jj,i_fld_X)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_to_all2all_buf_scalar
!
! ----------------------------------------------------------------------
!
      subroutine set_to_all2all_buf_tensor(NB, nnod_org, nitem_SR,      &
     &          npe_send, istack_send, inod_export,                     &
     &          ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
      integer(kind = kint), intent(in) :: npe_send, nitem_SR
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_export(istack_send(npe_send))
!
      real (kind=kreal), intent(in)::    d_org(nnod_org,ncomp_X)
!
      real (kind=kreal), intent(inout):: WS(NB*npe_send*nitem_SR)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, jj, kk
!
!
!$omp parallel private(neib,ist,num)
      do neib = 1, npe_send
        ist = istack_send(neib-1)
        num = istack_send(neib  ) - istack_send(neib-1)
!$omp do private(k,jj,kk)
        do k = 1, num
          jj = inod_export(k+ist)
          kk = i_fld_WS + (k-1)*NB + (neib-1)*nitem_SR*NB
          WS(kk  ) = d_org(jj,i_fld_X  )
          WS(kk+1) = d_org(jj,i_fld_X+1)
          WS(kk+2) = d_org(jj,i_fld_X+2)
          WS(kk+3) = d_org(jj,i_fld_X+3)
          WS(kk+4) = d_org(jj,i_fld_X+4)
          WS(kk+5) = d_org(jj,i_fld_X+5)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_to_all2all_buf_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_buf_vector(NB, nnod_new, nitem_SR,    &
     &          npe_recv, istack_recv, inod_import,                     &
     &          ncomp_X, i_fld_X, i_fld_WR, WR, d_new)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WR
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nitem_SR
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_import(istack_recv(npe_recv))
!
      real (kind=kreal), intent(in):: WR(NB*npe_recv*nitem_SR)
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
      integer (kind = kint) :: neib, ist, num, k, jj, kk
!
!
!$omp parallel do
      do jj = 1, nnod_new
        d_new(jj,i_fld_X  ) = 0.0d0
        d_new(jj,i_fld_X+1) = 0.0d0
        d_new(jj,i_fld_X+2) = 0.0d0
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
          kk = i_fld_WR + (k-1)*NB + (neib-1)*nitem_SR*NB
          d_new(jj,i_fld_X  ) = WR(kk  )
          d_new(jj,i_fld_X+1) = WR(kk+1)
          d_new(jj,i_fld_X+2) = WR(kk+2)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_from_all2all_buf_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_buf_scalar(NB, nnod_new, nitem_SR,    &
     &          npe_recv, istack_recv, inod_import,                     &
     &          ncomp_X, i_fld_X, i_fld_WR, WR, d_new)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WR
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nitem_SR
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_import(istack_recv(npe_recv))
!
      real (kind=kreal), intent(in):: WR(NB*npe_recv*nitem_SR)
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
      integer (kind = kint) :: neib, ist, num, k, jj, kk
!
!
!$omp parallel do
      do jj = 1, nnod_new
        d_new(jj,i_fld_X  ) = 0.0d0
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
          kk = i_fld_WR + (k-1)*NB + (neib-1)*nitem_SR*NB
          d_new(jj,i_fld_X  ) = WR(kk  )
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_from_all2all_buf_scalar
!
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_buf_tensor(NB, nnod_new, nitem_SR,    &
     &          npe_recv, istack_recv, inod_import,                     &
     &          ncomp_X, i_fld_X, i_fld_WR, WR, d_new)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WR
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nitem_SR
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_import(istack_recv(npe_recv))
!
      real (kind=kreal), intent(in):: WR(NB*npe_recv*nitem_SR)
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
      integer (kind = kint) :: neib, ist, num, k, jj, kk
!
!
!$omp parallel do
      do jj = 1, nnod_new
        d_new(jj,i_fld_X  ) = 0.0d0
        d_new(jj,i_fld_X+1) = 0.0d0
        d_new(jj,i_fld_X+2) = 0.0d0
        d_new(jj,i_fld_X+3) = 0.0d0
        d_new(jj,i_fld_X+4) = 0.0d0
        d_new(jj,i_fld_X+5) = 0.0d0
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
          kk = i_fld_WR + (k-1)*NB + (neib-1)*nitem_SR*NB
          d_new(jj,i_fld_X  ) = WR(kk  )
          d_new(jj,i_fld_X+1) = WR(kk+1)
          d_new(jj,i_fld_X+2) = WR(kk+2)
          d_new(jj,i_fld_X+3) = WR(kk+3)
          d_new(jj,i_fld_X+4) = WR(kk+4)
          d_new(jj,i_fld_X+5) = WR(kk+5)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_from_all2all_buf_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_buf_rev_vect(NB, nnod_new, nitem_SR,  &
     &          npe_recv, irev_import, ncomp_X, i_fld_X, i_fld_WR,      &
     &          WR, d_new)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WR
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_new
      integer(kind = kint), intent(in) :: nitem_SR, npe_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(NB*(npe_recv*nitem_SR+1))
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
      integer (kind = kint) :: k, jj
!
!
      do k = i_fld_WR, i_fld_WR+2
        WR(NB*npe_recv*nitem_SR+k) = 0.0d0
      end do
!
!$omp parallel do private(k,jj)
      do k = 1, nnod_new
        jj = NB*(irev_import(k)-1) + i_fld_WR
        d_new(k,i_fld_X  ) = WR(jj)
        d_new(k,i_fld_X+1) = WR(jj+1)
        d_new(k,i_fld_X+2) = WR(jj+2)
      end do
!$omp end parallel do
!
      end subroutine set_from_all2all_buf_rev_vect
!
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_buf_rev_scl(NB, nnod_new, nitem_SR,   &
     &          npe_recv, irev_import, ncomp_X, i_fld_X, i_fld_WR,      &
     &          WR, d_new)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WR
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_new
      integer(kind = kint), intent(in) :: nitem_SR, npe_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(NB*(npe_recv*nitem_SR+1))
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
      integer (kind = kint) :: k, jj
!
!
      WR(NB*npe_recv*nitem_SR+i_fld_WR  ) = 0.0d0
!
!$omp parallel do private(k,jj)
      do k = 1, nnod_new
        jj = NB*(irev_import(k)-1) + i_fld_WR
        d_new(k,i_fld_X  ) = WR(jj)
      end do
!$omp end parallel do
!
      end subroutine set_from_all2all_buf_rev_scl
!
! ----------------------------------------------------------------------
!
      subroutine set_from_all2all_buf_rev_tsr(NB, nnod_new, nitem_SR,   &
     &          npe_recv, irev_import, ncomp_X, i_fld_X, i_fld_WR,      &
     &          WR, d_new)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WR
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_new
      integer(kind = kint), intent(in) :: nitem_SR, npe_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(NB*(npe_recv*nitem_SR+1))
!
      real (kind=kreal), intent(inout):: d_new(nnod_new,ncomp_X)
!
      integer (kind = kint) :: k, jj
!
!
      do k = i_fld_WR, i_fld_WR+5
        WR(NB*npe_recv*nitem_SR+k) = 0.0d0
      end do
!
!$omp parallel do private(k,jj)
      do k = 1, nnod_new
        jj = NB*(irev_import(k)-1) + i_fld_WR
        d_new(k,i_fld_X  ) = WR(jj)
        d_new(k,i_fld_X+1) = WR(jj+1)
        d_new(k,i_fld_X+2) = WR(jj+2)
        d_new(k,i_fld_X+3) = WR(jj+3)
        d_new(k,i_fld_X+4) = WR(jj+4)
        d_new(k,i_fld_X+5) = WR(jj+5)
      end do
!$omp end parallel do
!
      end subroutine set_from_all2all_buf_rev_tsr
!
! ----------------------------------------------------------------------
!
      end module sph_field_to_all2all
