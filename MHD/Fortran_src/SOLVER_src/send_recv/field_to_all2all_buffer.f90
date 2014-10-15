!>@file   field_to_all2all_buffer.f90
!!@brief  module field_to_all2all_buffer
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
      module field_to_all2all_buffer
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
!
      end module field_to_all2all_buffer
