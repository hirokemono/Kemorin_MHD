!>@file   field_to_send_buffer.f90
!!@brief  module field_to_send_buffer
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine set_to_send_buf_vector(NB, nnod_org, nnod_send,      &
!!     &          inod_export, ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!      subroutine set_to_send_buf_scalar(NB, nnod_org, nnod_send,      &
!!     &          inod_export, ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!      subroutine set_to_send_buf_tensor(NB, nnod_org, nnod_send,      &
!!     &          inod_export, ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  d_org(nnod_org,NB)   Send data
!
      module field_to_send_buffer
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
      subroutine set_to_send_buf_vector(NB, nnod_org, nnod_send,        &
     &          inod_export, ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS, nnod_send
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    d_org(nnod_org,ncomp_X)
!
      real (kind=kreal), intent(inout):: WS(NB*nnod_send)
!
!
      integer (kind = kint) :: k, kk, jj
!
!
!$omp parallel do private(k,jj,kk)
      do k = 1, nnod_send
        kk = NB*(k-1) + i_fld_WS
        jj = inod_export(k)
        WS(kk  ) = d_org(jj,i_fld_X  )
        WS(kk+1) = d_org(jj,i_fld_X+1)
        WS(kk+2) = d_org(jj,i_fld_X+2)
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_to_send_buf_scalar(NB, nnod_org, nnod_send,        &
     &          inod_export, ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS, nnod_send
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    d_org(nnod_org,ncomp_X)
!
      real (kind=kreal), intent(inout):: WS(NB*nnod_send)
!
!
      integer (kind = kint) :: k, kk, jj
!
!
!$omp parallel do private(k,jj,kk)
      do k = 1, nnod_send
        kk = NB*(k-1) + i_fld_WS
        jj = inod_export(k)
        WS(kk  ) = d_org(jj,i_fld_X)
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_scalar
!
!-----------------------------------------------------------------------
!
      subroutine set_to_send_buf_tensor(NB, nnod_org, nnod_send,        &
     &          inod_export, ncomp_X, i_fld_X, i_fld_WS, d_org, WS)
!
      integer(kind = kint), intent(in) :: NB, i_fld_WS, nnod_send
      integer(kind = kint), intent(in) :: ncomp_X, i_fld_X, nnod_org
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      real (kind=kreal), intent(in)::    d_org(nnod_org,ncomp_X)
!
      real (kind=kreal), intent(inout):: WS(NB*nnod_send)
!
!
      integer (kind = kint) :: k, kk, jj
!
!
!$omp parallel do private(k,jj,kk)
      do k = 1, nnod_send
        kk = NB*(k-1) + i_fld_WS
        jj = inod_export(k)
        WS(kk  ) = d_org(jj,i_fld_X  )
        WS(kk+1) = d_org(jj,i_fld_X+1)
        WS(kk+2) = d_org(jj,i_fld_X+2)
        WS(kk+3) = d_org(jj,i_fld_X+3)
        WS(kk+4) = d_org(jj,i_fld_X+4)
        WS(kk+5) = d_org(jj,i_fld_X+5)
      end do
!$omp end parallel do
!
      end subroutine set_to_send_buf_tensor
!
! ----------------------------------------------------------------------
!
      end module field_to_send_buffer
