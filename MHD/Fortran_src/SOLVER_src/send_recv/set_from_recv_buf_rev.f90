!>@file   set_from_recv_buf_rev.f90
!!@brief  module set_from_recv_buf_rev
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine set_from_recv_buf_rev_1(nnod_new,                    &
!!     &          nnod_recv, irev_import, WR, X_new)
!!      subroutine set_from_recv_buf_rev_2(nnod_new,                    &
!!     &          nnod_recv, irev_import, WR, X_new)
!!      subroutine set_from_recv_buf_rev_3(nnod_new,                    &
!!     &          nnod_recv, irev_import, WR, X_new)
!!      subroutine set_from_recv_buf_rev_6(nnod_new,                    &
!!     &          nnod_recv, irev_import, WR, X_new)
!!      subroutine set_from_recv_buf_rev_N(NB, nnod_new,                &
!!     &          npe_recv, nnod_recv, istack_recv, irev_import,        &
!!     &          WR, X_new)
!!
!!      subroutine set_from_recv_buf_rev_int(nnod_new,                  &
!!     &          nnod_recv, irev_import, iWR, iX_new)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module set_from_recv_buf_rev
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
      subroutine set_from_recv_buf_rev_1(nnod_new,                      &
     &          nnod_recv, irev_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(nnod_recv+1)
!
      real (kind=kreal), intent(inout):: X_new(nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      WR(nnod_recv+1) = 0.0d0
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X_new(k  ) = WR(j  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_1
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_2(nnod_new,                      &
     &          nnod_recv, irev_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(2*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X_new(2*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      WR(2*nnod_recv+1) = 0.0d0
      WR(2*nnod_recv+2) = 0.0d0
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X_new(2*k-1) = WR(2*j-1)
        X_new(2*k  ) = WR(2*j  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_2
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_3(nnod_new,                      &
     &          nnod_recv, irev_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(3*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X_new(3*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      WR(3*nnod_recv+1) = 0.0d0
      WR(3*nnod_recv+2) = 0.0d0
      WR(3*nnod_recv+3) = 0.0d0
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X_new(3*k-2) = WR(3*j-2)
        X_new(3*k-1) = WR(3*j-1)
        X_new(3*k  ) = WR(3*j  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_3
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_6(nnod_new,                      &
     &          nnod_recv, irev_import, WR, X_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(6*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X_new(6*nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      do k = 1, 6
        WR(6*nnod_recv+k) = 0.0d0
      end do
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X_new(6*k-5) = WR(6*j- 5)
        X_new(6*k-4) = WR(6*j- 4)
        X_new(6*k-3) = WR(6*j- 3)
        X_new(6*k-2) = WR(6*j- 2)
        X_new(6*k-1) = WR(6*j- 1)
        X_new(6*k  ) = WR(6*j   )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_6
!
! ----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_N(NB, nnod_new,                  &
     &          npe_recv, nnod_recv, istack_recv, irev_import,          &
     &          WR, X_new)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: npe_recv, nnod_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(inout):: WR(NB*(nnod_recv+1))
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, nd, jj, kk
!
!
      do k = 1, NB
        WR(NB*nnod_recv+k) = 0.0d0
      end do
!
!$omp parallel private(nd,neib,ist,num)
      do neib = 1, npe_recv
        ist = istack_recv(neib-1)
        num = istack_recv(neib  ) - istack_recv(neib-1)
        do nd = 1, NB
!$omp do private(k,jj,kk)
          do k = 1, num
            jj   = nd + NB * (k-1)
            kk   = irev_import(k+ist) + (nd-1) * num + NB*ist
            X_new(jj) = WR(kk)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine set_from_recv_buf_rev_N
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_from_recv_buf_rev_int(nnod_new,                    &
     &          nnod_recv, irev_import, iWR, iX_new)
!
      integer(kind = kint), intent(in) :: nnod_new, nnod_recv
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      integer (kind=kint), intent(inout):: iWR(nnod_recv+1)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
      integer (kind = kint) :: k, j
!
!
      iWR(nnod_recv+1) = 0
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        iX_new(k  ) = iWR(j  )
      end do
!$omp end parallel do
!
      end subroutine set_from_recv_buf_rev_int
!
! ----------------------------------------------------------------------
!
      end module set_from_recv_buf_rev
