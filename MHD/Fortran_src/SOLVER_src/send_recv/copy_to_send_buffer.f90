!>@file   copy_to_send_buffer.f90
!!@brief  module copy_to_send_buffer
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine copy_to_send_buf_N(NB, ntot_send, X_org, WS)
!!      subroutine swap_to_send_buf_N(NB, nnod_org,                     &
!!     &          npe_send, nnod_send, istack_send, X_org, WS)
!!
!!      subroutine copy_to_send_buf_int(ntot_send, iX_org, iWS)
!!
!!      subroutine copy_to_send_buf_3xN(NB, ntot_send,                  &
!!     &                                X1_org, X2_org, X3_org, WS)
!!      subroutine swap_to_send_buf_3xN(NB, nnod_org,                   &
!!     &          npe_send, nnod_send, istack_send,                     &
!!     &          X1_org, X2_org, X3_org, WS)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to send
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  X_org(NB*nnod_org)   Send data
!
      module copy_to_send_buffer
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
      subroutine copy_to_send_buf_N(NB, ntot_send, X_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: ntot_send
      real (kind=kreal), intent(in) :: X_org(NB*ntot_send)
!
      real (kind=kreal), intent(inout):: WS(NB*ntot_send)
!
!
!$omp parallel workshare
       WS(1:NB*ntot_send)= X_org(1:NB*ntot_send)
!$omp end parallel workshare
!
      end subroutine copy_to_send_buf_N
!
! ----------------------------------------------------------------------
!
      subroutine swap_to_send_buf_N(NB, nnod_org,                       &
     &          npe_send, nnod_send, istack_send, X_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(NB*nnod_send)
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
            jj = nd + (k+ist - 1) * NB
            kk = k + (nd-1)*num + NB*ist
            WS(kk         ) = X_org(jj)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine swap_to_send_buf_N
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_int(ntot_send, iX_org, iWS)
!
      integer(kind = kint), intent(in) :: ntot_send
      integer (kind=kint), intent(in)::    iX_org(ntot_send)
!
      integer (kind=kint), intent(inout):: iWS(ntot_send)
!
!
!$omp parallel workshare
       iWS(1:ntot_send)= iX_org(1:ntot_send)
!$omp end parallel workshare
!
      end subroutine copy_to_send_buf_int
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_to_send_buf_3xN(NB, ntot_send,                    &
     &                                X1_org, X2_org, X3_org, WS)
!
      integer(kind = kint), intent(in) :: NB, ntot_send
      real (kind=kreal), intent(in)::    X1_org(NB*ntot_send)
      real (kind=kreal), intent(in)::    X2_org(NB*ntot_send)
      real (kind=kreal), intent(in)::    X3_org(NB*ntot_send)
!
      real (kind=kreal), intent(inout):: WS(3*NB*ntot_send)
!
      integer (kind = kint) :: k
!
!
!$omp parallel do private(k)
      do k = 1, ntot_send
        WS(3*NB*k-3*NB+1:3*NB*k-2*NB) = X1_org(NB*k-NB+1:NB*k)
        WS(3*NB*k-2*NB+1:3*NB*k-  NB) = X2_org(NB*k-NB+1:NB*k)
        WS(3*NB*k-  NB+1:3*NB*k     ) = X3_org(NB*k-NB+1:NB*k)
      end do
!$omp end parallel do
!
      end subroutine copy_to_send_buf_3xN
!
! ----------------------------------------------------------------------
!
      subroutine swap_to_send_buf_3xN(NB, nnod_org,                     &
     &          npe_send, nnod_send, istack_send,                       &
     &          X1_org, X2_org, X3_org, WS)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: npe_send, nnod_send
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      real (kind=kreal), intent(in)::    X1_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(3*NB*nnod_send)
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
            jj = nd + (k+ist - 1) * NB
            kk = k + (nd-1)*num + NB*ist
            WS(kk         ) = X1_org(jj)
            WS(kk+  nd*num) = X2_org(jj)
            WS(kk+2*nd*num) = X3_org(jj)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine swap_to_send_buf_3xN
!
! ----------------------------------------------------------------------
!
      end module copy_to_send_buffer
