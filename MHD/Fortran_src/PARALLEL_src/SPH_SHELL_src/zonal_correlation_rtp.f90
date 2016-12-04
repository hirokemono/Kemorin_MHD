!>@file   zonal_correlation_rtp.f90
!!@brief  module zonal_correlation_rtp
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2014
!
!>@brief  Swap array for phi componene
!!
!!@verbatim
!!      subroutine ovrwrt_zonal_rms_ratio_rtp                           &
!!     &        (numdir, nnod_rtp, nidx_rtp, ref_rtp, dst_rtp)
!!      subroutine ovrwrt_zonal_correlate_rtp                           &
!!     &        (numdir, nnod_rtp, nidx_rtp, ref_rtp, dst_rtp)
!!@endverbatim
!
      module zonal_correlation_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit  none
! 
      real(kind = kreal), allocatable :: v1_ave(:)
      real(kind = kreal), allocatable :: v2_ave(:)
      real(kind = kreal), allocatable :: v1_msq(:)
      real(kind = kreal), allocatable :: v2_msq(:)
      real(kind = kreal), allocatable :: ratio(:)
      real(kind = kreal), allocatable :: corlat(:)
!
      private :: v1_ave, v2_ave, v1_msq, v2_msq, corlat
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine ovrwrt_zonal_rms_ratio_rtp                             &
     &        (numdir, nnod_rtp, nidx_rtp, ref_rtp, dst_rtp)
!
      integer(kind = kint), intent(in) :: numdir, nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(inout) :: ref_rtp(nnod_rtp,numdir)
      real(kind = kreal), intent(inout) :: dst_rtp(nnod_rtp,numdir)
!
      integer(kind = kint) :: i_klm, kr_lt, mphi, nd
!
!
      allocate(v1_msq(nidx_rtp(1)*nidx_rtp(2)))
      allocate(v2_msq(nidx_rtp(1)*nidx_rtp(2)))
      allocate(ratio(nidx_rtp(1)*nidx_rtp(2)))
!
!
      do nd = 1, numdir
!$omp parallel workshare
        v1_msq(1:nidx_rtp(1)*nidx_rtp(2)) = zero
        v2_msq(1:nidx_rtp(1)*nidx_rtp(2)) = zero
        ratio(1:nidx_rtp(1)*nidx_rtp(2)) = zero
!$omp end parallel workshare
!
        do mphi = 1, nidx_rtp(3)
!$omp parallel do private(i_klm,kr_lt)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            v1_msq(kr_lt) =  v1_msq(kr_lt) + ref_rtp(i_klm,nd)**2
            v2_msq(kr_lt) =  v2_msq(kr_lt) + dst_rtp(i_klm,nd)**2
          end do
!$omp end parallel do
        end do
!$omp parallel do private(kr_lt)
        do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
          if(v1_msq(kr_lt) .le. zero) then
            ratio(kr_lt) =  zero
          else
            ratio(kr_lt) =  sqrt(v2_msq(kr_lt)) / sqrt(v1_msq(kr_lt))
          end if
        end do
!$omp end parallel do
!
        do mphi = 1, nidx_rtp(3)
!$omp parallel do private(i_klm,kr_lt)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            dst_rtp(i_klm,nd) = ratio(kr_lt)
          end do
!$omp end parallel do
        end do
      end do
!
      deallocate(v1_msq, v2_msq, ratio)
!
      end subroutine ovrwrt_zonal_rms_ratio_rtp
!
!-----------------------------------------------------------------------
!
      subroutine ovrwrt_zonal_correlate_rtp                             &
     &        (numdir, nnod_rtp, nidx_rtp, ref_rtp, dst_rtp)
!
      integer(kind = kint), intent(in) :: numdir, nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(inout) :: ref_rtp(nnod_rtp,numdir)
      real(kind = kreal), intent(inout) :: dst_rtp(nnod_rtp,numdir)
!
      integer(kind = kint) :: i_klm, kr_lt, mphi, nd
!
!
      allocate(v1_ave(nidx_rtp(1)*nidx_rtp(2)))
      allocate(v2_ave(nidx_rtp(1)*nidx_rtp(2)))
      allocate(v1_msq(nidx_rtp(1)*nidx_rtp(2)))
      allocate(v2_msq(nidx_rtp(1)*nidx_rtp(2)))
      allocate(corlat(nidx_rtp(1)*nidx_rtp(2)))
!
!
      do nd = 1, numdir
!$omp parallel workshare
        v1_ave(1:nidx_rtp(1)*nidx_rtp(2)) = zero
        v2_ave(1:nidx_rtp(1)*nidx_rtp(2)) = zero
        v1_msq(1:nidx_rtp(1)*nidx_rtp(2)) = zero
        v2_msq(1:nidx_rtp(1)*nidx_rtp(2)) = zero
        corlat(1:nidx_rtp(1)*nidx_rtp(2)) = zero
!$omp end parallel workshare
!
        do mphi = 1, nidx_rtp(3)
!$omp parallel do private(i_klm,kr_lt)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            v1_ave(kr_lt) =  v1_ave(kr_lt) + ref_rtp(i_klm,nd)
            v2_ave(kr_lt) =  v2_ave(kr_lt) + dst_rtp(i_klm,nd)
          end do
!$omp end parallel do
        end do
!
!$omp parallel do private(kr_lt)
        do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
          v1_ave(kr_lt) =  v1_ave(kr_lt) / dble(nidx_rtp(3))
          v2_ave(kr_lt) =  v2_ave(kr_lt) / dble(nidx_rtp(3))
        end do
!$omp end parallel do
!
        do mphi = 1, nidx_rtp(3)
!$omp parallel do private(i_klm,kr_lt)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            ref_rtp(i_klm,nd) = ref_rtp(i_klm,nd) - v1_ave(kr_lt)
            dst_rtp(i_klm,nd) = dst_rtp(i_klm,nd) - v2_ave(kr_lt)
          end do
!$omp end parallel do
        end do
!
        do mphi = 1, nidx_rtp(3)
!$omp parallel do private(i_klm,kr_lt)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            v1_msq(kr_lt) =  v1_msq(kr_lt) + ref_rtp(i_klm,nd)**2
            v2_msq(kr_lt) =  v2_msq(kr_lt) + dst_rtp(i_klm,nd)**2
            corlat(kr_lt) =  corlat(kr_lt)                              &
     &                      + ref_rtp(i_klm,nd)*dst_rtp(i_klm,nd)
          end do
!$omp end parallel do
        end do
!$omp parallel do private(kr_lt)
        do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
          if((v1_msq(kr_lt) * v2_msq(kr_lt)) .le. zero) then
            corlat(kr_lt) =  zero
          else
            corlat(kr_lt) =  corlat(kr_lt)                              &
     &                   / sqrt(v1_msq(kr_lt) * v2_msq(kr_lt))
          end if
        end do
!$omp end parallel do
!
        do mphi = 1, nidx_rtp(3)
!$omp parallel do private(i_klm,kr_lt)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            dst_rtp(i_klm,nd) = corlat(kr_lt)
          end do
!$omp end parallel do
        end do
      end do
!
      deallocate(v1_ave, v2_ave, v1_msq, v2_msq, corlat)
!
      end subroutine ovrwrt_zonal_correlate_rtp
!
!-----------------------------------------------------------------------
!
      end module zonal_correlation_rtp
