!>@file  cal_layerd_ave_correlate.f90
!!       module cal_layerd_ave_correlate
!!
!!@author H. Matsui
!!@date   Programmed in Nov., 2009
!
!> @brief Take correlation in one group
!!
!!@verbatim
!!      subroutine divide_layers_ave_by_vol                             &
!!     &         (n_layer_d, ncomp_sgl, ncomp_dble, numdir,             &
!!     &          a_vol_layer, ave_les, rms_les, ave_1, ave_2,          &
!!     &          rms_1, rms_2, rms_ratio)
!!      subroutine divide_all_layer_ave_by_vol                          &
!!     &         (ncomp_sgl, ncomp_dble, numdir, vol_d, ave_wg, rms_wg, &
!!     &          ave_1, ave_2, rms_1, rms_2, rms_ratio)
!!
!!      subroutine cal_layered_correlation                              &
!!     &         (n_layer_d, ncomp_sgl, ncomp_dble, numdir,             &
!!     &          a_vol_layer, cov_les, sig_les, cor_d, cov_d)
!!      subroutine cal_all_layer_correlation(ncomp_sgl, ncomp_dble,     &
!!     &          numdir, vol_d, cov_wg, sig_wg, cor_d, cov_d)
!!@endverbatim
!
      module cal_layerd_ave_correlate
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine divide_layers_ave_by_vol                               &
     &         (n_layer_d, ncomp_sgl, ncomp_dble, numdir,               &
     &          a_vol_layer, ave_les, rms_les, ave_1, ave_2,            &
     &          rms_1, rms_2, rms_ratio)
!
      integer (kind = kint), intent(in) :: n_layer_d, numdir
      integer (kind = kint), intent(in) :: ncomp_sgl, ncomp_dble
      real(kind = kreal), intent(in) :: a_vol_layer(n_layer_d)
      real(kind = kreal), intent(in) :: ave_les(n_layer_d,ncomp_dble)
      real(kind = kreal), intent(in) :: rms_les(n_layer_d,ncomp_dble)
!
      real(kind = kreal), intent(inout) :: ave_1(n_layer_d,numdir)
      real(kind = kreal), intent(inout) :: ave_2(n_layer_d,numdir)
      real(kind = kreal), intent(inout) :: rms_1(n_layer_d,numdir)
      real(kind = kreal), intent(inout) :: rms_2(n_layer_d,numdir)
      real(kind = kreal), intent(inout) :: rms_ratio(n_layer_d,numdir)
!
      integer (kind = kint) :: nd, nd2, igrp
!
!$omp parallel
      do nd = 1, numdir
        nd2 = nd + ncomp_sgl
!$omp do
        do igrp = 1, n_layer_d
          ave_1(igrp,nd) = ave_les(igrp,nd ) * a_vol_layer(igrp)
          ave_2(igrp,nd) = ave_les(igrp,nd2) * a_vol_layer(igrp)
          rms_1(igrp,nd) = rms_les(igrp,nd ) * a_vol_layer(igrp)
          rms_2(igrp,nd) = rms_les(igrp,nd2) * a_vol_layer(igrp)
        end do
!$omp end do nowait
!$omp do
        do igrp = 1, n_layer_d
          if ( rms_2(igrp,nd) .eq. 0.0d0) then
            rms_ratio(igrp,nd) = -1.0d0
          else
            rms_ratio(igrp,nd) = rms_1(igrp,nd) / rms_2(igrp,nd)
          end if
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine divide_layers_ave_by_vol
!
!  ---------------------------------------------------------------------
!
      subroutine divide_all_layer_ave_by_vol                            &
     &         (ncomp_sgl, ncomp_dble, numdir, vol_d, ave_wg, rms_wg,   &
     &          ave_1, ave_2, rms_1, rms_2, rms_ratio)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ncomp_sgl, ncomp_dble
      real(kind = kreal), intent(in) :: vol_d
      real(kind = kreal), intent(in) :: ave_wg(ncomp_dble)
      real(kind = kreal), intent(in) :: rms_wg(ncomp_dble)
!
      real(kind = kreal), intent(inout) :: ave_1(numdir)
      real(kind = kreal), intent(inout) :: ave_2(numdir)
      real(kind = kreal), intent(inout) :: rms_1(numdir)
      real(kind = kreal), intent(inout) :: rms_2(numdir)
      real(kind = kreal), intent(inout) :: rms_ratio(numdir)
!
      integer (kind = kint) :: nd, nd2
!
      do nd = 1, numdir
        nd2 = nd + ncomp_sgl
        ave_1(nd) = ave_wg(nd ) / vol_d
        ave_2(nd) = ave_wg(nd2) / vol_d
        rms_1(nd) = rms_wg(nd ) / vol_d
        rms_2(nd) = rms_wg(nd2) / vol_d
!
        if ( rms_2(nd) .eq. 0.0d0) then
          rms_ratio(nd) = -1.0d0
        else
          rms_ratio(nd) = rms_1(nd) / rms_2(nd)
        end if
      end do
!
      end subroutine divide_all_layer_ave_by_vol
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_layered_correlation                                &
     &         (n_layer_d, ncomp_sgl, ncomp_dble, numdir,               &
     &          a_vol_layer, cov_les, sig_les, cor_d, cov_d)
!
      integer (kind = kint), intent(in) :: n_layer_d, numdir
      integer (kind = kint), intent(in) :: ncomp_sgl, ncomp_dble
      real(kind = kreal), intent(in) :: a_vol_layer(n_layer_d)
      real(kind = kreal), intent(in) :: cov_les(n_layer_d,ncomp_sgl)
!
      real(kind = kreal), intent(inout) :: sig_les(n_layer_d,ncomp_dble)
      real(kind = kreal), intent(inout) :: cor_d(n_layer_d,numdir)
      real(kind = kreal), intent(inout) :: cov_d(n_layer_d,numdir)
!
      integer (kind = kint) ::nd, nd2, igrp
!
!$omp parallel
      do nd = 1, numdir
        nd2 = nd + ncomp_sgl
!$omp do
        do igrp = 1, n_layer_d
          sig_les(igrp,nd ) = sqrt( sig_les(igrp,nd ) )
          sig_les(igrp,nd2) = sqrt( sig_les(igrp,nd2) )
          cor_d(igrp,nd) = sig_les(igrp,nd)*sig_les(igrp,nd2)
        end do
!$omp end do nowait
!$omp do
        do igrp = 1, n_layer_d
          if ( cor_d(igrp,nd) .eq. 0.0d0) then
            cor_d(igrp,nd) = 0.0d0
          else
            cor_d(igrp,nd) = cov_les(igrp,nd) / cor_d(igrp,nd)
            cov_d(igrp,nd) = cov_les(igrp,nd) * a_vol_layer(igrp)
          end if
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine cal_layered_correlation
!
!  ---------------------------------------------------------------------
!
      subroutine cal_all_layer_correlation(ncomp_sgl, ncomp_dble,       &
     &          numdir, vol_d, cov_wg, sig_wg, cor_d, cov_d)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ncomp_sgl, ncomp_dble
      real(kind = kreal), intent(in) :: vol_d
      real(kind = kreal), intent(in) :: cov_wg(ncomp_sgl)
!
      real(kind = kreal), intent(inout) :: sig_wg(ncomp_dble)
      real(kind = kreal), intent(inout) :: cor_d(numdir)
      real(kind = kreal), intent(inout) :: cov_d(numdir)
!
      integer (kind = kint) ::nd, nd2
!
      do nd = 1, numdir
        nd2 = nd + ncomp_sgl
        sig_wg(nd ) = sqrt( sig_wg(nd ) )
        sig_wg(nd2) = sqrt( sig_wg(nd2) )
        cor_d(nd) = sig_wg(nd)*sig_wg(nd2)
!
        if ( cor_d(nd) .eq. 0.0d0) then
          cor_d(nd) = 0.0d0
        else
          cor_d(nd) = cov_wg(nd) / cor_d(nd)
          cov_d(nd) = cov_wg(nd) / vol_d
        end if
      end do
!
      end subroutine cal_all_layer_correlation
!
!  ---------------------------------------------------------------------
!
      end module cal_layerd_ave_correlate
