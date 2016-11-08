!!@brief  module zonal_lsq_4_model_coefs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine cal_sph_model_coefs                                  &
!!     &          numdir, nidx_rtp, sgs_zl, sgs_zt, sgs_c)
!!
!!      subroutine int_zonal_for_model_coefs_pin                        &
!!     &        (nnod_rtp, nidx_rtp, frc_simi, frc_wide, sgs_zl, sgs_zt)
!!      subroutine int_zonal_for_model_coefs_pout                       &
!!     &        (nnod_rtp, nidx_rtp, frc_simi, frc_wide, sgs_zl, sgs_zt)
!!
!!      subroutine product_model_coefs_pin                              &
!!     &         (nnod_rtp, nidx_rtp, sgs_c, frc_rtp)
!!      subroutine product_model_coefs_pout                             &
!!     &         (nnod_rtp, nidx_rtp, sgs_c, frc_rtp)
!!@endverbatim
!
      module zonal_lsq_4_model_coefs
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sph_model_coefs                                    &
     &          numdir, nidx_rtp, sgs_zl, sgs_zt, sgs_c)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(nidx_rtp(1)*nidx_rtp(2),numdir)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(nidx_rtp(1)*nidx_rtp(2),numdir)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_c(nidx_rtp(1)*nidx_rtp(2))
!
!
      sgs_c(1:nidx_rtp(1)*nidx_rtp(2)) = 0.0d0
!
!$omp parallel
      do nd = 1, numdir
!$omp do private(kl)
        do kl = 1, nidx_rtp(1)*nidx_rtp(2)
          sgs_c(kl) = sgs_c(kl) + sgs_zl(kl,nd) / sgs_zt(kl,nd)
        end do
!$omp end do
      end do
!$omp end parallel
!
      sgs_c(1:nidx_rtp(1)*nidx_rtp(2))                                  &
     &          = sgs_c(1:nidx_rtp(1)*nidx_rtp(2)) / dble(numdir)
!
      end subroutine cal_sph_model_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_zonal_for_model_coefs_pin                          &
     &         (nnod_rtp, nidx_rtp, frc_simi, frc_wide, sgs_zl, sgs_zt)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(in) :: frc_simi(nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(nidx_rtp(1)*nidx_rtp(2))
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(nidx_rtp(1)*nidx_rtp(2))
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp workshare
      sgs_zl(1:nidx_rtp(1)*nidx_rtp(2)) = 0.0d0
      sgs_zt(1:nidx_rtp(1)*nidx_rtp(2)) = 0.0d0
!$omp end workshare
!
!$omp do private(kl,m,i1)
      do kl = 1, nidx_rtp(1)*nidx_rtp(2)
        sgs_zl(kl) = 0.0d0
        sgs_zt(kl) = 0.0d0
        do m = 1, nidx_rtp(3)
          i1 = m + (kl-1)*nidx_rtp(1)
          sgs_zl(kl) = sgs_l(kl) + frc_wide(i1) * frc_simi(i1)
          sgs_zt(kl) = sgs_t(kl) + frc_wide(i1) * frc_wide(i1)
        end do
      end do
!$omp end do
!
      end subroutine int_zonal_for_model_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine int_zonal_for_model_coefs_pout                         &
     &         (nnod_rtp, nidx_rtp, frc_simi, frc_wide, sgs_zl, sgs_zt)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: ncomp_frc
!
      real(kind = kreal), intent(in) :: frc_simi(nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(nidx_rtp(1)*nidx_rtp(2))
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(nidx_rtp(1)*nidx_rtp(2))
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp workshare
      sgs_zl(1:nidx_rtp(1)*nidx_rtp(2)) = 0.0d0
      sgs_zt(1:nidx_rtp(1)*nidx_rtp(2)) = 0.0d0
!$omp end workshare
!
      do m = 1, nidx_rtp(3)
!$omp do private(kl,i1)
        do kl = 1, nidx_rtp(1)*nidx_rtp(2)
          i1 = kl + (m-1)*nidx_rtp(1)*nidx_rtp(2)
          sgs_zl(kl) = sgs_l(kl) + frc_wide(i1) * frc_rtp(i1)
          sgs_zt(kl) = sgs_t(kl) + frc_wide(i1) * frc_wide(i1)
        end do
!$omp end do
      end do
 !
      end subroutine int_zonal_for_model_coefs_pout
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine product_model_coefs_pin                                &
     &         (nnod_rtp, nidx_rtp, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: sgs_c(nidx_rtp(1)*nidx_rtp(2))
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp)
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp do private(kl,m,i1)
      do kl = 1, nidx_rtp(1)*nidx_rtp(2)
        do m = 1, nidx_rtp(3)
          i1 = m + (kl-1)*nidx_rtp(1)
          frc_rtp(i1) = sgs_c(kl) * frc_rtp(i1)
        end do
      end do
!$omp end do
!
      end subroutine product_model_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine product_model_coefs_pout                               &
     &         (nnod_rtp, nidx_rtp, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind = kreal), intent(in)                                    &
     &                   :: sgs_c(nidx_rtp(1)*nidx_rtp(2))
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp)
!
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp do private(kl,m,i1)
      do m = 1, nidx_rtp(3)
        do kl = 1, nidx_rtp(1)*nidx_rtp(2)
          i1 = kl + (m-1)*nidx_rtp(1)*nidx_rtp(2)
          frc_rtp(i1) = sgs_c(kl) * frc_rtp(i1)
        end do
      end do
!$omp end do
 !
      end subroutine product_model_coefs_pout
!
!  ---------------------------------------------------------------------
!
      end module zonal_lsq_4_model_coefs
 