!!@brief  module prod_SGS_model_coefs_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine product_model_coefs_pin                              &
!!     &         (isgs, nphi, nnod_med, nfld_sgs, sgs_c,                &
!!     &          ifld, numdir, nnod_rtp, ncomp, frc_rtp)
!!      subroutine product_single_buo_coefs_pin                         &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!!      subroutine product_double_buo_coefs_pin                         &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_rtp)
!!
!!      subroutine product_model_coefs_pout                             &
!!     &         (isgs, nphi, nnod_med, nfld_sgs, sgs_c,                &
!!     &          ifld, numdir, nnod_rtp, ncomp, frc_rtp)
!!      subroutine product_single_buo_coefs_pout                        &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!!      subroutine product_double_buo_coefs_pout                        &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_rtp)
!!@endverbatim
!
      module prod_SGS_model_coefs_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine product_model_coefs_pin                                &
     &         (isgs, nphi, nnod_med, nfld_sgs, sgs_c,                  &
     &          ifld, numdir, nnod_rtp, ncomp, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_med, nfld_sgs
      integer(kind = kint), intent(in) :: isgs, nphi
      real(kind = kreal), intent(in) :: sgs_c(nnod_med,nfld_sgs)
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp
      integer(kind = kint), intent(in) :: ifld, numdir
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,ncomp)
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 0, numdir-1
!$omp do private(kl,m,i1)
        do kl = 1, nnod_med
          do m = 1, nphi
            i1 = m + (kl-1)*nphi
            frc_rtp(i1,ifld+nd) = sgs_c(kl,isgs) * frc_rtp(i1,ifld+nd)
          end do
        end do
!$omp end do
     end do
!$omp end parallel
!
      end subroutine product_model_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine product_single_buo_coefs_pin                           &
     &         (nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
!
      real(kind = kreal), intent(in) :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,n_vector)
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp parallel do private(kl,m,i1)
      do kl = 1, nnod_med
        do m = 1, nphi
          i1 = m + (kl-1)*nphi
          frc_rtp(i1,1) = (one + sgs_c(kl)) * frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c(kl)) * frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c(kl)) * frc_rtp(i1,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine product_single_buo_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_buo_coefs_pin                           &
     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
!
      real(kind = kreal), intent(in) :: sgs_c1(nnod_med)
      real(kind = kreal), intent(in) :: sgs_c2(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,n_vector)
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp parallel do private(kl,m,i1)
      do kl = 1, nnod_med
        do m = 1, nphi
          i1 = m + (kl-1)*nphi
          frc_rtp(i1,1) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine product_double_buo_coefs_pin
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine product_model_coefs_pout                               &
     &         (isgs, nphi, nnod_med, nfld_sgs, sgs_c,                  &
     &          ifld, numdir, nnod_rtp, ncomp, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_med, nfld_sgs
      integer(kind = kint), intent(in) :: isgs, nphi
      real(kind = kreal), intent(in) :: sgs_c(nnod_med,nfld_sgs)
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp
      integer(kind = kint), intent(in) :: ifld, numdir
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,ncomp)
!
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 0, numdir-1
!$omp do private(m,kl,i1)
        do m = 1, nphi
          do kl = 1, nnod_med
            i1 = kl + (m-1)*nnod_med
            frc_rtp(i1,ifld+nd) = sgs_c(kl,isgs) * frc_rtp(i1,ifld+nd)
          end do
        end do
!$omp end do
     end do
!$omp end parallel
!
      end subroutine product_model_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine product_single_buo_coefs_pout                          &
     &         (nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,n_vector)
!
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp parallel do private(m,kl,i1)
      do m = 1, nphi
        do kl = 1, nnod_med
          i1 = kl + (m-1)*nnod_med
          frc_rtp(i1,1) = (one + sgs_c(kl)) * frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c(kl)) * frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c(kl)) * frc_rtp(i1,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine product_single_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_buo_coefs_pout                          &
     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c1(nnod_med)
      real(kind = kreal), intent(in) :: sgs_c2(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,n_vector)
!
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp parallel do private(m,kl,i1)
      do m = 1, nphi
        do kl = 1, nnod_med
          i1 = kl + (m-1)*nnod_med
          frc_rtp(i1,1) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine product_double_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      end module prod_SGS_model_coefs_sph
 