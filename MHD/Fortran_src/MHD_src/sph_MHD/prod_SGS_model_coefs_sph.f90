!!@brief  module prod_SGS_model_coefs_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine sel_product_model_coefs                              &
!!     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
!!      subroutine sel_product_single_buo_coefs(numdir,                 &
!!     &          nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
!!      subroutine sel_product_double_buo_coefs(numdir,                 &
!!     &         nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_simi)
!!
!!      subroutine product_model_coefs_pout                             &
!!     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!!@endverbatim
!
      module prod_SGS_model_coefs_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: product_model_coefs_pin
      private :: product_single_buo_coefs_pin
      private :: product_double_buo_coefs_pin
      private :: product_single_buo_coefs_pout
      private :: product_double_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_product_model_coefs                                &
     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_simi(nnod_rtp,numdir)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call product_model_coefs_pin                                    &
     &     (numdir, nnod_rtp, nnod_med, nphi,  sgs_c, frc_simi)
      else
        call product_model_coefs_pout                                   &
     &     (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
      end if
!
      end subroutine sel_product_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine sel_product_single_buo_coefs(numdir,                   &
     &          nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_simi(nnod_rtp,numdir)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call product_single_buo_coefs_pin                               &
     &     (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
      else
        call product_single_buo_coefs_pout                              &
     &     (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
      end if
!
      end subroutine sel_product_single_buo_coefs
!
! ----------------------------------------------------------------------
!
      subroutine sel_product_double_buo_coefs(numdir,                   &
     &         nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_simi)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c1(nnod_med)
      real(kind = kreal), intent(in) :: sgs_c2(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_simi(nnod_rtp,numdir)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call product_double_buo_coefs_pin                               &
     &     (numdir, nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_simi)
      else
        call product_double_buo_coefs_pout                              &
     &     (numdir, nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_simi)
      end if
!
      end subroutine sel_product_double_buo_coefs
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine product_model_coefs_pin                                &
     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
!
      real(kind = kreal), intent(in)                                    &
     &                   :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,numdir)
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 1, numdir
!$omp do private(kl,m,i1)
        do kl = 1, nnod_med
          do m = 1, nphi
            i1 = m + (kl-1)*nphi
            frc_rtp(i1,nd) = sgs_c(kl) * frc_rtp(i1,nd)
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
     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
!
      real(kind = kreal), intent(in) :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,numdir)
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 1, numdir
!$omp do private(kl,m,i1)
        do kl = 1, nnod_med
          do m = 1, nphi
            i1 = m + (kl-1)*nphi
            frc_rtp(i1,nd) = (one + sgs_c(kl)) * frc_rtp(i1,nd)
          end do
        end do
!$omp end do
     end do
!$omp end parallel
!
      end subroutine product_single_buo_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_buo_coefs_pin                           &
     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2,       &
     &          frc_rtp)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
!
      real(kind = kreal), intent(in) :: sgs_c1(nnod_med)
      real(kind = kreal), intent(in) :: sgs_c2(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,numdir)
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 1, numdir
!$omp do private(kl,m,i1)
        do kl = 1, nnod_med
          do m = 1, nphi
            i1 = m + (kl-1)*nphi
            frc_rtp(i1,nd) = (one + sgs_c1(kl) + sgs_c2(kl))            &
     &                      * frc_rtp(i1,nd)
          end do
        end do
!$omp end do
     end do
!$omp end parallel
!
      end subroutine product_double_buo_coefs_pin
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine product_model_coefs_pout                               &
     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,numdir)
!
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 1, numdir
!$omp do private(kl,m,i1)
        do m = 1, nphi
          do kl = 1, nnod_med
            i1 = kl + (m-1)*nnod_med
            frc_rtp(i1,nd) = sgs_c(kl) * frc_rtp(i1,nd)
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
     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,numdir)
!
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 1, numdir
!$omp do private(kl,m,i1)
        do m = 1, nphi
          do kl = 1, nnod_med
            i1 = kl + (m-1)*nnod_med
            frc_rtp(i1,nd) = (one + sgs_c(kl)) * frc_rtp(i1,nd)
          end do
        end do
!$omp end do
     end do
!$omp end parallel
!
      end subroutine product_single_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_buo_coefs_pout                          &
     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2,       &
     &          frc_rtp)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c1(nnod_med)
      real(kind = kreal), intent(in) :: sgs_c2(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,numdir)
!
!
      integer(kind = kint) :: kl, m, i1, nd
!
!
!$omp parallel
      do nd = 1, numdir
!$omp do private(kl,m,i1)
        do m = 1, nphi
          do kl = 1, nnod_med
            i1 = kl + (m-1)*nnod_med
            frc_rtp(i1,nd) = (one + sgs_c1(kl) + sgs_c2(kl))            &
     &                      * frc_rtp(i1,nd)
          end do
        end do
!$omp end do
     end do
!$omp end parallel
!
      end subroutine product_double_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      end module prod_SGS_model_coefs_sph
 