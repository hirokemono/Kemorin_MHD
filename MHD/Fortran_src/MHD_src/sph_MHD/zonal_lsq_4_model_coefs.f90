!!@brief  module zonal_lsq_4_model_coefs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine sel_int_zonal_for_model_coefs                        &
!!     &         (numdir, nnod_rtp, nidx_rtp, frc_simi, frc_wide,       &
!!     &          sgs_zl, sgs_zt)
!!      subroutine sel_product_model_coefs                              &
!!     &         (numdir, nnod_rtp, nidx_rtp, sgs_c, frc_simi)
!!      subroutine cal_sph_model_coefs                                  &
!!     &         (numdir, nnod_med, sgs_zl, sgs_zt, sgs_c)
!!@endverbatim
!
      module zonal_lsq_4_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: int_zonal_for_model_coefs_pin
      private :: int_zonal_for_model_coefs_pout
      private :: product_model_coefs_pin, product_model_coefs_pout
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_int_zonal_for_model_coefs                          &
     &         (numdir, nnod_rtp, nidx_rtp, frc_simi, frc_wide,         &
     &          sgs_zl, sgs_zt)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(in) :: frc_simi(nnod_rtp,numdir)
      real(kind = kreal), intent(in) :: frc_wide(nnod_rtp,numdir)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(nidx_rtp(1)*nidx_rtp(2),numdir)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(nidx_rtp(1)*nidx_rtp(2),numdir)
!
      integer(kind = kint) :: nd
!
!
!$omp parallel
      if(iflag_FFT .eq. iflag_FFTW) then
        do nd = 1, numdir
          call int_zonal_for_model_coefs_pin(nnod_rtp, nidx_rtp,        &
     &        frc_simi(1,nd), frc_wide(1,nd),                           &
     &        sgs_zl(1,nd), sgs_zt(1,nd))
        end do
      else
        do nd = 1, numdir
          call int_zonal_for_model_coefs_pout(nnod_rtp, nidx_rtp,       &
     &        frc_simi(1,nd), frc_wide(1,nd),                           &
     &        sgs_zl(1,nd), sgs_zt(1,nd))
        end do
      end if
!$omp end parallel
!
      end subroutine sel_int_zonal_for_model_coefs
!
! ----------------------------------------------------------------------
!
      subroutine sel_product_model_coefs                                &
     &         (numdir, nnod_rtp, nidx_rtp, sgs_c, frc_simi)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(in) :: sgs_c(nidx_rtp(1)*nidx_rtp(2))
!
      real(kind = kreal), intent(inout) :: frc_simi(nnod_rtp,numdir)
!
      integer(kind = kint) :: nd
!
!
!$omp parallel
      if(iflag_FFT .eq. iflag_FFTW) then
        do nd = 1, numdir
          call product_model_coefs_pin(nnod_rtp, nidx_rtp,              &
     &        sgs_c, frc_simi(1,nd))
        end do
      else
        do nd = 1, numdir
          call product_model_coefs_pout(nnod_rtp, nidx_rtp,             &
     &        sgs_c, frc_simi(1,nd))
        end do
      end if
!$omp end parallel
!
      end subroutine sel_product_model_coefs
!
! ----------------------------------------------------------------------
!
      subroutine cal_sph_model_coefs                                    &
     &         (numdir, nnod_med, sgs_zl, sgs_zt, sgs_c)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numdir, nnod_med
      real(kind = kreal), intent(in) :: sgs_zl(nnod_med,numdir)
      real(kind = kreal), intent(in) :: sgs_zt(nnod_med,numdir)
!
      real(kind = kreal), intent(inout) :: sgs_c(nnod_med)
!
!
      if(numdir .eq. n_vector) then
        call cal_vector_sph_model_coefs                                 &
     &     (nnod_med, sgs_zl, sgs_zt, sgs_c)
      else
        call cal_scalar_sph_model_coefs                                 &
     &     (nnod_med, sgs_zl, sgs_zt, sgs_c)
      end if
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
          i1 = m + (kl-1)*nidx_rtp(3)
          sgs_zl(kl) = sgs_zl(kl) + frc_wide(i1) * frc_simi(i1)
          sgs_zt(kl) = sgs_zt(kl) + frc_wide(i1) * frc_wide(i1)
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
          sgs_zl(kl) = sgs_zl(kl) + frc_wide(i1) * frc_simi(i1)
          sgs_zt(kl) = sgs_zt(kl) + frc_wide(i1) * frc_wide(i1)
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
          i1 = m + (kl-1)*nidx_rtp(3)
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
!  ---------------------------------------------------------------------
!
      subroutine cal_scalar_sph_model_coefs                             &
     &         (nnod_med, sgs_zl, sgs_zt, sgs_c)
!
      real(kind = kreal), intent(in) :: sgs_zl(nnod_med)
      real(kind = kreal), intent(in) :: sgs_zt(nnod_med)
!
      real(kind = kreal), intent(inout) :: sgs_c(nnod_med)
!
      integer(kind = kint) :: nnod_med
!
!
!$omp parallel workshare
      sgs_c(1:nnod_med) = sgs_zl(1:nnod_med) / sgs_zt(1:nnod_med)
!$omp end parallel workshare
!
      end subroutine cal_scalar_sph_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine cal_vector_sph_model_coefs                             &
     &         (nnod_med, sgs_zl, sgs_zt, sgs_c)
!
      real(kind = kreal), intent(in) :: sgs_zl(nnod_med,3)
      real(kind = kreal), intent(in) :: sgs_zt(nnod_med,3)
!
      real(kind = kreal), intent(inout) :: sgs_c(nnod_med)
!
      integer(kind = kint) :: nnod_med
!
!
      write(*,*) 'tako', sgs_zl(:,1:3)
!$omp parallel workshare
      sgs_c(1:nnod_med) = (sgs_zl(1:nnod_med,1) / sgs_zt(1:nnod_med,1)  &
     &                   + sgs_zl(1:nnod_med,2) / sgs_zt(1:nnod_med,2)  &
     &                   + sgs_zl(1:nnod_med,3) / sgs_zt(1:nnod_med,3)) &
     &                    / three
!$omp end parallel workshare
!
      end subroutine cal_vector_sph_model_coefs
!
!  ---------------------------------------------------------------------
!
      end module zonal_lsq_4_model_coefs
 