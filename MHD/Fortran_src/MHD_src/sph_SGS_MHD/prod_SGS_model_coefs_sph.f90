!!@brief  module prod_SGS_model_coefs_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine product_model_coefs_pin                              &
!!     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!!      subroutine product_single_buo_coefs_pin                         &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
!!      subroutine product_double_buo_coefs_pin                         &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_simi)
!!
!!      subroutine product_model_coefs_pout                             &
!!     &         (numdir, nnod_rtp, nnod_med, nphi, sgs_c, frc_rtp)
!!      subroutine product_single_buo_coefs_pout                        &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
!!      subroutine product_double_buo_coefs_pout                        &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_simi)
!!
!!
!!      subroutine prod_dbl_radial_buo_coefs_pin                        &
!!     &         (nnod_rtp, nidx_rtp, sgs_c1, sgs_c2, frc_rtp)
!!      subroutine prod_dbl_radial_buo_coefs_pout                       &
!!     &         (nnod_rtp, nidx_rtp, sgs_c1, sgs_c2, frc_rtp)
!!      subroutine prod_dbl_radial_buo_coefs_rj                         &
!!     &         (nnod_rj, nidx_rj, sgs_c1, sgs_c2, d_rj)
!!
!!      subroutine product_single_vol_buo_coefs(nnod, sgs_c, d_nod)
!!      subroutine product_double_vol_buo_coefs                         &
!!     &         (nnod, sgs_c1, sgs_c2, d_nod)
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
!$omp do private(kl,m,i1)
      do kl = 1, nnod_med
        do m = 1, nphi
          i1 = m + (kl-1)*nphi
          frc_rtp(i1,1) = (one + sgs_c(kl)) * frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c(kl)) * frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c(kl)) * frc_rtp(i1,3)
        end do
      end do
!$omp end do
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
!$omp do private(kl,m,i1)
      do kl = 1, nnod_med
        do m = 1, nphi
          i1 = m + (kl-1)*nphi
          frc_rtp(i1,1) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,3)
        end do
      end do
!$omp end do
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
      do nd = 1, numdir
!$omp do private(m,kl,i1)
        do m = 1, nphi
          do kl = 1, nnod_med
            i1 = kl + (m-1)*nnod_med
            frc_rtp(i1,nd) = sgs_c(kl) * frc_rtp(i1,nd)
          end do
        end do
!$omp end do
     end do
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
!$omp do private(m,kl,i1)
      do m = 1, nphi
        do kl = 1, nnod_med
          i1 = kl + (m-1)*nnod_med
          frc_rtp(i1,1) = (one + sgs_c(kl)) * frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c(kl)) * frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c(kl)) * frc_rtp(i1,3)
        end do
      end do
!$omp end do
!
      end subroutine product_single_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_buo_coefs_pout                          &
     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c1(nnod_med)
      real(kind = kreal), intent(in) :: sgs_c2(nnod_med)
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,n_vector)
!
!
      integer(kind = kint) :: kl, m, i1
!
!
!$omp do private(m,kl,i1)
      do m = 1, nphi
        do kl = 1, nnod_med
          i1 = kl + (m-1)*nnod_med
          frc_rtp(i1,1) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c1(kl) + sgs_c2(kl))*frc_rtp(i1,3)
        end do
      end do
!$omp end do
!
      end subroutine product_double_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine prod_dbl_radial_buo_coefs_pin                          &
     &         (nnod_rtp, nidx_rtp, sgs_c1, sgs_c2, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(in) :: sgs_c1(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sgs_c2(nidx_rtp(1))
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,n_vector)
!
      integer(kind = kint) :: k, l, m, i1
!
!
!$omp do private(k,l,m,i1)
      do l = 1, nidx_rtp(2)
        do k = 1, nidx_rtp(1)
          do m = 1, nidx_rtp(3)
            i1 = m + (k-1) * nidx_rtp(3)                                &
     &          + (l-1) * nidx_rtp(2)*nidx_rtp(3)
            frc_rtp(i1,1) = (one + sgs_c1(k) + sgs_c2(k))*frc_rtp(i1,1)
            frc_rtp(i1,2) = (one + sgs_c1(k) + sgs_c2(k))*frc_rtp(i1,2)
            frc_rtp(i1,3) = (one + sgs_c1(k) + sgs_c2(k))*frc_rtp(i1,3)
          end do
        end do
      end do
!$omp end do
!
      end subroutine prod_dbl_radial_buo_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine prod_dbl_radial_buo_coefs_pout                         &
     &         (nnod_rtp, nidx_rtp, sgs_c1, sgs_c2, frc_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: sgs_c1(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sgs_c2(nidx_rtp(1))
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,n_vector)
!
!
      integer(kind = kint) :: k, lm, i1
!
!
!$omp do private(k,lm,i1)
      do lm = 1, nidx_rtp(2)*nidx_rtp(3)
        do k = 1, nidx_rtp(1)
          i1 = k + (lm-1) * nidx_rtp(1)
          frc_rtp(i1,1) = (one + sgs_c1(k) + sgs_c2(k))*frc_rtp(i1,1)
          frc_rtp(i1,2) = (one + sgs_c1(k) + sgs_c2(k))*frc_rtp(i1,2)
          frc_rtp(i1,3) = (one + sgs_c1(k) + sgs_c2(k))*frc_rtp(i1,3)
        end do
      end do
!$omp end do
!
      end subroutine prod_dbl_radial_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine prod_dbl_radial_buo_coefs_rj                           &
     &         (nnod_rj, nidx_rj, sgs_c1, sgs_c2, d_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint), intent(in) :: nidx_rj(2)
!
      real(kind = kreal), intent(in) :: sgs_c1(0:nidx_rj(1))
      real(kind = kreal), intent(in) :: sgs_c2(0:nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,n_vector)
!
      integer(kind = kint) :: k, j, i1
!
!
!$omp do private(k,j,i1)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
            i1 = j + (k-1) * nidx_rj(2)
            d_rj(i1,1) = (one + sgs_c1(k) + sgs_c2(k))*d_rj(i1,1)
            d_rj(i1,2) = (one + sgs_c1(k) + sgs_c2(k))*d_rj(i1,2)
            d_rj(i1,3) = (one + sgs_c1(k) + sgs_c2(k))*d_rj(i1,3)
          end do
      end do
!$omp end do
!
      end subroutine prod_dbl_radial_buo_coefs_rj
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine product_single_vol_buo_coefs(nnod, sgs_c, d_nod)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: sgs_c
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,n_vector)
!
!
!$omp workshare
      d_nod(1:nnod,1) = (one + sgs_c) * d_nod(1:nnod,1)
      d_nod(1:nnod,2) = (one + sgs_c) * d_nod(1:nnod,2)
      d_nod(1:nnod,3) = (one + sgs_c) * d_nod(1:nnod,3)
!$omp end workshare
!
      end subroutine product_single_vol_buo_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_vol_buo_coefs                           &
     &         (nnod, sgs_c1, sgs_c2, d_nod)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: sgs_c1
      real(kind = kreal), intent(in) :: sgs_c2
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,n_vector)
!
!
!$omp workshare
      d_nod(1:nnod,1) = (one + sgs_c1 + sgs_c2)*d_nod(1:nnod,1)
      d_nod(1:nnod,2) = (one + sgs_c1 + sgs_c2)*d_nod(1:nnod,2)
      d_nod(1:nnod,3) = (one + sgs_c1 + sgs_c2)*d_nod(1:nnod,3)
!$omp end workshare
!
      end subroutine product_double_vol_buo_coefs
!
!  ---------------------------------------------------------------------
!
      end module prod_SGS_model_coefs_sph
 