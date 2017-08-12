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
!!      subroutine product_model_coefs_pout(nnod_med, nphi, sgs_c,      &
!!     &          ifld, numdir, nnod_rtp, ncomp, frc_rtp)
!!
!!      subroutine product_single_buo_coefs_pout                        &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c, frc_simi)
!!      subroutine product_double_buo_coefs_pout                        &
!!     &         (nnod_rtp, nnod_med, nphi, sgs_c1, sgs_c2, frc_simi)
!!
!!
!!      subroutine prod_dbl_radial_buo_coefs_pin                        &
!!     &         (nidx_rtp, sgs_c, ifld, nnod_rtp, ncomp, frc_rtp)
!!      subroutine prod_dbl_radial_buo_coefs_pout                       &
!!     &         (nidx_rtp, sgs_c, ifld, nnod_rtp, ncomp, frc_rtp)
!!      subroutine prod_dbl_radial_buo_coefs_rj                         &
!!     &         (nidx_rj, sgs_c, ifld, nnod_rj, ncomp, d_rj)
!!
!!      subroutine product_single_vol_buo_coefs                         &
!!     &         (sgs_c, ifld, nnod, ncomp, d_nod)
!!      subroutine product_double_vol_buo_coefs                         &
!!     &         (sgs_c, ifld, nnod, ncomp, d_nod)
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
      integer(kind = kint), intent(in) :: nnod_med, nphi
      real(kind = kreal), intent(in) :: sgs_c1(nnod_med)
      real(kind = kreal), intent(in) :: sgs_c2(nnod_med)
      integer(kind = kint), intent(in) :: nnod_rtp
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
     &         (nidx_rtp, sgs_c, ifld, nnod_rtp, ncomp, frc_rtp)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: sgs_c(nidx_rtp(1),2)
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp, ifld
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,ncomp)
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
            frc_rtp(i1,ifld  ) = (one + sgs_c(k,1) + sgs_c(k,2))        &
     &                          * frc_rtp(i1,ifld  )
            frc_rtp(i1,ifld+1) = (one + sgs_c(k,1) + sgs_c(k,2))        &
     &                          * frc_rtp(i1,ifld+1)
            frc_rtp(i1,ifld+2) = (one + sgs_c(k,1) + sgs_c(k,2))        &
     &                          * frc_rtp(i1,ifld+2)
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
     &         (nidx_rtp, sgs_c, ifld, nnod_rtp, ncomp, frc_rtp)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: sgs_c(nidx_rtp(1),2)
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp, ifld
!
      real(kind = kreal), intent(inout) :: frc_rtp(nnod_rtp,ncomp)
!
!
      integer(kind = kint) :: k, lm, i1
!
!
!$omp do private(k,lm,i1)
      do lm = 1, nidx_rtp(2)*nidx_rtp(3)
        do k = 1, nidx_rtp(1)
          i1 = k + (lm-1) * nidx_rtp(1)
          frc_rtp(i1,ifld  ) = (one + sgs_c(k,1) + sgs_c(k,2))          &
     &                        * frc_rtp(i1,ifld  )
          frc_rtp(i1,ifld+1) = (one + sgs_c(k,1) + sgs_c(k,2))          &
     &                        * frc_rtp(i1,ifld+1)
          frc_rtp(i1,ifld+2) = (one + sgs_c(k,1) + sgs_c(k,2))          &
     &                        * frc_rtp(i1,ifld+2)
        end do
      end do
!$omp end do
!
      end subroutine prod_dbl_radial_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine prod_dbl_radial_buo_coefs_rj                           &
     &         (nidx_rj, sgs_c, ifld, nnod_rtp, ncomp, d_rj)
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
!
      real(kind = kreal), intent(in) :: sgs_c(0:nidx_rj(1),2)
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp, ifld
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rtp,ncomp)
!
      integer(kind = kint) :: k, j, i1
!
!
!$omp do private(k,j,i1)
      do k = 1, nidx_rj(1)
        do j = 1, nidx_rj(2)
            i1 = j + (k-1) * nidx_rj(2)
            d_rj(i1,ifld  ) = (one + sgs_c(k,1) + sgs_c(k,2))           &
     &                       * d_rj(i1,ifld  )
            d_rj(i1,ifld+1) = (one + sgs_c(k,1) + sgs_c(k,2))           &
     &                       * d_rj(i1,ifld+1)
            d_rj(i1,ifld+2) = (one + sgs_c(k,1) + sgs_c(k,2))           &
     &                       * d_rj(i1,ifld+2)
          end do
      end do
!$omp end do
!
      end subroutine prod_dbl_radial_buo_coefs_rj
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine product_single_vol_buo_coefs                           &
     &         (sgs_c, ifld, nnod, ncomp, d_nod)
!
      real(kind = kreal), intent(in) :: sgs_c
      integer(kind = kint), intent(in) :: nnod, ncomp, ifld
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp)
!
!
!$omp parallel workshare
      d_nod(1:nnod,ifld  ) = (one + sgs_c) * d_nod(1:nnod,ifld  )
      d_nod(1:nnod,ifld+1) = (one + sgs_c) * d_nod(1:nnod,ifld+1)
      d_nod(1:nnod,ifld+2) = (one + sgs_c) * d_nod(1:nnod,ifld+2)
!$omp end parallel workshare
!
      end subroutine product_single_vol_buo_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_vol_buo_coefs                           &
     &         (sgs_c, ifld, nnod, ncomp, d_nod)
!
      real(kind = kreal), intent(in) :: sgs_c(2)
      integer(kind = kint), intent(in) :: nnod, ncomp, ifld
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp)
!
!
!$omp parallel workshare
      d_nod(1:nnod,ifld  ) = (one + sgs_c(1) + sgs_c(2))                &
     &                      * d_nod(1:nnod,ifld  )
      d_nod(1:nnod,ifld+1) = (one + sgs_c(1) + sgs_c(2))                &
     &                      * d_nod(1:nnod,ifld+1)
      d_nod(1:nnod,ifld+2) = (one + sgs_c(1) + sgs_c(2))                &
     &                      * d_nod(1:nnod,ifld+2)
!$omp end parallel workshare
!
      end subroutine product_double_vol_buo_coefs
!
!  ---------------------------------------------------------------------
!
      end module prod_SGS_model_coefs_sph
 