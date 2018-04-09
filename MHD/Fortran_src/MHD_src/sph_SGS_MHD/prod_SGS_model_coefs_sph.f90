!!@brief  module prod_SGS_model_coefs_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine product_fixed_model_coefs                            &
!!     &         (const_Csim, sph_rtp, ifld, numdir, ncomp, frc_rtp)
!!      subroutine product_model_coefs_pin                              &
!!     &         (const_Csim, isgs, sph_rtp, sph_d_grp, nfld_sgs, sgs_c,&
!!     &          ifld, numdir, ncomp, frc_rtp)
!!      subroutine product_single_buo_coefs_pin                         &
!!     &         (sph_rtp, sph_d_grp, sgs_c, frc_rtp)
!!      subroutine product_double_buo_coefs_pin                         &
!!     &         (sph_rtp, sph_d_grp, sgs_c1, sgs_c2, frc_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!
!!      subroutine product_model_coefs_pout                             &
!!     &         (const_Csim, isgs, sph_rtp, sph_d_grp, nfld_sgs, sgs_c,&
!!     &          ifld, numdir, ncomp, frc_rtp)
!!      subroutine product_single_buo_coefs_pout                        &
!!     &         (sph_rtp, sph_d_grp, sgs_c, frc_rtp)
!!      subroutine product_double_buo_coefs_pout                        &
!!     &         (sph_rtp, sph_d_grp, sgs_c1, sgs_c2, frc_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!@endverbatim
!
      module prod_SGS_model_coefs_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_spheric_rtp_data
      use t_groups_sph_dynamic
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine product_fixed_model_coefs                              &
     &         (const_Csim, sph_rtp, ifld, numdir, ncomp, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      real(kind = kreal), intent(in) :: const_Csim
      integer(kind = kint), intent(in) :: ncomp, ifld, numdir
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,ncomp)
!
      integer(kind = kint) :: nd
!
!
!$omp parallel
      do nd = 0, numdir-1
!$omp workshare
        frc_rtp(1:sph_rtp%nnod_rtp,ifld+nd)                             &
     &        = const_Csim * frc_rtp(1:sph_rtp%nnod_rtp,ifld+nd)
!$omp end workshare
     end do
!$omp end parallel
!
      end subroutine product_fixed_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine product_model_coefs_pin                                &
     &         (const_Csim, isgs, sph_rtp, sph_d_grp, nfld_sgs, sgs_c,  &
     &          ifld, numdir, ncomp, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: const_Csim
      integer(kind = kint), intent(in) :: nfld_sgs, isgs
      real(kind = kreal), intent(in)                                    &
     &                   :: sgs_c(sph_d_grp%ngrp_dynamic,nfld_sgs)
      integer(kind = kint), intent(in) :: ncomp, ifld, numdir
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: frc_rtp(sph_rtp%nnod_rtp,ncomp)
!
      integer(kind = kint) :: nd, m, l, k, i1, klgrp
!
!
!$omp parallel
      do nd = 0, numdir-1
!$omp do private(m,l,k,i1,klgrp)
        do l = 1, sph_rtp%nidx_rtp(2)
          do k = 1, sph_rtp%nidx_rtp(1)
            klgrp = sph_d_grp%kgrp_dynamic(k)                           &
     &             + (sph_d_grp%lgrp_dynamic(l) - 1)                    &
     &              * (sph_d_grp%ngrp_rt(1) + 1)
            do m = 1, sph_rtp%nidx_rtp(3)
              i1 = m + (k-1) * sph_rtp%nidx_rtp(3)                      &
     &            + (l-1) * sph_rtp%nidx_rtp(3) * sph_rtp%nidx_rtp(1)
              frc_rtp(i1,ifld+nd) = const_Csim * sgs_c(klgrp,isgs)      &
     &                             * frc_rtp(i1,ifld+nd)
            end do
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
     &         (sph_rtp, sph_d_grp, sgs_c, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: sgs_c(sph_d_grp%ngrp_dynamic)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,n_vector)
!
      integer(kind = kint) :: m, l, k, i1, klgrp
!
!
!$omp parallel do private(m,l,k,i1,klgrp)
      do l = 1, sph_rtp%nidx_rtp(2)
        do k = 1, sph_rtp%nidx_rtp(1)
          klgrp = sph_d_grp%kgrp_dynamic(k)                             &
     &           + (sph_d_grp%lgrp_dynamic(l) - 1)                      &
     &            * (sph_d_grp%ngrp_rt(1) + 1)
          do m = 1, sph_rtp%nidx_rtp(3)
            i1 = m + (k-1) * sph_rtp%nidx_rtp(3)                        &
     &             + (l-1) * sph_rtp%nidx_rtp(3) * sph_rtp%nidx_rtp(1)
!
            frc_rtp(i1,1) = (one + sgs_c(klgrp)) * frc_rtp(i1,1)
            frc_rtp(i1,2) = (one + sgs_c(klgrp)) * frc_rtp(i1,2)
            frc_rtp(i1,3) = (one + sgs_c(klgrp)) * frc_rtp(i1,3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine product_single_buo_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_buo_coefs_pin                           &
     &         (sph_rtp, sph_d_grp, sgs_c1, sgs_c2, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: sgs_c1(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(in) :: sgs_c2(sph_d_grp%ngrp_dynamic)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,n_vector)
!
      integer(kind = kint) :: m, l, k, i1, klgrp
!
!
!$omp parallel do private(m,l,k,i1,klgrp)
      do l = 1, sph_rtp%nidx_rtp(2)
        do k = 1, sph_rtp%nidx_rtp(1)
          klgrp = sph_d_grp%kgrp_dynamic(k)                             &
     &           + (sph_d_grp%lgrp_dynamic(l) - 1)                      &
     &            * (sph_d_grp%ngrp_rt(1) + 1)
          do m = 1, sph_rtp%nidx_rtp(3)
            i1 = m + (k-1) * sph_rtp%nidx_rtp(3)                        &
     &             + (l-1) * sph_rtp%nidx_rtp(3) * sph_rtp%nidx_rtp(1)
!
            frc_rtp(i1,1)                                               &
     &            = (one + sgs_c1(klgrp) + sgs_c2(klgrp))*frc_rtp(i1,1)
            frc_rtp(i1,2)                                               &
     &            = (one + sgs_c1(klgrp) + sgs_c2(klgrp))*frc_rtp(i1,2)
            frc_rtp(i1,3)                                               &
     &            = (one + sgs_c1(klgrp) + sgs_c2(klgrp))*frc_rtp(i1,3)
          end do
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
     &         (const_Csim, isgs, sph_rtp, sph_d_grp, nfld_sgs, sgs_c,  &
     &          ifld, numdir, ncomp, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: const_Csim
      integer(kind = kint), intent(in) :: nfld_sgs, isgs
      real(kind = kreal), intent(in)                                    &
     &                   :: sgs_c(sph_d_grp%ngrp_dynamic,nfld_sgs)
!
      integer(kind = kint), intent(in) :: ncomp, ifld, numdir
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: frc_rtp(sph_rtp%nnod_rtp,ncomp)
!
      integer(kind = kint) :: nd, m, l, k, i1, klgrp
!
!
!$omp parallel
      do nd = 0, numdir-1
!$omp do private(m,l,k,i1,klgrp)
        do m = 1, sph_rtp%nidx_rtp(3)
          do l = 1, sph_rtp%nidx_rtp(2)
            do k = 1, sph_rtp%nidx_rtp(1)
              i1 = k + (l-1)*sph_rtp%nidx_rtp(1)                        &
     &               + (m-1)*sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(2)
               klgrp = sph_d_grp%kgrp_dynamic(k)                        &
     &                + (sph_d_grp%lgrp_dynamic(l) - 1)                 &
     &                 * (sph_d_grp%ngrp_rt(1) + 1)
!
              frc_rtp(i1,ifld+nd) = const_Csim * sgs_c(klgrp,isgs)      &
     &                             * frc_rtp(i1,ifld+nd)
            end do
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
     &         (sph_rtp, sph_d_grp, sgs_c, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: sgs_c(sph_d_grp%ngrp_dynamic)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,n_vector)
!
      integer(kind = kint) :: m, l, k, i1, klgrp
!
!
!$omp parallel do private(m,l,k,i1,klgrp)
      do m = 1, sph_rtp%nidx_rtp(3)
        do l = 1, sph_rtp%nidx_rtp(2)
          do k = 1, sph_rtp%nidx_rtp(1)
            i1 = k + (l-1)*sph_rtp%nidx_rtp(1)                          &
     &             + (m-1)*sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(2)
            klgrp = sph_d_grp%kgrp_dynamic(k)                           &
     &             + (sph_d_grp%lgrp_dynamic(l) - 1)                    &
     &              * (sph_d_grp%ngrp_rt(1) + 1)
!
            frc_rtp(i1,1) = (one + sgs_c(klgrp)) * frc_rtp(i1,1)
            frc_rtp(i1,2) = (one + sgs_c(klgrp)) * frc_rtp(i1,2)
            frc_rtp(i1,3) = (one + sgs_c(klgrp)) * frc_rtp(i1,3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine product_single_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine product_double_buo_coefs_pout                          &
     &         (sph_rtp, sph_d_grp, sgs_c1, sgs_c2, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: sgs_c1(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(in) :: sgs_c2(sph_d_grp%ngrp_dynamic)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: frc_rtp(sph_rtp%nnod_rtp,n_vector)
!
      integer(kind = kint) :: m, l, k, i1, klgrp
!
!
!$omp parallel do private(m,l,k,i1,klgrp)
      do m = 1, sph_rtp%nidx_rtp(3)
        do l = 1, sph_rtp%nidx_rtp(2)
          do k = 1, sph_rtp%nidx_rtp(1)
            i1 = k + (l-1)*sph_rtp%nidx_rtp(1)                          &
     &             + (m-1)*sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(2)
            klgrp = sph_d_grp%kgrp_dynamic(k)                           &
     &             + (sph_d_grp%lgrp_dynamic(l) - 1)                    &
     &              * (sph_d_grp%ngrp_rt(1) + 1)
!
            frc_rtp(i1,1)                                               &
     &            = (one + sgs_c1(klgrp) + sgs_c2(klgrp))*frc_rtp(i1,1)
            frc_rtp(i1,2)                                               &
     &            = (one + sgs_c1(klgrp) + sgs_c2(klgrp))*frc_rtp(i1,2)
            frc_rtp(i1,3)                                               &
     &            = (one + sgs_c1(klgrp) + sgs_c2(klgrp))*frc_rtp(i1,3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine product_double_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      end module prod_SGS_model_coefs_sph
 