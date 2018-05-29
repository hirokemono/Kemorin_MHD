!!@brief  module zonal_lsq_4_model_coefs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine sel_int_zonal_4_model_coefs(sph_rtp, sph_d_grp,      &
!!     &          numdir, frc_simi, frc_wide,  sgs_zl, sgs_zt)
!!      subroutine sel_int_zonal_4_buo_coefs(sph_rtp, sph_d_grp,        &
!!     &          frc_simi, frc_wide, sgs_zl, sgs_zt)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!@endverbatim
!
      module zonal_lsq_4_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_groups_sph_dynamic
!
      implicit none
!
      private :: int_zonal_for_model_coefs_pin
      private :: int_zonal_for_model_coefs_pout
      private :: int_zonal_buo_coefs_pin, int_zonal_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_int_zonal_4_model_coefs(sph_rtp, sph_d_grp,        &
     &          numdir, frc_simi, frc_wide,  sgs_zl, sgs_zt)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: frc_simi(sph_rtp%nnod_rtp,numdir)
      real(kind = kreal), intent(in) :: frc_wide(sph_rtp%nnod_rtp,numdir)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic,numdir)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic,numdir)
!
      integer(kind = kint) :: nd
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        do nd = 1, numdir
          call int_zonal_for_model_coefs_pin(sph_rtp, sph_d_grp,        &
     &      frc_simi(1,nd), frc_wide(1,nd), sgs_zl(1,nd), sgs_zt(1,nd))
        end do
      else
        do nd = 1, numdir
          call int_zonal_for_model_coefs_pout(sph_rtp, sph_d_grp,       &
     &      frc_simi(1,nd), frc_wide(1,nd), sgs_zl(1,nd), sgs_zt(1,nd))
        end do
      end if
!
      end subroutine sel_int_zonal_4_model_coefs
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_zonal_4_buo_coefs(sph_rtp, sph_d_grp,          &
     &          frc_simi, frc_wide, sgs_zl, sgs_zt)
!
      use m_FFT_selector
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: frc_simi(sph_rtp%nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(sph_rtp%nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call int_zonal_buo_coefs_pin(sph_rtp, sph_d_grp,                &
     &      frc_simi, frc_wide, sgs_zl, sgs_zt)
      else
        call int_zonal_buo_coefs_pout(sph_rtp, sph_d_grp,               &
     &      frc_simi, frc_wide, sgs_zl, sgs_zt)
      end if
!
      end subroutine sel_int_zonal_4_buo_coefs
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_zonal_for_model_coefs_pin(sph_rtp, sph_d_grp,      &
     &          frc_simi, frc_wide, sgs_zl, sgs_zt)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: frc_simi(sph_rtp%nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(sph_rtp%nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
      integer(kind = kint) :: klgrp, kgrp, lgrp, kst, ked, lst, led
      integer(kind = kint) :: k,l,m,kr,lt,i1
!
!
!$omp parallel workshare
      sgs_zl(1:sph_d_grp%ngrp_dynamic) = 0.0d0
      sgs_zt(1:sph_d_grp%ngrp_dynamic) = 0.0d0
!$omp end parallel workshare
!
!$omp  parallel do                                                      &
!$omp& private(klgrp,kgrp,lgrp,kst,ked,lst,led,k,l,kr,lt,i1)
      do lgrp = 1, sph_d_grp%ngrp_rt(2)
        lst = sph_d_grp%istack_dynamic_lt(lgrp-1) + 1
        led = sph_d_grp%istack_dynamic_lt(lgrp)
        do kgrp = 1, sph_d_grp%ngrp_rt(1)
          kst = sph_d_grp%istack_dynamic_kr(kgrp-1) + 1
          ked = sph_d_grp%istack_dynamic_kr(kgrp)
          klgrp = kgrp + (lgrp - 1) * (sph_d_grp%ngrp_rt(1) + 1)
!
          do l = lst, led
            lt = sph_d_grp%lt_dynamic(l)
            do k = kst, ked
              kr = sph_d_grp%kr_dynamic(k)
              do m = 1, sph_rtp%nidx_rtp(3)
                i1 = m + (kr-1)*sph_rtp%nidx_rtp(3)                     &
     &                 + (lt-1)*sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(3)
!
                sgs_zl(klgrp) = sgs_zl(klgrp)                           &
     &                         + frc_wide(i1) * frc_simi(i1)
                sgs_zt(klgrp) = sgs_zt(klgrp)                           &
     &                         + frc_wide(i1) * frc_wide(i1)
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      do lgrp = 1, sph_d_grp%ngrp_rt(2)
        klgrp = lgrp * (sph_d_grp%ngrp_rt(1) + 1)
        sgs_zt(klgrp) = 1.0d-30
      end do
!
      end subroutine int_zonal_for_model_coefs_pin
!
!  ---------------------------------------------------------------------
!
      subroutine int_zonal_buo_coefs_pin(sph_rtp, sph_d_grp,            &
     &          frc_simi, frc_wide, sgs_zl, sgs_zt)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: frc_simi(sph_rtp%nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(sph_rtp%nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
      integer(kind = kint) :: klgrp, kgrp, lgrp, kst, ked, lst, led
      integer(kind = kint) :: k,l,m,kr,lt,i1
!
!
!$omp parallel workshare
      sgs_zl(1:sph_d_grp%ngrp_dynamic) = 0.0d0
      sgs_zt(1:sph_d_grp%ngrp_dynamic) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(klgrp,kgrp,lgrp,kst,ked,lst,led,k,l,kr,lt,i1)
      do lgrp = 1, sph_d_grp%ngrp_rt(2)
        lst = sph_d_grp%istack_dynamic_lt(lgrp-1) + 1
        led = sph_d_grp%istack_dynamic_lt(lgrp)
        do kgrp = 1, sph_d_grp%ngrp_rt(1)
          kst = sph_d_grp%istack_dynamic_kr(kgrp-1) + 1
          ked = sph_d_grp%istack_dynamic_kr(kgrp)
          klgrp = kgrp + (lgrp - 1) * (sph_d_grp%ngrp_rt(1) + 1)
!
          do l = lst, led
            lt = sph_d_grp%lt_dynamic(l)
            do k = kst, ked
              kr = sph_d_grp%kr_dynamic(k)
              do m = 1, sph_rtp%nidx_rtp(3)
                i1 = m + (kr-1)*sph_rtp%nidx_rtp(3)                     &
     &                 + (lt-1)*sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(3)
!
                sgs_zl(klgrp) = sgs_zl(klgrp)                           &
     &                         + abs(frc_wide(i1) * frc_simi(i1))
                sgs_zt(klgrp) = sgs_zt(klgrp)                           &
     &                         + frc_wide(i1) * frc_wide(i1)
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      do lgrp = 1, sph_d_grp%ngrp_rt(2)
        klgrp = lgrp * (sph_d_grp%ngrp_rt(1) + 1)
        sgs_zt(klgrp) = 1.0d-30
      end do
!
      end subroutine int_zonal_buo_coefs_pin
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_zonal_for_model_coefs_pout(sph_rtp, sph_d_grp,     &
     &          frc_simi, frc_wide, sgs_zl, sgs_zt)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: frc_simi(sph_rtp%nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(sph_rtp%nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
      integer(kind = kint) :: klgrp, kgrp, lgrp, kst, ked, lst, led
      integer(kind = kint) :: k,l,m,kr,lt,i1
!
!
!$omp parallel workshare
      sgs_zl(1:sph_d_grp%ngrp_dynamic) = 0.0d0
      sgs_zt(1:sph_d_grp%ngrp_dynamic) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel
      do m = 1, sph_rtp%nidx_rtp(3)
!$omp do private(klgrp,kgrp,lgrp,kst,ked,lst,led,k,l,kr,lt,i1)
        do lgrp = 1, sph_d_grp%ngrp_rt(2)
          lst = sph_d_grp%istack_dynamic_lt(lgrp-1) + 1
          led = sph_d_grp%istack_dynamic_lt(lgrp)
          do kgrp = 1, sph_d_grp%ngrp_rt(1)
            kst = sph_d_grp%istack_dynamic_kr(kgrp-1) + 1
            ked = sph_d_grp%istack_dynamic_kr(kgrp)
            klgrp = kgrp + (lgrp - 1) * (sph_d_grp%ngrp_rt(1) + 1)
!
            do l = lst, led
              lt = sph_d_grp%lt_dynamic(l)
              do k = kst, ked
                kr = sph_d_grp%kr_dynamic(k)
                i1 = kr + (lt-1)*sph_rtp%nidx_rtp(1)                    &
     &                  + (m-1)*sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(2)
!
                sgs_zl(klgrp) = sgs_zl(klgrp)                           &
     &                         + frc_wide(i1) * frc_simi(i1)
                sgs_zt(klgrp) = sgs_zt(klgrp)                           &
     &                         + frc_wide(i1) * frc_wide(i1)
              end do
            end do
          end do
        end do
!$omp end do
      end do
!$omp end parallel
!
      do lgrp = 1, sph_d_grp%ngrp_rt(2)
        klgrp = lgrp * (sph_d_grp%ngrp_rt(1) + 1)
        sgs_zt(klgrp) = 1.0d-30
      end do
!
      end subroutine int_zonal_for_model_coefs_pout
!
!  ---------------------------------------------------------------------
!
      subroutine int_zonal_buo_coefs_pout(sph_rtp, sph_d_grp,           &
     &          frc_simi, frc_wide,  sgs_zl, sgs_zt)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in) :: frc_simi(sph_rtp%nnod_rtp)
      real(kind = kreal), intent(in) :: frc_wide(sph_rtp%nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
      integer(kind = kint) :: klgrp, kgrp, lgrp, kst, ked, lst, led
      integer(kind = kint) :: k,l,m,kr,lt,i1
!
!
!$omp parallel workshare
      sgs_zl(1:sph_d_grp%ngrp_dynamic) = 0.0d0
      sgs_zt(1:sph_d_grp%ngrp_dynamic) = 0.0d0
!$omp end parallel workshare
!
      do m = 1, sph_rtp%nidx_rtp(3)
!$omp parallel do private(klgrp,kgrp,lgrp,kst,ked,lst,led,k,l,kr,lt,i1)
        do lgrp = 1, sph_d_grp%ngrp_rt(2)
          lst = sph_d_grp%istack_dynamic_lt(lgrp-1) + 1
          led = sph_d_grp%istack_dynamic_lt(lgrp)
          do kgrp = 1, sph_d_grp%ngrp_rt(1)
            kst = sph_d_grp%istack_dynamic_kr(kgrp-1) + 1
            ked = sph_d_grp%istack_dynamic_kr(kgrp)
            klgrp = kgrp + (lgrp - 1) * (sph_d_grp%ngrp_rt(1) + 1)
!
            do l = lst, led
              lt = sph_d_grp%lt_dynamic(l)
              do k = kst, ked
                kr = sph_d_grp%kr_dynamic(k)
                i1 = kr + (lt-1)*sph_rtp%nidx_rtp(1)                    &
     &                  + (m-1)*sph_rtp%nidx_rtp(1)*sph_rtp%nidx_rtp(2)
!
                sgs_zl(klgrp) = sgs_zl(klgrp)                           &
     &                         + abs(frc_wide(i1) * frc_simi(i1))
                sgs_zt(klgrp) = sgs_zt(klgrp)                           &
     &                         + frc_wide(i1) * frc_wide(i1)
              end do
            end do
          end do
        end do
!$omp end parallel do
      end do
!
      do lgrp = 1, sph_d_grp%ngrp_rt(2)
        klgrp = lgrp * (sph_d_grp%ngrp_rt(1) + 1)
        sgs_zt(klgrp) = 1.0d-30
      end do
!
      end subroutine int_zonal_buo_coefs_pout
!
!  ---------------------------------------------------------------------
!
      end module zonal_lsq_4_model_coefs
 