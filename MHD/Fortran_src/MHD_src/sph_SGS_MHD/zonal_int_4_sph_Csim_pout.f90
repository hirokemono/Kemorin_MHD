!>@file   zonal_int_4_sph_Csim_pout.f90
!!@brief  module zonal_int_4_sph_Csim_pout
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine zonal_int_vector_Csim_pout(sph_rtp, sph_d_grp,       &
!!     &          frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
!!      subroutine zonal_int_tentor_Csim_pout(sph_rtp, sph_d_grp,       &
!!     &          frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
!!
!!      subroutine int_zonal_buo_coefs_pout(sph_rtp, sph_d_grp,         &
!!     &          frc_simi, frc_wide,  sgs_zl, sgs_zt)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!@endverbatim
!
      module zonal_int_4_sph_Csim_pout
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
      subroutine zonal_int_vector_Csim_pout(sph_rtp, sph_d_grp,         &
     &          frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_simi(sph_rtp%nnod_rtp,3)
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_wide(sph_rtp%nnod_rtp,3)
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_dble(sph_rtp%nnod_rtp,3)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
      integer(kind = kint) :: klgrp, kgrp, lgrp, kst, ked, lst, led
      integer(kind = kint) :: k,l,m,kr,lt,i1
      real(kind = kreal) :: diff, sim2
!
!
!$omp parallel workshare
      sgs_zl(1:sph_d_grp%ngrp_dynamic) = 0.0d0
      sgs_zt(1:sph_d_grp%ngrp_dynamic) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel
      do m = 1, sph_rtp%nidx_rtp(3)
!$omp do private(klgrp,kgrp,lgrp,kst,ked,lst,led,k,l,kr,lt,i1,diff,sim2)
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
                diff                                                    &
     &            = (frc_wide(i1,1) - frc_dble(i1,1)) * frc_simi(i1,1)  &
     &            + (frc_wide(i1,2) - frc_dble(i1,2)) * frc_simi(i1,2)  &
     &            + (frc_wide(i1,3) - frc_dble(i1,3)) * frc_simi(i1,3)
                sim2 = frc_simi(i1,1) * frc_simi(i1,1)                  &
     &              +  frc_simi(i1,2) * frc_simi(i1,2)                  &
     &              +  frc_simi(i1,3) * frc_simi(i1,3)
!
                sgs_zl(klgrp) = sgs_zl(klgrp) + diff * sim2
                sgs_zt(klgrp) = sgs_zt(klgrp) + diff * diff
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
      end subroutine zonal_int_vector_Csim_pout
!
!  ---------------------------------------------------------------------
!
      subroutine zonal_int_tentor_Csim_pout(sph_rtp, sph_d_grp,         &
     &          frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_simi(sph_rtp%nnod_rtp,6)
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_wide(sph_rtp%nnod_rtp,6)
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_dble(sph_rtp%nnod_rtp,6)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
      integer(kind = kint) :: klgrp, kgrp, lgrp, kst, ked, lst, led
      integer(kind = kint) :: k,l,m,kr,lt,i1
      real(kind = kreal) :: diff, sim2
!
!
!$omp parallel workshare
      sgs_zl(1:sph_d_grp%ngrp_dynamic) = 0.0d0
      sgs_zt(1:sph_d_grp%ngrp_dynamic) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel
      do m = 1, sph_rtp%nidx_rtp(3)
!$omp do private(klgrp,kgrp,lgrp,kst,ked,lst,led,k,l,kr,lt,i1,diff,sim2)
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
                diff                                                    &
     &            = (frc_wide(i1,1) - frc_dble(i1,1)) * frc_simi(i1,1)  &
     &            + two * (frc_wide(i1,2) - frc_dble(i1,2))             &
     &                  * frc_simi(i1,2)                                &
     &            + two * (frc_wide(i1,3) - frc_dble(i1,3))             &
     &                  * frc_simi(i1,3)                                &
     &            + (frc_wide(i1,4) - frc_dble(i1,4)) * frc_simi(i1,4)  &
     &            + two * (frc_wide(i1,5) - frc_dble(i1,5))             &
     &                  * frc_simi(i1,5)                                &
     &            + (frc_wide(i1,6) - frc_dble(i1,6)) * frc_simi(i1,6)
                sim2 = frc_simi(i1,1) * frc_simi(i1,1)                  &
     &              +  two * frc_simi(i1,2) * frc_simi(i1,2)            &
     &              +  two * frc_simi(i1,3) * frc_simi(i1,3)            &
     &              +  frc_simi(i1,4) * frc_simi(i1,4)                  &
     &              +  two * frc_simi(i1,5) * frc_simi(i1,5)            &
     &              +  frc_simi(i1,6) * frc_simi(i1,6)
!
                sgs_zl(klgrp) = sgs_zl(klgrp) + diff * sim2
                sgs_zt(klgrp) = sgs_zt(klgrp) + diff * diff
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
      end subroutine zonal_int_tentor_Csim_pout
!
!  ---------------------------------------------------------------------
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
      end module zonal_int_4_sph_Csim_pout
 