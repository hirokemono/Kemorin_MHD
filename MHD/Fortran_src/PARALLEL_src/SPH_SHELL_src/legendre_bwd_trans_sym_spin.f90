!>@file   legendre_bwd_trans_sym_spin.f90
!!@brief  module legendre_bwd_trans_sym_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform with symmetry
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine leg_bwd_trans_vector_sym_spin(ncomp, nvector,        &
!!     &          sp_rlm_spin, vr_rtm_spin)
!!      subroutine leg_bwd_trans_scalar_sym_spin                        &
!!     &          (ncomp, nvector, nscalar, sp_rlm_spin, vr_rtm_spin)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_spin
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm_spin(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm_spin(nidx_rtm(2),nidx_rtm(1)*ncomp,nidx_rtm(3))
!!      real(kind = kreal), allocatable :: vr_rtm_spin(:,:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm_spin(j_rlm,k_rtm,icomp)
!!        size: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm_spin(:,:)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_sym_spin
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_vector_sym_spin(ncomp, nvector,          &
     &          sp_rlm_spin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(1)*ncomp,nidx_rtm(3),nidx_rtm(2))
!
      integer(kind = kint) :: nb_nri, ip, kst, ked, kr_nd, k_rtm
      integer(kind = kint) :: lp, lst, nl_rtm, l_rtm, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm, j_rlm, jj
      real(kind = kreal) :: ss_r, sa_t, ta_p, sa_r, ss_t, ts_p
      real(kind = kreal) :: ts_t, ss_p, ta_t, sa_p
      real(kind = kreal) :: Pg3_je(maxdegree_rlm)
      real(kind = kreal) :: dPdt_je(maxdegree_rlm)
      real(kind = kreal) :: Pgv_je(maxdegree_rlm)
      real(kind = kreal) :: Pg3_jo(maxdegree_rlm)
      real(kind = kreal) :: dPdt_jo(maxdegree_rlm)
      real(kind = kreal) :: Pgv_jo(maxdegree_rlm)
!
      real(kind = kreal) :: pol_e(maxdegree_rlm)
      real(kind = kreal) :: dpl_e(maxdegree_rlm)
      real(kind = kreal) :: tor_e(maxdegree_rlm)
      real(kind = kreal) :: pol_o(maxdegree_rlm)
      real(kind = kreal) :: dpl_o(maxdegree_rlm)
      real(kind = kreal) :: tor_o(maxdegree_rlm)
!
!
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,kr_nd,lp,lst,nl_rtm,jst,nj_rlm,    &
!$omp&                    j_rlm,jj,l_rtm,lp_rtm,ln_rtm,k_rtm,           &
!$omp&                    mp_rlm,mn_rlm,ts_t,ss_p,ta_t,sa_p,            &
!$omp&                    ss_r,sa_t,ta_p,sa_r,ss_t,ts_p,                &
!$omp&                    pol_e,dpl_e,tor_e,pol_o,dpl_o,tor_o,          &
!$omp&                    Pg3_je,dPdt_je,Pgv_je,Pg3_jo,dPdt_jo,Pgv_jo)
      do ip = 1, np_smp
        kst = nvector*idx_rtm_smp_stack(ip-1,1)
        ked = nvector*idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = (lstack_block_rtm(lp-1) + 1)/2
          nl_rtm = (lstack_block_rtm(lp  ) + 1)/2                       &
     &            - (lstack_block_rtm(lp-1) + 1)/2
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            do l_rtm = 1, nl_rtm
              lp_rtm =  l_rtm + lst
              ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!   even l-m
              do jj = 1, (nj_rlm+1)/2
                j_rlm = 2*jj+jst-1
                Pg3_je(jj) =  P_jl(j_rlm,lp_rtm) * g_sph_rlm(j_rlm,3)
                dPdt_je(jj) = dPdt_jl(j_rlm,lp_rtm)
                Pgv_je(jj) = -P_jl(j_rlm,lp_rtm)                        &
     &                      * dble(idx_gl_1d_rlm_j(j_rlm,3))            &
     &                       *asin_theta_1d_rtm(lp_rtm)
              end do
!   odd l-m
              do jj = 1, nj_rlm/2
                j_rlm = 2*jj+jst
                Pg3_jo(jj) =  P_jl(j_rlm,lp_rtm) * g_sph_rlm(j_rlm,3)
                dPdt_jo(jj) = dPdt_jl(j_rlm,lp_rtm)
                Pgv_jo(jj) = -P_jl(j_rlm,lp_rtm)                        &
     &                      * dble(idx_gl_1d_rlm_j(j_rlm,3))            &
     &                       *asin_theta_1d_rtm(lp_rtm)
              end do
!
              do kr_nd = kst+1, ked
!   even l-m
                do jj = 1, (nj_rlm+1)/2
                  j_rlm = 2*jj + jst-1
                  pol_e(jj) = sp_rlm_spin(j_rlm,kr_nd         )
                  dpl_e(jj) = sp_rlm_spin(j_rlm,kr_nd+nb_nri  )
                  tor_e(jj) = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)
                end do
!   odd l-m
                do jj = 1, nj_rlm/2
                  j_rlm = 2*jj + jst
                  pol_o(jj) = sp_rlm_spin(j_rlm,kr_nd          )
                  dpl_o(jj) = sp_rlm_spin(j_rlm,kr_nd+nb_nri   )
                  tor_o(jj) = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)
                end do
!
!   even l-m
                ss_r = 0.0d0
                sa_t = 0.0d0
                ta_p = 0.0d0
!
                ts_t = 0.0d0
                ss_p = 0.0d0
                do jj = 1, (nj_rlm+1)/2
                  ss_r = ss_r + pol_e(jj) * Pg3_je(jj)
                  sa_t = sa_t + dpl_e(jj) * dPdt_je(jj)
                  ta_p = ta_p - tor_e(jj) * dPdt_je(jj)
!
                  ts_t = ts_t + tor_e(jj) * Pgv_je(jj)
                  ss_p = ss_p + dpl_e(jj) * Pgv_je(jj)
                end do
!
!   odd l-m
                sa_r = 0.0d0
                ss_t = 0.0d0
                ts_p = 0.0d0
!
                ta_t = 0.0d0
                sa_p = 0.0d0
                do jj = 1, nj_rlm/2
                  sa_r = sa_r + pol_o(jj) * Pg3_jo(jj)
                  ss_t = ss_t + dpl_o(jj) * dPdt_jo(jj)
                  ts_p = ts_p - tor_o(jj) * dPdt_jo(jj)
!
                  ta_t = ta_t + tor_o(jj) * Pgv_jo(jj)
                  sa_p = sa_p + dpl_o(jj) * Pgv_jo(jj)
                end do
!
                vr_rtm_spin(kr_nd,         mp_rlm,lp_rtm)               &
     &               = vr_rtm_spin(kr_nd,         mp_rlm,lp_rtm)        &
     &                + ss_r + sa_r
                vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,lp_rtm)               &
     &               = vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,lp_rtm)        &
     &                + sa_t + ss_t
                vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,lp_rtm)               &
     &               = vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,lp_rtm)        &
     &                + ta_p + ts_p
!
                vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,lp_rtm)               &
     &               = vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,lp_rtm)        &
     &                + ts_t + ta_t
                vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,lp_rtm)               &
     &               = vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,lp_rtm)        &
     &                + ss_p + sa_p
!
!
                vr_rtm_spin(kr_nd,         mp_rlm,ln_rtm)               &
     &               = vr_rtm_spin(kr_nd,         mp_rlm,ln_rtm)        &
     &                + ss_r - sa_r
                vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,ln_rtm)               &
     &               = vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,ln_rtm)        &
     &                - sa_t + ss_t
                vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,ln_rtm)               &
     &               = vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,ln_rtm)        &
     &                - ta_p + ts_p
!
                vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,ln_rtm)               &
     &               = vr_rtm_spin(kr_nd+nb_nri,  mn_rlm,ln_rtm)        &
     &                + ts_t - ta_t
                vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,ln_rtm)               &
     &               = vr_rtm_spin(kr_nd+2*nb_nri,mn_rlm,ln_rtm)        &
     &                + ss_p - sa_p
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      if(mod(nidx_rtm(2),2) .eq. 0) return
      lp_rtm = (nidx_rtm(2)+1) / 2
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,kr_nd,mp_rlm)
      do ip = 1, np_smp
        kst = nvector*idx_rtm_smp_stack(ip-1,1)
        ked = nvector*idx_rtm_smp_stack(ip,  1)
        do mp_rlm = 1, nidx_rtm(3)
          do kr_nd = kst+1, ked
            vr_rtm_spin(kr_nd,         mp_rlm,lp_rtm)                   &
     &           = half * vr_rtm_spin(kr_nd,         mp_rlm,lp_rtm)
            vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,lp_rtm)                   &
     &           = half * vr_rtm_spin(kr_nd+nb_nri,  mp_rlm,lp_rtm)
            vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,lp_rtm)                   &
     &           = half * vr_rtm_spin(kr_nd+2*nb_nri,mp_rlm,lp_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_bwd_trans_vector_sym_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_sym_spin                          &
     &          (ncomp, nvector, nscalar, sp_rlm_spin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(1)*ncomp,nidx_rtm(3),nidx_rtm(2))
!
      integer(kind = kint) :: ip, kst, ked, kr_nd
      integer(kind = kint) :: lp, lst, nl_rtm, l_rtm, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, jj, j_rlm
      real(kind = kreal) :: ss_r, sa_r
      real(kind = kreal) :: P_je(maxdegree_rlm)
      real(kind = kreal) :: P_jo(maxdegree_rlm)
      real(kind = kreal) :: pol_e(maxdegree_rlm)
      real(kind = kreal) :: pol_o(maxdegree_rlm)
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,kr_nd,lp,lst,nl_rtm,jj,j_rlm,      &
!$omp&                    mp_rlm,l_rtm,lp_rtm,ln_rtm,jst,nj_rlm,        &
!$omp&                    ss_r,sa_r,P_je,P_jo,pol_e,pol_o)
      do ip = 1, np_smp
        kst = 3*nvector*nidx_rtm(1)                                     &
     &       + nscalar*idx_rtm_smp_stack(ip-1,1)
        ked = 3*nvector*nidx_rtm(1)                                     &
     &       + nscalar*idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = (lstack_block_rtm(lp-1) + 1)/2
          nl_rtm = (lstack_block_rtm(lp  ) + 1)/2                       &
     &            - (lstack_block_rtm(lp-1) + 1)/2
!
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
            do l_rtm = 1, nl_rtm
              lp_rtm =  l_rtm + lst
              ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!   even l-m
              do jj = 1, (nj_rlm+1)/2
                j_rlm = 2*jj + jst - 1
                P_je(jj) = P_jl(j_rlm,lp_rtm)
              end do
!   odd l-m
              do jj = 1, nj_rlm/2
                j_rlm = 2*jj + jst
                P_jo(jj) = P_jl(j_rlm,lp_rtm)
              end do
!
              do kr_nd = kst+1, ked
!   even l-m
                do jj = 1, (nj_rlm+1)/2
                  j_rlm = 2*jj + jst - 1
                  pol_e(jj) = sp_rlm_spin(j_rlm,kr_nd)
                end do
!   odd l-m
                do jj = 1, nj_rlm/2
                  j_rlm = 2*jj + jst
                  pol_o(jj) = sp_rlm_spin(j_rlm,kr_nd)
                end do
!
!   even l-m
                ss_r = 0.0d0
                do jj = 1, (nj_rlm+1)/2
                  ss_r = ss_r + pol_e(jj) * P_je(jj)
                end do
!   odd l-m
                sa_r = 0.0d0
                do jj = 1, nj_rlm/2
                  sa_r = sa_r + pol_o(jj) * P_jo(jj)
                end do
!
                vr_rtm_spin(kr_nd,mp_rlm,lp_rtm)                        &
     &               = vr_rtm_spin(kr_nd,mp_rlm,lp_rtm) + ss_r + sa_r
                vr_rtm_spin(kr_nd,mp_rlm,ln_rtm)                        &
     &               = vr_rtm_spin(kr_nd,mp_rlm,ln_rtm) + ss_r - sa_r
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
!   Equator (if necessary)
      if(mod(nidx_rtm(2),2) .eq. 0) return
      lp_rtm = (nidx_rtm(2)+1) / 2
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,kr_nd,mp_rlm)
      do ip = 1, np_smp
        kst = 3*nvector*nidx_rtm(1)                                     &
     &       + nscalar*idx_rtm_smp_stack(ip-1,1)
        ked = 3*nvector*nidx_rtm(1)                                     &
     &       + nscalar*idx_rtm_smp_stack(ip,  1)
        do mp_rlm = 1, nidx_rtm(3)
          do kr_nd = kst+1, ked
            vr_rtm_spin(kr_nd,mp_rlm,lp_rtm)                            &
     &          = half * vr_rtm_spin(kr_nd,mp_rlm,lp_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_bwd_trans_scalar_sym_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_sym_spin
