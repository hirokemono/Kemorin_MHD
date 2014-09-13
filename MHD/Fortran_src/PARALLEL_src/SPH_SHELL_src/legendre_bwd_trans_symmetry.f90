!>@file   legendre_bwd_trans_symmetry.f90
!!@brief  module legendre_bwd_trans_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  backward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine leg_bwd_trans_vector_sym_org(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_symmetry
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
      subroutine leg_bwd_trans_vector_sym_org(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, i_rtm, i_rlm
      integer(kind = kint) :: lp, lst, nl_rtm, l_rtm, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm, j_rlm
      real(kind = kreal) :: a2r_1d_rlm_r
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
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,j_rlm,nd,k_rlm,i_rlm,a2r_1d_rlm_r)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do k_rlm = kst, ked
          a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          do j_rlm = 1, nidx_rlm(2)
            do nd = 1, nvector
              i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) * a2r_1d_rlm_r
              sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1) * a_r_1d_rlm_r(k_rlm)
              sp_rlm(i_rlm  ) = sp_rlm(i_rlm  ) * a_r_1d_rlm_r(k_rlm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,jst,nj_rlm,j_rlm,nd, &
!$omp&                    k_rlm,l_rtm,lp_rtm,ln_rtm,i_rlm,i_rtm,        &
!$omp&                    mp_rlm,mn_rlm,ts_t,ss_p,ta_t,sa_p,            &
!$omp&                    ss_r,sa_t,ta_p,sa_r,ss_t,ts_p,                &
!$omp&                    pol_e,dpl_e,tor_e,pol_o,dpl_o,tor_o,          &
!$omp&                    Pg3_je,dPdt_je,Pgv_je,Pg3_jo,dPdt_jo,Pgv_jo)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = (lstack_block_rtm(lp-1) + 1)/2
          nl_rtm = (lstack_block_rtm(lp  ) + 1)/2                       &
     &            - (lstack_block_rtm(lp-1) + 1)/2
!
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            do k_rlm = kst, ked
              do l_rtm = 1, nl_rtm
                lp_rtm =  l_rtm + lst
                ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!   even l-m
                do j_rlm = 1, (nj_rlm+1)/2
                  Pg3_je(j_rlm) = P_jl(2*j_rlm+jst-1,lp_rtm)            &
     &                           * g_sph_rlm(2*j_rlm+jst-1,3)
                  dPdt_je(j_rlm) = dPdt_jl(2*j_rlm+jst-1,lp_rtm)
                  Pgv_je(j_rlm) = -P_jl(2*j_rlm+jst-1,lp_rtm)           &
     &                         * dble(idx_gl_1d_rlm_j(2*j_rlm+jst-1,3)) &
     &                          *asin_theta_1d_rtm(lp_rtm)
                end do
!   odd l-m
                do j_rlm = 1, nj_rlm/2
                  Pg3_jo(j_rlm) = P_jl(2*j_rlm+jst,  lp_rtm)            &
     &                           * g_sph_rlm(2*j_rlm+jst,  3)
                  dPdt_jo(j_rlm) = dPdt_jl(2*j_rlm+jst,  lp_rtm)
                  Pgv_jo(j_rlm) = -P_jl(2*j_rlm+jst,  lp_rtm)           &
     &                         * dble(idx_gl_1d_rlm_j(2*j_rlm+jst,  3)) &
     &                          *asin_theta_1d_rtm(lp_rtm)
                end do
!
                do nd = 1, nvector
!   even l-m
                  do j_rlm = 1, (nj_rlm+1)/2
                    i_rlm = 3*nd                                        &
     &                     + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
                    pol_e(j_rlm) = sp_rlm(i_rlm-2)
                    dpl_e(j_rlm) = sp_rlm(i_rlm-1)
                    tor_e(j_rlm) = sp_rlm(i_rlm  )
                  end do
!   odd l-m
                  do j_rlm = 1, nj_rlm/2
                    i_rlm = 3*nd                                        &
     &                     + ncomp * ((2*j_rlm+jst-1) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
                    pol_o(j_rlm) = sp_rlm(i_rlm-2)
                    dpl_o(j_rlm) = sp_rlm(i_rlm-1)
                    tor_o(j_rlm) = sp_rlm(i_rlm  )
                  end do
!
!   even l-m
                  ss_r = 0.0d0
                  sa_t = 0.0d0
                  ta_p = 0.0d0
!
                  ts_t = 0.0d0
                  ss_p = 0.0d0
                  do j_rlm = 1, (nj_rlm+1)/2
                    ss_r = ss_r + pol_e(j_rlm) * Pg3_je(j_rlm)
                    sa_t = sa_t + dpl_e(j_rlm) * dPdt_je(j_rlm)
                    ta_p = ta_p - tor_e(j_rlm) * dPdt_je(j_rlm)
!
                    ts_t = ts_t + tor_e(j_rlm) * Pgv_je(j_rlm)
                    ss_p = ss_p + dpl_e(j_rlm) * Pgv_je(j_rlm)
                  end do
!
!   odd l-m
                  sa_r = 0.0d0
                  ss_t = 0.0d0
                  ts_p = 0.0d0
!
                  ta_t = 0.0d0
                  sa_p = 0.0d0
                  do j_rlm = 1, nj_rlm/2
                    sa_r = sa_r + pol_o(j_rlm) * Pg3_jo(j_rlm)
                    ss_t = ss_t + dpl_o(j_rlm) * dPdt_jo(j_rlm)
                    ts_p = ts_p - tor_o(j_rlm) * dPdt_jo(j_rlm)
!
                    ta_t = ta_t + tor_o(j_rlm) * Pgv_jo(j_rlm)
                    sa_p = sa_p + dpl_o(j_rlm) * Pgv_jo(j_rlm)
                  end do
!
                  i_rtm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                  vr_rtm(i_rtm-2) = vr_rtm(i_rtm-2) + ss_r + sa_r
                  vr_rtm(i_rtm-1) = vr_rtm(i_rtm-1) + sa_t + ss_t
                  vr_rtm(i_rtm  ) = vr_rtm(i_rtm  ) + ta_p + ts_p
!
                  i_rtm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mn_rlm-1) * istep_rtm(3))
                  vr_rtm(i_rtm-1) = vr_rtm(i_rtm-1) + ts_t + ta_t
                  vr_rtm(i_rtm  ) = vr_rtm(i_rtm  ) + ss_p + sa_p
!
                  i_rtm = 3*nd + ncomp*((ln_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                  vr_rtm(i_rtm-2) = vr_rtm(i_rtm-2) + ss_r - sa_r
                  vr_rtm(i_rtm-1) = vr_rtm(i_rtm-1) - sa_t + ss_t
                  vr_rtm(i_rtm  ) = vr_rtm(i_rtm  ) - ta_p + ts_p
!
                  i_rtm = 3*nd + ncomp*((ln_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mn_rlm-1) * istep_rtm(3))
                  vr_rtm(i_rtm-1) = vr_rtm(i_rtm-1) + ts_t - ta_t
                  vr_rtm(i_rtm  ) = vr_rtm(i_rtm  ) + ss_p - sa_p
                end do
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
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,nd,k_rlm,mp_rlm,i_rtm)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do mp_rlm = 1, nidx_rtm(3)
          do k_rlm = kst, ked
            do nd = 1, nvector
              i_rtm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)           &
     &                            + (k_rlm-1) *  istep_rtm(1)           &
     &                            + (mp_rlm-1) * istep_rtm(3))
              vr_rtm(i_rtm-2) = half * vr_rtm(i_rtm-2)
              vr_rtm(i_rtm-1) = half * vr_rtm(i_rtm-1)
              vr_rtm(i_rtm  ) = half * vr_rtm(i_rtm  )
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_bwd_trans_vector_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, i_rtm, i_rlm
      integer(kind = kint) :: lp, lst, nl_rtm, l_rtm, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, j_rlm
      real(kind = kreal) :: ss_r, sa_r
      real(kind = kreal) :: P_je(maxdegree_rlm)
      real(kind = kreal) :: P_jo(maxdegree_rlm)
      real(kind = kreal) :: pol_e(maxdegree_rlm)
      real(kind = kreal) :: pol_o(maxdegree_rlm)
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,j_rlm,nd,mp_rlm,     &
!$omp&                    l_rtm,lp_rtm,ln_rtm,i_rtm,i_rlm,jst,nj_rlm,   &
!$omp&                    k_rlm,ss_r,sa_r,P_je,P_jo,pol_e,pol_o)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = (lstack_block_rtm(lp-1) + 1)/2
          nl_rtm = (lstack_block_rtm(lp  ) + 1)/2                       &
     &            - (lstack_block_rtm(lp-1) + 1)/2
!
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            do k_rlm = kst, ked
!
              do l_rtm = 1, nl_rtm
                lp_rtm =  l_rtm + lst
                ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!   even l-m
                do j_rlm = 1, (nj_rlm+1)/2
                  P_je(j_rlm) = P_jl(2*j_rlm+jst-1,lp_rtm)
                end do
!   odd l-m
                do j_rlm = 1, nj_rlm/2
                  P_jo(j_rlm) = P_jl(2*j_rlm+jst,  lp_rtm)
                end do
!
                do nd = 1, nscalar
!   even l-m
                  do j_rlm = 1, (nj_rlm+1)/2
                    i_rlm = nd + 3*nvector                              &
     &                     + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
                    pol_e(j_rlm) = sp_rlm(i_rlm)
                  end do
!   odd l-m
                  do j_rlm = 1, nj_rlm/2
                    i_rlm = nd + 3*nvector                              &
     &                     + ncomp * ((2*j_rlm+jst-1) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
                    pol_o(j_rlm) = sp_rlm(i_rlm)
                  end do
!
!   even l-m
                  ss_r = 0.0d0
                  do j_rlm = 1, (nj_rlm+1)/2
                    ss_r = ss_r + pol_e(j_rlm) * P_je(j_rlm)
                  end do
!   odd l-m
                  sa_r = 0.0d0
                  do j_rlm = 1, nj_rlm/2
                    sa_r = sa_r + pol_o(j_rlm) * P_jo(j_rlm)
                  end do
                  i_rtm = nd + 3*nvector                                &
     &                        + ncomp*((lp_rtm-1) * istep_rtm(2)        &
     &                               + (k_rlm-1) *  istep_rtm(1)        &
     &                               + (mp_rlm-1) * istep_rtm(3))
                  vr_rtm(i_rtm) = vr_rtm(i_rtm) + ss_r + sa_r
!
                  i_rtm = nd + 3*nvector                                &
     &                        + ncomp*((ln_rtm-1) * istep_rtm(2)        &
     &                               + (k_rlm-1) *  istep_rtm(1)        &
     &                               + (mp_rlm-1) * istep_rtm(3))
                  vr_rtm(i_rtm) = vr_rtm(i_rtm) + ss_r - sa_r
                end do
              end do
!
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
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,nd,k_rlm,mp_rlm,i_rtm)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do mp_rlm = 1, nidx_rtm(3)
          do k_rlm = kst, ked
            do nd = 1, nscalar
              i_rtm = nd + 3*nvector                                    &
     &                   + ncomp*((lp_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mp_rlm-1) * istep_rtm(3))
              vr_rtm(i_rtm) = half * vr_rtm(i_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_bwd_trans_scalar_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_symmetry
