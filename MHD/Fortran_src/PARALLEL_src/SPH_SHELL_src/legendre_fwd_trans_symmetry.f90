!>@file   legendre_fwd_trans_symmetry.f90
!!@brief  module legendre_fwd_trans_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine leg_fwd_trans_vector_sym_org(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_symmetry
!
      use m_precision
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
      subroutine leg_fwd_trans_vector_sym_org(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, i_rlm, ll
      integer(kind = kint) :: l_rtm, ipn_rtm, inn_rtm, ips_rtm, ins_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm, j_rlm, jj
      real(kind = kreal) :: r2_1d_rlm_r
      real(kind = kreal) :: pol_s, dpl_s, tor_s, pol_a, dpl_a, tor_a
      real(kind = kreal) :: Pvw_le( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: dPvw_le( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: Pgvw_le( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: Pvw_lo( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: dPvw_lo( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: Pgvw_lo( (nidx_rtm(2)+1)/2 )
!
      real(kind = kreal) :: symp_r( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: symp_t( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: symp_p( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: symn_t( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: symn_p( (nidx_rtm(2)+1)/2 )
!
      real(kind = kreal) :: asmp_r( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: asmp_t( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: asmp_p( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: asmn_t( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: asmn_p( (nidx_rtm(2)+1)/2 )
!
!
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kst,ked,ll,l_rtm,jj,j_rlm,k_rlm,           &
!$omp&                    nd,i_rlm,ipn_rtm,ips_rtm,inn_rtm,ins_rtm,     &
!$omp&                    mp_rlm,mn_rlm,jst,nj_rlm,r2_1d_rlm_r,         &
!$omp&                    pol_s,dpl_s,tor_s,pol_a,dpl_a,tor_a,          &
!$omp&                    symp_r,symp_t,symp_p,symn_t,symn_p,           &
!$omp&                    asmp_r,asmp_t,asmp_p,asmn_t,asmn_p,           &
!$omp&                    Pvw_le,dPvw_le,Pgvw_le,Pvw_lo,dPvw_lo,Pgvw_lo)
      do ip = 1, np_smp
        kst = idx_rlm_smp_stack(ip-1,1) + 1
        ked = idx_rlm_smp_stack(ip,  1)
        do k_rlm = kst, ked
          r2_1d_rlm_r = radius_1d_rlm_r(k_rlm)*radius_1d_rlm_r(k_rlm)
!
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!    even l-m
!    odd  l-m
            do j_rlm = 1, nj_rlm/2
              do l_rtm = 1, (nidx_rtm(2)+1)/2
                Pvw_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)              &
     &               * g_sph_rlm(2*j_rlm+jst-1,7)* weight_rtm(l_rtm)
                dPvw_le(l_rtm) = dPdt_rtm(l_rtm,2*j_rlm+jst-1)          &
     &               * g_sph_rlm(2*j_rlm+jst-1,7)* weight_rtm(l_rtm)
                Pgvw_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)             &
     &               * dble(idx_gl_1d_rlm_j(2*j_rlm+jst-1,3))           &
     &                * asin_theta_1d_rtm(l_rtm)                        &
     &                * g_sph_rlm(2*j_rlm+jst-1,7)* weight_rtm(l_rtm)
!
                Pvw_lo(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst  )              &
     &               * g_sph_rlm(2*j_rlm+jst  ,7)* weight_rtm(l_rtm)
                dPvw_lo(l_rtm) = dPdt_rtm(l_rtm,2*j_rlm+jst  )          &
     &               * g_sph_rlm(2*j_rlm+jst  ,7)* weight_rtm(l_rtm)
                Pgvw_lo(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst  )             &
     &               * dble(idx_gl_1d_rlm_j(2*j_rlm+jst  ,3))           &
     &                * asin_theta_1d_rtm(l_rtm)                        &
     &                * g_sph_rlm(2*j_rlm+jst  ,7)* weight_rtm(l_rtm)
              end do
!
              do nd = 1, nvector
                do l_rtm = 1, nidx_rtm(2)/2
                  ipn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mp_rlm-1) * istep_rtm(3))
                  ips_rtm = 3*nd                                        &
     &                   + ncomp*((nidx_rtm(2) - l_rtm) * istep_rtm(2)  &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm-2) + vr_rtm(ips_rtm-2)
                  symp_t(l_rtm) = vr_rtm(ipn_rtm-1) + vr_rtm(ips_rtm-1)
                  symp_p(l_rtm) = vr_rtm(ipn_rtm  ) + vr_rtm(ips_rtm  )
                  asmp_r(l_rtm) = vr_rtm(ipn_rtm-2) - vr_rtm(ips_rtm-2)
                  asmp_t(l_rtm) = vr_rtm(ipn_rtm-1) - vr_rtm(ips_rtm-1)
                  asmp_p(l_rtm) = vr_rtm(ipn_rtm  ) - vr_rtm(ips_rtm  )
!
                  inn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mn_rlm-1) * istep_rtm(3))
                  ins_rtm = 3*nd                                        &
     &                   + ncomp*((nidx_rtm(2) - l_rtm) * istep_rtm(2)  &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mn_rlm-1) * istep_rtm(3))
                  symn_t(l_rtm) = vr_rtm(inn_rtm-1) + vr_rtm(ins_rtm-1)
                  symn_p(l_rtm) = vr_rtm(inn_rtm  ) + vr_rtm(ins_rtm  )
                  asmn_t(l_rtm) = vr_rtm(inn_rtm-1) - vr_rtm(ins_rtm-1)
                  asmn_p(l_rtm) = vr_rtm(inn_rtm  ) - vr_rtm(ins_rtm  )
                end do
!   Equator (if necessary)
                do ll = 2*(nidx_rtm(2)/2)+1, nidx_rtm(2)
                  l_rtm = (ll+1) /2
                  ipn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm-2)
                  symp_t(l_rtm) = vr_rtm(ipn_rtm-1)
                  symp_p(l_rtm) = vr_rtm(ipn_rtm  )
                  asmp_t(l_rtm) = 0.0d0
                  asmp_p(l_rtm) = 0.0d0
                  asmp_r(l_rtm) = 0.0d0
!
                  inn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mn_rlm-1) * istep_rtm(3))
                  symn_t(l_rtm) = vr_rtm(inn_rtm-1)
                  symn_p(l_rtm) = vr_rtm(inn_rtm  )
                  asmn_t(l_rtm) = 0.0d0
                  asmn_p(l_rtm) = 0.0d0
                end do
!
                pol_s = 0.0d0
                dpl_s = 0.0d0
                tor_s = 0.0d0
                pol_a = 0.0d0
                dpl_a = 0.0d0
                tor_a = 0.0d0
                do l_rtm = 1, (nidx_rtm(2)+1)/2
                  pol_s = pol_s +  symp_r(l_rtm) * Pvw_le(l_rtm)
                  dpl_s = dpl_s + (asmp_t(l_rtm) * dPvw_le(l_rtm)       &
     &                           - symn_p(l_rtm) * Pgvw_le(l_rtm))
                  tor_s = tor_s - (symn_t(l_rtm) * Pgvw_le(l_rtm)       &
     &                           + asmp_p(l_rtm) * dPvw_le(l_rtm))
                  pol_a = pol_a +  asmp_r(l_rtm) * Pvw_lo(l_rtm)
                  dpl_a = dpl_a + (symp_t(l_rtm) * dPvw_lo(l_rtm)       &
     &                           - asmn_p(l_rtm) * Pgvw_lo(l_rtm))
                  tor_a = tor_a - (asmn_t(l_rtm) * Pgvw_lo(l_rtm)       &
     &                           + symp_p(l_rtm) * dPvw_lo(l_rtm))
                end do
!
                i_rlm = 3*nd + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) + pol_s * r2_1d_rlm_r
                sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                       &
     &                           + dpl_s * radius_1d_rlm_r(k_rlm)
                sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                       &
     &                           + tor_s * radius_1d_rlm_r(k_rlm)
!
                i_rlm = 3*nd + ncomp * ((2*j_rlm+jst-1) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) + pol_a * r2_1d_rlm_r
                sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                       &
     &                           + dpl_a * radius_1d_rlm_r(k_rlm)
                sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                       &
     &                           + tor_a * radius_1d_rlm_r(k_rlm)
              end do
            end do
!
!   the last even l-m
            do jj = 2*(nidx_rlm(2)/2)+1, nidx_rlm(2)
              j_rlm = (jj+1) / 2
              do l_rtm = 1, nidx_rtm(2)
                Pvw_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)              &
     &               * g_sph_rlm(2*j_rlm+jst-1,7)* weight_rtm(l_rtm)
                dPvw_le(l_rtm) = dPdt_rtm(l_rtm,2*j_rlm+jst-1)          &
     &               * g_sph_rlm(2*j_rlm+jst-1,7)* weight_rtm(l_rtm)
                Pgvw_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)             &
     &               * dble(idx_gl_1d_rlm_j(2*j_rlm+jst-1,3))           &
     &                * asin_theta_1d_rtm(l_rtm)                        &
     &                * g_sph_rlm(2*j_rlm+jst-1,7)* weight_rtm(l_rtm)
              end do
!
              do nd = 1, nvector
                do l_rtm = 1, nidx_rtm(2)/2
                  ipn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mp_rlm-1) * istep_rtm(3))
                  ips_rtm = 3*nd                                        &
     &                   + ncomp*((nidx_rtm(2) - l_rtm) * istep_rtm(2)  &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm-2) + vr_rtm(ips_rtm-2)
                  asmp_t(l_rtm) = vr_rtm(ipn_rtm-1) - vr_rtm(ips_rtm-1)
                  asmp_p(l_rtm) = vr_rtm(ipn_rtm  ) - vr_rtm(ips_rtm  )
!
                  inn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mn_rlm-1) * istep_rtm(3))
                  ins_rtm = 3*nd                                        &
     &                   + ncomp*((nidx_rtm(2) - l_rtm) * istep_rtm(2)  &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mn_rlm-1) * istep_rtm(3))
                  symn_t(l_rtm) = vr_rtm(inn_rtm-1) + vr_rtm(ins_rtm-1)
                  symn_p(l_rtm) = vr_rtm(inn_rtm  ) + vr_rtm(ins_rtm  )
                end do
!   Equator (if necessary)
                do ll = 2*(nidx_rtm(2)/2)+1, nidx_rtm(2)
                  l_rtm = (ll+1) /2
                  ipn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm-2)
                  asmp_t(l_rtm) = 0.0d0
                  asmp_p(l_rtm) = 0.0d0
!
                  inn_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)      &
     &                                  + (k_rlm-1) *  istep_rtm(1)     &
     &                                  + (mn_rlm-1) * istep_rtm(3))
                  symn_t(l_rtm) = vr_rtm(inn_rtm-1)
                  symn_p(l_rtm) = vr_rtm(inn_rtm  )
                end do
!
                pol_s = 0.0d0
                dpl_s = 0.0d0
                tor_s = 0.0d0
                do l_rtm = 1, (nidx_rtm(2)+1)/2
                  pol_s = pol_s +  symp_r(l_rtm) * Pvw_le(l_rtm)
                  dpl_s = dpl_s + (asmp_t(l_rtm) * dPvw_le(l_rtm)       &
     &                           - symn_p(l_rtm) * Pgvw_le(l_rtm))
                  tor_s = tor_s - (symn_t(l_rtm) * Pgvw_le(l_rtm)       &
     &                           + asmp_p(l_rtm) * dPvw_le(l_rtm))
                end do
!
                i_rlm = 3*nd + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) + pol_s * r2_1d_rlm_r
                sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                       &
     &                           + dpl_s * radius_1d_rlm_r(k_rlm)
                sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                       &
     &                           + tor_s * radius_1d_rlm_r(k_rlm)
              end do
            end do
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_fwd_trans_vector_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, i_rlm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, j_rlm, jj
      integer(kind = kint) :: ll, l_rtm, ipn_rtm, ips_rtm
      real(kind = kreal) :: pol_s, pol_a
      real(kind = kreal) :: Pws_le( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: Pws_lo( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: symp_r( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: asmp_r( (nidx_rtm(2)+1)/2 )
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,jj,j_rlm,k_rlm,mp_rlm,             &
!$omp&                    nd,jst,nj_rlm,i_rlm,ipn_rtm,ips_rtm,ll,l_rtm, &
!$omp&                    pol_s,pol_a,Pws_le,Pws_lo,symp_r,asmp_r)
      do ip = 1, np_smp
        kst = idx_rlm_smp_stack(ip-1,1) + 1
        ked = idx_rlm_smp_stack(ip,  1)
        do k_rlm = kst, ked
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!    even l-m
!    odd  l-m
            do j_rlm = 1, nj_rlm/2
              do l_rtm = 1, (nidx_rtm(2)+1)/2
                Pws_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)              &
     &                         * g_sph_rlm(2*j_rlm+jst-1,6)             &
     &                          * weight_rtm(l_rtm)
                Pws_lo(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst  )              &
     &                         * g_sph_rlm(2*j_rlm+jst  ,6)             &
     &                          * weight_rtm(l_rtm)
              end do
!
              do nd = 1, nscalar
                do l_rtm = 1, nidx_rtm(2)/2
                  ipn_rtm = nd + 3*nvector                              &
     &                         + ncomp*((l_rtm-1) *  istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                  ips_rtm = nd + 3*nvector                              &
     &                  + ncomp*((nidx_rtm(2) - l_rtm) *  istep_rtm(2)  &
     &                                     + (k_rlm-1) *  istep_rtm(1)  &
     &                                     + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm) + vr_rtm(ips_rtm)
                  asmp_r(l_rtm) = vr_rtm(ipn_rtm) - vr_rtm(ips_rtm)
                end do
!   Equator (if necessary)
                do ll = 2*(nidx_rtm(2)/2)+1, nidx_rtm(2)
                  l_rtm = (ll+1) /2
                  ipn_rtm = nd + 3*nvector                              &
     &                         + ncomp*((l_rtm-1) *  istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm)
                  asmp_r(l_rtm) = 0.0d0
                end do
!
                pol_s = 0.0d0
                pol_a = 0.0d0
                do l_rtm = 1, (nidx_rtm(2)+1)/2
                  pol_s = pol_s + symp_r(l_rtm) * Pws_le(l_rtm)
                  pol_a = pol_a + asmp_r(l_rtm) * Pws_lo(l_rtm)
                end do
!
                i_rlm = nd + 3*nvector                                  &
     &                     + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm) = sp_rlm(i_rlm)  + pol_s
!
                i_rlm = nd + 3*nvector                                  &
     &                     + ncomp * ((2*j_rlm+jst-1) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm) = sp_rlm(i_rlm)  + pol_a
              end do
            end do
!
!   the last even l-m
            do jj = 2*(nidx_rlm(2)/2)+1, nidx_rlm(2)
              j_rlm = (jj+1) / 2
              do l_rtm = 1, (nidx_rtm(2)+1)/2
                Pws_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)              &
     &                         * g_sph_rlm(2*j_rlm+jst-1,6)             &
     &                          * weight_rtm(l_rtm)
              end do
!
              do nd = 1, nscalar
                do l_rtm = 1, nidx_rtm(2)/2
                  ipn_rtm = nd + 3*nvector                              &
     &                         + ncomp*((l_rtm-1) *  istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                  ips_rtm = nd + 3*nvector                              &
     &                  + ncomp*((nidx_rtm(2) - l_rtm) *  istep_rtm(2)  &
     &                                     + (k_rlm-1) *  istep_rtm(1)  &
     &                                     + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm) + vr_rtm(ips_rtm)
                end do
!   Equator (if necessary)
                do ll = 2*(nidx_rtm(2)/2)+1, nidx_rtm(2)
                  l_rtm = (ll+1) /2
                  ipn_rtm = nd + 3*nvector                              &
     &                         + ncomp*((l_rtm-1) *  istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                  symp_r(l_rtm) = vr_rtm(ipn_rtm)
                end do
!
                pol_s = 0.0d0
                do l_rtm = 1, (nidx_rtm(2)+1)/2
                  pol_s = pol_s + symp_r(l_rtm) * Pws_le(l_rtm)
                end do
!
                i_rlm = nd + 3*nvector                                  &
     &                     + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm) = sp_rlm(i_rlm)  + pol_s
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine leg_fwd_trans_scalar_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_symmetry
