!>@file   legendre_fwd_trans_symmetry.f90
!!@brief  module legendre_fwd_trans_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine leg_fwd_trans_vector_sym_org(ncomp, nvector,         &
!!     &          irev_sr_rtm, n_WR, WR, sp_rlm)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,&
!!     &          irev_sr_rtm, n_WR, WR, sp_rlm)
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
      subroutine leg_fwd_trans_vector_sym_org(ncomp, nvector,           &
     &          irev_sr_rtm, n_WR, WR, sp_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, i_rlm, ll
      integer(kind = kint) :: l_rtm, ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm, j_rlm, jj
      integer(kind = kint) :: nle_rtm, nlo_rtm
      real(kind = kreal) :: r2_1d_rlm_r, g7e, g7o, gme, gmo
      real(kind = kreal) :: pol_s, pol_a
      real(kind = kreal) :: dpoldt_o, dpoldp_o, dtordp_o, dtordt_o
      real(kind = kreal) :: dpoldt_e, dpoldp_e, dtordt_e, dtordp_e
      real(kind = kreal) :: Pvw_le( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: dPvw_le( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: Pvw_lo( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: dPvw_lo( (nidx_rtm(2)+1)/2 )
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
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kst,ked,ll,l_rtm,jj,j_rlm,k_rlm,           &
!$omp&                    nd,i_rlm,ip_rtpm,ip_rtnm,in_rtpm,in_rtnm,     &
!$omp&                    ipp_recv,ipn_recv,inp_recv,inn_recv,          &
!$omp&                    mp_rlm,mn_rlm,jst,nj_rlm,r2_1d_rlm_r,g7e,g7o, &
!$omp&                    pol_s,dpoldt_e,dpoldp_e,dtordt_e,dtordp_e,    &
!$omp&                    pol_a,dpoldt_o,dpoldp_o,dtordp_o,dtordt_o,    &
!$omp&                    symp_r,symp_t,symp_p,symn_t,symn_p,           &
!$omp&                    asmp_r,asmp_t,asmp_p,asmn_t,asmn_p,           &
!$omp&                    Pvw_le,dPvw_le,Pvw_lo,dPvw_lo,gme,gmo)
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
              g7e = g_sph_rlm(2*j_rlm+jst-1,7)
              g7o =  g_sph_rlm(2*j_rlm+jst  ,7)
              gme = dble(idx_gl_1d_rlm_j(2*j_rlm+jst-1,3))
              gmo = dble(idx_gl_1d_rlm_j(2*j_rlm+jst  ,3))
              do l_rtm = 1, (nidx_rtm(2)+1)/2
                Pvw_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)
                dPvw_le(l_rtm) = dPdt_rtm(l_rtm,2*j_rlm+jst-1)
!
                Pvw_lo(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst  )
                dPvw_lo(l_rtm) = dPdt_rtm(l_rtm,2*j_rlm+jst  )
              end do
!
              do nd = 1, nvector
                call set_vr_rtm_vector_symmetry                         &
     &             (nd, k_rlm, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,        &
     &              ncomp, irev_sr_rtm, n_WR, WR,                       &
     &              symp_r, asmp_t, asmp_p, symn_t, symn_p, asmp_r,     &
     &              symp_t, symp_p, asmn_t, asmn_p)
!
                pol_s = 0.0d0
                dpoldt_e = 0.0d0
                dpoldp_e = 0.0d0
                dtordp_e = 0.0d0
                dtordt_e = 0.0d0
                pol_a = 0.0d0
                dpoldt_o = 0.0d0
                dpoldp_o = 0.0d0
                dtordp_o = 0.0d0
                dtordt_o = 0.0d0
                do l_rtm = 1, (nidx_rtm(2)+1)/2
                  pol_s = pol_s +  symp_r(l_rtm) * Pvw_le(l_rtm)
                  dpoldt_e = dpoldt_e + asmp_t(l_rtm) * dPvw_le(l_rtm)
                  dpoldp_e = dpoldp_e + symn_p(l_rtm) * Pvw_le(l_rtm)
                  dtordp_e = dtordp_e + symn_t(l_rtm) * Pvw_le(l_rtm)
                  dtordt_e = dtordt_e + asmp_p(l_rtm) * dPvw_le(l_rtm)
                  pol_a = pol_a +  asmp_r(l_rtm) * Pvw_lo(l_rtm)
                  dpoldt_o = dpoldt_o + symp_t(l_rtm) * dPvw_lo(l_rtm)
                  dpoldp_o = dpoldp_o + asmn_p(l_rtm) * Pvw_lo(l_rtm)
                  dtordp_o = dtordp_o + asmn_t(l_rtm) * Pvw_lo(l_rtm)
                  dtordt_o = dtordt_o + symp_p(l_rtm) * dPvw_lo(l_rtm)
                end do
!
                i_rlm = 3*nd + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2)                       &
     &                           + pol_s * r2_1d_rlm_r * g7e
                sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                       &
     &                           + (dpoldt_e - dpoldp_e*gme)            &
     &                            * radius_1d_rlm_r(k_rlm) * g7e
                sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                       &
     &                           + (-dtordp_e*gme + dtordt_e)           &
     &                            * radius_1d_rlm_r(k_rlm) * g7e
!
                i_rlm = 3*nd + ncomp * ((2*j_rlm+jst-1) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2)                       &
     &                           + pol_a * r2_1d_rlm_r * g7o
                sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                       &
     &                           + (dpoldt_o - dpoldp_o*gmo)            &
     &                            * radius_1d_rlm_r(k_rlm) * g7o
                sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                       &
     &                           + (-dtordp_o*gmo - dtordt_o)           &
     &                            * radius_1d_rlm_r(k_rlm) *g7o
              end do
            end do
!
!   the last even l-m
            do jj = 2*(nidx_rlm(2)/2)+1, nidx_rlm(2)
              j_rlm = (jj+1) / 2
              g7e = g_sph_rlm(2*j_rlm+jst-1,7)
              gme = dble(idx_gl_1d_rlm_j(2*j_rlm+jst-1,3))
              do l_rtm = 1, nidx_rtm(2)
                Pvw_le(l_rtm) = P_rtm(l_rtm,2*j_rlm+jst-1)
                dPvw_le(l_rtm) = dPdt_rtm(l_rtm,2*j_rlm+jst-1)
              end do
!
              do nd = 1, nvector
                call set_vr_rtm_vector_symmetry                         &
     &             (nd, k_rlm, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,        &
     &              ncomp, irev_sr_rtm, n_WR, WR,                       &
     &              symp_r, asmp_t, asmp_p, symn_t, symn_p, asmp_r,     &
     &              symp_t, symp_p, asmn_t, asmn_p)
!
                pol_s = 0.0d0
                dpoldt_e = 0.0d0
                dpoldp_e = 0.0d0
                dtordp_e = 0.0d0
                dtordt_e = 0.0d0
                do l_rtm = 1, (nidx_rtm(2)+1)/2
                  pol_s = pol_s +  symp_r(l_rtm) * Pvw_le(l_rtm)
                  dpoldt_e = dpoldt_e + asmp_t(l_rtm) * dPvw_le(l_rtm)
                  dpoldp_e = dpoldp_e + symn_p(l_rtm) * Pvw_le(l_rtm)
                  dtordp_e = dtordp_e + symn_t(l_rtm) * Pvw_le(l_rtm)
                  dtordt_e = dtordt_e + asmp_p(l_rtm) * dPvw_le(l_rtm)
                end do
!
                i_rlm = 3*nd + ncomp * ((2*j_rlm+jst-2) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
                sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2)                       &
     &                           + pol_s * g7e*r2_1d_rlm_r * g7e
                sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                       &
     &                           + (dpoldt_e - dpoldp_e*gme)            &
     &                             * g7e*radius_1d_rlm_r(k_rlm) * g7e
                sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                       &
     &                           + (-dtordp_e*gme  + dtordt_e)          &
     &                             * g7e*radius_1d_rlm_r(k_rlm) * g7e
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
      subroutine leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,  &
     &          irev_sr_rtm, n_WR, WR, sp_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, i_rlm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, j_rlm, jj, ll, l_rtm
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
      real(kind = kreal) :: pol_s, pol_a
      real(kind = kreal) :: Pws_le( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: Pws_lo( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: symp_r( (nidx_rtm(2)+1)/2 )
      real(kind = kreal) :: asmp_r( (nidx_rtm(2)+1)/2 )
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,jj,j_rlm,k_rlm,mp_rlm,             &
!$omp&                    nd,jst,nj_rlm,i_rlm,ip_rtpm,ip_rtnm,ll,l_rtm, &
!$omp&                    ipp_recv,ipn_recv,pol_s,pol_a,Pws_le,Pws_lo,  &
!$omp&                    symp_r,asmp_r)
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
                  ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  ip_rtnm = 1 + (nidx_rtm(2) - l_rtm) *  istep_rtm(2)   &
     &                        + (k_rlm-1) *              istep_rtm(1)   &
     &                        + (mp_rlm-1) *             istep_rtm(3)
                  ipp_recv = nd + 3*nvector                             &
     &                          + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
                  ipn_recv = nd + 3*nvector                             &
     &                          + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
                  symp_r(l_rtm) = WR(ipp_recv) + WR(ipn_recv)
                  asmp_r(l_rtm) = WR(ipp_recv) - WR(ipn_recv)
                end do
!   Equator (if necessary)
                do ll = 2*(nidx_rtm(2)/2)+1, nidx_rtm(2)
                  l_rtm = (ll+1) /2
                  ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  ipp_recv = nd + 3*nvector                             &
     &                          + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
                  symp_r(l_rtm) = WR(ipp_recv)
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
                  ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  ip_rtnm = 1 + (nidx_rtm(2) - l_rtm) *  istep_rtm(2)   &
     &                        + (k_rlm-1) *              istep_rtm(1)   &
     &                        + (mp_rlm-1) *             istep_rtm(3)
                  ipp_recv = nd + 3*nvector                             &
     &                          + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
                  ipn_recv = nd + 3*nvector                             &
     &                          + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
                  symp_r(l_rtm) = WR(ipp_recv) + WR(ipn_recv)
                end do
!   Equator (if necessary)
                do ll = 2*(nidx_rtm(2)/2)+1, nidx_rtm(2)
                  l_rtm = (ll+1) /2
                  ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  ipp_recv = nd + 3*nvector                             &
     &                          + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
                  symp_r(l_rtm) = WR(ipp_recv)
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
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_symmetry                             &
     &         (nd, k_rlm, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,            &
     &          ncomp, irev_sr_rtm, n_WR, WR,                           &
     &          symp_r, asmp_t, asmp_p, symn_t, symn_p, asmp_r,         &
     &          symp_t, symp_p, asmn_t, asmn_p)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: nd, k_rlm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp_r(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_t(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_p(nle_rtm)
      real(kind = kreal), intent(inout) :: symn_t(nle_rtm)
      real(kind = kreal), intent(inout) :: symn_p(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_r(nle_rtm)
      real(kind = kreal), intent(inout) :: symp_t(nle_rtm)
      real(kind = kreal), intent(inout) :: symp_p(nle_rtm)
      real(kind = kreal), intent(inout) :: asmn_t(nle_rtm)
      real(kind = kreal), intent(inout) :: asmn_p(nle_rtm)
!
!
      integer(kind = kint) :: lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
!
        ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mn_rlm-1) * istep_rtm(3)
        in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mn_rlm-1) * istep_rtm(3)
        ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
        ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
        inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
        inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
        symp_r(lp_rtm) = (WR(ipp_recv-2) + WR(ipn_recv-2)) * wp_rtm
        symp_t(lp_rtm) = (WR(ipp_recv-1) + WR(ipn_recv-1)) * wp_rtm
        symp_p(lp_rtm) = (WR(ipp_recv  ) + WR(ipn_recv  )) * wp_rtm
!
        asmp_r(lp_rtm) = (WR(ipp_recv-2) - WR(ipn_recv-2)) * wp_rtm
        asmp_t(lp_rtm) = (WR(ipp_recv-1) - WR(ipn_recv-1)) * wp_rtm
        asmp_p(lp_rtm) = (WR(ipp_recv  ) - WR(ipn_recv  )) * wp_rtm
!
        symn_t(lp_rtm) = (WR(inp_recv-1) + WR(inn_recv-1))              &
     &                  * wp_rtm * asin_rtm
        symn_p(lp_rtm) = (WR(inp_recv  ) + WR(inn_recv  ))              &
     &                  * wp_rtm * asin_rtm
!
        asmn_t(lp_rtm) = (WR(inp_recv-1) - WR(inn_recv-1))              &
     &                  * wp_rtm * asin_rtm
        asmn_p(lp_rtm) = (WR(inp_recv  ) - WR(inn_recv  ))              &
     &                  * wp_rtm * asin_rtm
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
!
        ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
        in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
        ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
        inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
        symp_r(lp_rtm) = WR(ipp_recv-2) * wp_rtm
        symp_t(lp_rtm) = WR(ipp_recv-1) * wp_rtm
        symp_p(lp_rtm) = WR(ipp_recv  ) * wp_rtm
!
        asmp_r(lp_rtm) = 0.0d0
        asmp_t(lp_rtm) = 0.0d0
        asmp_p(lp_rtm) = 0.0d0
!
        symn_t(lp_rtm) = WR(inp_recv-1) * wp_rtm * asin_rtm
        symn_p(lp_rtm) = WR(inp_recv  ) * wp_rtm * asin_rtm
!
        asmn_t(lp_rtm) = 0.0d0
        asmn_p(lp_rtm) = 0.0d0
      end do
!
      end subroutine set_vr_rtm_vector_symmetry
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_symmetry
