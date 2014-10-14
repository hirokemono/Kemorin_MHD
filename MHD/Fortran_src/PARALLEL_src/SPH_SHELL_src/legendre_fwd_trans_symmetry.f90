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
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,&
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
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
      use m_legendre_work_sym_matmul
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
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, ie_rlm, io_rlm
      integer(kind = kint) :: mp_rlm, mn_rlm, ie_send, io_send
      integer(kind = kint) :: jst, nj_rlm, jj, je_rlm, jo_rlm
      integer(kind = kint) :: nle_rtm, nlo_rtm, n_jk_e, n_jk_o
      real(kind = kreal) :: r1_1d_rlm_r, r2_1d_rlm_r, gme, gmo
!
!
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kst,ked,jj,k_rlm,nd,je_rlm,jo_rlm,         &
!$omp&                    mp_rlm,mn_rlm,jst,nj_rlm,n_jk_e,n_jk_o,       &
!$omp&                    r1_1d_rlm_r,r2_1d_rlm_r,                      &
!$omp&                    ie_rlm,io_rlm,ie_send,io_send,gme,gmo)
      do ip = 1, np_smp
        kst = idx_rlm_smp_stack(ip-1,1) + 1
        ked = idx_rlm_smp_stack(ip,  1)
        do k_rlm = kst, ked
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = r1_1d_rlm_r*r1_1d_rlm_r
!
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
!    even l-m
!    odd  l-m
            do jj = 1, nj_rlm/2
              je_rlm = 2*jj + jst - 1
              jo_rlm = 2*jj + jst
              gme = dble(idx_gl_1d_rlm_j(je_rlm,3))
              gmo = dble(idx_gl_1d_rlm_j(jo_rlm,3))
!
              do nd = 1, nvector
                call set_vr_rtm_vector_symmetry                         &
     &             (nd, k_rlm, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,        &
     &              ncomp, irev_sr_rtm, n_WR, WR, symp_r(1,ip),         &
     &              asmp_t(1,ip), asmp_p(1,ip), symn_t(1,ip),           &
     &              symn_p(1,ip), asmp_r(1,ip), symp_t(1,ip),           &
     &              symp_p(1,ip), asmn_t(1,ip), asmn_p(1,ip))
!
                ie_rlm = 1 + (je_rlm-1) * istep_rlm(2)                  &
     &                     + (k_rlm-1) *  istep_rlm(1)
                io_rlm = 1 + (jo_rlm-1) * istep_rlm(2)                  &
     &                     + (k_rlm-1)  * istep_rlm(1)
                ie_send = 3*nd-2 + (irev_sr_rlm(ie_rlm) - 1) * ncomp
                io_send = 3*nd-2 + (irev_sr_rlm(io_rlm) - 1) * ncomp
                call cal_even_sp_rlm_vector(nth_hemi_rtm,               &
     &              g_sph_rlm(je_rlm,7), gme, r1_1d_rlm_r, r2_1d_rlm_r, &
     &              Ps_rtm(1,jj+jst), dPsdt_rtm(1,jj+jst),              &
     &              symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),           &
     &              symn_t(1,ip), symn_p(1,ip), WS(ie_send))
!
                call cal_odd_sp_rlm_vector(nth_hemi_rtm,                &
     &             g_sph_rlm(jo_rlm,7), gmo, r1_1d_rlm_r, r2_1d_rlm_r,  &
     &             Ps_rtm(1,jj+jst+n_jk_e), dPsdt_rtm(1,jj+jst+n_jk_e), &
     &             asmp_r(1,ip), symp_t(1,ip), symp_p(1,ip),            &
     &             asmn_t(1,ip), asmn_p(1,ip), WS(io_send))
              end do
            end do
!
!   the last even l-m
            do jj = nj_rlm/2+1, (nj_rlm+1)/2
              je_rlm = 2*jj + jst - 1
              gme = dble(idx_gl_1d_rlm_j(je_rlm,3))
!
              do nd = 1, nvector
                call set_vr_rtm_vector_symmetry                         &
     &             (nd, k_rlm, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,        &
     &              ncomp, irev_sr_rtm, n_WR, WR, symp_r(1,ip),         &
     &              asmp_t(1,ip), asmp_p(1,ip), symn_t(1,ip),           &
     &              symn_p(1,ip), asmp_r(1,ip), symp_t(1,ip),           &
     &              symp_p(1,ip), asmn_t(1,ip), asmn_p(1,ip))
!
                ie_rlm = 1 + (je_rlm-1) * istep_rlm(2)                  &
     &                     + (k_rlm-1) *  istep_rlm(1)
                ie_send = 3*nd-2 + (irev_sr_rlm(ie_rlm) - 1) * ncomp
                call cal_even_sp_rlm_vector(nth_hemi_rtm,               &
     &              g_sph_rlm(je_rlm,7), gme, r1_1d_rlm_r, r2_1d_rlm_r, &
     &              Ps_rtm(1,jj+jst), dPsdt_rtm(1,jj+jst),              &
     &              symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),           &
     &              symn_t(1,ip), symn_p(1,ip), WS(ie_send))
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
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd
      integer(kind = kint) :: nle_rtm, nlo_rtm, je_rlm, jo_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, jj, n_jk_e, n_jk_o
!
!
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,jj,k_rlm,mp_rlm,n_jk_e,n_jk_o,     &
!$omp&                    nd,jst,nj_rlm,ie_rlm,io_rlm,je_rlm,jo_rlm,    &
!$omp&                    ie_send,io_send)
      do ip = 1, np_smp
        kst = idx_rlm_smp_stack(ip-1,1) + 1
        ked = idx_rlm_smp_stack(ip,  1)
        do k_rlm = kst, ked
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
!    even l-m
!    odd  l-m
            do jj = 1, nj_rlm/2
              je_rlm = 2*jj + jst - 1
              jo_rlm = 2*jj + jst
              do nd = 1, nscalar
                call set_vr_rtm_scalar_symmetry(nd, k_rlm, mp_rlm,      &
     &              nle_rtm, nlo_rtm, ncomp, nvector,                   &
     &              irev_sr_rtm, n_WR, WR, symp(1,ip), asmp(1,ip))
!
                ie_rlm = 1 + (je_rlm-1) * istep_rlm(2)                  &
     &                     + (k_rlm-1) *  istep_rlm(1)
                io_rlm = 1 + (jo_rlm-1) * istep_rlm(2)                  &
     &                     + (k_rlm-1) *  istep_rlm(1)
                ie_send = nd + 3*nvector                                &
     &                       + (irev_sr_rlm(ie_rlm) - 1) * ncomp
                io_send = nd + 3*nvector                                &
     &                       + (irev_sr_rlm(io_rlm) - 1) * ncomp
                call cal_even_sp_rlm_scalar                             &
     &             (nle_rtm, g_sph_rlm(je_rlm,6),                       &
     &              Ps_rtm(1,jj+jst), symp(1,ip), WS(ie_send))
!
                call cal_odd_sp_rlm_scalar                              &
     &             (nle_rtm, g_sph_rlm(jo_rlm,6),                       &
     &              Ps_rtm(1,jj+jst+n_jk_e), asmp(1,ip), WS(io_send))
              end do
            end do
!
!   the last even l-m
            do jj = nj_rlm/2+1, (nj_rlm+1)/2
              je_rlm = 2*jj + jst - 1
              do nd = 1, nscalar
                call set_vr_rtm_scalar_symmetry(nd, k_rlm, mp_rlm,      &
     &              nle_rtm, nlo_rtm, ncomp, nvector,                   &
     &              irev_sr_rtm, n_WR, WR, symp(1,ip), asmp(1,ip))
!
                ie_rlm = 1 + (je_rlm-1) * istep_rlm(2)                  &
     &                     + (k_rlm-1) *  istep_rlm(1)
                ie_send = nd + 3*nvector                                &
     &                       + (irev_sr_rlm(ie_rlm) - 1) * ncomp
                call cal_even_sp_rlm_scalar                             &
     &             (nle_rtm, g_sph_rlm(je_rlm,6),                       &
     &              Ps_rtm(1,jj+jst), symp(1,ip), WS(ie_send))
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
      subroutine set_vr_rtm_scalar_symmetry                             &
     &         (nd, k_rlm, mp_rlm, nle_rtm, nlo_rtm,                    &
     &          ncomp, nvector, irev_sr_rtm, n_WR, WR, symp, asmp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      integer(kind = kint), intent(in) :: nd, k_rlm
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp(nle_rtm)
!
!
      integer(kind = kint) :: l_rtm, ip_rtpm, ip_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv
      real(kind = kreal) :: wp_rtm
!
!
      do l_rtm = 1, nlo_rtm
        wp_rtm = weight_rtm(l_rtm)
        ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        ip_rtnm = 1 + (nidx_rtm(2) - l_rtm) *  istep_rtm(2)             &
     &              + (k_rlm-1) *              istep_rtm(1)             &
     &              + (mp_rlm-1) *             istep_rtm(3)
        ipp_recv = nd + 3*nvector + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
        ipn_recv = nd + 3*nvector + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
        symp(l_rtm) = (WR(ipp_recv) + WR(ipn_recv)) * wp_rtm
        asmp(l_rtm) = (WR(ipp_recv) - WR(ipn_recv)) * wp_rtm
      end do
!   Equator (if necessary)
      do l_rtm = nlo_rtm, nle_rtm
        wp_rtm = weight_rtm(l_rtm)
        ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)               &
     &              + (k_rlm-1) *  istep_rtm(1)               &
     &              + (mp_rlm-1) * istep_rtm(3)
        ipp_recv = nd + 3*nvector                   &
     &                + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
        symp(l_rtm) = WR(ipp_recv) * wp_rtm
        asmp(l_rtm) = 0.0d0
      end do
!
      end subroutine set_vr_rtm_scalar_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine cal_even_sp_rlm_vector(nth_hemi_rtm, g7e, gme,         &
     &          r1_1d_rlm_r, r2_1d_rlm_r, Pvw_le, dPvw_le,              &
     &          symp_r, asmp_t, asmp_p, symn_t, symn_p, sp_rlm)
!
      integer(kind = kint), intent(in) :: nth_hemi_rtm
      real (kind=kreal), intent(in) :: Pvw_le(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: dPvw_le(nth_hemi_rtm)
!
      real (kind=kreal), intent(in) :: symp_r(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: asmp_t(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: asmp_p(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: symn_t(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: symn_p(nth_hemi_rtm)
!
      real (kind=kreal), intent(in) :: g7e, gme
      real (kind=kreal), intent(in) :: r1_1d_rlm_r, r2_1d_rlm_r
!
      real (kind=kreal), intent(inout) :: sp_rlm(3)
!
      real(kind = kreal) :: pol_e, dpoldt_e, dpoldp_e
      real(kind = kreal) :: dtordt_e, dtordp_e
!
      integer(kind = kint) :: l_rtm
!
!
      pol_e = 0.0d0
      dpoldt_e = 0.0d0
      dpoldp_e = 0.0d0
      dtordp_e = 0.0d0
      dtordt_e = 0.0d0
      do l_rtm = 1, nth_hemi_rtm
        pol_e = pol_e +  symp_r(l_rtm) * Pvw_le(l_rtm)
        dpoldt_e = dpoldt_e + asmp_t(l_rtm) * dPvw_le(l_rtm)
        dpoldp_e = dpoldp_e + symn_p(l_rtm) * Pvw_le(l_rtm)
        dtordp_e = dtordp_e + symn_t(l_rtm) * Pvw_le(l_rtm)
        dtordt_e = dtordt_e + asmp_p(l_rtm) * dPvw_le(l_rtm)
      end do
!
      sp_rlm(1) = sp_rlm(1) + pol_e * r2_1d_rlm_r * g7e
      sp_rlm(2) = sp_rlm(2)                                             &
     &                 + ( dpoldt_e - dpoldp_e*gme) * r1_1d_rlm_r*g7e
      sp_rlm(3) = sp_rlm(3)                                             &
     &                 + (-dtordp_e*gme + dtordt_e) * r1_1d_rlm_r*g7e
!
      end subroutine cal_even_sp_rlm_vector
!
! -----------------------------------------------------------------------
!
      subroutine cal_odd_sp_rlm_vector(nth_hemi_rtm, g7o, gmo,         &
     &          r1_1d_rlm_r, r2_1d_rlm_r, Pvw_lo, dPvw_lo,             &
     &          asmp_r, symp_t, symp_p, asmn_t, asmn_p, sp_rlm)
!
      integer(kind = kint), intent(in) :: nth_hemi_rtm
      real (kind=kreal), intent(in) :: Pvw_lo(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: dPvw_lo(nth_hemi_rtm)
!
      real (kind=kreal), intent(in) :: asmp_r(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: symp_t(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: symp_p(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: asmn_t(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: asmn_p(nth_hemi_rtm)
!
      real (kind=kreal), intent(in) :: r1_1d_rlm_r, r2_1d_rlm_r
      real (kind=kreal), intent(in) :: g7o, gmo
!
      real (kind=kreal), intent(inout) :: sp_rlm(3)
!
      real(kind = kreal) :: pol_o, dpoldt_o, dpoldp_o
      real(kind = kreal) :: dtordt_o, dtordp_o
!
      integer(kind = kint) :: l_rtm
!
!
      pol_o =    0.0d0
      dpoldt_o = 0.0d0
      dpoldp_o = 0.0d0
      dtordp_o = 0.0d0
      dtordt_o = 0.0d0
      do l_rtm = 1, nth_hemi_rtm
        pol_o = pol_o +  asmp_r(l_rtm) * Pvw_lo(l_rtm)
        dpoldt_o = dpoldt_o + symp_t(l_rtm) * dPvw_lo(l_rtm)
        dpoldp_o = dpoldp_o + asmn_p(l_rtm) * Pvw_lo(l_rtm)
        dtordp_o = dtordp_o + asmn_t(l_rtm) * Pvw_lo(l_rtm)
        dtordt_o = dtordt_o + symp_p(l_rtm) * dPvw_lo(l_rtm)
      end do
      sp_rlm(1) = sp_rlm(1) + pol_o * r2_1d_rlm_r * g7o
      sp_rlm(2) = sp_rlm(2)                                             &
     &           + ( dpoldt_o - dpoldp_o*gmo) * r1_1d_rlm_r * g7o
      sp_rlm(3) = sp_rlm(3)                                             &
     &           + (-dtordp_o*gmo - dtordt_o) * r1_1d_rlm_r * g7o
!
      end subroutine cal_odd_sp_rlm_vector
!
! -----------------------------------------------------------------------
!
      subroutine cal_even_sp_rlm_scalar                                 &
     &          (nth_hemi_rtm, g6e, Pws_le, symp, sp_rlm)
!
      integer(kind = kint), intent(in) :: nth_hemi_rtm
      real (kind=kreal), intent(in) :: Pws_le(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: symp(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: g6e
!
      real (kind=kreal), intent(inout) :: sp_rlm
!
      real(kind = kreal) :: pol_e
      integer(kind = kint) :: l_rtm
!
!
      pol_e = 0.0d0
      do l_rtm = 1, nth_hemi_rtm
        pol_e = pol_e + symp(l_rtm) * Pws_le(l_rtm)
      end do
      sp_rlm = sp_rlm  + pol_e * g6e
!
      end subroutine cal_even_sp_rlm_scalar
!
! -----------------------------------------------------------------------
!
      subroutine cal_odd_sp_rlm_scalar                                  &
     &         (nth_hemi_rtm, g6o, Pws_lo, asmp, sp_rlm)
!
      integer(kind = kint), intent(in) :: nth_hemi_rtm
      real (kind=kreal), intent(in) :: Pws_lo(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: asmp(nth_hemi_rtm)
      real (kind=kreal), intent(in) :: g6o
!
      real (kind=kreal), intent(inout) :: sp_rlm
!
      real(kind = kreal) :: pol_o
      integer(kind = kint) :: l_rtm
!
!
      pol_o = 0.0d0
      do l_rtm = 1, nth_hemi_rtm
        pol_o = pol_o + asmp(l_rtm) * Pws_lo(l_rtm)
      end do
      sp_rlm = sp_rlm  + pol_o * g6o
!
      end subroutine cal_odd_sp_rlm_scalar
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_symmetry
