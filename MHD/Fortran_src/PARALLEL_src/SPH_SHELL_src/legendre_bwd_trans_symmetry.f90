!>@file   legendre_bwd_trans_symmetry.f90
!!@brief  module legendre_bwd_trans_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  backward Legendre transform considering symmetry
!!
!!@verbatim
!!      subroutine leg_bwd_trans_vector_sym_org(ncomp, nvector,         &
!!     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,&
!!     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
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
      subroutine leg_bwd_trans_vector_sym_org(ncomp, nvector,           &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd, n_jk_e, n_jk_o
      integer(kind = kint) :: ip_rtpm,  in_rtpm,  ip_rtnm,  in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
      integer(kind = kint) :: lp, lst, nl_rtm, ll, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm, jj
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,jst,jj,nd,k_rlm,ll,  &
!$omp&                    lp_rtm,ln_rtm,nj_rlm,n_jk_e,n_jk_o,           &
!$omp&                    ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,              &
!$omp&                    ipp_send,inp_send,ipn_send,inn_send,          &
!$omp&                    mp_rlm,mn_rlm,a1r_1d_rlm_r,a2r_1d_rlm_r)
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
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
            do k_rlm = kst, ked
              a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
              a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
              do ll = 1, nl_rtm
                lp_rtm =  ll + lst
                ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
                do nd = 1, nvector
                  ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mn_rlm-1) * istep_rtm(3)
                  ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mp_rlm-1) * istep_rtm(3)
                  in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)               &
     &                        + (k_rlm-1) *  istep_rtm(1)               &
     &                        + (mn_rlm-1) * istep_rtm(3)
                  ipp_send = 3*nd-2 + (irev_sr_rtm(ip_rtpm)-1) * ncomp
                  inp_send = 3*nd-2 + (irev_sr_rtm(in_rtpm)-1) * ncomp
                  ipn_send = 3*nd-2 + (irev_sr_rtm(ip_rtnm)-1) * ncomp
                  inn_send = 3*nd-2 + (irev_sr_rtm(in_rtnm)-1) * ncomp
!
                  call set_sp_rlm_vector_symmetry                       &
     &               (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,       &
     &                ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,             &
     &                pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),      &
     &                dtordt_e(1,ip), dtordp_e(1,ip),                   &
     &                pol_o(1,ip), dpoldt_o(1,ip), dpoldp_o(1,ip),      &
     &                dtordt_o(1,ip), dtordp_o(1,ip))
!
                  call cal_vr_rtm_vector_symmetry(nj_rlm,               &
     &                Ps_jl(1+jst,lp_rtm), dPsdt_jl(1+jst,lp_rtm),      &
     &                Ps_jl(1+jst+n_jk_e,lp_rtm),                       &
     &                dPsdt_jl(1+jst+n_jk_e,lp_rtm),                    &
     &                asin_theta_1d_rtm(lp_rtm),                        &
     &                pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),      &
     &                dtordt_e(1,ip), dtordp_e(1,ip),                   &
     &                pol_o(1,ip), dpoldt_o(1,ip), dpoldp_o(1,ip),      &
     &                dtordt_o(1,ip), dtordp_o(1,ip), WS(ipp_send),     &
     &                 WS(inp_send), WS(ipn_send), WS(inn_send))
                end do
              end do
            end do
          end do
        end do
!
!   Equator (if necessary)
        do lp_rtm = nidx_rtm(2)/2+1, (nidx_rtm(2)+1)/2
          do mp_rlm = 1, nidx_rtm(3)
            do k_rlm = kst, ked
              do nd = 1, nvector
                ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                 &
     &                      + (k_rlm-1) *  istep_rtm(1)                 &
     &                      + (mp_rlm-1) * istep_rtm(3)
                ipp_send = 3*nd-2 + (irev_sr_rtm(ip_rtpm)-1) * ncomp
                WS(ipp_send  ) = half * WS(ipp_send  )
                WS(ipp_send+1) = half * WS(ipp_send+1)
                WS(ipp_send+2) = half * WS(ipp_send+2)
              end do
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
      subroutine leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,  &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, kst, ked, k_rlm, nd
      integer(kind = kint) :: ip_rtm, in_rtm, ip_send, in_send
      integer(kind = kint) :: lp, lst, nl_rtm, ll, lp_rtm, ln_rtm
      integer(kind = kint) :: mp_rlm, jst, nj_rlm, jj, n_jk_e, n_jk_o
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,nl_rtm,jj,nd,mp_rlm,        &
!$omp&                    ll,lp_rtm,ln_rtm,jst,nj_rlm,n_jk_e,n_jk_o,    &
!$omp&                    ip_send,in_send,k_rlm,ip_rtm,in_rtm)
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
            n_jk_e = (nj_rlm+1) / 2
            n_jk_o =  nj_rlm - n_jk_e
            do k_rlm = kst, ked
!
              do ll = 1, nl_rtm
                lp_rtm =  ll + lst
                ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
                do nd = 1, nscalar
                  ip_rtm = 1 + (lp_rtm-1) * istep_rtm(2)                &
     &                       + (k_rlm-1) *  istep_rtm(1)                &
     &                       + (mp_rlm-1) * istep_rtm(3)
                  in_rtm = 1 + (ln_rtm-1) * istep_rtm(2)                &
     &                       + (k_rlm-1) *  istep_rtm(1)                &
     &                       + (mp_rlm-1) * istep_rtm(3)
                  ip_send = nd + 3*nvector                              &
     &                         + (irev_sr_rtm(ip_rtm)-1) * ncomp
                  in_send = nd + 3*nvector                              &
     &                         + (irev_sr_rtm(in_rtm)-1) * ncomp
!
                  call set_sp_rlm_scalar_symmetry(jst, nd, k_rlm,       &
     &               ncomp, nvector, n_WR, irev_sr_rlm, WR,             &
     &               nj_rlm, scl_e(1,ip), scl_o(1,ip))
!
                  call cal_vr_rtm_scalar_symmetry(nj_rlm,               &
     &                Ps_jl(1+jst,lp_rtm), Ps_jl(1+jst+n_jk_e,lp_rtm),  &
     &                scl_e(1,ip), scl_o(1,ip),                         &
     &                WS(ip_send), WS(in_send))
                end do
              end do
!
            end do
          end do
        end do
!
!   Equator (if necessary)
        do lp_rtm = nidx_rtm(2)/2+1, (nidx_rtm(2)+1)/2
          do mp_rlm = 1, nidx_rtm(3)
            do k_rlm = kst, ked
              do nd = 1, nscalar
                ip_rtm = 1 + (lp_rtm-1) * istep_rtm(2)                  &
     &                     + (k_rlm-1) *  istep_rtm(1)                  &
     &                     + (mp_rlm-1) * istep_rtm(3)
                ip_send = nd + 3*nvector                                &
     &                       + (irev_sr_rtm(ip_rtm)-1) * ncomp
                WS(ip_send) = half * WS(ip_send)
              end do
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
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vector_symmetry                             &
     &       (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,               &
     &        ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,                     &
     &        pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,            &
     &        pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
!
      use m_precision
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      implicit none
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      real(kind = kreal), intent(in)  :: a1r_1d_rlm_r, a2r_1d_rlm_r
      integer(kind = kint), intent(in) :: ncomp, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: pol_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dpoldt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dpoldp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dtordt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: dtordp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: pol_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dpoldt_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dpoldp_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dtordt_o(nj_rlm/2)
      real(kind = kreal), intent(inout) :: dtordp_o(nj_rlm/2)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
      real(kind = kreal) :: g3, gm
!
!
!   even l-m
      do jj = 1, (nj_rlm+1)/2
        j_rlm = 2*jj + jst - 1
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        pol_e(jj) = WR(i_recv-2) *    a2r_1d_rlm_r * g3
        dpoldt_e(jj) = WR(i_recv-1) * a1r_1d_rlm_r
        dpoldp_e(jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
        dtordt_e(jj) = WR(i_recv  ) * a1r_1d_rlm_r
        dtordp_e(jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
      end do
!   odd l-m
      do jj = 1, nj_rlm/2
        j_rlm = 2*jj + jst
        g3 = g_sph_rlm(j_rlm,3)
        gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        pol_o(jj) = WR(i_recv-2) *    a2r_1d_rlm_r * g3
        dpoldt_o(jj) = WR(i_recv-1) * a1r_1d_rlm_r
        dpoldp_o(jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
        dtordt_o(jj) = WR(i_recv  ) * a1r_1d_rlm_r
        dtordp_o(jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
      end do
!
      end subroutine set_sp_rlm_vector_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_symmetry                             &
     &       (jst, nd, k_rlm, ncomp, nvector, n_WR, irev_sr_rlm, WR,    &
     &        nj_rlm, scl_e, scl_o)
!
      use m_precision
      use m_spheric_parameter
      implicit none
!
      integer(kind = kint), intent(in) :: jst, nd, k_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector, n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(inout) :: scl_e((nj_rlm+1)/2)
      real(kind = kreal), intent(inout) :: scl_o(nj_rlm/2)
!
      integer(kind = kint) :: jj, j_rlm, i_rlm, i_recv
!
!
!   even l-m
      do jj = 1, (nj_rlm+1)/2
        j_rlm = 2*jj + jst - 1
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        scl_e(jj) = WR(i_recv)
      end do
!   odd l-m
      do jj = 1, nj_rlm/2
        j_rlm = 2*jj + jst
        i_rlm = 1 + (j_rlm-1) * istep_rlm(2) + (k_rlm-1) * istep_rlm(1)
        i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
        scl_o(jj) = WR(i_recv)
      end do
!
      end subroutine set_sp_rlm_scalar_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vector_symmetry                             &
     &       (nj_rlm, Pg3_je, dPdt_je, Pg3_jo, dPdt_jo, asin_t,         &
     &        pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,            &
     &        pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o,            &
     &        vr_pp, vr_np, vr_pn, vr_nn)
!
      use m_precision
      implicit none
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: Pg3_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dPdt_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: Pg3_jo(nj_rlm/2)
      real(kind = kreal), intent(in) :: dPdt_jo(nj_rlm/2)
!
      real(kind = kreal), intent(in) :: pol_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dpoldt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dpoldp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dtordt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dtordp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: pol_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dpoldt_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dpoldp_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dtordt_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dtordp_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: asin_t
!
      real(kind = kreal), intent(inout) :: vr_pp(3), vr_np(3)
      real(kind = kreal), intent(inout) :: vr_pn(3), vr_nn(3)
!
      integer(kind = kint) :: jj
!
      real(kind = kreal) :: symp_r, asmp_t, asmp_p, symp_t, symp_p
      real(kind = kreal) :: asmp_r, symn_t, symn_p, asmn_t, asmn_p
!
!
!   even l-m
      symp_r = 0.0d0
      asmp_t = 0.0d0
      asmp_p = 0.0d0
!
      symn_t = 0.0d0
      symn_p = 0.0d0
      do jj = 1, (nj_rlm+1)/2
        symp_r = symp_r + pol_e(jj) * Pg3_je(jj)
        asmp_t = asmp_t + dpoldt_e(jj) * dPdt_je(jj)
        asmp_p = asmp_p + dtordt_e(jj) * dPdt_je(jj)
!
        symn_t = symn_t + dtordp_e(jj) * Pg3_je(jj)
        symn_p = symn_p + dpoldp_e(jj) * Pg3_je(jj)
      end do
      symn_t = -symn_t * asin_t
      symn_p = -symn_p * asin_t
!
!   odd l-m
      asmp_r = 0.0d0
      symp_t = 0.0d0
      symp_p = 0.0d0
!
      asmn_t = 0.0d0
      asmn_p = 0.0d0
      do jj = 1, nj_rlm/2
        asmp_r = asmp_r + pol_o(jj) * Pg3_jo(jj)
        symp_t = symp_t + dpoldt_o(jj) * dPdt_jo(jj)
        symp_p = symp_p + dtordt_o(jj) * dPdt_jo(jj)
!
        asmn_t = asmn_t + dtordp_o(jj) * Pg3_jo(jj)
        asmn_p = asmn_p + dpoldp_o(jj) * Pg3_jo(jj)
      end do
      asmn_t = -asmn_t * asin_t
      asmn_p = -asmn_p * asin_t
!
      vr_pp(1) = vr_pp(1) + symp_r + asmp_r
      vr_pp(2) = vr_pp(2) + asmp_t + symp_t
      vr_pp(3) = vr_pp(3) - asmp_p - symp_p
!
      vr_np(2) = vr_np(2) + symn_t + asmn_t
      vr_np(3) = vr_np(3) + symn_p + asmn_p
!
      vr_pn(1) = vr_pn(1) + symp_r - asmp_r
      vr_pn(2) = vr_pn(2) - asmp_t + symp_t
      vr_pn(3) = vr_pn(3) + asmp_p - symp_p
!
      vr_nn(2) = vr_nn(2) + symn_t - asmn_t
      vr_nn(3) = vr_nn(3) + symn_p - asmn_p
!
      end subroutine cal_vr_rtm_vector_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_symmetry(nj_rlm, P_je, P_jo,         &
     &          scl_e, scl_o, vr_p, vr_n)
!
      use m_precision
      implicit none
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: P_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: P_jo(nj_rlm/2)
!
      real(kind = kreal), intent(in) :: scl_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: scl_o(nj_rlm/2)
!
      real(kind = kreal), intent(inout) :: vr_p, vr_n
!
      integer(kind = kint) :: jj
!
      real(kind = kreal) :: symp_r, asmp_r
!
!
!   even l-m
      symp_r = 0.0d0
      asmp_r = 0.0d0
      do jj = 1, (nj_rlm+1)/2
        symp_r = symp_r + scl_e(jj) * P_je(jj)
      end do
!   odd l-m
      do jj = 1, nj_rlm/2
        asmp_r = asmp_r + scl_o(jj) * P_jo(jj)
      end do
      vr_p = vr_p + symp_r + asmp_r
      vr_n = vr_n + symp_r - asmp_r
!
      end subroutine cal_vr_rtm_scalar_symmetry
!
! -----------------------------------------------------------------------
