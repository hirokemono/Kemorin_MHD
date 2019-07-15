!>@file   leg_fwd_trans_sym_mat_tj.f90
!!@brief  module leg_fwd_trans_sym_mat_tj
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_sym_mat_tj(ncomp, nvector, nscalar, &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_tsp)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tsp
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module leg_fwd_trans_sym_mat_tj
!
      use m_precision
!
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
      use matmul_for_legendre_trans
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_legendre_work_sym_mat_jt
      use m_elapsed_labels_SPH_TRNS
!
      implicit none
!
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_sym_mat_tj(ncomp, nvector, nscalar,   &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          n_WR, n_WS, WR, WS, WK_l_tsp)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: weight_rtm(sph_rtm%nidx_rtm(2))
      type(leg_trns_theta_omp_work), intent(inout) :: WK_l_tsp
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: mp_rlm
      integer(kind = kint) :: nkrs, nkrt, lst_rtm
      integer(kind = kint) :: ip, jst
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nkrs = (3*nvector+nscalar) * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        jst = idx_trns%lstack_rlm(mp_rlm-1)
!
!$omp parallel do private(ip,lst_rtm)
        do ip = 1, np_smp
          lst_rtm = WK_l_tsp%lst_rtm(ip)
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+15)
          call set_vr_rtm_sym_mat_rin                                   &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm, weight_rtm,          &
     &        mp_rlm, WK_l_tsp%lst_rtm(ip),                             &
     &        WK_l_tsp%nle_rtm(ip), WK_l_tsp%nlo_rtm(ip),               &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WR, WR,      &
     &        WK_l_tsp%Fmat(ip)%symp_r(1), WK_l_tsp%Fmat(ip)%asmp_p(1), &
     &        WK_l_tsp%Fmat(ip)%asmp_r(1), WK_l_tsp%Fmat(ip)%symp_p(1) )
        end do
!$omp end parallel do
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+15)
!
!  even l-m
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+16)
!$omp parallel do private(ip,lst_rtm)
        do ip = 1, np_smp
          lst_rtm = WK_l_tsp%lst_rtm(ip)
          call matmul_fwd_leg_trans_tstlop                              &
     &       (nkrs, WK_l_tsp%n_jk_e(mp_rlm), WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Fmat(ip)%symp_r(1),                              &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%Pse_tj,                          &
     &        WK_l_tsp%Smat(ip)%pol_e(1))
          call matmul_fwd_leg_trans_tstlop                              &
     &       (nkrt, WK_l_tsp%n_jk_e(mp_rlm), WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Fmat(ip)%asmp_p(1),                              &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%dPsedt_tj,                       &
     &        WK_l_tsp%Smat(ip)%tor_e(1))
!
!  odd l-m
          call matmul_fwd_leg_trans_tstlop                              &
     &       (nkrs, WK_l_tsp%n_jk_o(mp_rlm), WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Fmat(ip)%asmp_r(1),                              &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%Pso_tj,                          &
     &        WK_l_tsp%Smat(ip)%pol_o(1))
          call matmul_fwd_leg_trans_tstlop                              &
     &       (nkrt, WK_l_tsp%n_jk_o(mp_rlm), WK_l_tsp%nle_rtm(ip),      &
     &        WK_l_tsp%Fmat(ip)%symp_p(1),                              &
     &        WK_l_tsp%Pmat(mp_rlm,ip)%dPsodt_tj,                       &
     &        WK_l_tsp%Smat(ip)%tor_o(1))
        end do
!$omp end parallel do
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+16)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+17)
!$omp parallel private(ip)
        do ip = 2, np_smp
!$omp workshare
          WK_l_tsp%Smat(1)%pol_e(1:nkrs*WK_l_tsp%n_jk_e(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%pol_e(1:nkrs*WK_l_tsp%n_jk_e(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%pol_e(1:nkrs*WK_l_tsp%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tsp%Smat(1)%tor_e(1:nkrt*WK_l_tsp%n_jk_e(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%tor_e(1:nkrt*WK_l_tsp%n_jk_e(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%tor_e(1:nkrt*WK_l_tsp%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tsp%Smat(1)%pol_o(1:nkrs*WK_l_tsp%n_jk_o(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%pol_o(1:nkrs*WK_l_tsp%n_jk_o(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%pol_o(1:nkrs*WK_l_tsp%n_jk_o(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tsp%Smat(1)%tor_o(1:nkrt*WK_l_tsp%n_jk_o(mp_rlm))        &
     &      =  WK_l_tsp%Smat(1)%tor_o(1:nkrt*WK_l_tsp%n_jk_o(mp_rlm))   &
     &       + WK_l_tsp%Smat(ip)%tor_o(1:nkrt*WK_l_tsp%n_jk_o(mp_rlm))
!$omp end workshare nowait
        end do
!$omp end parallel
!
          call cal_sp_rlm_sym_mat_rin                                   &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                       &
     &        sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,               &
     &        sph_rlm%radius_1d_rlm_r, g_sph_rlm, jst,                  &
     &        WK_l_tsp%n_jk_o(mp_rlm), WK_l_tsp%n_jk_e(mp_rlm),         &
     &        WK_l_tsp%Smat(1)%pol_e(1), WK_l_tsp%Smat(1)%pol_o(1),     &
     &        WK_l_tsp%Smat(1)%tor_e(1), WK_l_tsp%Smat(1)%tor_o(1),     &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WS, WS)
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+17)
!
      end do
!
      end subroutine legendre_f_trans_sym_mat_tj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_sym_mat_rin(nnod_rtm, nidx_rtm,             &
     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,      &
     &         mp_rlm, lst_rtm, nle_rtm, nlo_rtm,                       &
     &         ncomp_recv, nvector, nscalar, irev_sr_rtm, n_WR, WR,     &
     &         symp_r, asmp_p, asmp_r, symp_p)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: lst_rtm, nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp_recv
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_p(2*nvector,nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_p(2*nvector,nidx_rlm(1),nle_rtm)
!
      integer(kind = kint) :: k_rlm, nd, mn_rlm
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      mn_rlm = nidx_rtm(3) - mp_rlm + 1
!
      do lt = 1, nlo_rtm
        lp_rtm = lst_rtm + lt
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_recv
            inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_recv
            inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp_recv
!
            symp_r(3*nd-2,k_rlm,lt)                                     &
     &          = (WR(ipp_recv-2) + WR(ipn_recv-2)) * wp_rtm
            symp_p(2*nd,  k_rlm,lt)                                     &
     &          = (WR(ipp_recv-1) + WR(ipn_recv-1)) * wp_rtm
            symp_p(2*nd-1,k_rlm,lt)                                     &
     &          = (WR(ipp_recv  ) + WR(ipn_recv  )) * wp_rtm
!
            asmp_r(3*nd-2,k_rlm,lt)                                     &
     &          = (WR(ipp_recv-2) - WR(ipn_recv-2)) * wp_rtm
            asmp_p(2*nd,  k_rlm,lt)                                     &
     &          = (WR(ipp_recv-1) - WR(ipn_recv-1)) * wp_rtm
            asmp_p(2*nd-1,k_rlm,lt)                                     &
     &          = (WR(ipp_recv  ) - WR(ipn_recv  )) * wp_rtm
!
            symp_r(3*nd-1,k_rlm,lt)                                     &
     &          = (WR(inp_recv-1) + WR(inn_recv-1)) * wp_rtm * asin_rtm
            symp_r(3*nd,  k_rlm,lt)                                     &
     &          = (WR(inp_recv  ) + WR(inn_recv  )) * wp_rtm * asin_rtm
!
            asmp_r(3*nd-1,k_rlm,lt)                                     &
     &          = (WR(inp_recv-1) - WR(inn_recv-1)) * wp_rtm * asin_rtm
            asmp_r(3*nd,  k_rlm,lt)                                     &
     &          = (WR(inp_recv  ) - WR(inn_recv  )) * wp_rtm * asin_rtm
          end do
        end do
!
        wp_rtm = weight_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
          do nd = 1, nscalar
            ipp_recv = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            ipn_recv = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_recv
!
            symp_r(nd+3*nvector,k_rlm,lt)                               &
     &               = (WR(ipp_recv) + WR(ipn_recv)) * wp_rtm
            asmp_r(nd+3*nvector,k_rlm,lt)                               &
     &               = (WR(ipp_recv) - WR(ipn_recv)) * wp_rtm
          end do
        end do
      end do
!
!   Equator (if necessary)
      do lt = nlo_rtm+1, nle_rtm
        lp_rtm = lst_rtm + lt
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
            inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_recv
!
            symp_r(3*nd-2,k_rlm,lt) = WR(ipp_recv-2) * wp_rtm
            symp_p(2*nd,  k_rlm,lt) = WR(ipp_recv-1) * wp_rtm
            symp_p(2*nd-1,k_rlm,lt) = WR(ipp_recv  ) * wp_rtm
!
            asmp_r(3*nd-2,k_rlm,lt) = 0.0d0
            asmp_p(2*nd,  k_rlm,lt) = 0.0d0
            asmp_p(2*nd-1,k_rlm,lt) = 0.0d0
!
            symp_r(3*nd-1,k_rlm,lt) = WR(inp_recv-1)                    &
     &                                   * wp_rtm * asin_rtm
            symp_r(3*nd,  k_rlm,lt) = WR(inp_recv  )                    &
     &                                   * wp_rtm * asin_rtm
!
            asmp_r(3*nd-1,k_rlm,lt) = 0.0d0
            asmp_r(3*nd,  k_rlm,lt) = 0.0d0
          end do
        end do
!
        lp_rtm = lst_rtm + lt
        wp_rtm = weight_rtm(lp_rtm)
        do k_rlm = 1, nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
          do nd = 1, nscalar
            ipp_recv = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_recv
!
            symp_r(nd+3*nvector,k_rlm,lt) = WR(ipp_recv) * wp_rtm
            asmp_r(nd+3*nvector,k_rlm,lt) = 0.0d0
          end do
        end do
      end do
!
      end subroutine set_vr_rtm_sym_mat_rin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_sym_mat_rin(nnod_rlm, nidx_rlm,             &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         jst, n_jk_o, n_jk_e, pol_e, pol_o,                       &
     &         tor_e, tor_o, ncomp_send, nvector, nscalar,              &
     &         irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &            :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_e(3*nvector+nscalar,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_o(3*nvector+nscalar,nidx_rlm(1),n_jk_o)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_e(2*nvector,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_o(2*nvector,nidx_rlm(1),n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g6e, g6o, g7e, g7o, gme, gmo, r1, r2
!
!
!$omp parallel do                                                       &
!$omp& private(jj,kr_nd,nd,k_rlm,g6e,g6o,g7e,g7o,gme,gmo,r1,r2)
      do jj = 1, n_jk_o
        g6e = g_sph_rlm(2*jj+jst-1,6)
        g6o = g_sph_rlm(2*jj+jst,  6)
        g7e = g_sph_rlm(2*jj+jst-1,7)
        g7o = g_sph_rlm(2*jj+jst,  7)
        gme = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        gmo = dble(idx_gl_1d_rlm_j(2*jj+jst,  3))
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_e(3*nd-2,k_rlm,jj) = pol_e(3*nd-2,k_rlm,jj) * r2*g7e
          tor_e(2*nd,  k_rlm,jj) = tor_e(2*nd,  k_rlm,jj) * r1*g7e
          pol_e(3*nd,  k_rlm,jj) = pol_e(3*nd,  k_rlm,jj) * r1*g7e*gme
          tor_e(2*nd-1,k_rlm,jj) = tor_e(2*nd-1,k_rlm,jj) * r1*g7e
          pol_e(3*nd-1,k_rlm,jj) = pol_e(3*nd-1,k_rlm,jj) * r1*g7e*gme
!
          pol_o(3*nd-2,k_rlm,jj) = pol_o(3*nd-2,k_rlm,jj) * r2*g7o
          tor_o(2*nd,  k_rlm,jj) = tor_o(2*nd,  k_rlm,jj) * r1*g7o
          pol_o(3*nd,  k_rlm,jj) = pol_o(3*nd,  k_rlm,jj) * r1*g7o*gmo
          tor_o(2*nd-1,k_rlm,jj) = tor_o(2*nd-1,k_rlm,jj) * r1*g7o
          pol_o(3*nd-1,k_rlm,jj) = pol_o(3*nd-1,k_rlm,jj) * r1*g7o*gmo
        end do
!
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          pol_e(nd+3*nvector,k_rlm,jj)                                  &
     &            = pol_e(nd+3*nvector,k_rlm,jj) * g6e
          pol_o(nd+3*nvector,k_rlm,jj)                                  &
     &            = pol_o(nd+3*nvector,k_rlm,jj) * g6o
        end do
      end do
!$omp end parallel do
!
!$omp parallel do                                                       &
!$omp& private(kr_nd,k_rlm,nd,jj,ie_rlm,io_rlm,ie_send,io_send)
      do jj = 1, n_jk_o
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
          io_send = 3*nd + (irev_sr_rlm(io_rlm) - 1) * ncomp_send
!
!  even l-m
          WS(ie_send-2) = WS(ie_send-2) + pol_e(3*nd-2,k_rlm,jj)
          WS(ie_send-1) = WS(ie_send-1) - pol_e(3*nd,  k_rlm,jj)        &
     &                                  + tor_e(2*nd,  k_rlm,jj)
          WS(ie_send  ) = WS(ie_send  ) - pol_e(3*nd-1,k_rlm,jj)        &
     &                                  - tor_e(2*nd-1,k_rlm,jj)
!  odd l-m
          WS(io_send-2) = WS(io_send-2) + pol_o(3*nd-2,k_rlm,jj)
          WS(io_send-1) = WS(io_send-1) - pol_o(3*nd,  k_rlm,jj)        &
     &                                  + tor_o(2*nd,  k_rlm,jj)
          WS(io_send  ) = WS(io_send  ) - pol_o(3*nd-1,k_rlm,jj)        &
     &                                  - tor_o(2*nd-1,k_rlm,jj)
        end do
!
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = nd + 3*nvector                                      &
     &             + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
          io_send = nd + 3*nvector                                      &
     &             + (irev_sr_rlm(io_rlm) - 1) * ncomp_send
!
          WS(ie_send) = WS(ie_send) + pol_e(nd+3*nvector,k_rlm,jj)
          WS(io_send) = WS(io_send) + pol_o(nd+3*nvector,k_rlm,jj)
        end do
      end do
!$omp end parallel do
!
      do jj = n_jk_o+1, n_jk_e
        g6e = g_sph_rlm(2*jj+jst-1,6)
        g6o = g_sph_rlm(2*jj+jst,  6)
        g7e = g_sph_rlm(2*jj+jst-1,7)
        g7o = g_sph_rlm(2*jj+jst,  7)
        gme = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        gmo = dble(idx_gl_1d_rlm_j(2*jj+jst,  3))
!$omp parallel do private(kr_nd,nd,k_rlm,r1,r2)
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_e(3*nd-2,k_rlm,jj) = pol_e(3*nd-2,k_rlm,jj) * r2*g7e
          tor_e(2*nd,  k_rlm,jj) = tor_e(2*nd,  k_rlm,jj) * r1*g7e
          pol_e(3*nd,  k_rlm,jj) = pol_e(3*nd,  k_rlm,jj) * r1*g7e*gme
          tor_e(2*nd-1,k_rlm,jj) = tor_e(2*nd-1,k_rlm,jj) * r1*g7e
          pol_e(3*nd-1,k_rlm,jj) = pol_e(3*nd-1,k_rlm,jj) * r1*g7e*gme
        end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,nd,k_rlm,r1,r2)
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          pol_e(nd+3*nvector,k_rlm,jj)                                  &
     &            = pol_e(nd+3*nvector,k_rlm,jj) * g6e
        end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,k_rlm,nd,ie_rlm,ie_send)
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
!
          WS(ie_send-2) = WS(ie_send-2) + pol_e(3*nd-2,k_rlm,jj)
          WS(ie_send-1) = WS(ie_send-1) - pol_e(3*nd,  k_rlm,jj)        &
     &                                  + tor_e(2*nd,  k_rlm,jj)
          WS(ie_send  ) = WS(ie_send  ) - pol_e(3*nd-1,k_rlm,jj)        &
     &                                  - tor_e(2*nd-1,k_rlm,jj)
        end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,k_rlm,nd,ie_rlm,ie_send)
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = nd + 3*nvector                                      &
     &                 + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
          WS(ie_send) = WS(ie_send) + pol_e(nd+3*nvector,k_rlm,jj)
        end do
!$omp end parallel do
      end do
!
      end subroutine cal_sp_rlm_sym_mat_rin
!
! -----------------------------------------------------------------------
!
      subroutine matmul_fwd_leg_trans_tstlop(nkr, n_jk, nl_rtm,         &
     &          V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
!
      if(nkr .eq. 0) return
      S_kj = matmul(V_kl,P_lj)
!
      end subroutine matmul_fwd_leg_trans_tstlop
!
! ----------------------------------------------------------------------
!
      end module leg_fwd_trans_sym_mat_tj
