!>@file   legendre_fwd_trans_testloop.f90
!!@brief  module legendre_fwd_trans_testloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_test(ncomp, nvector, nscalar,&
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          n_WR, n_WS, WR, WS, WK_l_tst)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_testloop
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
      use t_legendre_work_testlooop
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
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
      subroutine legendre_f_trans_vector_test(ncomp, nvector, nscalar,  &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          n_WR, n_WS, WR, WS, WK_l_tst)
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
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: mp_rlm, mn_rlm, nle_rtm, nlo_rtm
      integer(kind = kint) :: nkrs,  nkrt
      integer(kind = kint) :: jst, jst_h
      integer(kind = kint) :: n_jk_e, n_jk_o
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rlm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nle_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nlo_rtm = sph_rtm%nidx_rtm(2) / 2
        nkrs = ncomp * sph_rlm%nidx_rlm(1)
        nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
          jst = idx_trns%lstack_rlm(mp_rlm-1)
          jst_h = idx_trns%lstack_even_rlm(mp_rlm) + 1
          n_jk_e = idx_trns%lstack_even_rlm(mp_rlm)                 &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o = idx_trns%lstack_rlm(mp_rlm)                      &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+15)
          call set_vr_rtm_vec_testloop                            &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm, weight_rtm,          &
     &        mp_rlm, mn_rlm, nle_rtm, nlo_rtm,       &
     &        ncomp, nvector, comm_rtm%irev_sr, n_WR, WR,               &
     &        WK_l_tst%symp_r(1,1), WK_l_tst%asmp_p(1,1),             &
     &        WK_l_tst%asmp_r(1,1), WK_l_tst%symp_p(1,1) )
          call set_vr_rtm_scl_testloop                            &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, weight_rtm,           &
     &        mp_rlm, nle_rtm, nlo_rtm,                                 &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WR, WR,      &
     &        WK_l_tst%symp_r(1,1), WK_l_tst%asmp_r(1,1))
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+15)
!
!  even l-m
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+16)
          call matmul_fwd_leg_trans(nkrs, n_jk_e,               &
     &        WK_l_tst%nth_sym, WK_l_tst%symp_r(1,1),                  &
     &        WK_l_tst%Ps_tj(1,jst+1), WK_l_tst%pol_e(1,1))
          call matmul_fwd_leg_trans(nkrt, n_jk_e,               &
     &        WK_l_tst%nth_sym, WK_l_tst%asmp_p(1,1),                  &
     &        WK_l_tst%dPsdt_tj(1,jst+1), WK_l_tst%tor_e(1,1))
!
!  odd l-m
          call matmul_fwd_leg_trans(nkrs, n_jk_o,               &
     &        WK_l_tst%nth_sym, WK_l_tst%asmp_r(1,1),                  &
     &        WK_l_tst%Ps_tj(1,jst_h), WK_l_tst%pol_o(1,1))
          call matmul_fwd_leg_trans(nkrt, n_jk_o,               &
     &        WK_l_tst%nth_sym, WK_l_tst%symp_p(1,1),                  &
     &        WK_l_tst%dPsdt_tj(1,jst_h), WK_l_tst%tor_o(1,1))
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+16)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+17)
          call cal_sp_rlm_vec_testloop                            &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                       &
     &        sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,               &
     &        sph_rlm%radius_1d_rlm_r, g_sph_rlm,                       &
     &        jst, n_jk_o, n_jk_e,        &
     &        WK_l_tst%pol_e(1,1), WK_l_tst%pol_o(1,1),               &
     &        WK_l_tst%tor_e(1,1), WK_l_tst%tor_o(1,1),               &
     &        ncomp, nvector, comm_rlm%irev_sr, n_WS, WS)
          call cal_sp_rlm_scl_testloop(sph_rlm%nnod_rlm,          &
     &        sph_rlm%nidx_rlm, sph_rlm%istep_rlm, g_sph_rlm,           &
     &        jst, n_jk_o, n_jk_e,        &
     &        WK_l_tst%pol_e(1,1), WK_l_tst%pol_o(1,1),               &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WS, WS)
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+17)
!
        end do
!
      end subroutine legendre_f_trans_vector_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vec_testloop(nnod_rtm, nidx_rtm,      &
     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,      &
     &         mp_rlm, mn_rlm, nle_rtm, nlo_rtm,              &
     &         ncomp, nvector, irev_sr_rtm, n_WR, WR,                   &
     &         symp_r, asmp_p, asmp_r, symp_p)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_r(ncomp*nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_p(2*nidx_rlm(1)*nvector,nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: asmp_r(ncomp*nidx_rlm(1),nle_rtm)
      real(kind=kreal), intent(inout)                                   &
     &         :: symp_p(2*nidx_rlm(1)*nvector,nle_rtm)
!
!
      integer(kind = kint) :: kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(lp_rtm,ln_rtm,kk,k_rlm,nd,                    &
!$omp&                    ip_rtpm,ip_rtnm,in_rtpm,in_rtnm,              &
!$omp&                    ipp_recv,ipn_recv,inp_recv,inn_recv,          &
!$omp&                    asin_rtm,wp_rtm)
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
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
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
          inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
          symp_r(kk,lp_rtm) =      (WR(ipp_recv-2) + WR(ipn_recv-2))    &
     &                             * wp_rtm
          symp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) + WR(ipn_recv-1))    &
     &                             * wp_rtm
          symp_p(kk,lp_rtm) =      (WR(ipp_recv  ) + WR(ipn_recv  ))    &
     &                             * wp_rtm
!
          asmp_r(kk,lp_rtm) = (WR(ipp_recv-2) - WR(ipn_recv-2))*wp_rtm
          asmp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) - WR(ipn_recv-1))    &
     &                             * wp_rtm
          asmp_p(kk,lp_rtm) = (WR(ipp_recv  ) - WR(ipn_recv  ))*wp_rtm
!
          symp_r(kk+nkrv,lp_rtm) =   (WR(inp_recv-1) + WR(inn_recv-1))  &
     &                             * wp_rtm * asin_rtm
          symp_r(kk+2*nkrv,lp_rtm) = (WR(inp_recv  ) + WR(inn_recv  ))  &
     &                             * wp_rtm * asin_rtm
!
          asmp_r(kk+nkrv,lp_rtm) =   (WR(inp_recv-1) - WR(inn_recv-1))  &
     &                             * wp_rtm * asin_rtm
          asmp_r(kk+2*nkrv,lp_rtm) = (WR(inp_recv  ) - WR(inn_recv  ))  &
     &                             * wp_rtm * asin_rtm
        end do
        end do
      end do
!$omp end parallel do
!
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
          symp_r(kk,lp_rtm) =      WR(ipp_recv-2) * wp_rtm
          symp_p(kk+nkrv,lp_rtm) = WR(ipp_recv-1) * wp_rtm
          symp_p(kk,lp_rtm) =      WR(ipp_recv  ) * wp_rtm
!
          asmp_r(kk,lp_rtm) =      0.0d0
          asmp_p(kk+nkrv,lp_rtm) = 0.0d0
          asmp_p(kk,lp_rtm) =      0.0d0
!
          symp_r(kk+nkrv,lp_rtm) =   WR(inp_recv-1) * wp_rtm * asin_rtm
          symp_r(kk+2*nkrv,lp_rtm) = WR(inp_recv  ) * wp_rtm * asin_rtm
!
          asmp_r(kk+nkrv,lp_rtm) =   0.0d0
          asmp_r(kk+2*nkrv,lp_rtm) = 0.0d0
        end do
        end do
      end do
!
      end subroutine set_vr_rtm_vec_testloop
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scl_testloop                          &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,    &
     &          mp_rlm,  nle_rtm, nlo_rtm,                    &
     &          ncomp, nvector, nscalar, irev_sr_rtm,                   &
     &          n_WR, WR, symp, asmp)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &         :: symp(ncomp*nidx_rlm(1),nle_rtm)
      real(kind = kreal), intent(inout)                                 &
     &         :: asmp(ncomp*nidx_rlm(1),nle_rtm)
!
      integer(kind = kint) :: kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
      real(kind = kreal) :: wp_rtm
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(lp_rtm,ln_rtm,kk,k_rlm,nd,                    &
!$omp&                    ip_rtpm,ip_rtnm,ipp_recv,ipn_recv,wp_rtm)
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm = weight_rtm(lp_rtm)
        do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
          symp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) + WR(ipn_recv))        &
     &                            * wp_rtm
          asmp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) - WR(ipn_recv))        &
     &                            * wp_rtm
        end do
        end do
      end do
!$omp end parallel do
!
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                + (k_rlm-1) *  istep_rtm(1)                   &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                 &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          symp(kk+3*nkrv,lp_rtm) = WR(ipp_recv) * wp_rtm
          asmp(kk+3*nkrv,lp_rtm) = 0.0d0
        end do
        end do
      end do
!
      end subroutine set_vr_rtm_scl_testloop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_vec_testloop(nnod_rlm, nidx_rlm,      &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         jst, n_jk_o, n_jk_e, pol_e, pol_o,             &
     &         tor_e, tor_o, ncomp, nvector, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &            :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_e(ncomp*nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_o(ncomp*nidx_rlm(1),n_jk_o)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_e(2*nvector*nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_o(2*nvector*nidx_rlm(1),n_jk_o)
!
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, k_rlm, nkrv
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g7, gm, r1, r2
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(jj,kk,nd,k_rlm,g7,gm,r1,r2)
      do jj = 1, n_jk_e
        g7 = g_sph_rlm(2*jj+jst-1,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_e(kk,jj) =        pol_e(kk,jj) *        r2 * g7
          tor_e(kk+nkrv,jj) =   tor_e(kk+nkrv,jj) *   r1 * g7
          pol_e(kk+2*nkrv,jj) = pol_e(kk+2*nkrv,jj) * r1 * g7 * gm
          tor_e(kk,jj) =        tor_e(kk,jj) *        r1 * g7
          pol_e(kk+nkrv,jj) =   pol_e(kk+nkrv,jj) *   r1 * g7 * gm
        end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(jj,kk,nd,k_rlm,g7,gm,r1,r2)
      do jj = 1, n_jk_o
        g7 = g_sph_rlm(2*jj+jst,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst,3))
        do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_o(kk,jj) =        pol_o(kk,jj) *        r2 * g7
          tor_o(kk+nkrv,jj) =   tor_o(kk+nkrv,jj) *   r1 * g7
          pol_o(kk+2*nkrv,jj) = pol_o(kk+2*nkrv,jj) * r1 * g7 * gm
          tor_o(kk,jj) =        tor_o(kk,jj) *        r1 * g7
          pol_o(kk+nkrv,jj) =   pol_o(kk+nkrv,jj) *   r1 * g7 * gm
        end do
        end do
      end do
!$omp end parallel do
!
!
!$omp parallel do private(kk,k_rlm,nd,jj,ie_rlm,io_rlm,ie_send,io_send)
      do jj = 1, n_jk_o
        do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          io_send = 3*nd + (irev_sr_rlm(io_rlm) - 1) * ncomp
!
!  even l-m
          WS(ie_send-2) = WS(ie_send-2) + pol_e(kk,jj)
          WS(ie_send-1) = WS(ie_send-1)                                 &
     &                   - pol_e(kk+2*nkrv,jj) + tor_e(kk+nkrv,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - pol_e(kk+nkrv,jj) -   tor_e(kk,jj)
!  odd l-m
          WS(io_send-2) = WS(io_send-2) + pol_o(kk,jj)
          WS(io_send-1) = WS(io_send-1)                                 &
     &                   - pol_o(kk+2*nkrv,jj) + tor_o(kk+nkrv,jj)
          WS(io_send  ) = WS(io_send  )                                 &
     &                   - pol_o(kk+nkrv,jj) -   tor_o(kk,jj)
        end do
        end do
      end do
!$omp end parallel do
!
      do jj = n_jk_o+1, n_jk_e
!$omp parallel do private(kk,k_rlm,nd,ie_rlm,ie_send)
        do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp
!
          WS(ie_send-2) = WS(ie_send-2) + pol_e(kk,jj)
          WS(ie_send-1) = WS(ie_send-1)                                 &
     &                   - pol_e(kk+2*nkrv,jj) + tor_e(kk+nkrv,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - pol_e(kk+nkrv,jj) -   tor_e(kk,jj)
        end do
        end do
!$omp end parallel do
      end do
!
      end subroutine cal_sp_rlm_vec_testloop
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scl_testloop                          &
     &         (nnod_rlm, nidx_rlm, istep_rlm, g_sph_rlm,               &
     &          jst, n_jk_o, n_jk_e, scl_e, scl_o,            &
     &          ncomp, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout)                                 &
     &           :: scl_e(ncomp*nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &           :: scl_o(ncomp*nidx_rlm(1),n_jk_o)
!
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  kk, k_rlm, nkrv
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g6
!
!
      nkrv = nidx_rlm(1) * nvector
!$omp parallel do private(jj,nd,k_rlm,kk,g6)
      do jj = 1, n_jk_e
        g6 = g_sph_rlm(2*jj+jst-1,6)
        do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
          scl_e(kk+3*nkrv,jj) = scl_e(kk+3*nkrv,jj) * g6
        end do
      end do
      end do
!$omp end parallel do
!
!$omp parallel do private(jj,nd,k_rlm,kk,g6)
      do jj = 1, n_jk_o
        g6 = g_sph_rlm(2*jj+jst,6)
        do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
          kk = k_rlm + (nd-1) * nidx_rlm(1)
          scl_o(kk+3*nkrv,jj) = scl_o(kk+3*nkrv,jj) * g6
        end do
      end do
      end do
!$omp end parallel do
!
!$omp parallel do private(kk,k_rlm,nd,jj,ie_rlm,io_rlm,ie_send,io_send)
      do nd = 1, nscalar
      do k_rlm = 1, nidx_rlm(1)
        kk = k_rlm + (nd-1) * nidx_rlm(1)
        do jj = 1, n_jk_o
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = nd + 3*nvector + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          io_send = nd + 3*nvector + (irev_sr_rlm(io_rlm) - 1) * ncomp
!
          WS(ie_send) = WS(ie_send) + scl_e(kk+3*nkrv,jj)
          WS(io_send) = WS(io_send) + scl_o(kk+3*nkrv,jj)
        end do
!
        do jj = n_jk_o+1, n_jk_e
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = nd + 3*nvector + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          WS(ie_send) = WS(ie_send) + scl_e(kk+3*nkrv,jj)
        end do
      end do
      end do
!$omp end parallel do
!
      end subroutine cal_sp_rlm_scl_testloop
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_testloop
