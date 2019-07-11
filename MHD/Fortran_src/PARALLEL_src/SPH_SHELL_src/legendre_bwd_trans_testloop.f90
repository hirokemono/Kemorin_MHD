!>@file   legendre_bwd_trans_testloop.f90
!!@brief  module legendre_bwd_trans_testloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_test(ncomp, nvector, nscalar,&
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm,                         &
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
      module legendre_bwd_trans_testloop
!
      use m_precision
!
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_legendre_work_testlooop
      use m_elapsed_labels_SPH_TRNS
!
      use matmul_for_legendre_trans
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
      subroutine legendre_b_trans_vector_test(ncomp, nvector, nscalar,  &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,         &
     &          asin_theta_1d_rtm, g_sph_rlm,                           &
     &          n_WR, n_WS, WR, WS, WK_l_tst)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm
      integer(kind = kint) :: nkrs, nkrt, lst_rtm
      integer(kind = kint) :: ip, jst
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nkrs = (3*nvector + nscalar) * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
      write(*,*) 'bwd nkrs', nkrs, WK_l_tst%nkrs
      write(*,*) 'bwd nkrt', nkrt, WK_l_tst%nkrt
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        jst = idx_trns%lstack_rlm(mp_rlm-1)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+12)
          call set_sp_rlm_vec_testloop                            &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm, &
     &        jst, WK_l_tst%Pmat(mp_rlm)%n_jk_e,                        &
     &        WK_l_tst%Pmat(mp_rlm)%n_jk_o,                          &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WR, WR,     &
     &        WK_l_tst%pol_e(1), WK_l_tst%tor_e(1),               &
     &        WK_l_tst%pol_o(1), WK_l_tst%tor_o(1) )
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+12)
!
        do ip = 1, np_smp
          lst_rtm = WK_l_tst%Fmat(ip)%lst_rtm
!   even l-m
          if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+13)
          call matmul_bwd_leg_trans_tstlop                              &
     &       (WK_l_tst%Fmat(ip)%nle_rtm, nkrs,                      &
     &        WK_l_tst%Pmat(mp_rlm)%n_jk_e,                             &
     &        WK_l_tst%Pmat(mp_rlm)%Pse_jt(1,lst_rtm+1), WK_l_tst%pol_e(1), &
     &        WK_l_tst%Fmat(ip)%symp_r(1))
          call matmul_bwd_leg_trans_tstlop                              &
     &       (WK_l_tst%Fmat(ip)%nle_rtm, nkrt,                      &
     &        WK_l_tst%Pmat(mp_rlm)%n_jk_e,              &
     &        WK_l_tst%Pmat(mp_rlm)%dPsedt_jt(1,lst_rtm+1), WK_l_tst%tor_e(1), &
     &        WK_l_tst%Fmat(ip)%asmp_p(1))
!   odd l-m
          call matmul_bwd_leg_trans_tstlop                              &
     &       (WK_l_tst%Fmat(ip)%nle_rtm, nkrs,                      &
     &        WK_l_tst%Pmat(mp_rlm)%n_jk_o,               &
     &        WK_l_tst%Pmat(mp_rlm)%Pso_jt(1,lst_rtm+1), WK_l_tst%pol_o(1), &
     &        WK_l_tst%Fmat(ip)%asmp_r(1))
          call matmul_bwd_leg_trans_tstlop                              &
     &       (WK_l_tst%Fmat(ip)%nle_rtm, nkrt,                      &
     &        WK_l_tst%Pmat(mp_rlm)%n_jk_o,               &
     &        WK_l_tst%Pmat(mp_rlm)%dPsodt_jt(1,lst_rtm+1), WK_l_tst%tor_o(1),     &
     &        WK_l_tst%Fmat(ip)%symp_p(1))
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+13)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+14)
          call cal_vr_rtm_vec_testloop                            &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm, mp_rlm,    &
     &        WK_l_tst%Fmat(ip)%lst_rtm,  &
     &        WK_l_tst%Fmat(ip)%nle_rtm,  &
     &        WK_l_tst%Fmat(ip)%nlo_rtm,  &
     &        WK_l_tst%Fmat(ip)%symp_r(1), WK_l_tst%Fmat(ip)%asmp_p(1), &
     &        WK_l_tst%Fmat(ip)%asmp_r(1), WK_l_tst%Fmat(ip)%symp_p(1), &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
          if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+14)
        end do
!
      end do
!
      end subroutine legendre_b_trans_vector_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vec_testloop(nnod_rlm, nidx_rlm,      &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          jst, n_jk_e, n_jk_o, ncomp_recv, nvector, nscalar,    &
     &          irev_sr_rlm, n_WR, WR,  pol_e, tor_e, pol_o, tor_o)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp_recv
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                                 &
     &           :: pol_e(3*nvector+nscalar,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_e(2*nvector,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &           :: pol_o(3*nvector+nscalar,nidx_rlm(1),n_jk_o)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_o(2*nvector,nidx_rlm(1),n_jk_o)
!
      integer(kind = kint) :: jj, k_rlm, nd
      integer(kind = kint) :: j_rlm, i_rlm, i_recv
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
!$omp parallel do private(k_rlm,nd,a1r_1d_rlm_r,a2r_1d_rlm_r,        &
!$omp&                    jj,j_rlm,i_rlm,i_recv,g3,gm)
      do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
          a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
          do jj = 1, n_jk_e
            j_rlm = 2*jj + jst - 1
            g3 = g_sph_rlm(j_rlm,3)
            gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
            i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                        &
     &                + (k_rlm-1) * istep_rlm(1)
            i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
!
            pol_e(3*nd-2,k_rlm,jj) = WR(i_recv-2) * a2r_1d_rlm_r * g3
            tor_e(2*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r
            pol_e(3*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
            tor_e(2*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r
            pol_e(3*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
          end do
!
!   odd l-m
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst
            g3 = g_sph_rlm(j_rlm,3)
            gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
            i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                        &
     &                + (k_rlm-1) * istep_rlm(1)
            i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
!
            pol_o(3*nd-2,k_rlm,jj) = WR(i_recv-2) * a2r_1d_rlm_r * g3
            tor_o(2*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r
            pol_o(3*nd,  k_rlm,jj) = WR(i_recv-1) * a1r_1d_rlm_r * gm
            tor_o(2*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r
            pol_o(3*nd-1,k_rlm,jj) = WR(i_recv  ) * a1r_1d_rlm_r * gm
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(k_rlm,nd,jj,i_rlm,i_recv)
      do nd = 1, nscalar
        do k_rlm = 1, nidx_rlm(1)
!   even l-m
          do jj = 1, n_jk_e
            i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                 &
     &                + (k_rlm-1) *        istep_rlm(1)
            i_recv = nd + 3*nvector                                     &
     &              + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
            pol_e(nd+3*nvector,k_rlm,jj) = WR(i_recv)
          end do
!
!   odd l-m
          do jj = 1, n_jk_o
            i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                 &
     &                + (k_rlm-1) *        istep_rlm(1)
            i_recv = nd + 3*nvector                                     &
     &              + (irev_sr_rlm(i_rlm) - 1) * ncomp_recv
            pol_o(nd+3*nvector,k_rlm,jj) = WR(i_recv)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_sp_rlm_vec_testloop
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vec_testloop(nnod_rtm, nidx_rtm,      &
     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,                 &
     &          mp_rlm, lst_rtm, nle_rtm, nlo_rtm, symp_r, asmp_p,      &
     &          asmp_r, symp_p, ncomp_send, nvector, nscalar,      &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in)                                &
     &           :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: lst_rtm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      real(kind = kreal), intent(inout)                             &
     &           :: symp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind = kreal), intent(in)                                &
     &           :: asmp_p(2*nvector,nidx_rlm(1),nle_rtm)
      real(kind = kreal), intent(inout)                             &
     &           :: asmp_r(3*nvector+nscalar,nidx_rlm(1),nle_rtm)
      real(kind = kreal), intent(in)                                &
     &           :: symp_p(2*nvector,nidx_rlm(1),nle_rtm)
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: k_rlm, nd, mn_rlm
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      mn_rlm = nidx_rtm(3) - mp_rlm + 1
!
      do lt = 1, nle_rtm
        lp_rtm = lst_rtm + lt
        do nd = 1, nvector
          do k_rlm = 1, nidx_rlm(1)
            symp_r(3*nd-1,k_rlm,lt) = - symp_r(3*nd-1,k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            symp_r(3*nd,  k_rlm,lt) = - symp_r(3*nd,  k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(3*nd-1,k_rlm,lt) = - asmp_r(3*nd-1,k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(3*nd,  k_rlm,lt) = - asmp_r(3*nd,  k_rlm,lt) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
          end do
        end do
      end do
!
      do lt = 1, nlo_rtm
        lp_rtm = lst_rtm + lt
        do k_rlm = 1, nidx_rlm(1)
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
            ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
            inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2)                             &
     &                      + symp_r(3*nd-2,k_rlm,lt)               &
     &                      + asmp_r(3*nd-2,k_rlm,lt)
            WS(ipp_send-1) = WS(ipp_send-1)                             &
     &                      + asmp_p(2*nd,  k_rlm,lt)               &
     &                      + symp_p(2*nd,  k_rlm,lt)
            WS(ipp_send  ) = WS(ipp_send  )                             &
     &                      - asmp_p(2*nd-1,k_rlm,lt)               &
     &                      - symp_p(2*nd-1,k_rlm,lt)
!
            WS(inp_send-1) = WS(inp_send-1)                             &
     &                      + symp_r(3*nd-1,k_rlm,lt)               &
     &                      + asmp_r(3*nd-1,k_rlm,lt)
            WS(inp_send  ) = WS(inp_send  )                             &
     &                      + symp_r(3*nd,  k_rlm,lt)               &
     &                      + asmp_r(3*nd,  k_rlm,lt)
!
!
            WS(ipn_send-2) = WS(ipn_send-2)                             &
     &                      + symp_r(3*nd-2,k_rlm,lt)               &
     &                      - asmp_r(3*nd-2,k_rlm,lt)
            WS(ipn_send-1) = WS(ipn_send-1)                             &
     &                      - asmp_p(2*nd,  k_rlm,lt)               &
     &                      + symp_p(2*nd,  k_rlm,lt)
            WS(ipn_send  ) = WS(ipn_send  )                             &
     &                      + asmp_p(2*nd-1,k_rlm,lt)               &
     &                      - symp_p(2*nd-1,k_rlm,lt)
!
            WS(inn_send-1) = WS(inn_send-1)                             &
     &                      + symp_r(3*nd-1,k_rlm,lt)               &
     &                      - asmp_r(3*nd-1,k_rlm,lt)
            WS(inn_send  ) = WS(inn_send  )                             &
     &                      + symp_r(3*nd,  k_rlm,lt)               &
     &                      - asmp_r(3*nd,  k_rlm,lt)
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            ipn_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtnm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                  + symp_r(nd+3*nvector,k_rlm,lt)             &
     &                  + asmp_r(nd+3*nvector,k_rlm,lt)
            WS(ipn_send) = WS(ipn_send)                                 &
     &                  + symp_r(nd+3*nvector,k_rlm,lt)             &
     &                  - asmp_r(nd+3*nvector,k_rlm,lt)
          end do
        end do
      end do
!
      do lt = nlo_rtm+1, nle_rtm
        lp_rtm = lst_rtm + lt
        do k_rlm = 1, nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp_send
!
            WS(ipp_send-2) = WS(ipp_send-2) + symp_r(3*nd-2,k_rlm,lt)
            WS(ipp_send-1) = WS(ipp_send-1) + symp_p(2*nd,  k_rlm,lt)
            WS(ipp_send  ) = WS(ipp_send  ) - symp_p(2*nd-1,k_rlm,lt)
!
            WS(inp_send-1) = WS(inp_send-1) + symp_r(3*nd-1,k_rlm,lt)
            WS(inp_send  ) = WS(inp_send  ) + symp_r(3*nd,  k_rlm,lt)
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp_send
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                    + symp_r(nd+3*nvector,k_rlm,lt)
          end do
        end do
      end do
!
      end subroutine cal_vr_rtm_vec_testloop
!
! -----------------------------------------------------------------------
!
      subroutine matmul_bwd_leg_trans_tstlop(nl_rtm, nkr, n_jk,         &
     &          P_jl, S_kj, V_kl)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_jl(n_jk,nl_rtm)
      real(kind = kreal), intent(in) :: S_kj(nkr,n_jk)
!
      real(kind = kreal), intent(inout) :: V_kl(nkr,nl_rtm)
!
!
      if(nkr .eq. 0) return
      V_kl = matmul(S_kj,P_jl)
!
      end subroutine matmul_bwd_leg_trans_tstlop
!
! ----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
