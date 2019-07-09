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
      integer(kind = kint) :: nl_rtm, mp_rlm, mn_rlm
      integer(kind = kint) :: nkrs,  nkrt
      integer(kind = kint) :: jst
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nl_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
        nkrs = ncomp * sph_rlm%nidx_rlm(1)
        nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
          jst = idx_trns%lstack_rlm(mp_rlm-1)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+12)
          call set_sp_rlm_vec_testloop                            &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r, g_sph_rlm, &
     &        jst, WK_l_tst%Pmat(mp_rlm)%n_jk_e,                        &
     &        WK_l_tst%Pmat(mp_rlm)%n_jk_o,                          &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WR, WR,     &
     &        WK_l_tst%pol_e(1,1), WK_l_tst%tor_e(1,1),               &
     &        WK_l_tst%pol_o(1,1), WK_l_tst%tor_o(1,1) )
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+12)
!
!   even l-m
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+13)
          call matmul_bwd_leg_trans_tstlop                              &
     &       (nl_rtm, nkrs, WK_l_tst%Pmat(mp_rlm)%n_jk_e,               &
     &        WK_l_tst%Pmat(mp_rlm)%Pse_tj, WK_l_tst%pol_e(1,1),        &
     &        WK_l_tst%symp_r(1,1))
          call matmul_bwd_leg_trans_tstlop                              &
     &       (nl_rtm, nkrt, WK_l_tst%Pmat(mp_rlm)%n_jk_e,              &
     &        WK_l_tst%Pmat(mp_rlm)%dPsedt_tj, WK_l_tst%tor_e(1,1),     &
     &        WK_l_tst%asmp_p(1,1))
!   odd l-m
          call matmul_bwd_leg_trans_tstlop                              &
     &       (nl_rtm, nkrs, WK_l_tst%Pmat(mp_rlm)%n_jk_o,               &
     &        WK_l_tst%Pmat(mp_rlm)%Pso_tj, WK_l_tst%pol_o(1,1),        &
     &        WK_l_tst%asmp_r(1,1))
          call matmul_bwd_leg_trans_tstlop                              &
     &       (nl_rtm, nkrt, WK_l_tst%Pmat(mp_rlm)%n_jk_o,               &
     &        WK_l_tst%Pmat(mp_rlm)%dPsodt_tj, WK_l_tst%tor_o(1,1),     &
     &        WK_l_tst%symp_p(1,1))
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+13)
!
      if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+14)
          call cal_vr_rtm_vec_testloop                            &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, asin_theta_1d_rtm,                      &
     &        mp_rlm, mn_rlm, nl_rtm,                 &
     &        WK_l_tst%symp_r(1,1), WK_l_tst%asmp_p(1,1),             &
     &        WK_l_tst%asmp_r(1,1), WK_l_tst%symp_p(1,1),             &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+14)
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
     &          jst, n_jk_e, n_jk_o, ncomp, nvector, nscalar,    &
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
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: pol_e(n_jk_e,nidx_rlm(1),ncomp)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_e(n_jk_e,nidx_rlm(1),2*nvector)
      real(kind = kreal), intent(inout)                                 &
     &           :: pol_o(n_jk_o,nidx_rlm(1),ncomp)
      real(kind = kreal), intent(inout)                                 &
     &           :: tor_o(n_jk_o,nidx_rlm(1),2*nvector)
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
            i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
            pol_e(jj,k_rlm,3*nd-2) = WR(i_recv-2) * a2r_1d_rlm_r * g3
            tor_e(jj,k_rlm,2*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r
            pol_e(jj,k_rlm,3*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r * gm
            tor_e(jj,k_rlm,2*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r
            pol_e(jj,k_rlm,3*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r * gm
          end do
!
!   odd l-m
          do jj = 1, n_jk_o
            j_rlm = 2*jj + jst
            g3 = g_sph_rlm(j_rlm,3)
            gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
            i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                        &
     &                + (k_rlm-1) * istep_rlm(1)
            i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
            pol_o(jj,k_rlm,3*nd-2) = WR(i_recv-2) * a2r_1d_rlm_r * g3
            tor_o(jj,k_rlm,2*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r
            pol_o(jj,k_rlm,3*nd  ) = WR(i_recv-1) * a1r_1d_rlm_r * gm
            tor_o(jj,k_rlm,2*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r
            pol_o(jj,k_rlm,3*nd-1) = WR(i_recv  ) * a1r_1d_rlm_r * gm
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
            i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
            pol_e(jj,k_rlm,nd+3*nvector) = WR(i_recv)
          end do
!
!   odd l-m
          do jj = 1, n_jk_o
            i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                 &
     &                + (k_rlm-1) *        istep_rlm(1)
            i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
            pol_o(jj,k_rlm,nd+3*nvector) = WR(i_recv)
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
     &          mp_rlm, mn_rlm, nl_rtm, symp_r, asmp_p,       &
     &          asmp_r, symp_p, ncomp, nvector, nscalar,      &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in)                                &
     &           :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nl_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout)                             &
     &           :: symp_r(nl_rtm,nidx_rlm(1),ncomp)
      real(kind = kreal), intent(in)                                &
     &           :: asmp_p(nl_rtm,nidx_rlm(1),2*nvector)
      real(kind = kreal), intent(inout)                             &
     &           :: asmp_r(nl_rtm,nidx_rlm(1),ncomp)
      real(kind = kreal), intent(in)                                &
     &           :: symp_p(nl_rtm,nidx_rlm(1),2*nvector)
!
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
!$omp parallel do private(nd,k_rlm,lp_rtm)
      do nd = 1, nvector
        do k_rlm = 1, nidx_rlm(1)
          do lp_rtm = 1, nl_rtm
            symp_r(lp_rtm,k_rlm,3*nd-1) = - symp_r(lp_rtm,k_rlm,3*nd-1) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            symp_r(lp_rtm,k_rlm,3*nd  ) = - symp_r(lp_rtm,k_rlm,3*nd  ) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(lp_rtm,k_rlm,3*nd-1) = - asmp_r(lp_rtm,k_rlm,3*nd-1) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
            asmp_r(lp_rtm,k_rlm,3*nd  ) = - asmp_r(lp_rtm,k_rlm,3*nd  ) &
     &                                    * asin_theta_1d_rtm(lp_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(k_rlm,nd,lp_rtm,ln_rtm,                    &
!$omp&                    ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,              &
!$omp&                    ipp_send,inp_send,ipn_send,inn_send)
      do k_rlm = 1, nidx_rlm(1)
        do lp_rtm = 1, nidx_rtm(2)/2
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
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
            ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
            inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
            WS(ipp_send-2) = WS(ipp_send-2)                             &
     &                      + symp_r(lp_rtm,k_rlm,3*nd-2)               &
     &                      + asmp_r(lp_rtm,k_rlm,3*nd-2)
            WS(ipp_send-1) = WS(ipp_send-1)                             &
     &                      + asmp_p(lp_rtm,k_rlm,2*nd  )               &
     &                      + symp_p(lp_rtm,k_rlm,2*nd  )
            WS(ipp_send  ) = WS(ipp_send  )                             &
     &                      - asmp_p(lp_rtm,k_rlm,2*nd-1)               &
     &                      - symp_p(lp_rtm,k_rlm,2*nd-1)
!
            WS(inp_send-1) = WS(inp_send-1)                             &
     &                      + symp_r(lp_rtm,k_rlm,3*nd-1)               &
     &                      + asmp_r(lp_rtm,k_rlm,3*nd-1)
            WS(inp_send  ) = WS(inp_send  )                             &
     &                      + symp_r(lp_rtm,k_rlm,3*nd  )               &
     &                      + asmp_r(lp_rtm,k_rlm,3*nd  )
!
!
            WS(ipn_send-2) = WS(ipn_send-2)                             &
     &                      + symp_r(lp_rtm,k_rlm,3*nd-2)               &
     &                      - asmp_r(lp_rtm,k_rlm,3*nd-2)
            WS(ipn_send-1) = WS(ipn_send-1)                             &
     &                      - asmp_p(lp_rtm,k_rlm,2*nd  )               &
     &                      + symp_p(lp_rtm,k_rlm,2*nd  )
            WS(ipn_send  ) = WS(ipn_send  )                             &
     &                      + asmp_p(lp_rtm,k_rlm,2*nd-1)               &
     &                      - symp_p(lp_rtm,k_rlm,2*nd-1)
!
            WS(inn_send-1) = WS(inn_send-1)                             &
     &                      + symp_r(lp_rtm,k_rlm,3*nd-1)               &
     &                      - asmp_r(lp_rtm,k_rlm,3*nd-1)
            WS(inn_send  ) = WS(inn_send  )                             &
     &                      + symp_r(lp_rtm,k_rlm,3*nd  )               &
     &                      - asmp_r(lp_rtm,k_rlm,3*nd  )
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            ipn_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                  + symp_r(lp_rtm,k_rlm,nd+3*nvector)             &
     &                  + asmp_r(lp_rtm,k_rlm,nd+3*nvector)
            WS(ipn_send) = WS(ipn_send)                                 &
     &                  + symp_r(lp_rtm,k_rlm,nd+3*nvector)             &
     &                  - asmp_r(lp_rtm,k_rlm,nd+3*nvector)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(k_rlm,nd,lp_rtm,ip_rtpm,in_rtpm,    &
!$omp&                    ipp_send,inp_send)
      do k_rlm = 1, nidx_rlm(1)
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          do nd = 1, nvector
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
            WS(ipp_send-2) = WS(ipp_send-2) + symp_r(lp_rtm,k_rlm,3*nd-2)
            WS(ipp_send-1) = WS(ipp_send-1) + symp_p(lp_rtm,k_rlm,2*nd  )
            WS(ipp_send  ) = WS(ipp_send  ) - symp_p(lp_rtm,k_rlm,2*nd-1)
!
            WS(inp_send-1) = WS(inp_send-1) + symp_r(lp_rtm,k_rlm,3*nd-1)
            WS(inp_send  ) = WS(inp_send  ) + symp_r(lp_rtm,k_rlm,3*nd  )
          end do
!
          do nd = 1, nscalar
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                    + symp_r(lp_rtm,k_rlm,nd+3*nvector)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_vr_rtm_vec_testloop
!
! -----------------------------------------------------------------------
!
      subroutine matmul_bwd_leg_trans_tstlop(nl_rtm, nkr, n_jk,         &
     &          P_lj, S_jk, V_lk)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
!
      if(nkr .eq. 0) return
      V_lk = matmul(P_lj,S_jk)
!
      end subroutine matmul_bwd_leg_trans_tstlop
!
! ----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
