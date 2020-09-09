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
!!        type(sph_shell_parameters), intent(in) :: sph_params
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
      use t_spheric_parameter
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
      subroutine legendre_f_trans_vector_test                           &
     &         (iflag_matmul, ncomp, nvector, nscalar, sph_params,      &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns, leg,    &
     &          n_WR, n_WS, WR, WS, WK_l_tst)
!
      use t_schmidt_poly_on_rtm
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: mm, mp_rlm, mn_rlm, lp_rtm, ln_rtm
      integer(kind = kint) :: nkrs, nkrt, lt, lt2
      integer(kind = kint) :: ip, jst
      integer(kind = kint) :: lst
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
        mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(mp_rlm,2))
        jst = idx_trns%lstack_rlm(mp_rlm-1)
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+16)
!$omp parallel do private(ip,lt,lp_rtm,ln_rtm,lt2,lst)
        do ip = 1, np_smp
          WK_l_tst%Smat(ip)%pol_e(1:WK_l_tst%n_pol_e) = 0.0d0
          WK_l_tst%Smat(ip)%tor_e(1:WK_l_tst%n_tor_e) = 0.0d0
          WK_l_tst%Smat(ip)%pol_o(1:WK_l_tst%n_pol_e) = 0.0d0
          WK_l_tst%Smat(ip)%tor_o(1:WK_l_tst%n_tor_e) = 0.0d0
!
          do lt2 = 1, WK_l_tst%nlo_rtm(ip) / 8
            call legendre_fwd_trans_8lat_test                         &
     &         (lt2, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,             &
     &          iflag_matmul, ncomp, nvector, nscalar, sph_params,    &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,            &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),     &
     &          WK_l_tst%lst_rtm(ip), WK_l_tst%Fmat(ip),              &
     &          WK_l_tst%Pmat(ip), WK_l_tst%Smat(ip))
          end do
          lst = 1 + int(WK_l_tst%nlo_rtm(ip)/8) * 8
!
          do lt2 = 1 + lst/4, WK_l_tst%nlo_rtm(ip) / 4
            do lt = lt2*4-3, lt2*4
              lp_rtm = WK_l_tst%lst_rtm(ip) + lt
              ln_rtm = sph_rtm%nidx_rtm(2) - lp_rtm + 1
              call legendre_fwd_trans_1lat_test                         &
     &           (lp_rtm, ln_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,  &
     &            iflag_matmul, ncomp, nvector, nscalar, sph_params,    &
     &            sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,            &
     &            WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),     &
     &            WK_l_tst%Fmat(ip), WK_l_tst%Pmat(ip),                 &
     &            WK_l_tst%Smat(ip))
            end do
          end do
!
          lst = 1 + int(WK_l_tst%nlo_rtm(ip)/4) * 4
          do lt2 = 1 + lst/2, WK_l_tst%nlo_rtm(ip) / 2
            do lt = lt2*2-1, lt2*2
              lp_rtm = WK_l_tst%lst_rtm(ip) + lt
              ln_rtm = sph_rtm%nidx_rtm(2) - lp_rtm + 1
              call legendre_fwd_trans_1lat_test                         &
     &           (lp_rtm, ln_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,  &
     &            iflag_matmul, ncomp, nvector, nscalar, sph_params,    &
     &            sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,            &
     &            WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),     &
     &            WK_l_tst%Fmat(ip), WK_l_tst%Pmat(ip),                 &
     &            WK_l_tst%Smat(ip))
            end do
          end do
!
          lst = 1 + int(WK_l_tst%nlo_rtm(ip)/2) * 2
          do lt = lst, WK_l_tst%nlo_rtm(ip)
            lp_rtm = WK_l_tst%lst_rtm(ip) + lt
            ln_rtm = sph_rtm%nidx_rtm(2) - lp_rtm + 1
            call legendre_fwd_trans_1lat_test                           &
     &         (lp_rtm, ln_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,    &
     &         iflag_matmul, ncomp, nvector, nscalar, sph_params,       &
     &         sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,               &
     &         WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),        &
     &         WK_l_tst%Fmat(ip), WK_l_tst%Pmat(ip), WK_l_tst%Smat(ip))
          end do
!
!   Equator (if necessary)
          if(WK_l_tst%nle_rtm(ip) .gt. WK_l_tst%nlo_rtm(ip)) then
            lp_rtm = WK_l_tst%lst_rtm(ip) + WK_l_tst%nle_rtm(ip)
            call legendre_fwd_trans_eq_test                             &
     &        (lp_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,             &
     &         iflag_matmul, ncomp, nvector, nscalar, sph_params,       &
     &         sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,               &
     &         WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),        &
     &         WK_l_tst%Fmat(ip), WK_l_tst%Pmat(ip), WK_l_tst%Smat(ip))
          end if
        end do
!$omp end parallel do
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+16)
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+17)
!$omp parallel private(ip)
        do ip = 2, np_smp
!$omp workshare
          WK_l_tst%Smat(1)%pol_e(1:nkrs*WK_l_tst%n_jk_e(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%pol_e(1:nkrs*WK_l_tst%n_jk_e(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%pol_e(1:nkrs*WK_l_tst%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tst%Smat(1)%tor_e(1:nkrt*WK_l_tst%n_jk_e(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%tor_e(1:nkrt*WK_l_tst%n_jk_e(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%tor_e(1:nkrt*WK_l_tst%n_jk_e(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tst%Smat(1)%pol_o(1:nkrs*WK_l_tst%n_jk_o(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%pol_o(1:nkrs*WK_l_tst%n_jk_o(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%pol_o(1:nkrs*WK_l_tst%n_jk_o(mp_rlm))
!$omp end workshare nowait
!$omp workshare
          WK_l_tst%Smat(1)%tor_o(1:nkrt*WK_l_tst%n_jk_o(mp_rlm))        &
     &      =  WK_l_tst%Smat(1)%tor_o(1:nkrt*WK_l_tst%n_jk_o(mp_rlm))   &
     &       + WK_l_tst%Smat(ip)%tor_o(1:nkrt*WK_l_tst%n_jk_o(mp_rlm))
!$omp end workshare nowait
        end do
!$omp end parallel
!
          call cal_sp_rlm_sym_mat_rin                                   &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm,                       &
     &        sph_rlm%istep_rlm, sph_rlm%idx_gl_1d_rlm_j,               &
     &        sph_rlm%radius_1d_rlm_r, leg%g_sph_rlm, jst,              &
     &        WK_l_tst%n_jk_o(mp_rlm), WK_l_tst%n_jk_e(mp_rlm),         &
     &        WK_l_tst%Smat(1)%pol_e(1), WK_l_tst%Smat(1)%pol_o(1),     &
     &        WK_l_tst%Smat(1)%tor_e(1), WK_l_tst%Smat(1)%tor_o(1),     &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WS, WS)
      if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+17)
!
      end do
!
      end subroutine legendre_f_trans_vector_test
!
! -----------------------------------------------------------------------
!
      subroutine matvec_leg_trans(nkr, n_jk, V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kl(nkr)
      real(kind = kreal), intent(in) :: P_lj(n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          S_kj(kk,jj) = S_kj(kk,jj) + V_kl(kk) * P_lj(jj)
        end do
      end do
!
      end subroutine matvec_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmat8_leg_trans                                      &
     &         (iflag_matmul, nkr, n_jk, V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kl(nkr,8)
      real(kind = kreal), intent(in) :: P_lj(8,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj(1:nkr,1:n_jk) = S_kj(1:nkr,1:n_jk)                         &
     &                    + matmul(V_kl(1:nkr,1:8),P_lj(1:8,1:n_jk))
      else
        do jj = 1, n_jk
          do kk = 1, nkr
            S_kj(kk,jj) = S_kj(kk,jj)                                   &
     &             + V_kl(kk,1) * P_lj(1,jj) + V_kl(kk,2) * P_lj(2,jj)  &
     &             + V_kl(kk,3) * P_lj(3,jj) + V_kl(kk,4) * P_lj(4,jj)  &
     &             + V_kl(kk,5) * P_lj(5,jj) + V_kl(kk,6) * P_lj(6,jj)  &
     &             + V_kl(kk,7) * P_lj(7,jj) + V_kl(kk,8) * P_lj(8,jj)
          end do
        end do
      end if
!
      end subroutine matmat8_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_j1                            &
     &         (l_truncation, sph_rlm, mm, jst_rlm,                     &
     &          g_colat_rtm, n_jk_e, n_jk_o, Pmat)
!
      use schmidt_fix_m
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      integer(kind = kint), intent(in) :: l_truncation, mm
      real(kind= kreal), intent(in) :: g_colat_rtm
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(leg_omp_matrix), intent(inout) :: Pmat
!
      integer(kind = kint) :: j_rlm, jj
      integer(kind = kint) :: l
      real(kind = kreal) :: p_m(0:l_truncation), dp_m(0:l_truncation)
      real(kind = kreal) :: pmp1(0:l_truncation), pmn1(0:l_truncation)
      real(kind = kreal) :: df_m(0:l_truncation+2)
!
!
      call schmidt_legendres_m(l_truncation, mm, g_colat_rtm,           &
     &                         p_m, dp_m, pmn1, pmp1, df_m)
!
      do jj = 1, n_jk_e
        j_rlm = 2*jj - 1
        l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
        Pmat%Pse_jt(jj,1) =     p_m(l)
        Pmat%dPsedt_jt(jj,1) =  dp_m(l)
      end do
!
      do jj = 1, n_jk_o
        j_rlm = 2*jj
        l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
        Pmat%Pso_jt(jj,1) =     p_m(l)
        Pmat%dPsodt_jt(jj,1) =  dp_m(l)
      end do
!
      end subroutine set_each_sym_leg_omp_mat_j1
!
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_j8                            &
     &         (l_truncation, sph_rlm, mm, jst_rlm,                     &
     &          g_colat_rtm, n_jk_e, n_jk_o, Pmat)
!
      use schmidt_fix_m
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      integer(kind = kint), intent(in) :: jst_rlm
!
      integer(kind = kint), intent(in) :: l_truncation, mm
      real(kind= kreal), intent(in) :: g_colat_rtm(8)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(leg_omp_matrix), intent(inout) :: Pmat
!
      integer(kind = kint) :: j_rlm, jj
      integer(kind = kint) :: l, lt
      real(kind = kreal) :: p_m(0:l_truncation), dp_m(0:l_truncation)
      real(kind = kreal) :: pmp1(0:l_truncation), pmn1(0:l_truncation)
      real(kind = kreal) :: df_m(0:l_truncation+2)
!
!
      do lt = 1, 8
        call schmidt_legendres_m(l_truncation, mm, g_colat_rtm(lt),     &
     &                           p_m, dp_m, pmn1, pmp1, df_m)
!
        do jj = 1, n_jk_e
          j_rlm = 2*jj - 1
          l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
          Pmat%Pse_tj(lt,jj) =     p_m(l)
          Pmat%dPsedt_tj(lt,jj) =  dp_m(l)
        end do
!
        do jj = 1, n_jk_o
          j_rlm = 2*jj
          l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
          Pmat%Pso_tj(lt,jj) =     p_m(l)
          Pmat%dPsodt_tj(lt,jj) =  dp_m(l)
        end do
      end do
!
      end subroutine set_each_sym_leg_omp_mat_j8
!
! -----------------------------------------------------------------------
!
      subroutine legendre_fwd_trans_8lat_test                           &
     &         (lt2, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,               &
     &          iflag_matmul, ncomp, nvector, nscalar, sph_params,      &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          n_jk_e, n_jk_o, lst_rtm, Fmat, Pmat, Smat)
!
      use t_schmidt_poly_on_rtm
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: lt2
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o, lst_rtm
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_omp_matrix), intent(inout) :: Pmat
      type(spectr_matrix_omp), intent(inout) :: Smat
!
      integer(kind = kint) :: lt, lp_rtm, ln_rtm
      integer(kind = kint) :: kst_s, kst_t
!
      do lt = 1, 8
        lp_rtm = lst_rtm + (lt2-1)*8 + lt
        ln_rtm = sph_rtm%nidx_rtm(2) - lp_rtm + 1
        kst_s = (lt-1) * nkrs + 1
        kst_t = (lt-1) * nkrt + 1
!
        call set_vr_rtm_lt_sym_mat_rin                                  &
     &     (lp_rtm, ln_rtm, sph_rtm%nnod_rtm,                           &
     &      sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                        &
     &      leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),             &
     &      mp_rlm, mn_rlm, ncomp, nvector, nscalar,                    &
     &      comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(kst_s),             &
     &      Fmat%asmp_p(kst_t), Fmat%asmp_r(kst_s), Fmat%symp_p(kst_t))
      end do
!
!    Set Legendre polynomials
      lp_rtm = lst_rtm + (lt2-1)*8 + 1
      call set_each_sym_leg_omp_mat_j8                                  &
     &   (sph_params%l_truncation, sph_rlm, mm, jst,                    &
     &    leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o, Pmat)
!
      call matmat8_leg_trans(iflag_matmul, nkrs, n_jk_e,                &
     &    Fmat%symp_r(1), Pmat%Pse_tj,    Smat%pol_e(1))
      call matmat8_leg_trans(iflag_matmul, nkrt, n_jk_e,                &
     &    Fmat%asmp_p(1), Pmat%dPsedt_tj, Smat%tor_e(1))
!
!  odd l-m
      call matmat8_leg_trans(iflag_matmul, nkrs, n_jk_o,                &
     &    Fmat%asmp_r(1), Pmat%Pso_tj,    Smat%pol_o(1))
      call matmat8_leg_trans(iflag_matmul, nkrt, n_jk_o,                &
     &    Fmat%symp_p(1), Pmat%dPsodt_tj, Smat%tor_o(1))
!
      end subroutine legendre_fwd_trans_8lat_test
!
! -----------------------------------------------------------------------
!
      subroutine legendre_fwd_trans_1lat_test                           &
     &         (lp_rtm, ln_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,    &
     &          iflag_matmul, ncomp, nvector, nscalar, sph_params,      &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &           n_jk_e, n_jk_o, Fmat, Pmat, Smat)
!
      use t_schmidt_poly_on_rtm
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: lp_rtm, ln_rtm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_omp_matrix), intent(inout) :: Pmat
      type(spectr_matrix_omp), intent(inout) :: Smat
!
!
      call set_vr_rtm_lt_sym_mat_rin                                    &
     &   (lp_rtm, ln_rtm, sph_rtm%nnod_rtm,                             &
     &    sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                          &
     &    leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),               &
     &    mp_rlm, mn_rlm, ncomp, nvector, nscalar,                      &
     &    comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(1),                   &
     &    Fmat%asmp_p(1), Fmat%asmp_r(1), Fmat%symp_p(1))
!
!      Set Legendre polynomials
      call set_each_sym_leg_omp_mat_j1                                  &
     &   (sph_params%l_truncation, sph_rlm, mm, jst,                    &
     &    leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o, Pmat)
!
      call matvec_leg_trans(nkrs, n_jk_e,                               &
     &    Fmat%symp_r(1), Pmat%Pse_jt,    Smat%pol_e(1))
      call matvec_leg_trans(nkrt, n_jk_e,                               &
     &    Fmat%asmp_p(1), Pmat%dPsedt_jt, Smat%tor_e(1))
!
!  odd l-m
      call matvec_leg_trans(nkrs, n_jk_o,                               &
     &    Fmat%asmp_r(1), Pmat%Pso_jt,    Smat%pol_o(1))
      call matvec_leg_trans(nkrt, n_jk_o,                               &
     &    Fmat%symp_p(1), Pmat%dPsodt_jt, Smat%tor_o(1))
!
      end subroutine legendre_fwd_trans_1lat_test
!
! -----------------------------------------------------------------------
!
      subroutine legendre_fwd_trans_eq_test                             &
     &         (lp_rtm, jst, mm, mp_rlm, mn_rlm, nkrs, nkrt,            &
     &          iflag_matmul, ncomp, nvector, nscalar, sph_params,      &
     &          sph_rtm, sph_rlm, comm_rtm, leg, n_WR, WR,              &
     &          n_jk_e, n_jk_o, Fmat, Pmat, Smat)
!
      use t_schmidt_poly_on_rtm
      use set_vr_rtm_sym_mat_tsmp
      use cal_sp_rlm_sym_mat_tsmp
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: lp_rtm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: mm, jst
      integer(kind = kint), intent(in) :: nkrs, nkrt
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: n_jk_e, n_jk_o
      type(field_matrix_omp), intent(inout) :: Fmat
      type(leg_omp_matrix), intent(inout) :: Pmat
      type(spectr_matrix_omp), intent(inout) :: Smat
!
!
!   Equator (if necessary)
      call set_vr_rtm_eq_sym_mat_rin(lp_rtm, sph_rtm%nnod_rtm,          &
     &    sph_rtm%istep_rtm, sph_rlm%nidx_rlm,                          &
     &    leg%asin_t_rtm(lp_rtm), leg%weight_rtm(lp_rtm),               &
     &    mp_rlm, mn_rlm, ncomp, nvector, nscalar,                      &
     &    comm_rtm%irev_sr, n_WR, WR, Fmat%symp_r(1),                   &
     &    Fmat%asmp_p(1), Fmat%asmp_r(1), Fmat%symp_p(1))
!
!      Set Legendre polynomials
      call set_each_sym_leg_omp_mat_j1                                  &
     &   (sph_params%l_truncation, sph_rlm,                             &
     &    mm, jst, leg%g_colat_rtm(lp_rtm), n_jk_e, n_jk_o, Pmat)
!
      call matvec_leg_trans(nkrs, n_jk_e,                               &
     &    Fmat%symp_r(1), Pmat%Pse_jt,    Smat%pol_e(1))
      call matvec_leg_trans(nkrt, n_jk_e,                               &
     &    Fmat%asmp_p(1), Pmat%dPsedt_jt, Smat%tor_e(1))
!
!  odd l-m
      call matvec_leg_trans(nkrs, n_jk_o,                               &
     &    Fmat%asmp_r(1), Pmat%Pso_jt,    Smat%pol_o(1))
      call matvec_leg_trans(nkrt, n_jk_o,                               &
     &    Fmat%symp_p(1), Pmat%dPsodt_jt, Smat%tor_o(1))
!
      end subroutine legendre_fwd_trans_eq_test
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_testloop
