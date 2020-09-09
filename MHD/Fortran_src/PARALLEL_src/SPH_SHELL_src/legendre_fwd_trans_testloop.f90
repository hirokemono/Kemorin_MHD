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
      integer(kind = kint) :: mp_rlm
      integer(kind = kint) :: nkrs, nkrt, lt, lst_rtm, kst_s, kst_t
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
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+16)
!$omp parallel do private(ip,lst_rtm,lt,kst_s,kst_t)
        do ip = 1, np_smp
          lst_rtm = WK_l_tst%lst_rtm(ip)
!
          call set_vr_rtm_sym_mat_rin                                   &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, leg%asin_t_rtm, leg%weight_rtm,         &
     &        mp_rlm, WK_l_tst%lst_rtm(ip),                             &
     &        WK_l_tst%nle_rtm(ip), WK_l_tst%nlo_rtm(ip),               &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WR, WR,      &
     &        WK_l_tst%Fmat(ip)%symp_r(1), WK_l_tst%Fmat(ip)%asmp_p(1), &
     &        WK_l_tst%Fmat(ip)%asmp_r(1), WK_l_tst%Fmat(ip)%symp_p(1))
!
!      Set Legendre polynomials
          call set_each_sym_leg_omp_mat_tj                              &
     &       (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2),                 &
     &        jst, leg%P_rtm, leg%dPdt_rtm,                             &
     &        WK_l_tst%lst_rtm(ip), WK_l_tst%nle_rtm(ip),               &
     &        WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),         &
     &        WK_l_tst%Pmat(mp_rlm,ip))
!
          WK_l_tst%Smat(ip)%pol_e(1:WK_l_tst%n_pol_e) = 0.0d0
          WK_l_tst%Smat(ip)%tor_e(1:WK_l_tst%n_tor_e) = 0.0d0
          WK_l_tst%Smat(ip)%pol_o(1:WK_l_tst%n_pol_e) = 0.0d0
          WK_l_tst%Smat(ip)%tor_o(1:WK_l_tst%n_tor_e) = 0.0d0
!
          do lt = 1, WK_l_tst%nlo_rtm(ip)
            kst_s = (lt-1) * nkrs + 1
            kst_t = (lt-1) * nkrt + 1
!
            call matvec_leg_trans(                       &
     &        lt, nkrs, WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%symp_r(kst_s),                          &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pse_tj,                          &
     &        WK_l_tst%Smat(ip)%pol_e(1))
            call matvec_leg_trans(                       &
     &        lt, nkrt, WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%asmp_p(kst_t),                          &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsedt_tj,                       &
     &        WK_l_tst%Smat(ip)%tor_e(1))
!
!  odd l-m
            call matvec_leg_trans(                       &
     &        lt, nkrs, WK_l_tst%n_jk_o(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%asmp_r(kst_s),                              &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pso_tj,                          &
     &        WK_l_tst%Smat(ip)%pol_o(1))
            call matvec_leg_trans(                       &
     &        lt, nkrt, WK_l_tst%n_jk_o(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%symp_p(kst_t),                          &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsodt_tj,                       &
     &        WK_l_tst%Smat(ip)%tor_o(1))
          end do
!
!   Equator (if necessary)
          do lt = WK_l_tst%nlo_rtm(ip)+1, WK_l_tst%nle_rtm(ip)
            kst_s = (lt-1) * nkrs + 1
            kst_t = (lt-1) * nkrt + 1
!
            call matvec_leg_trans(                       &
     &        lt, nkrs, WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%symp_r(kst_s),                          &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pse_tj,                          &
     &        WK_l_tst%Smat(ip)%pol_e(1))
            call matvec_leg_trans(                       &
     &        lt, nkrt, WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%asmp_p(kst_t),                          &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsedt_tj,                       &
     &        WK_l_tst%Smat(ip)%tor_e(1))
!
!  odd l-m
            call matvec_leg_trans(                       &
     &        lt, nkrs, WK_l_tst%n_jk_o(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%asmp_r(kst_s),                              &
     &        WK_l_tst%Pmat(mp_rlm,ip)%Pso_tj,                          &
     &        WK_l_tst%Smat(ip)%pol_o(1))
            call matvec_leg_trans(                       &
     &        lt, nkrt, WK_l_tst%n_jk_o(mp_rlm), WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%Fmat(ip)%symp_p(kst_t),                          &
     &        WK_l_tst%Pmat(mp_rlm,ip)%dPsodt_tj,                       &
     &        WK_l_tst%Smat(ip)%tor_o(1))
          end do
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
      subroutine matmul_fwd_leg_trans_lj                                &
     &         (nkr, n_jk, nl_rtm, V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: lt
!
!
      if(n_jk*nkr .eq. 0) return

      do lt = 1, nl_rtm
        call matvec_leg_trans(lt, nkr, n_jk, nl_rtm,                    &
     &      V_kl(1,lt), P_lj, S_kj)
      end do
!
      end subroutine matmul_fwd_leg_trans_lj
!
! ----------------------------------------------------------------------
!
      subroutine matvec_leg_trans                                       &
     &         (lt, nkr, n_jk, nl_rtm, V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: lt
      integer(kind = kint), intent(in) :: nl_rtm, n_jk, nkr
      real(kind = kreal), intent(in) :: V_kl(nkr)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          S_kj(kk,jj) = S_kj(kk,jj) + V_kl(kk) * P_lj(lt,jj)
        end do
      end do
!
      end subroutine matvec_leg_trans
!
! ----------------------------------------------------------------------
!
      end module legendre_fwd_trans_testloop
