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
      use t_spheric_parameter
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
      subroutine legendre_b_trans_vector_test                           &
     &         (iflag_matmul, ncomp, nvector, nscalar, sph_params,      &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm,  idx_trns, leg,   &
     &          n_WR, n_WS, WR, WS, WK_l_tst)
!
      use t_schmidt_poly_on_rtm
      use set_sp_rlm_sym_mat_tsmp
      use cal_vr_rtm_sym_mat_tsmp
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm, mm
      integer(kind = kint) :: nkrs, nkrt, lst_rtm, l_rtm
      integer(kind = kint) :: ip, jst, jed, jnum
      integer(kind = kint) :: lt, kst_s, kst_t
!
!
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nkrs = (3*nvector + nscalar) * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvector * sph_rlm%nidx_rlm(1)
!
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(mp_rlm,2))
        jst = idx_trns%lstack_rlm(mp_rlm-1)
        jed = idx_trns%lstack_rlm(mp_rlm)
        jnum = idx_trns%lstack_rlm(mp_rlm) - jst
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+12)
          call set_sp_rlm_sym_mat_rin                                   &
     &       (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,    &
     &        sph_rlm%idx_gl_1d_rlm_j, sph_rlm%a_r_1d_rlm_r,            &
     &        leg%g_sph_rlm, &
     &        jst, WK_l_tst%n_jk_e(mp_rlm),  WK_l_tst%n_jk_o(mp_rlm),   &
     &        ncomp, nvector, nscalar, comm_rlm%irev_sr, n_WR, WR,      &
     &        WK_l_tst%Smat(1)%pol_e(1), WK_l_tst%Smat(1)%tor_e(1),     &
     &        WK_l_tst%Smat(1)%pol_o(1), WK_l_tst%Smat(1)%tor_o(1) )
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+12)
!
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+14)
!$omp parallel do private(ip,lst_rtm,l_rtm,lt,kst_s,kst_t)
        do ip = 1, np_smp
          lst_rtm = WK_l_tst%lst_rtm(ip)
!
!   even l-m
          do lt = 1, WK_l_tst%nle_rtm(ip)
            kst_s = (lt-1) * nkrs + 1
            kst_t = (lt-1) * nkrt + 1
!
!      Set Legendre polynomials
            l_rtm = lst_rtm + lt
            call set_each_sym_leg_omp_mat_j                             &
     &         (sph_params%l_truncation, sph_rlm,                       &
     &          mm, jst, leg%g_colat_rtm(l_rtm),                        &
     &          WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),       &
     &          WK_l_tst%Pmat(mp_rlm,ip))
!
            call matvec_bwd_leg_trans_Pj(iflag_matmul,                  &
     &          nkrs, WK_l_tst%n_jk_e(mp_rlm),                          &
     &          WK_l_tst%Smat(1)%pol_e(1),                              &
     &          WK_l_tst%Pmat(mp_rlm,ip)%Pse_jt(1,1),                   &
     &          WK_l_tst%Fmat(ip)%symp_r(kst_s))
            call matvec_bwd_leg_trans_Pj(iflag_matmul,                  &
     &          nkrt, WK_l_tst%n_jk_e(mp_rlm),                          &
     &          WK_l_tst%Smat(1)%tor_e(1),                              &
     &          WK_l_tst%Pmat(mp_rlm,ip)%dPsedt_jt(1,1),                &
     &          WK_l_tst%Fmat(ip)%asmp_p(kst_t))
!   odd l-m
            call matvec_bwd_leg_trans_Pj(iflag_matmul,                  &
     &          nkrs, WK_l_tst%n_jk_o(mp_rlm),                          &
     &          WK_l_tst%Smat(1)%pol_o(1),                              &
     &          WK_l_tst%Pmat(mp_rlm,ip)%Pso_jt(1,1),                   &
     &          WK_l_tst%Fmat(ip)%asmp_r(kst_s))
            call matvec_bwd_leg_trans_Pj(iflag_matmul,                  &
     &          nkrt, WK_l_tst%n_jk_o(mp_rlm),                          &
     &          WK_l_tst%Smat(1)%tor_o(1),                              &
     &          WK_l_tst%Pmat(mp_rlm,ip)%dPsodt_jt(1,1),                &
     &          WK_l_tst%Fmat(ip)%symp_p(kst_t))
          end do
!
          lst_rtm = WK_l_tst%lst_rtm(ip)
          call cal_vr_rtm_sym_mat_rin                                   &
     &       (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,    &
     &        sph_rlm%nidx_rlm, leg%asin_t_rtm,                         &
     &        mp_rlm, WK_l_tst%lst_rtm(ip),                             &
     &        WK_l_tst%nle_rtm(ip), WK_l_tst%nlo_rtm(ip),               &
     &        WK_l_tst%Fmat(ip)%symp_r(1), WK_l_tst%Fmat(ip)%asmp_p(1), &
     &        WK_l_tst%Fmat(ip)%asmp_r(1), WK_l_tst%Fmat(ip)%symp_p(1), &
     &        ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
        end do
!$omp end parallel do
          if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+14)
!
      end do
!
      end subroutine legendre_b_trans_vector_test
!
! -----------------------------------------------------------------------
!
      subroutine set_each_sym_leg_omp_mat_j                             &
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
!$omp parallel do private(jj,j_rlm,l)
      do jj = 1, n_jk_e
        j_rlm = 2*jj - 1
        l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
        Pmat%Pse_jt(jj,1) =     p_m(l)
        Pmat%dPsedt_jt(jj,1) =  dp_m(l)
      end do
!$omp end parallel do
!
!$omp parallel do private(jj,j_rlm,l)
      do jj = 1, n_jk_o
        j_rlm = 2*jj
        l =  sph_rlm%idx_gl_1d_rlm_j(jst_rlm+j_rlm,2)
        Pmat%Pso_jt(jj,1) =     p_m(l)
        Pmat%dPsodt_jt(jj,1) =  dp_m(l)
      end do
!$omp end parallel do
!
      end subroutine set_each_sym_leg_omp_mat_j
!
! -----------------------------------------------------------------------
!
      subroutine matvec_bwd_leg_trans_Pj(iflag_matmul,                  &
     &          nkr, n_jk, S_kj, P_j, V_k)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: S_kj(nkr,n_jk)
      real(kind = kreal), intent(in) :: P_j(n_jk)
!
      real(kind = kreal), intent(inout) :: V_k(nkr)
!
      integer(kind = kint) :: jj, kk
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_k(1:nkr) = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_k(1:nkr) = matmul(S_kj(1:nkr,1:n_jk),P_j(1:n_jk))
      else
        V_k(1:nkr) = 0.0d0
        do jj = 1, n_jk
          do kk = 1, nkr
            V_k(kk) = V_k(kk) + S_kj(kk,jj) * P_j(jj)
          end do
        end do
      end if
!
      end subroutine matvec_bwd_leg_trans_Pj
!
! ----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
