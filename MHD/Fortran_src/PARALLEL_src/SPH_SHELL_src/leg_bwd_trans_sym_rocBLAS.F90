!>@file   leg_bwd_trans_sym_rocBLAS.F90
!!@brief  module leg_bwd_trans_sym_rocBLAS
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine leg_backward_trans_rocBLAS(ncomp, nvector,           &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,       &
!!     &          asin_theta_1d_rtm, g_sph_rlm,                         &
!!     &          n_WR, n_WS, WR, WS, WK_l_bsm, rocBLAS_WK)
!!        integer(kind = kint), intent(in) :: iflag_matmul
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!!        integer(kind = kint), intent(in) :: n_WR, n_WS
!!        real (kind=kreal), intent(inout):: WR(n_WR)
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!        type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module leg_bwd_trans_sym_rocBLAS
!
      use m_precision
      use m_constants
!
      use m_work_time
      use calypso_mpi
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_leg_trans_sym_matmul_big
      use m_elapsed_labels_SPH_TRNS
!
      use t_rocBLAS_legendre_trans
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
      subroutine leg_backward_trans_rocBLAS                             &
     &         (iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_bsm, rocBLAS_WK)
!
      use set_s_rlm_l_matmul_big_smp
      use set_v_rtm_lg_matmul_big_smp
      use rocBLAS_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
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
      type(leg_trns_bsym_mul_work), intent(inout) :: WK_l_bsm
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(kind = kint) :: nl_rtm, mp_rlm
      integer(kind = kint) :: nkr
      integer(kind = kint) :: nkrs,  nkrt
      integer(kind = kint) :: jst, jst_h
      integer(kind = kint) :: n_jk_e, n_jk_o
!
!
      if(ncomp .le. 0) return
!$omp parallel workshare
      WS(1:ncomp*comm_rtm%ntot_item_sr) = 0.0d0
!$omp end parallel workshare
!
      nl_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nkr =  sph_rlm%nidx_rlm(1)
      nkrs = ncomp*nkr
      nkrt = 2*nvector*nkr
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        jst = idx_trns%lstack_rlm(mp_rlm-1)
        jst_h = idx_trns%lstack_even_rlm(mp_rlm) + 1
        n_jk_e = idx_trns%lstack_even_rlm(mp_rlm)                       &
     &          - idx_trns%lstack_rlm(mp_rlm-1)
        n_jk_o = idx_trns%lstack_rlm(mp_rlm)                            &
     &          - idx_trns%lstack_even_rlm(mp_rlm)
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+9)
        call set_sv_rlm_sym_matmul_big_smp                              &
     &     (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,      &
     &      sph_rlm%idx_gl_1d_rlm_j, sph_rlm%ar_1d_rlm, leg%g_sph_rlm,  &
     &      nkr, jst, n_jk_e, n_jk_o, ncomp, nvector,                   &
     &      comm_rlm%irev_sr, n_WR, WR,                                 &
     &      WK_l_bsm%pol_e(1,1), WK_l_bsm%tor_e(1,1),                   &
     &      WK_l_bsm%pol_o(1,1), WK_l_bsm%tor_o(1,1))
        call set_sc_rlm_sym_matmul_big_smp                              &
     &     (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,      &
     &      nkr, jst, n_jk_e, n_jk_o, ncomp, nvector, nscalar,          &
     &      comm_rlm%irev_sr, n_WR, WR,                                 &
     &      WK_l_bsm%pol_e(1,1), WK_l_bsm%pol_o(1,1))
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+9)
!
!   even l-m
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+11)
        call ROCm_matmul_bwd_leg_trans                                  &
     &     (iflag_matmul, nl_rtm, nkrs, n_jk_e,                         &
     &      WK_l_bsm%Ps_tj(1,jst+1), WK_l_bsm%pol_e(1,1),               &
     &      WK_l_bsm%symp_r(1,1), rocBLAS_WK)
        call ROCm_matmul_bwd_leg_trans                                  &
     &     (iflag_matmul, nl_rtm, nkrt, n_jk_e,                         &
     &      WK_l_bsm%dPsdt_tj(1,jst+1), WK_l_bsm%tor_e(1,1),            &
     &      WK_l_bsm%asmp_p(1,1), rocBLAS_WK)
!   odd l-m
        call ROCm_matmul_bwd_leg_trans                                  &
     &     (iflag_matmul, nl_rtm, nkrs, n_jk_o,                         &
     &      WK_l_bsm%Ps_tj(1,jst_h), WK_l_bsm%pol_o(1,1),               &
     &      WK_l_bsm%asmp_r(1,1), rocBLAS_WK)
        call ROCm_matmul_bwd_leg_trans                                  &
     &     (iflag_matmul, nl_rtm, nkrt, n_jk_o,                         &
     &      WK_l_bsm%dPsdt_tj(1,jst_h), WK_l_bsm%tor_o(1,1),            &
     &      WK_l_bsm%symp_p(1,1), rocBLAS_WK)
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+11)
!
        if(iflag_SDT_time) call start_elapsed_time(ist_elapsed_SDT+12)
        call cal_v_rtm_sym_matmul_big_smp                               &
     &     (sph_rtm%nnod_rtm, sph_rtm%nidx_rtm, sph_rtm%istep_rtm,      &
     &      leg%asin_t_rtm, nkr,  mp_rlm, idx_trns%mn_rlm(mp_rlm),      &
     &      nl_rtm, WK_l_bsm%symp_r(1,1), WK_l_bsm%asmp_p(1,1),         &
     &      WK_l_bsm%asmp_r(1,1), WK_l_bsm%symp_p(1,1),                 &
     &      ncomp, nvector, comm_rtm%irev_sr, n_WS, WS)
        call cal_s_rtm_sym_matmul_big_smp(sph_rtm%nnod_rtm,             &
     &      sph_rtm%nidx_rtm, sph_rtm%istep_rtm, nkr, mp_rlm, nl_rtm,   &
     &      WK_l_bsm%symp_r(1,1), WK_l_bsm%asmp_r(1,1),                 &
     &      ncomp, nvector, nscalar, comm_rtm%irev_sr, n_WS, WS)
        if(iflag_SDT_time) call end_elapsed_time(ist_elapsed_SDT+12)
      end do
!
      end subroutine leg_backward_trans_rocBLAS
!
! -----------------------------------------------------------------------
!
      end module leg_bwd_trans_sym_rocBLAS
