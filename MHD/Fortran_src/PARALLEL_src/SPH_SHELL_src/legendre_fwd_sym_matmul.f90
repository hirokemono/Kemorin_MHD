!>@file   legendre_fwd_sym_matmul.f90
!!@brief  module legendre_fwd_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine alloc_vec_fleg_mat_test(nvector)
!!      subroutine alloc_scl_fleg_mat_test(nscalar)
!!      subroutine dealloc_vec_fleg_mat_test
!!      subroutine dealloc_scl_fleg_mat_test
!!
!!      subroutine leg_f_trans_vec_sym_matmul(ncomp, nvector,           &
!!     &          irev_sr_rtm, n_WR, WR, sp_rlm_1)
!!      subroutine leg_f_trans_scl_sym_matmul(ncomp, nvector, nscalar,  &
!!     &          irev_sr_rtm, n_WR, WR, sp_rlm_1)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm(nidx_rtm(2),nidx_rtm(1)*ncomp,nidx_rtm(3))
!!      real(kind = kreal), allocatable :: vr_rtm(:,:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm(j_rlm,k_rtm,icomp)
!!        size: sp_rlm(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm(:,:)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_sym_matmul
!
      use m_precision
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      implicit none
!
      integer(kind = kint), private :: num_jl
      real(kind = kreal), allocatable, private :: Pvw_le(:,:)
      real(kind = kreal), allocatable, private :: dPvw_le(:,:)
      real(kind = kreal), allocatable, private :: Pgvw_le(:,:)
      real(kind = kreal), allocatable, private :: Pvw_lo(:,:)
      real(kind = kreal), allocatable, private :: dPvw_lo(:,:)
      real(kind = kreal), allocatable, private :: Pgvw_lo(:,:)
!
      real(kind = kreal), allocatable, private :: Pws_le(:,:)
      real(kind = kreal), allocatable, private :: Pws_lo(:,:)
!
      integer(kind = kint), private :: nvec_jk
      real(kind = kreal), allocatable, private :: pol_e(:,:)
      real(kind = kreal), allocatable, private :: pol_o(:,:)
      real(kind = kreal), allocatable, private :: dpoldt_e(:,:)
      real(kind = kreal), allocatable, private :: dpoldp_e(:,:)
      real(kind = kreal), allocatable, private :: dpoldt_o(:,:)
      real(kind = kreal), allocatable, private :: dpoldp_o(:,:)
      real(kind = kreal), allocatable, private :: dtordt_e(:,:)
      real(kind = kreal), allocatable, private :: dtordp_e(:,:)
      real(kind = kreal), allocatable, private :: dtordt_o(:,:)
      real(kind = kreal), allocatable, private :: dtordp_o(:,:)
!
      integer(kind = kint), private :: nvec_lk
      real(kind = kreal), allocatable, private :: symp_r(:,:)
      real(kind = kreal), allocatable, private :: asmp_t(:,:)
      real(kind = kreal), allocatable, private :: asmp_p(:,:)
      real(kind = kreal), allocatable, private :: symn_t(:,:)
      real(kind = kreal), allocatable, private :: symn_p(:,:)
      real(kind = kreal), allocatable, private :: asmp_r(:,:)
      real(kind = kreal), allocatable, private :: symp_t(:,:)
      real(kind = kreal), allocatable, private :: symp_p(:,:)
      real(kind = kreal), allocatable, private :: asmn_t(:,:)
      real(kind = kreal), allocatable, private :: asmn_p(:,:)
!
      integer(kind = kint), private :: nscl_jk
      real(kind = kreal), allocatable, private :: scl_e(:,:)
      real(kind = kreal), allocatable, private :: scl_o(:,:)
!
      integer(kind = kint), private :: nscl_lk
      real(kind = kreal), allocatable, private :: symp(:,:)
      real(kind = kreal), allocatable, private :: asmp(:,:)
!
      real(kind = kreal), private :: st_elapsed
      real(kind = kreal), private :: elaps(4)
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_vec_fleg_mat_test(nvector)
!
      integer(kind = kint), intent(in) ::nvector
!
!
      num_jl = maxdegree_rlm * ((nidx_rtm(2)+1)/2)
      allocate(Pvw_le(num_jl,np_smp))
      allocate(dPvw_le(num_jl,np_smp))
      allocate(Pgvw_le(num_jl,np_smp))
      allocate(Pvw_lo(num_jl,np_smp))
      allocate(dPvw_lo(num_jl,np_smp))
      allocate(Pgvw_lo(num_jl,np_smp))
!
!      nvec_jk = ((lmax_block_rtm+1)/2) * maxidx_rlm_smp(1)*nvector
      nvec_jk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
      allocate(pol_o(nvec_jk,np_smp))
      allocate(dpoldt_o(nvec_jk,np_smp))
      allocate(dpoldp_o(nvec_jk,np_smp))
      allocate(dtordt_o(nvec_jk,np_smp))
      allocate(dtordp_o(nvec_jk,np_smp))
!
!      nvec_lk = ((lmax_block_rtm+1)/2) * maxidx_rlm_smp(1)*nvector
      nvec_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symp_t(nvec_lk,np_smp))
      allocate(symp_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
!
      allocate(asmp_r(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
      allocate(asmn_t(nvec_lk,np_smp))
      allocate(asmn_p(nvec_lk,np_smp))
!
      end subroutine alloc_vec_fleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine alloc_scl_fleg_mat_test(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
!
      num_jl = maxdegree_rlm * ((nidx_rtm(2)+1)/2)
      allocate(Pws_le(num_jl,np_smp))
      allocate(Pws_lo(num_jl,np_smp))
!
      nscl_jk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
      allocate(scl_o(nscl_jk,np_smp))
!
      nscl_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
      allocate(asmp(nscl_lk,np_smp))
!
      end subroutine alloc_scl_fleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_vec_fleg_mat_test
!
!
      deallocate(Pvw_le, dPvw_le, Pgvw_le, Pvw_lo, dPvw_lo, Pgvw_lo)
!
      deallocate(pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
      deallocate(pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
!
      deallocate(symp_r, symp_t, symp_p, symn_t, symn_p)
      deallocate(asmp_r, asmp_t, asmp_p, asmn_t, asmn_p)
!
      end subroutine dealloc_vec_fleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_scl_fleg_mat_test
!
!
      deallocate(Pws_le, Pws_lo)
      deallocate(scl_e, scl_o)
      deallocate(symp, asmp)
!
      end subroutine dealloc_scl_fleg_mat_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_vec_sym_matmul(ncomp, nvector,             &
     &          irev_sr_rtm, n_WR, WR, sp_rlm_1)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: sp_rlm_1(ncomp*nnod_rlm)
!
      integer(kind = kint) :: ip, nb_nri, kr_nd, kk, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm
      integer(kind = kint) :: lp_rtm, ln_rtm, i_kl, i_lj, i_kj
      integer(kind = kint) :: nd, ll, mp_rlm, mn_rlm, je_rlm, jo_rlm, jj
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
      integer(kind = kint) :: nle_rtm, nlo_rtm
      real(kind = kreal) :: r2_1d_rlm_r
!
!
      call alloc_vec_fleg_mat_test(nvector)
!
      elaps(1:4) = 0
      nb_nri = nvector*nidx_rlm(1)
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kr_nd,ll,lp_rtm,ln_rtm,jj,je_rlm,jo_rlm,   &
!$omp&                    ie_rlm,io_rlm,mp_rlm,mn_rlm,i_kl,i_lj,i_kj,   &
!$omp&                    k_rlm,kk,nd,ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,  &
!$omp&                    ipp_recv,ipn_recv,inp_recv,inn_recv,          &
!$omp&                    st_elapsed)                                   &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_e(ip) = (nj_rlm(ip)+1)/2
          n_jk_o(ip) = nj_rlm(ip)/2
!
            st_elapsed = MPI_WTIME()
          do jj = 1, n_jk_o(ip)
            je_rlm = 2*jj + jst(ip) - 1
            jo_rlm = 2*jj + jst(ip)
! Symmetric component
            do lp_rtm = 1, nle_rtm
              i_lj = lp_rtm + (jj-1) * nle_rtm
!
              Pvw_le(i_lj,ip) = P_rtm(lp_rtm,je_rlm)                    &
     &               * g_sph_rlm(je_rlm,7)* weight_rtm(lp_rtm)
              Pgvw_le(i_lj,ip) = P_rtm(lp_rtm,je_rlm)                   &
     &               * dble(idx_gl_1d_rlm_j(je_rlm,3))                  &
     &                * asin_theta_1d_rtm(lp_rtm)                       &
     &                * g_sph_rlm(je_rlm,7)* weight_rtm(lp_rtm)
!
              dPvw_lo(i_lj,ip) = dPdt_rtm(lp_rtm,jo_rlm)                &
     &               * g_sph_rlm(jo_rlm,7)* weight_rtm(lp_rtm)
            end do
! anti-Symmetric component
            do lp_rtm = 1, nlo_rtm
              i_lj = lp_rtm + (jj-1) * nlo_rtm
!
              dPvw_le(i_lj,ip) = dPdt_rtm(lp_rtm,je_rlm)                &
     &               * g_sph_rlm(je_rlm,7)* weight_rtm(lp_rtm)
!
              Pvw_lo(i_lj,ip) = P_rtm(lp_rtm,jo_rlm)                    &
     &               * g_sph_rlm(jo_rlm,7)* weight_rtm(lp_rtm)
              Pgvw_lo(i_lj,ip) = P_rtm(lp_rtm,jo_rlm)                   &
     &               * dble(idx_gl_1d_rlm_j(jo_rlm,3))                  &
     &                * asin_theta_1d_rtm(lp_rtm)                       &
     &                * g_sph_rlm(jo_rlm,7)* weight_rtm(lp_rtm)
            end do
          end do
          do jj = n_jk_o(ip)+1, n_jk_e(ip)
            je_rlm = 2*jj + jst(ip) - 1
! Symmetric component
            do lp_rtm = 1, nle_rtm
              i_lj = lp_rtm + (jj-1) * nle_rtm
!
              Pvw_le(i_lj,ip) = P_rtm(lp_rtm,je_rlm)                    &
     &               * g_sph_rlm(je_rlm,7)* weight_rtm(lp_rtm)
              Pgvw_le(i_lj,ip) = P_rtm(lp_rtm,je_rlm)                   &
     &               * dble(idx_gl_1d_rlm_j(je_rlm,3))                  &
     &                * asin_theta_1d_rtm(lp_rtm)                       &
     &                * g_sph_rlm(je_rlm,7)* weight_rtm(lp_rtm)
            end do
! anti-Symmetric component
            do lp_rtm = 1, nlo_rtm
              i_lj = lp_rtm + (jj-1) * nlo_rtm
!
              dPvw_le(i_lj,ip) = dPdt_rtm(lp_rtm,je_rlm)                &
     &               * g_sph_rlm(je_rlm,7)* weight_rtm(lp_rtm)
            end do
          end do
            elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
!
            st_elapsed = MPI_WTIME()
          do lp_rtm = 1, nlo_rtm
            ln_rtm = nidx_rtm(2) - lp_rtm + 1
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mp_rlm-1) * istep_rtm(3)
              ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mp_rlm-1) * istep_rtm(3)
              in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mn_rlm-1) * istep_rtm(3)
              in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mn_rlm-1) * istep_rtm(3)
              ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
              ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
              inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
              inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
              i_kl = kk + (lp_rtm-1) * nkr(ip)
!
              symp_r(i_kl,ip) = WR(ipp_recv-2) + WR(ipn_recv-2)
              symp_t(i_kl,ip) = WR(ipp_recv-1) + WR(ipn_recv-1)
              symp_p(i_kl,ip) = WR(ipp_recv  ) + WR(ipn_recv  )
!
              asmp_r(i_kl,ip) = WR(ipp_recv-2) - WR(ipn_recv-2)
              asmp_t(i_kl,ip) = WR(ipp_recv-1) - WR(ipn_recv-1)
              asmp_p(i_kl,ip) = WR(ipp_recv  ) - WR(ipn_recv  )
!
              symn_t(i_kl,ip) = WR(inp_recv-1) + WR(inn_recv-1)
              symn_p(i_kl,ip) = WR(inp_recv  ) + WR(inn_recv  )
!
              asmn_t(i_kl,ip) = WR(inp_recv-1) - WR(inn_recv-1)
              asmn_p(i_kl,ip) = WR(inp_recv  ) - WR(inn_recv  )
            end do
          end do
!   Equator (if necessary)
          do ll = 2*(nlo_rtm)+1, nidx_rtm(2)
            lp_rtm = (ll+1) /2
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mp_rlm-1) * istep_rtm(3)
              in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mn_rlm-1) * istep_rtm(3)
              ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
              inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
              i_kl = kk + (lp_rtm-1) * nkr(ip)
!
              symp_r(i_kl,ip) = WR(ipp_recv-2)
              symp_t(i_kl,ip) = WR(ipp_recv-1)
              symp_p(i_kl,ip) = WR(ipp_recv  )
!
              symn_t(i_kl,ip) = WR(inp_recv-1)
              symn_p(i_kl,ip) = WR(inp_recv  )
            end do
          end do
            elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
            st_elapsed = MPI_WTIME()
!  even l-m
          call matmul_fwd_leg_trans(nkr(ip), n_jk_e(ip), nle_rtm,       &
     &        symp_r(1,ip), Pvw_le(1,ip), pol_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_e(ip), nlo_rtm,       &
     &        asmp_t(1,ip), dPvw_le(1,ip), dpoldt_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_e(ip), nle_rtm,       &
     &        symn_p(1,ip), Pgvw_le(1,ip), dpoldp_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_e(ip), nle_rtm,       &
     &        symn_t(1,ip), Pgvw_le(1,ip), dtordp_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_e(ip), nlo_rtm,       &
     &        asmp_p(1,ip), dPvw_le(1,ip), dtordt_e(1,ip))
!
!  odd l-m
          call matmul_fwd_leg_trans(nkr(ip), n_jk_o(ip), nlo_rtm,       &
     &        asmp_r(1,ip), Pvw_lo(1,ip), pol_o(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_o(ip), nle_rtm,       &
     &        symp_t(1,ip), dPvw_lo(1,ip), dpoldt_o(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_o(ip), nlo_rtm,       &
     &        asmn_p(1,ip), Pgvw_lo(1,ip), dpoldp_o(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_o(ip), nlo_rtm,       &
     &        asmn_t(1,ip), Pgvw_lo(1,ip), dtordp_o(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_o(ip), nle_rtm,       &
     &        symp_p(1,ip), dPvw_lo(1,ip), dtordt_o(1,ip))
            elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
            st_elapsed = MPI_WTIME()
          do kk = 1, nkr(ip)
            kr_nd = kk + kst(ip)
            k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
            nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
            do jj = 1, n_jk_o(ip)
              i_kj = kk + (jj-1) * nkr(ip)
              je_rlm = 2*jj + jst(ip) - 1
              jo_rlm = 2*jj + jst(ip)
              ie_rlm = 3*nd + ncomp * ((je_rlm-1) * istep_rlm(2)        &
     &                               + (k_rlm-1) *  istep_rlm(1))
              io_rlm = 3*nd + ncomp * ((jo_rlm-1) * istep_rlm(2)        &
     &                              + (k_rlm-1) * istep_rlm(1))
!
!  even l-m
              sp_rlm_1(ie_rlm-2) = sp_rlm_1(ie_rlm-2) + pol_e(i_kj,ip)
              sp_rlm_1(ie_rlm-1) = sp_rlm_1(ie_rlm-1)                       &
     &                         - dpoldp_e(i_kj,ip) + dpoldt_e(i_kj,ip)
              sp_rlm_1(ie_rlm  ) = sp_rlm_1(ie_rlm  )                       &
     &                         - dtordp_e(i_kj,ip) - dtordt_e(i_kj,ip)
!  odd l-m
              sp_rlm_1(io_rlm-2) = sp_rlm_1(io_rlm-2) + pol_o(i_kj,ip)
              sp_rlm_1(io_rlm-1) = sp_rlm_1(io_rlm-1)                       &
     &                         + dpoldt_o(i_kj,ip) - dpoldp_o(i_kj,ip)
              sp_rlm_1(io_rlm  ) = sp_rlm_1(io_rlm  )                       &
     &                         - dtordp_o(i_kj,ip) - dtordt_o(i_kj,ip)
            end do
            do jj = n_jk_o(ip)+1, n_jk_e(ip)
              i_kj = kk + (jj-1) * nkr(ip)
              je_rlm = 2*jj + jst(ip) - 1
              ie_rlm = 3*nd + ncomp * ((je_rlm-1) * istep_rlm(2)        &
     &                               + (k_rlm-1) *  istep_rlm(1))
!
              sp_rlm_1(ie_rlm-2) = sp_rlm_1(ie_rlm-2) + pol_e(i_kj,ip)
              sp_rlm_1(ie_rlm-1) = sp_rlm_1(ie_rlm-1)                       &
     &                         - dpoldp_e(i_kj,ip) + dpoldt_e(i_kj,ip)
              sp_rlm_1(ie_rlm  ) = sp_rlm_1(ie_rlm  )                       &
     &                         - dtordp_e(i_kj,ip) - dtordt_e(i_kj,ip)
            end do
          end do
            elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&        private(ip,kk,kr_nd,je_rlm,k_rlm,r2_1d_rlm_r,nd,ie_rlm)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do kk = 1, nkr(ip)
          kr_nd = kk + kst(ip)
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          r2_1d_rlm_r = radius_1d_rlm_r(k_rlm) * radius_1d_rlm_r(k_rlm)
          do je_rlm = 1, nidx_rlm(2)
!
            ie_rlm = 3*nd + ncomp * ((je_rlm-1) * istep_rlm(2)          &
     &                             + (k_rlm-1) * istep_rlm(1))
!
            sp_rlm_1(ie_rlm-2) = sp_rlm_1(ie_rlm-2)*r2_1d_rlm_r
            sp_rlm_1(ie_rlm-1) = sp_rlm_1(ie_rlm-1)*radius_1d_rlm_r(k_rlm)
            sp_rlm_1(ie_rlm  ) = sp_rlm_1(ie_rlm  )*radius_1d_rlm_r(k_rlm)
            end do
        end do
      end do
!$omp end parallel do
!
      call dealloc_vec_fleg_mat_test
      elapsed(46:49)                                                    &
     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scl_sym_matmul(ncomp, nvector, nscalar,    &
     &          irev_sr_rtm, n_WR, WR, sp_rlm_1)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: sp_rlm_1(ncomp*nnod_rlm)
!
      integer(kind = kint) :: ip, kr_nd, kk, k_rlm, nd, mp_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, je_rlm, jo_rlm, jj
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
      integer(kind = kint) :: ll, lp_rtm, ln_rtm, i_kl, i_lj, i_kj
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
      integer(kind = kint) :: nle_rtm, nlo_rtm
!
!
      call alloc_scl_fleg_mat_test(nscalar)
!
      elaps(1:4) = 0
      nle_rtm = (nidx_rtm(2) + 1)/2
      nlo_rtm = nidx_rtm(2) / 2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kr_nd,k_rlm,nd,kk,jj,je_rlm,jo_rlm,        &
!$omp&                    ll,lp_rtm,ln_rtm,i_kl,i_lj,i_kj,mp_rlm,       &
!$omp&                    ie_rlm,io_rlm,ip_rtpm,ip_rtnm,                &
!$omp&                    ipp_recv, ipn_recv,st_elapsed)                &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
          n_jk_e(ip) = (nj_rlm(ip)+1)/2
          n_jk_o(ip) = nj_rlm(ip)/2
!
!    even l-m
            st_elapsed = MPI_WTIME()
          do jj = 1, n_jk_e(ip)
            je_rlm = 2*jj + jst(ip) - 1
            do lp_rtm = 1, nle_rtm
              i_lj = lp_rtm + (jj-1) * nle_rtm
!
              Pws_le(i_lj,ip) = P_rtm(lp_rtm,je_rlm)                    &
     &                         * g_sph_rlm(je_rlm,6)*weight_rtm(lp_rtm)
            end do
          end do
!
!    odd l-m
          do jj = 1, n_jk_o(ip)
            jo_rlm = 2*jj + jst(ip)
            do lp_rtm = 1, nlo_rtm
              i_lj = lp_rtm + (jj-1) * nlo_rtm
!
              Pws_lo(i_lj,ip) = P_rtm(lp_rtm,jo_rlm)                    &
     &                       * g_sph_rlm(jo_rlm,6) * weight_rtm(lp_rtm)
            end do
          end do
          elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
            st_elapsed = MPI_WTIME()
          do lp_rtm = 1, nlo_rtm
            ln_rtm = nidx_rtm(2) - lp_rtm + 1
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mp_rlm-1) * istep_rtm(3)
              ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mp_rlm-1) * istep_rtm(3)
              ipp_recv = nd + 3*nvector                                 &
     &                      + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
              ipn_recv = nd + 3*nvector                                 &
     &                      + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
              i_kl = kk + (lp_rtm-1) * nkr(ip)
!
              symp(i_kl,ip) =  WR(ipp_recv) + WR(ipn_recv)
              asmp(i_kl,ip) =  WR(ipp_recv) - WR(ipn_recv)
            end do
          end do
!   Equator (if necessary)
          do ll = 2*(nlo_rtm)+1, nidx_rtm(2)
            lp_rtm = (ll+1) /2
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                    + (k_rlm-1) *  istep_rtm(1)                   &
     &                    + (mp_rlm-1) * istep_rtm(3)
              ipp_recv = nd + 3*nvector                                 &
     &                      + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
              i_kl = kk + (lp_rtm-1) * nkr(ip)
!
              symp(i_kl,ip) = WR(ipp_recv)
            end do
          end do
            elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
            st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), n_jk_e(ip), nle_rtm,       &
     &        symp(1,ip), Pws_le(1,ip), scl_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), n_jk_o(ip), nlo_rtm,       &
     &        asmp(1,ip), Pws_lo(1,ip), scl_o(1,ip))
            elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
            st_elapsed = MPI_WTIME()
          do kk = 1, nkr(ip)
            kr_nd = kk + kst(ip)
            k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
            nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
            do jj = 1, n_jk_o(ip)
              i_kj = kk + (jj-1) * nkr(ip)
!
              je_rlm = 2*jj + jst(ip) - 1
              jo_rlm = 2*jj + jst(ip)
              ie_rlm = nd + 3*nvector                                   &
     &                   + ncomp * ((je_rlm-1) * istep_rlm(2)           &
     &                             + (k_rlm-1) * istep_rlm(1))
              io_rlm = nd + 3*nvector                                   &
     &                   + ncomp * ((jo_rlm-1) * istep_rlm(2)           &
     &                            + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm_1(ie_rlm) = sp_rlm_1(ie_rlm) + scl_e(i_kj,ip)
              sp_rlm_1(io_rlm) = sp_rlm_1(io_rlm) + scl_o(i_kj,ip)
            end do
!
            do jj = n_jk_o(ip)+1, n_jk_e(ip)
              i_kj = kk + (jj-1) * nkr(ip)
!
              je_rlm = 2*jj + jst(ip) - 1
              ie_rlm = nd + 3*nvector                                   &
     &                   + ncomp * ((je_rlm-1) * istep_rlm(2)           &
     &                             + (k_rlm-1) * istep_rlm(1))
              sp_rlm_1(ie_rlm) = sp_rlm_1(ie_rlm) + scl_e(i_kj,ip)
            end do
          end do
            elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
      elapsed(46:49)                                                    &
     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      call dealloc_scl_fleg_mat_test
!
      end subroutine leg_f_trans_scl_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine matmat_fwd_leg_trans(nkr, n_jk, nl_rtm,                &
     &          V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          s = 0.0d0
          do ll = 1, nl_rtm
            s = s + P_lj(ll,jj) * V_kl(kk,ll)
          end do
          S_kj(kk,jj) = s
        end do
      end do
!
      end subroutine matmat_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmul_fwd_leg_trans(nkr, n_jk, nl_rtm,                &
     &          V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
!
      S_kj = matmul(V_kl,P_lj)
!
      end subroutine matmul_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine add_matmul_fwd_leg_trans(nkr, n_jk, nl_rtm,            &
     &          V_kl, P_lj, coef, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
!
      S_kj = coef * S_kj + matmul(V_kl,P_lj)
!
      end subroutine add_matmul_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine dgemm_fwd_leg_trans(nkr, n_jk, nl_rtm,                &
     &          V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
!
      call DGEMM('N', 'N', nkr, n_jk, nl_rtm, one,                     &
     &    V_kl, nkr, P_lj, nl_rtm, zero, S_kj, nkr)
!
      end subroutine dgemm_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      end module legendre_fwd_sym_matmul
