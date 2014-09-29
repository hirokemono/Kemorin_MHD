!>@file   legendre_fwd_trans_matmul.f90
!!@brief  module legendre_fwd_trans_matmul
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
!!      subroutine leg_f_trans_vector_matmul(ncomp, nvector,            &
!!     &          irev_sr_rtm, n_WR, WR, sp_rlm_1)
!!      subroutine leg_f_trans_scalar_matmul(ncomp, nvector, nscalar,   &
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
      module legendre_fwd_trans_matmul
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
!
      real(kind = kreal), allocatable, private :: Pws_le(:,:)
!
      integer(kind = kint), private :: nvec_jk
      real(kind = kreal), allocatable, private :: pol_e(:,:)
      real(kind = kreal), allocatable, private :: dpl_e(:,:)
      real(kind = kreal), allocatable, private :: tor_e(:,:)
      real(kind = kreal), allocatable, private :: dpl_o(:,:)
      real(kind = kreal), allocatable, private :: tor_o(:,:)
!
      integer(kind = kint), private :: nvec_lk
      real(kind = kreal), allocatable, private :: symp_r(:,:)
      real(kind = kreal), allocatable, private :: asmp_t(:,:)
      real(kind = kreal), allocatable, private :: asmp_p(:,:)
      real(kind = kreal), allocatable, private :: symn_t(:,:)
      real(kind = kreal), allocatable, private :: symn_p(:,:)
!
      integer(kind = kint), private :: nscl_jk
      real(kind = kreal), allocatable, private :: scl_e(:,:)
!
      integer(kind = kint), private :: nscl_lk
      real(kind = kreal), allocatable, private :: symp(:,:)
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
      num_jl = maxdegree_rlm * nidx_rtm(2)
      allocate(Pvw_le(num_jl,np_smp))
      allocate(dPvw_le(num_jl,np_smp))
      allocate(Pgvw_le(num_jl,np_smp))
!
      nvec_jk = nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpl_e(nvec_jk,np_smp))
      allocate(tor_e(nvec_jk,np_smp))
      allocate(dpl_o(nvec_jk,np_smp))
      allocate(tor_o(nvec_jk,np_smp))
!
      nvec_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
!
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
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
      num_jl = maxdegree_rlm * nidx_rtm(2)
      allocate(Pws_le(num_jl,np_smp))
!
      nscl_jk = nidx_rtm(2) * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
!
      nscl_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
!
      end subroutine alloc_scl_fleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_vec_fleg_mat_test
!
!
      deallocate(Pvw_le, dPvw_le, Pgvw_le)
!
      deallocate(pol_e, dpl_e, tor_e, dpl_o, tor_o)
!
      deallocate(symp_r, symn_t, symn_p)
      deallocate(asmp_t, asmp_p)
!
      end subroutine dealloc_vec_fleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_scl_fleg_mat_test
!
!
      deallocate(Pws_le)
      deallocate(scl_e)
      deallocate(symp)
!
      end subroutine dealloc_scl_fleg_mat_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_vector_matmul(ncomp, nvector,              &
     &          irev_sr_rtm, n_WR, WR, sp_rlm_1)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: sp_rlm_1(ncomp*nnod_rlm)
!
      integer(kind = kint) :: ip, nb_nri, kr_nd, kk, k_rlm, i_rlm
      integer(kind = kint) :: l_rtm, i_kl, i_lj, i_kj, ip_rtm, in_rtm
      integer(kind = kint) :: nd, ll, mp_rlm, mn_rlm, j_rlm, jj
      integer(kind = kint) :: ip_recv, in_recv
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      real(kind = kreal) :: r2_1d_rlm_r
!
!
      elaps(1:4) = 0
      call alloc_vec_fleg_mat_test(nvector)
!
      nb_nri = nvector*nidx_rlm(1)
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,kr_nd,ll,l_rtm,jj,j_rlm,nd,ip_rtm,in_rtm,  &
!$omp&                    k_rlm,kk,mp_rlm,mn_rlm,i_kl,i_lj,i_kj,i_rlm,  &
!$omp&                    ip_recv,in_recv,st_elapsed)                   &
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
!    even l-m
          st_elapsed = MPI_WTIME()
          do jj = 1, nj_rlm(ip)
            j_rlm = jj + jst(ip)
            do l_rtm = 1, nidx_rtm(2)
              i_lj = l_rtm + (jj-1) * nidx_rtm(2)
!
              Pvw_le(i_lj,ip) = P_rtm(l_rtm,j_rlm)                      &
     &               * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
              dPvw_le(i_lj,ip) = dPdt_rtm(l_rtm,j_rlm)                  &
     &               * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
              Pgvw_le(i_lj,ip) = P_rtm(l_rtm,j_rlm)                     &
     &               * dble(idx_gl_1d_rlm_j(j_rlm,3))                   &
     &                * asin_theta_1d_rtm(l_rtm)                        &
     &                * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
            end do
          end do
            elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          do l_rtm = 1, nidx_rtm(2)
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
              ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                    &
     &                   + (k_rlm-1) *  istep_rtm(1)                    &
     &                   + (mp_rlm-1) * istep_rtm(3)
              in_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                    &
     &                   + (k_rlm-1) *  istep_rtm(1)                    &
     &                   + (mn_rlm-1) * istep_rtm(3)
              ip_recv = 3*nd + (irev_sr_rtm(ip_rtm) - 1) * ncomp
              in_recv = 3*nd + (irev_sr_rtm(in_rtm) - 1) * ncomp
              i_kl = kk + (l_rtm-1) * nkr(ip)
!
              symp_r(i_kl,ip) =  WR(ip_recv-2)
!
              asmp_t(i_kl,ip) =  WR(ip_recv-1)
              asmp_p(i_kl,ip) =  WR(ip_recv  )
!
              symn_t(i_kl,ip) =  WR(in_recv-1)
              symn_p(i_kl,ip) =  WR(in_recv  )
            end do
          end do
            elaps(2) = MPI_WTIME() - st_elapsed + elaps(2) 
!
            st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symp_r(1,ip), Pvw_le(1,ip), pol_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        asmp_t(1,ip), dPvw_le(1,ip), dpl_o(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symn_p(1,ip), Pgvw_le(1,ip), dpl_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symn_t(1,ip), Pgvw_le(1,ip), tor_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        asmp_p(1,ip), dPvw_le(1,ip), tor_o(1,ip))
            elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          do jj = 1, nj_rlm(ip)
            j_rlm = jj + jst(ip)
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
              i_kj = kk + (jj-1) * nkr(ip)
!
              sp_rlm_1(i_rlm-2) = sp_rlm_1(i_rlm-2) + pol_e(i_kj,ip)
              sp_rlm_1(i_rlm-1) = sp_rlm_1(i_rlm-1)                         &
     &                - dpl_e(i_kj,ip) + dpl_o(i_kj,ip)
              sp_rlm_1(i_rlm  ) = sp_rlm_1(i_rlm  )                         &
     &                - tor_e(i_kj,ip) - tor_o(i_kj,ip)
            end do
          end do
            elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,kk,kr_nd,nd,j_rlm,k_rlm,r2_1d_rlm_r,i_rlm)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do kk = 1, nkr(ip)
          kr_nd = kk + kst(ip)
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          r2_1d_rlm_r = radius_1d_rlm_r(k_rlm) * radius_1d_rlm_r(k_rlm)
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)            &
     &                            + (k_rlm-1) * istep_rlm(1))
!
            sp_rlm_1(i_rlm-2) = sp_rlm_1(i_rlm-2) * r2_1d_rlm_r
            sp_rlm_1(i_rlm-1) = sp_rlm_1(i_rlm-1) * radius_1d_rlm_r(k_rlm)
            sp_rlm_1(i_rlm  ) = sp_rlm_1(i_rlm  ) * radius_1d_rlm_r(k_rlm)
            end do
        end do
      end do
!$omp end parallel do
!
      call dealloc_vec_fleg_mat_test
      elapsed(46:49)                                                    &
     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      end subroutine leg_f_trans_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_f_trans_scalar_matmul(ncomp, nvector, nscalar,     &
     &          irev_sr_rtm, n_WR, WR, sp_rlm_1)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: sp_rlm_1(ncomp*nnod_rlm)
!
      integer(kind = kint) :: ip, kr_nd, kk, ip_rtm, nd, k_rlm
      integer(kind = kint) :: mp_rlm, j_rlm, jj, i_rlm, i_recv
      integer(kind = kint) :: ll, l_rtm, i_kl, i_lj, i_kj
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
      call alloc_scl_fleg_mat_test(nscalar)
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kr_nd,kk,jj,j_rlm,mp_rlm,ip_rtm,nd,k_rlm,  &
!$omp&                    ll,l_rtm,i_kl,i_lj,i_kj,i_rlm,i_recv,         &
!$omp&                    st_elapsed)                                   &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
!    even l-m
          st_elapsed = MPI_WTIME()
          do jj = 1, nj_rlm(ip)
            j_rlm = jj + jst(ip)
            do l_rtm = 1, nidx_rtm(2)
              i_lj = l_rtm + (jj-1) * nidx_rtm(2)
!
              Pws_le(i_lj,ip) = P_rtm(l_rtm,j_rlm)                      &
     &                         * g_sph_rlm(j_rlm,6) * weight_rtm(l_rtm)
            end do
          end do
            elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          do l_rtm = 1, nidx_rtm(2)
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
              ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                    &
     &                   + (k_rlm-1) *  istep_rtm(1)                    &
     &                   + (mp_rlm-1) * istep_rtm(3)
              i_recv = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtm) - 1) * ncomp
              i_kl = kk + (l_rtm-1) * nkr(ip)
!
              symp(i_kl,ip) =  WR(i_recv)
            end do
          end do
            elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
            st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        Pws_le(1,ip), symp(1,ip), scl_e(1,ip))
            elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          do jj = 1, nj_rlm(ip)
            j_rlm = jj + jst(ip)
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip) + 3*nvector*nidx_rlm(1)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              i_rlm = nd + 3*nvector                                    &
     &                   + ncomp * ((j_rlm-1) * istep_rlm(2)            &
     &                            + (k_rlm-1) * istep_rlm(1))
              i_kj = kk + (jj-1) * nkr(ip)
!
              sp_rlm_1(i_rlm) = sp_rlm_1(i_rlm) + scl_e(i_kj,ip)
            end do
          end do
            elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
!
      elapsed(46:49)                                                    &
     &     = elaps(1:4) / dble(omp_get_max_threads()) + elapsed(46:49)
!
      call dealloc_scl_fleg_mat_test
!
      end subroutine leg_f_trans_scalar_matmul
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
      end module legendre_fwd_trans_matmul
