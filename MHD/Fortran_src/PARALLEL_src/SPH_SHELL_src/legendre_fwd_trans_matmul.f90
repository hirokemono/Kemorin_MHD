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
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!!      subroutine leg_f_trans_scalar_matmul(ncomp, nvector, nscalar,   &
!!     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
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
      use matmul_for_legendre_trans
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
      real(kind = kreal), allocatable, private :: dpoldp_e(:,:)
      real(kind = kreal), allocatable, private :: dtordp_e(:,:)
      real(kind = kreal), allocatable, private :: dpoldt_e(:,:)
      real(kind = kreal), allocatable, private :: dtordt_e(:,:)
!
      integer(kind = kint), private :: nvec_kl
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
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
!
      nvec_kl = nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_kl,np_smp))
      allocate(symn_t(nvec_kl,np_smp))
      allocate(symn_p(nvec_kl,np_smp))
!
      allocate(asmp_t(nvec_kl,np_smp))
      allocate(asmp_p(nvec_kl,np_smp))
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
      deallocate(pol_e, dpoldp_e, dtordp_e, dpoldt_e, dtordt_e)
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
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_legendre_for_matmul
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
      call alloc_vec_fleg_mat_test(nvector)
!
!$omp parallel do schedule(static)                                      &
!$omp             private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
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
!
          st_elapsed = MPI_WTIME()
          call set_fwd_leg_vector_matmul(jst(ip), nj_rlm(ip),           &
     &        num_jl, Pvw_le(1,ip), dPvw_le(1,ip), Pgvw_le(1,ip))
          elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          call set_vr_rtm_vector_matmul(kst(ip), nkr(ip),               &
     &        mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,             &
     &        nvec_kl, symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),        &
     &        symn_t(1,ip), symn_p(1,ip))
          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2) 
!
          st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symp_r(1,ip), Pvw_le(1,ip), pol_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        asmp_t(1,ip), dPvw_le(1,ip), dpoldt_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symn_p(1,ip), Pgvw_le(1,ip), dpoldp_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        symn_t(1,ip), Pgvw_le(1,ip), dtordp_e(1,ip))
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        asmp_p(1,ip), dPvw_le(1,ip), dtordt_e(1,ip))
            elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_vector_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                    &
     &        nvec_jk, pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),     &
     &        dtordt_e(1,ip), dtordp_e(1,ip), ncomp,                    &
     &        irev_sr_rlm, n_WS, WS)
          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
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
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
!
      use set_legendre_for_matmul
      use set_vr_rtm_for_leg_matmul
      use cal_sp_rlm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
      call alloc_scl_fleg_mat_test(nscalar)
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rlm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
          st_elapsed = MPI_WTIME()
          call set_fwd_leg_scalar_matmul                                &
     &         (jst(ip), nj_rlm(ip), num_jl, Pws_le(1,ip))
          elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          call set_vr_rtm_scalar_matmul(kst(ip), nkr(ip), mp_rlm,       &
     &        ncomp, nvector, irev_sr_rtm, n_WR, WR,                    &
     &        nscl_lk, symp(1,ip))
          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
          st_elapsed = MPI_WTIME()
          call matmul_fwd_leg_trans(nkr(ip), nj_rlm(ip), nidx_rtm(2),   &
     &        Pws_le(1,ip), symp(1,ip), scl_e(1,ip))
          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          call cal_sp_rlm_scalar_matmul                                 &
     &       (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),  nscl_jk,          &
     &       scl_e(1,ip), ncomp, nvector, irev_sr_rlm, n_WS, WS)
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
!
      end module legendre_fwd_trans_matmul
