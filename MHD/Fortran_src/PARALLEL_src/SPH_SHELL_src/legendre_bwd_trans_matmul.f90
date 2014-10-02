!>@file   legendre_bwd_trans_matmul.f90
!!@brief  module legendre_bwd_trans_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Developping version)
!!
!!@verbatim
!!      subroutine alloc_vec_bleg_mat_test(nvector)
!!      subroutine alloc_scl_bleg_mat_test(nscalar)
!!      subroutine dealloc_vec_bleg_mat_test
!!      subroutine dealloc_scl_bleg_mat_test
!!
!!      subroutine leg_b_trans_vector_matmul(ncomp, nvector)
!!      subroutine leg_b_trans_scalar_matmul(ncomp, nvector, nscalar)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rlm,icomp)
!!       size: vr_rtm(nidx_rtm(2),nidx_rtm(1)*ncomp,nidx_rtm(3))
!!      real(kind = kreal), allocatable :: vr_rtm(:,:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm(j_rlm,k_rlm,icomp)
!!        size: sp_rlm(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm(:,:)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_matmul
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
      integer(kind = kint), private :: num_lj
      real(kind = kreal), allocatable, private :: Pg3_je(:,:)
      real(kind = kreal), allocatable, private :: dPdt_je(:,:)
      real(kind = kreal), allocatable, private :: Pgv_je(:,:)
!
      real(kind = kreal), allocatable, private :: P_je(:,:)
!
      integer(kind = kint), private :: nvec_jk
      real(kind = kreal), allocatable, private :: pol_e(:,:)
      real(kind = kreal), allocatable, private :: dpl_e(:,:)
      real(kind = kreal), allocatable, private :: tor_e(:,:)
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
      subroutine alloc_vec_bleg_mat_test(nvector)
!
      integer(kind = kint), intent(in) ::nvector
!
!
      num_lj = nidx_rtm(2) * maxdegree_rlm
      allocate(Pg3_je(num_lj,np_smp))
      allocate(dPdt_je(num_lj,np_smp))
      allocate(Pgv_je(num_lj,np_smp))
!
      nvec_jk =nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpl_e(nvec_jk,np_smp))
      allocate(tor_e(nvec_jk,np_smp))
!
      nvec_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_vec_bleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine alloc_scl_bleg_mat_test(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
!
      num_lj = nidx_rtm(2) * maxdegree_rlm
      allocate(P_je(num_lj,np_smp))
!
      nscl_jk = nidx_rtm(2) * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
!
      nscl_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
!
      end subroutine alloc_scl_bleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_vec_bleg_mat_test
!
!
      deallocate(Pg3_je, dPdt_je, Pgv_je)
      deallocate(pol_e, dpl_e, tor_e)
!
      deallocate(symp_r, symn_t, symn_p, asmp_t, asmp_p)
!
      end subroutine dealloc_vec_bleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_scl_bleg_mat_test
!
!
      deallocate(P_je)
      deallocate(scl_e)
      deallocate(symp)
!
      end subroutine dealloc_scl_bleg_mat_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_vector_matmul(ncomp, nvector,              &
     &          irev_sr_rlm, n_WR, WR, vr_rtm)
!
      use set_legendre_for_matmul
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: nb_nri, ip, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
      call alloc_vec_bleg_mat_test(nvector)
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,mn_rlm,st_elapsed)                  &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst(ip) = lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
          st_elapsed = MPI_WTIME()
          call set_bwd_leg_vector_matmul(jst(ip), nj_rlm(ip), num_lj,   &
     &        Pg3_je(1,ip), dPdt_je(1,ip), Pgv_je(1,ip))
          elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          call set_sp_rlm_vector_matmul                                 &
     &         (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                  &
     &          ncomp, irev_sr_rlm, n_WR, WR,                           &
     &          nvec_jk, pol_e(1,ip), dpl_e(1,ip), tor_e(1,ip))
          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip),   &
     &        Pg3_je(1,ip), pol_e(1,ip), symp_r(1,ip))
          call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip),   &
     &        dPdt_je(1,ip), dpl_e(1,ip), asmp_t(1,ip))
          call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip),   &
     &        dPdt_je(1,ip), tor_e(1,ip), asmp_p(1,ip))
!
          call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip),   &
     &        Pgv_je(1,ip), tor_e(1,ip), symn_t(1,ip))
          call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip),   &
     &        Pgv_je(1,ip), dpl_e(1,ip), symn_p(1,ip))
          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vector_matmul                                 &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nvec_lk,                &
     &        symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip),                 &
     &        symn_t(1,ip), symn_p(1,ip), ncomp, vr_rtm)
           elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
      call dealloc_vec_bleg_mat_test
      elapsed(41:44)                                                    &
     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_scalar_matmul(ncomp, nvector, nscalar,     &
     &          irev_sr_rlm, n_WR, WR, vr_rtm)
!
      use set_legendre_for_matmul
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: ip, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      elaps(1:4) = 0
      call alloc_scl_bleg_mat_test(nscalar)
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,mp_rlm,st_elapsed)                         &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
!
        do mp_rlm = 1, nidx_rtm(3)
          jst(ip) = lstack_rlm(mp_rlm-1)
          nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
          st_elapsed = MPI_WTIME()
          call set_bwd_leg_scalar_matmul                                &
     &       (jst(ip), nj_rlm(ip), num_lj, P_je(1,ip))
          elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          call set_sp_rlm_scalar_matmul                                 &
     &         (kst(ip), nkr(ip), jst(ip), nj_rlm(ip),                  &
     &          ncomp, nvector, irev_sr_rlm, n_WR, WR,                  &
     &          nscl_jk, scl_e(1,ip))
          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip),   &
     &        P_je(1,ip), scl_e(1,ip), symp(1,ip))
          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_scalar_matmul(kst(ip), nkr(ip), mp_rlm,       &
     &        nscl_lk, symp(1,ip), ncomp, nvector, vr_rtm)
          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
      call dealloc_scl_bleg_mat_test
      elapsed(41:44)                                                    &
     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_scalar_matmul
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_matmul
