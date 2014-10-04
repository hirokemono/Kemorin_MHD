!>@file   legendre_bwd_sym_matmul.f90
!!@brief  module legendre_bwd_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Developping version)
!!
!!@verbatim
!!      subroutine alloc_vec_bleg_sym_mat(nvector)
!!      subroutine alloc_scl_bleg_sym_mat(nscalar)
!!      subroutine dealloc_vec_bleg_sym_mat
!!      subroutine dealloc_scl_bleg_sym_mat
!!
!!      subroutine leg_b_trans_vec_sym_matmul(ncomp, nvector,           &
!!     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!!      subroutine leg_b_trans_scl_sym_matmul(ncomp, nvector, nscalar,  &
!!     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm
!!        Output: vr_rtm
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
      module legendre_bwd_sym_matmul
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
!>     Maximum matrix size for Legendre polynomial
      integer(kind = kint), private :: num_lj
!>     Maximum for Legendre polynomials with evem (l-m)
!!     for radial component's transform
      real(kind = kreal), allocatable, private :: Pg3_je(:,:)
!>     Maximum for difference of Legendre polynomials with evem (l-m)
!!     for horizontal component's transform
      real(kind = kreal), allocatable, private :: dPdt_je(:,:)
!>     Maximum for Legendre polynomials with odd (l-m)
!!     for radial component's transform
      real(kind = kreal), allocatable, private :: Pg3_jo(:,:)
!>     Maximum for difference of Legendre polynomials with odd (l-m)
!!     for horizontal component's transform
      real(kind = kreal), allocatable, private :: dPdt_jo(:,:)
!
!>     Maximum for Legendre polynomials with evem (l-m)
!!     for scalar's transform
      real(kind = kreal), allocatable, private :: P_je(:,:)
!>     Maximum for Legendre polynomials with odd (l-m)
!!     for scalar's transform
      real(kind = kreal), allocatable, private :: P_jo(:,:)
!
!>     Maximum matrix size for spectr data
      integer(kind = kint), private :: nvec_jk
!>     Poloidal component with evem (l-m)
      real(kind = kreal), allocatable, private :: pol_e(:,:)
!>     radial difference of Poloidal component with evem (l-m)
      real(kind = kreal), allocatable, private :: dpoldt_e(:,:)
!>     radial difference of Poloidal component with evem (l-m)
      real(kind = kreal), allocatable, private :: dpoldp_e(:,:)
!>     Toroidal component with evem (l-m)
      real(kind = kreal), allocatable, private :: dtordt_e(:,:)
!>     Toroidal component with evem (l-m)
      real(kind = kreal), allocatable, private :: dtordp_e(:,:)
!>     Poloidal component with odd (l-m)
      real(kind = kreal), allocatable, private :: pol_o(:,:)
!>     radial difference of Poloidal component with odd (l-m)
      real(kind = kreal), allocatable, private :: dpoldt_o(:,:)
!>     radial difference of Poloidal component with odd (l-m)
      real(kind = kreal), allocatable, private :: dpoldp_o(:,:)
!>     Toroidal component with odd (l-m)
      real(kind = kreal), allocatable, private :: dtordt_o(:,:)
!>     Toroidal component with odd (l-m)
      real(kind = kreal), allocatable, private :: dtordp_o(:,:)
!
!>     Maximum matrix size for field data
      integer(kind = kint), private :: nvec_lk
!>     Symmetric radial component
      real(kind = kreal), allocatable, private :: symp_r(:,:)
!>     Anti-symmetric theta-component
      real(kind = kreal), allocatable, private :: asmp_t(:,:)
!>     Anti-symmetric phi-component
      real(kind = kreal), allocatable, private :: asmp_p(:,:)
!>     Symmetric theta-component with condugate order
      real(kind = kreal), allocatable, private :: symn_t(:,:)
!>     Symmetric phi-component with condugate order
      real(kind = kreal), allocatable, private :: symn_p(:,:)
!>     Anti-symmetric radial component
      real(kind = kreal), allocatable, private :: asmp_r(:,:)
!>     Symmetric theta-component
      real(kind = kreal), allocatable, private :: symp_t(:,:)
!>     Symmetric phi-component
      real(kind = kreal), allocatable, private :: symp_p(:,:)
!>     Anti-symmetric theta-component with condugate order
      real(kind = kreal), allocatable, private :: asmn_t(:,:)
!>     Anti-symmetric phi-component with condugate order
      real(kind = kreal), allocatable, private :: asmn_p(:,:)
!
!>     Maximum matrix size for spectr data
      integer(kind = kint), private :: nscl_jk
!>     Scalar with evem (l-m)
      real(kind = kreal), allocatable, private :: scl_e(:,:)
!>     Scalar with odd (l-m)
      real(kind = kreal), allocatable, private :: scl_o(:,:)
!
!>     Maximum matrix size for field data
      integer(kind = kint), private :: nscl_lk
!>     Symmetric scalar component
      real(kind = kreal), allocatable, private :: symp(:,:)
!>     Anti-symmetric scalar component
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
      subroutine alloc_vec_bleg_sym_mat(nvector)
!
      integer(kind = kint), intent(in) ::nvector
!
!      write(*,*) 'istep_rtp', istep_rtp, nidx_rtp
!      write(*,*) 'istep_rtm', istep_rtm, nidx_rtm
!      write(*,*) 'istep_rlm', istep_rlm, nidx_rlm
!      write(*,*) 'istep_rj', istep_rj, nidx_rj
!
!
      num_lj = ((nidx_rtm(2)+1)/2) * maxdegree_rlm
      allocate(Pg3_je(num_lj,np_smp))
      allocate(dPdt_je(num_lj,np_smp))
      allocate(Pg3_jo(num_lj,np_smp))
      allocate(dPdt_jo(num_lj,np_smp))
!
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
      nvec_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(asmp_r(nvec_lk,np_smp))
      allocate(symp_t(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(asmn_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(asmn_t(nvec_lk,np_smp))
      allocate(symp_p(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_vec_bleg_sym_mat
!
! -----------------------------------------------------------------------
!
      subroutine alloc_scl_bleg_sym_mat(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
!
      num_lj = ((nidx_rtm(2)+1)/2) * maxdegree_rlm
      allocate(P_je(num_lj,np_smp))
      allocate(P_jo(num_lj,np_smp))
!
      nscl_jk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
      allocate(scl_o(nscl_jk,np_smp))
!
      nscl_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
      allocate(asmp(nscl_lk,np_smp))
!
      end subroutine alloc_scl_bleg_sym_mat
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_vec_bleg_sym_mat
!
!
      deallocate(Pg3_je, dPdt_je, Pg3_jo, dPdt_jo)
!
      deallocate(pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
      deallocate(pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
!
      deallocate(symp_r, symp_t, symp_p, symn_t, symn_p)
      deallocate(asmp_r, asmp_t, asmp_p, asmn_t, asmn_p)
!
      end subroutine dealloc_vec_bleg_sym_mat
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_scl_bleg_sym_mat
!
!
      deallocate(P_je, P_jo)
      deallocate(scl_e, scl_o)
      deallocate(symp, asmp)
!
      end subroutine dealloc_scl_bleg_sym_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_vec_sym_matmul(ncomp, nvector,             &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      use set_legendre_for_matmul
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm, mn_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      call alloc_vec_bleg_sym_mat(nvector)
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
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
          n_jk_e(ip) = (nj_rlm(ip)+1)/2
          n_jk_o(ip) = nj_rlm(ip)/2
!
          st_elapsed = MPI_WTIME()
          call set_bwd_leg_vector_sym_matmul                            &
     &       (jst(ip), n_jk_e(ip), n_jk_o(ip), nl_rtm, num_lj,          &
     &        Pg3_je(1,ip), dPdt_je(1,ip), Pg3_jo(1,ip), dPdt_jo(1,ip))
            elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          call set_sp_rlm_vector_sym_matmul                             &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, irev_sr_rlm, n_WR, WR, nvec_jk, pol_e(1,ip),       &
     &        dpoldt_e(1,ip), dpoldp_e(1,ip), dtordt_e(1,ip),           &
     &        dtordp_e(1,ip), pol_o(1,ip), dpoldt_o(1,ip),              &
     &        dpoldp_o(1,ip), dtordt_o(1,ip), dtordp_o(1,ip))
          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),        &
     &        Pg3_je(1,ip), pol_e(1,ip), symp_r(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),        &
     &        dPdt_je(1,ip), dpoldt_e(1,ip), asmp_t(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),        &
     &        dPdt_je(1,ip), dtordt_e(1,ip), asmp_p(1,ip))
!
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),        &
     &        Pg3_je(1,ip), dtordp_e(1,ip), symn_t(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),        &
     &        Pg3_je(1,ip), dpoldp_e(1,ip), symn_p(1,ip))
!   odd l-m
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),        &
     &        Pg3_jo(1,ip), pol_o(1,ip), asmp_r(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),        &
     &        dPdt_jo(1,ip), dpoldt_o(1,ip), symp_t(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),        &
     &        dPdt_jo(1,ip), dtordt_o(1,ip), symp_p(1,ip))
!
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),        &
     &        Pg3_jo(1,ip), dtordp_o(1,ip), asmn_t(1,ip))
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),        &
     &        Pg3_jo(1,ip), dpoldp_o(1,ip), asmn_p(1,ip))
          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_vector_sym_matmul                             &
     &       (kst(ip), nkr(ip), mp_rlm, mn_rlm, nl_rtm, nvec_lk,        &
     &        symp_r(1,ip), asmp_t(1,ip), asmp_p(1,ip), symn_t(1,ip),   &
     &        symn_p(1,ip), asmp_r(1,ip), symp_t(1,ip), symp_p(1,ip),   &
     &        asmn_t(1,ip), asmn_p(1,ip), ncomp, irev_sr_rtm, n_WS, WS)
          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
      call dealloc_vec_bleg_sym_mat
      elapsed(41:44)                                                    &
     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_scl_sym_matmul(ncomp, nvector, nscalar,    &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
!
      use set_legendre_for_matmul
      use set_sp_rlm_for_leg_matmul
      use cal_vr_rtm_by_matmul
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      call alloc_scl_bleg_sym_mat(nscalar)
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
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
          n_jk_e(ip) = (nj_rlm(ip)+1)/2
          n_jk_o(ip) = nj_rlm(ip)/2
!
!   even l-m
          st_elapsed = MPI_WTIME()
          call set_bwd_leg_scalar_sym_matmul                            &
     &       (jst(ip), n_jk_e(ip), n_jk_o(ip), nl_rtm, num_lj,          &
     &        P_je(1,ip), P_jo(1,ip))
          elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
          st_elapsed = MPI_WTIME()
          call set_sp_rlm_scalar_sym_matmul                             &
     &       (kst(ip), nkr(ip), jst(ip), n_jk_e(ip), n_jk_o(ip),        &
     &        ncomp, nvector, irev_sr_rlm, n_WR, WR,                    &
     &        nscl_jk, scl_e(1,ip), scl_o(1,ip) )
          elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
          st_elapsed = MPI_WTIME()
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),        &
     &        P_je(1,ip), scl_e(1,ip), symp(1,ip))
!   odd l-m
          call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),        &
     &        P_jo(1,ip), scl_o(1,ip), asmp(1,ip))
          elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
          st_elapsed = MPI_WTIME()
          call cal_vr_rtm_scalar_sym_matmul(kst(ip), nkr(ip),           &
     &        mp_rlm, nl_rtm, nscl_lk, symp(1,ip), asmp(1,ip),          &
     &        ncomp, nvector, irev_sr_rtm, n_WS, WS)
          elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
!
        end do
      end do
!$omp end parallel do
!
      call dealloc_scl_bleg_sym_mat
      elapsed(41:44)                                                    &
     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      end subroutine leg_b_trans_scl_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_sym_matmul
