!>@file   t_legendre_work_testlooop.f90
!!@brief  module t_legendre_work_testlooop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine init_legendre_sym_mat_both(sph_params, sph_rtm,      &
!!     &          idx_trns, nvector, nscalar, WK_l_tst)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!!
!!      subroutine dealloc_leg_sym_mat_both(mphi_rtm, WK_l_tst)
!!        type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm(nth_rtm,nidx_rtm(1)*ncomp,nidx_rtm(3))
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
      module t_legendre_work_testlooop
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_spheric_parameter
      use t_spheric_rtm_data
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_field_matrices_4_legendre
      use t_legendre_matrix_4_trns
      use t_set_legendre_4_sph_trans
!
      use matmul_for_legendre_trans
!
      implicit none
!
      integer(kind = kint), parameter :: n_SSE2 =   itwo
      integer(kind = kint), parameter :: n_AVX =    ifour
      integer(kind = kint), parameter :: n_AVX512 = ieight
!
!>      Work structure for Legendre trasform by large matmul
      type leg_trns_testloop_work
!>         Number of harmonics order
        integer(kind = kint) :: mphi_rtm
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint), allocatable :: n_jk_e(:)
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint), allocatable :: n_jk_o(:)
!
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint) :: nmax_jk_e
!>         Number of meridional grid points in northern hemisphere
        integer(kind = kint) :: nmax_jk_o
!
!
        integer(kind = kint), allocatable :: lst_rtm(:)
        integer(kind = kint), allocatable :: nle_rtm(:)
        integer(kind = kint), allocatable :: nlo_rtm(:)
!
        type(leg_tj_omp_matrix), allocatable :: Ptj_mat(:)
        type(leg_jt_omp_matrix), allocatable :: Pjt_mat(:)
        type(work_make_legendre), allocatable :: wk_plm(:)
!
        type(field_matrix_omp), allocatable :: Fmat(:)
!
!
!>       size for work area of pol_e and pol_o
        integer(kind = kint) :: n_pol_e
!>       size for work area of tor_e and tor_o
        integer(kind = kint) :: n_tor_e
!
        type(spectr_matrix_omp), allocatable :: Smat(:)
      end type leg_trns_testloop_work
!
      private :: count_symmetric_leg_lj_omp
      private :: count_leg_sym_matmul_mtr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_size_of_field_mat_test                           &
     &         (np_smp, nri_rtm, istack_rtm_lt_smp, nvector, nscalar,   &
     &          lst_rtm, nle_rtm, nlo_rtm, Fmat)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: nri_rtm
      integer(kind = kint), intent(in) :: istack_rtm_lt_smp(0:np_smp)
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      integer(kind = kint), intent(inout) :: lst_rtm(np_smp)
      integer(kind = kint), intent(inout) :: nle_rtm(np_smp)
      integer(kind = kint), intent(inout) :: nlo_rtm(np_smp)
      type(field_matrix_omp), intent(inout) :: Fmat(np_smp)
!
      integer(kind = kint) :: ip, led
!
!
      led = 0
      do ip = 1, np_smp
        lst_rtm(ip) = led
        led = (istack_rtm_lt_smp(ip) + 1) / 2
        nle_rtm(ip) = led - lst_rtm(ip)
        nlo_rtm(ip) = nle_rtm(ip)
      end do
      nlo_rtm(np_smp) = istack_rtm_lt_smp(np_smp) / 2 - lst_rtm(np_smp)
!
      do ip = 1, np_smp
        Fmat(ip)%nvec_lk = n_AVX512 * nri_rtm * nvector
        Fmat(ip)%nscl_lk = n_AVX512 * nri_rtm * nscalar
        Fmat(ip)%n_sym_r = n_AVX512 * nri_rtm * (3*nvector + nscalar)
        Fmat(ip)%n_sym_p = n_AVX512 * nri_rtm * 2*nvector
      end do
!
      end subroutine count_size_of_field_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_sym_mat_both(sph_params, sph_rtm,        &
     &          idx_trns, nvector, nscalar, WK_l_tst)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: ip
!
!
      WK_l_tst%mphi_rtm = sph_rtm%nidx_rtm(3)
      allocate(WK_l_tst%n_jk_e(WK_l_tst%mphi_rtm))
      allocate(WK_l_tst%n_jk_o(WK_l_tst%mphi_rtm))
!
      allocate(WK_l_tst%Ptj_mat(np_smp))
      allocate(WK_l_tst%Pjt_mat(np_smp))
      allocate(WK_l_tst%wk_plm(np_smp))
!
      allocate(WK_l_tst%lst_rtm(np_smp))
      allocate(WK_l_tst%nle_rtm(np_smp))
      allocate(WK_l_tst%nlo_rtm(np_smp))
!
      allocate(WK_l_tst%Fmat(np_smp))
      allocate(WK_l_tst%Smat(np_smp))
!
      call count_size_of_field_mat_test                                 &
     &   (np_smp, sph_rtm%nidx_rtm(1), sph_rtm%istack_rtm_lt_smp,       &
     &    nvector, nscalar, WK_l_tst%lst_rtm, WK_l_tst%nle_rtm,         &
     &    WK_l_tst%nlo_rtm, WK_l_tst%Fmat)
      call count_symmetric_leg_lj_omp                                   &
     &   (WK_l_tst%mphi_rtm, idx_trns, WK_l_tst)
!
!
      do ip = 1, np_smp
        call alloc_cal_legendre_work                                    &
     &     (sph_params%l_truncation, WK_l_tst%wk_plm(ip))
        call alloc_each_sym_leg_omp_mat_tj(n_AVX512,                    &
     &      WK_l_tst%nmax_jk_e, WK_l_tst%nmax_jk_o,                     &
     &      WK_l_tst%Ptj_mat(ip))
        call alloc_each_sym_leg_omp_mat_jt(n_AVX512,                    &
     &      WK_l_tst%nmax_jk_e, WK_l_tst%nmax_jk_o,                     &
     &      WK_l_tst%Pjt_mat(ip))
      end do
!
      call count_leg_sym_matmul_mtr                                     &
     &   (sph_rtm%nidx_rtm, nvector, nscalar, idx_trns, WK_l_tst)
!
      call alloc_field_mat_omp(np_smp, WK_l_tst%Fmat)
      call alloc_spectr_mat_omp                                         &
     &   (WK_l_tst%n_pol_e, WK_l_tst%n_tor_e, np_smp, WK_l_tst%Smat)
!
      end subroutine init_legendre_sym_mat_both
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_sym_mat_both(WK_l_tst)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        call dealloc_each_sym_leg_mat_tj(WK_l_tst%Ptj_mat(ip))
        call dealloc_each_sym_leg_mat_jt(WK_l_tst%Pjt_mat(ip))
        call dealloc_cal_legendre_work(WK_l_tst%wk_plm(ip))
      end do
      call dealloc_spectr_mat_omp(np_smp, WK_l_tst%Smat)
      call dealloc_field_mat_omp(np_smp, WK_l_tst%Fmat)
!
      deallocate(WK_l_tst%Ptj_mat, WK_l_tst%Pjt_mat, WK_l_tst%wk_plm)
      deallocate(WK_l_tst%Smat)
      deallocate(WK_l_tst%Fmat)
!
      deallocate(WK_l_tst%n_jk_e, WK_l_tst%n_jk_o)
      deallocate(WK_l_tst%lst_rtm, WK_l_tst%nle_rtm, WK_l_tst%nlo_rtm)
!
      end subroutine dealloc_leg_sym_mat_both
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_leg_sym_matmul_mtr                              &
     &         (nidx_rtm, nvector, nscalar, idx_trns, WK_l_tst)
!
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: nvector, nscalar
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      WK_l_tst%n_pol_e = ((idx_trns%maxdegree_rlm+1)/2) *  nidx_rtm(1)  &
     &                  * (3*nvector + nscalar)
      WK_l_tst%n_tor_e = ((idx_trns%maxdegree_rlm+1)/2) *  nidx_rtm(1)  &
     &                  * 2*nvector
!
      end subroutine count_leg_sym_matmul_mtr
!
! -----------------------------------------------------------------------
!
      subroutine count_symmetric_leg_lj_omp                            &
     &         (mphi_rtm, idx_trns, WK_l_tst)
!
      use set_legendre_matrices
!
      integer(kind = kint), intent(in) :: mphi_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm
!
!
      do mp_rlm = 1, mphi_rtm
        WK_l_tst%n_jk_e(mp_rlm) = idx_trns%lstack_even_rlm(mp_rlm)      &
     &                           - idx_trns%lstack_rlm(mp_rlm-1)
        WK_l_tst%n_jk_o(mp_rlm) = idx_trns%lstack_rlm(mp_rlm)           &
     &                           - idx_trns%lstack_even_rlm(mp_rlm)
      end do
      WK_l_tst%nmax_jk_e = MAXVAL(WK_l_tst%n_jk_e)
      WK_l_tst%nmax_jk_o = MAXVAL(WK_l_tst%n_jk_o)
!
      end subroutine count_symmetric_leg_lj_omp
!
! -----------------------------------------------------------------------
!
      end module t_legendre_work_testlooop
