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
!!      subroutine init_legendre_sym_mat_both(sph_rtm, sph_rlm, leg,    &
!!     &          idx_trns, nvector, nscalar, WK_l_tst)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
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
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_field_matrices_4_legendre
      use t_legendre_matrix_4_trns
!
      use matmul_for_legendre_trans
!
      implicit none
!
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
        type(leg_omp_matrix), allocatable :: Pmat(:,:)
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
      private :: dealloc_sym_leg_omp_mat_jt
      private :: init_each_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_sym_mat_both(sph_rtm, sph_rlm, leg,      &
     &          idx_trns, nvector, nscalar, WK_l_tst)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: nvector, nscalar
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
!
      WK_l_tst%mphi_rtm = sph_rtm%nidx_rtm(3)
      allocate(WK_l_tst%n_jk_e(WK_l_tst%mphi_rtm))
      allocate(WK_l_tst%n_jk_o(WK_l_tst%mphi_rtm))
      allocate(WK_l_tst%Pmat(WK_l_tst%mphi_rtm,np_smp))
!
      allocate(WK_l_tst%lst_rtm(np_smp))
      allocate(WK_l_tst%nle_rtm(np_smp))
      allocate(WK_l_tst%nlo_rtm(np_smp))
!
      allocate(WK_l_tst%Fmat(np_smp))
      allocate(WK_l_tst%Smat(np_smp))
!
      call count_size_of_field_mat_omp                                  &
     &   (np_smp, sph_rtm%nidx_rtm(1), sph_rtm%istack_rtm_lt_smp,       &
     &    nvector, nscalar, WK_l_tst%lst_rtm, WK_l_tst%nle_rtm,         &
     &    WK_l_tst%nlo_rtm, WK_l_tst%Fmat)
      call count_symmetric_leg_lj_omp                                   &
     &   (sph_rtm%nidx_rtm(3), idx_trns, WK_l_tst)
!
!
      call init_each_sym_leg_omp_mat_jt                                 &
     &  (sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3), sph_rlm%nidx_rlm(2), &
     &   idx_trns%lstack_rlm, leg%P_rtm, leg%dPdt_rtm, WK_l_tst)
      call init_each_sym_leg_omp_mat_tj                                 &
     &  (sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3), sph_rlm%nidx_rlm(2), &
     &   idx_trns%lstack_rlm, leg%P_rtm, leg%dPdt_rtm, WK_l_tst)
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
!
      call dealloc_sym_leg_omp_mat_tj(WK_l_tst)
      call dealloc_sym_leg_omp_mat_jt(WK_l_tst)
      call dealloc_spectr_mat_omp(np_smp, WK_l_tst%Smat)
      call dealloc_field_mat_omp(np_smp, WK_l_tst%Fmat)
!
      deallocate(WK_l_tst%Pmat)
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
      WK_l_tst%nmax_jk_e = max(WK_l_tst%n_jk_e)
      WK_l_tst%nmax_jk_o = max(WK_l_tst%n_jk_o)
!
      end subroutine count_symmetric_leg_lj_omp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_each_sym_leg_omp_mat_jt                           &
     &         (nth_rtm, mphi_rtm, jmax_rlm, lstack_rlm,                &
     &          P_rtm, dPdt_rtm, WK_l_tst)
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm, ip
!
!
      do mp_rlm = 1, mphi_rtm
        do ip = 1, np_smp
          call alloc_each_sym_leg_omp_mat_jt(ieight,                    &
     &        WK_l_tst%nmax_jk_e, WK_l_tst%nmax_jk_o,                   &
     &        WK_l_tst%Pmat(mp_rlm,ip))
        end do
      end do
!
      end subroutine init_each_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine init_each_sym_leg_omp_mat_tj                           &
     &         (nth_rtm, mphi_rtm, jmax_rlm, lstack_rlm,                &
     &          P_rtm, dPdt_rtm, WK_l_tst)
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      integer(kind = kint), intent(in) :: lstack_rlm(0:mphi_rtm)
!
      real(kind= kreal), intent(in) :: P_rtm(nth_rtm,jmax_rlm)
      real(kind= kreal), intent(in) :: dPdt_rtm(nth_rtm,jmax_rlm)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: mp_rlm, ip
!
!
      do mp_rlm = 1, mphi_rtm
        do ip = 1, np_smp
          call alloc_each_sym_leg_omp_mat_tj(WK_l_tst%nle_rtm(ip),      &
     &        WK_l_tst%n_jk_e(mp_rlm), WK_l_tst%n_jk_o(mp_rlm),         &
     &        WK_l_tst%Pmat(mp_rlm,ip))
        end do
      end do
!
      end subroutine init_each_sym_leg_omp_mat_tj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sym_leg_omp_mat_jt(WK_l_tst)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: ip, mp_rlm
!
!
      do mp_rlm = 1, WK_l_tst%mphi_rtm
        do ip = 1, np_smp
          call dealloc_each_sym_leg_mat_jt(WK_l_tst%Pmat(mp_rlm,ip))
        end do
      end do
!
      end subroutine dealloc_sym_leg_omp_mat_jt
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sym_leg_omp_mat_tj(WK_l_tst)
!
      type(leg_trns_testloop_work), intent(inout) :: WK_l_tst
!
      integer(kind = kint) :: ip, mp_rlm
!
!
      do mp_rlm = 1, WK_l_tst%mphi_rtm
        do ip = 1, np_smp
          call dealloc_each_sym_leg_mat_tj(WK_l_tst%Pmat(mp_rlm,ip))
        end do
      end do
!
      end subroutine dealloc_sym_leg_omp_mat_tj
!
! -----------------------------------------------------------------------
!
      end module t_legendre_work_testlooop
