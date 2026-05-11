!>@file   t_rocBLAS_legendre_trans.F90
!!@brief  module t_rocBLAS_legendre_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Matrix products for Legendre transforms
!!
!!@verbatim
!!      subroutine max_size_rocBLAS_leg_trns                            &
!!     &         (ncomp_fwd, nvect_fwd, ncomp_bwd, nvect_bwd,           &
!!     &          sph_rtm, sph_rlm, idx_trns, rocBLAS_WK)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        integer(kind = kint), intent(in) :: ncomp_fwd, nvect_fwd
!!        integer(kind = kint), intent(in) :: ncomp_bwd, nvect_bwd
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!@endverbatim
!
      module t_rocBLAS_legendre_trans
!
      use ISO_C_BINDING  
!
      use m_precision
      use m_constants
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_work_4_sph_trans
!
      implicit none
!
      type rocBLAS_work
        type(c_ptr) :: handle = c_null_ptr
        integer(c_int) :: transa
        integer(c_int) :: transb

        integer(c_size_t) :: Nabytes
        integer(c_size_t) :: Nbbytes
        integer(c_size_t) :: Ncbytes
!
        integer(c_size_t) :: MaxAbytes
        integer(c_size_t) :: MaxBbytes
        integer(c_size_t) :: MaxCbytes
!
        type(c_ptr) :: A_cptr = c_null_ptr
        type(c_ptr) :: B_cptr = c_null_ptr
        type(c_ptr) :: C_cptr = c_null_ptr
      end type rocBLAS_work
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
#ifdef _AMD_ROCM_
! ----------------------------------------------------------------------
!
      subroutine max_size_rocBLAS_leg_trns                              &
     &         (ncomp_fwd, nvect_fwd, ncomp_bwd, nvect_bwd,             &
     &          sph_rtm, sph_rlm, idx_trns, rocBLAS_WK)
!
      use hipfort_rocblas
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp_fwd, nvect_fwd
      integer(kind = kint), intent(in) :: ncomp_bwd, nvect_bwd
!
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(kind = kint) :: nl_rtm, mp_rlm
      integer(kind = kint) :: nkr, nkrs, nkrt
      integer(kind = kint) :: n_jk_e, n_jk_o, n_jk
      integer(kind = kint) :: ncomp_max, nvect_max
!
!
      ncomp_max = max(ncomp_fwd, ncomp_bwd)
      nvect_max = max(nvect_fwd, nvect_bwd)
!
!  Forward transform
      nl_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      nkrs =   ncomp_max * sph_rlm%nidx_rlm(1)
      nkrt = 2*nvect_max * sph_rlm%nidx_rlm(1)
      nkr = max(nkrs, nkrt)
!
      rocBLAS_WK%MaxAbytes = 0
      rocBLAS_WK%MaxBbytes = 0
      rocBLAS_WK%MaxCbytes = 0
      do mp_rlm = 1, sph_rtm%nidx_rtm(3)
        n_jk_e = idx_trns%lstack_even_rlm(mp_rlm)                       &
     &          - idx_trns%lstack_rlm(mp_rlm-1)
        n_jk_o = idx_trns%lstack_rlm(mp_rlm)                            &
     &          - idx_trns%lstack_even_rlm(mp_rlm)
        n_jk = max(n_jk_e, n_jk_o)
!
        rocBLAS_WK%MaxAbytes                                            &
     &         = max(rocBLAS_WK%MaxAbytes, (nkr * nl_rtm))
        rocBLAS_WK%MaxBbytes                                            &
     &         = max(rocBLAS_WK%MaxBbytes, (nl_rtm * n_jk))
        rocBLAS_WK%MaxCbytes                                            &
     &         = max(rocBLAS_WK%MaxCbytes, (nkr * n_jk))
      end do
!
      rocBLAS_WK%MaxAbytes = kreal * rocBLAS_WK%MaxAbytes
      rocBLAS_WK%MaxBbytes = kreal * rocBLAS_WK%MaxBbytes
      rocBLAS_WK%MaxCbytes = kreal * rocBLAS_WK%MaxCbytes
!
      rocBLAS_WK%transa = rocblas_operation_none
      rocBLAS_WK%transb = rocblas_operation_none
!
      end subroutine max_size_rocBLAS_leg_trns
!
! ----------------------------------------------------------------------
#endif
!
      end module t_rocBLAS_legendre_trans
