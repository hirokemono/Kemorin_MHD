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
!!     &         (np_smp, ncomp_fwd, nvect_fwd, ncomp_bwd, nvect_bwd,   &
!!     &          sph_rtm, sph_rlm, idx_trns, rocBLAS_WK)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        integer(kind = kint), intent(in) :: np_smp
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
     &         (np_smp, ncomp_fwd, nvect_fwd, ncomp_bwd, nvect_bwd,     &
     &          sph_rtm, sph_rlm, idx_trns, rocBLAS_WK)
!
      use hipfort_rocblas
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: ncomp_fwd, nvect_fwd
      integer(kind = kint), intent(in) :: ncomp_bwd, nvect_bwd
!
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: nl_rtm, mp_rlm
      integer(kind = kint) :: nkr, nkrs,  nkrt
      integer(kind = kint) :: n_jk_e, n_jk_o
!
!
      rocBLAS_WK%transa = rocblas_operation_none
      rocBLAS_WK%transb = rocblas_operation_none
!
      rocBLAS_WK%MaxAbytes = 0
      rocBLAS_WK%MaxBbytes = 0
      rocBLAS_WK%MaxCbytes = 0
!
!  Forward transform
      nl_rtm = (sph_rtm%nidx_rtm(2) + 1)/2
      do ip = 1, np_smp
        nkr = sph_rlm%istack_rlm_kr_smp(ip)                             &
     &       - sph_rlm%istack_rlm_kr_smp(ip-1)
        nkrs = ncomp_fwd*nkr
        nkrt = 2*nvect_fwd*nkr
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          n_jk_e = idx_trns%lstack_even_rlm(mp_rlm)                     &
     &            - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o = idx_trns%lstack_rlm(mp_rlm)                          &
     &            - idx_trns%lstack_even_rlm(mp_rlm)
!
!  even l-m
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nkrs * nl_rtm))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (nl_rtm * n_jk_e))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nkrs * n_jk_e))
!
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nkrt * nl_rtm))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (nl_rtm * n_jk_e))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nkrt * n_jk_e))
!
!  odd l-m
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nkrs * nl_rtm))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (nl_rtm * n_jk_o))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nkrs * n_jk_o))
!
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nkrt * nl_rtm))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (nl_rtm * n_jk_o))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nkrt * n_jk_o))
        end do
      end do

!  Backward transform
      do ip = 1, np_smp
        nkr = sph_rlm%istack_rlm_kr_smp(ip)                             &
     &       - sph_rlm%istack_rlm_kr_smp(ip-1)
        nkrs = ncomp_bwd*nkr
        nkrt = 2*nvect_bwd*nkr
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          n_jk_e = idx_trns%lstack_even_rlm(mp_rlm)                     &
     &                - idx_trns%lstack_rlm(mp_rlm-1)
          n_jk_o = idx_trns%lstack_rlm(mp_rlm)                          &
     &                - idx_trns%lstack_even_rlm(mp_rlm)
!
!   even l-m
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nl_rtm * n_jk_e))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (n_jk_e * nkrs))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nl_rtm * nkrs))
!
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nl_rtm * n_jk_e))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (n_jk_e * nkrt))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nl_rtm * nkrt))
!   odd l-m
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nl_rtm * n_jk_o))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (n_jk_o * nkrs))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nl_rtm * nkrs))
!
          rocBLAS_WK%MaxAbytes                                          &
     &         = max(rocBLAS_WK%MaxAbytes, (nl_rtm * n_jk_o))
          rocBLAS_WK%MaxBbytes                                          &
     &         = max(rocBLAS_WK%MaxBbytes, (n_jk_o * nkrt))
          rocBLAS_WK%MaxCbytes                                          &
     &         = max(rocBLAS_WK%MaxCbytes, (nl_rtm * nkrt))
        end do
      end do
!
      rocBLAS_WK%MaxAbytes = kreal * rocBLAS_WK%MaxAbytes
      rocBLAS_WK%MaxBbytes = kreal * rocBLAS_WK%MaxBbytes
      rocBLAS_WK%MaxCbytes = kreal * rocBLAS_WK%MaxCbytes
!
      end subroutine max_size_rocBLAS_leg_trns
!
! ----------------------------------------------------------------------
#endif
!
      end module t_rocBLAS_legendre_trans
