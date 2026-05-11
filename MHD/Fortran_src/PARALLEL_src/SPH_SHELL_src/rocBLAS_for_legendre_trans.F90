!>@file   rocBLAS_for_legendre_trans.F90
!!@brief  module rocBLAS_for_legendre_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Matrix products for Legendre transforms
!!
!!@verbatim
!!      subroutine ROCm_matmul_fwd_leg_trans(iflag_matmul,              &
!!     &          nkr, n_jk, nl_rtm, V_kl, P_lj, S_kj, rocBLAS_WK)
!!        integer(kind = kint), intent(in) :: iflag_matmul
!!        integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
!!        real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
!!        real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!      subroutine ROCm_matmul_bwd_leg_trans(iflag_matmul,              &
!!     &          nl_rtm, nkr, n_jk, P_lj, S_jk, V_lk, rocBLAS_WK)
!!        integer(kind = kint), intent(in) :: iflag_matmul
!!        integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
!!        real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!!        real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!!        real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!
!!      subroutine ROCm_DGEMM_fwd_leg_trans_Pjl(iflag_matmul,           &
!!     &          n_jk, nkr, nl_rtm, P_jl, V_lk, S_jk, rocBLAS_WK)
!!        integer(kind = kint), intent(in) :: iflag_matmul
!!        integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
!!        real(kind = kreal), intent(in) :: P_jl(n_jk,nl_rtm)
!!        real(kind = kreal), intent(in) :: V_lk(nl_rtm,nkr)
!!        real(kind = kreal), intent(inout) :: S_jk(n_jk,nkr)
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!      subroutine ROCm_DGEMM_bwd_leg_trans_Pjl(iflag_matmul,           &
!!     &          nkr, nl_rtm, n_jk, S_kj, P_jl, V_kl, rocBLAS_WK)
!!        integer(kind = kint), intent(in) :: iflag_matmul
!!        integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
!!        real(kind = kreal), intent(in) :: S_kj(nkr,n_jk)
!!        real(kind = kreal), intent(in) :: P_jl(n_jk,nl_rtm)
!!        real(kind = kreal), intent(inout) :: V_kl(nkr,nl_rtm)
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!
!!      subroutine ROCm_DGEMM_fwd_leg_trans(iflag_matmul,               &
!!     &          nkr, n_jk, nl_rtm, V_kl, P_lj, coef, S_kj, rocBLAS_WK)
!!        integer(kind = kint), intent(in) :: iflag_matmul
!!        integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
!!        real(kind = kreal), intent(in) :: coef
!!        real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
!!        real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!      subroutine ROCm_DGEMM_bwd_leg_trans(iflag_matmul,               &
!!     &          nl_rtm, nkr, n_jk, P_lj, S_jk, coef, V_lk, rocBLAS_WK)
!!        integer(kind = kint), intent(in) :: iflag_matmul
!!        integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
!!        real(kind = kreal), intent(in) :: coef
!!        real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!!        real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!!        real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!!        type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!!@endverbatim
!!
!!@param   nkr     Number of radial grid and field
!!@param   n_jk    Number of spherical harmonic degree for transform
!!@param   nl_rtm  Number of meridional grids
!!
!!@param   P_lj    Matrix for Legendre polynomials
!!@param   V_kl    field data @f$ f(r,\theta,m) @f$ with V_kl(r,theta)
!!@param   S_kj    spectrum data @f$ f(r,l,m) @f$ with S_kj(r,l)
!!@param   V_lk    field data @f$ f(r,\theta,m) @f$ with V_kl(theta,r)
!!@param   S_jk    spectrum data @f$ f(r,l,m) @f$ with S_jk(l,r)
!
      module rocBLAS_for_legendre_trans
!
#ifdef _AMD_ROCM_
      use hipfort_rocblas
#endif
!
      use m_precision
      use m_constants
!
      use t_rocBLAS_legendre_trans
!
      implicit none
!
      integer(kind = kint), parameter :: iflag_OMP_offload = 4
      integer(kind = kint), parameter :: iflag_OMP_rocBLAS = 5
      integer(kind = kint), parameter :: iflag_rocBLAS =     6
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine ROCm_matmul_fwd_leg_trans(iflag_matmul,                &
     &          nkr, n_jk, nl_rtm, V_kl, P_lj, S_kj, rocBLAS_WK)
!
#ifdef _AMD_ROCM_
      use calypso_rocBLAS_DGEMM
#endif
!
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(c_int) :: n_jk_c, nkr_c, nl_rtm_c
      real(c_double), parameter :: one_c = 1.0d0, zero_c = 0.0d0
!
!
      if(n_jk*nkr .eq. 0) return
      nkr_c =    int(nkr,   kind=KIND(nkr_c))
      n_jk_c =   int(n_jk,  kind=KIND(n_jk_c))
      nl_rtm_c = int(nl_rtm,kind=KIND(nl_rtm_c))
      rocBLAS_WK%Nabytes = kreal * nkr *    nl_rtm
      rocBLAS_WK%Nbbytes = kreal * nl_rtm * n_jk
      rocBLAS_WK%Ncbytes = kreal * nkr *    n_jk
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj = matmul(V_kl,P_lj)
!
#ifdef _AMD_ROCM_
      else if(iflag_matmul .eq. iflag_OMP_offload) then
        call calypso_OpenMP_target_DGEMM(nkr_c, n_jk_c, nl_rtm_c,       &
     &      one_c, V_kl, nkr_c, P_lj, nl_rtm_c, zero_c, S_kj, nkr_c)
      else if(iflag_matmul .eq. iflag_OMP_rocBLAS) then
        call calypso_OpenMP_rocBLAS_dgemm                               &
     &     (rocBLAS_WK%handle, rocBLAS_WK%transa, rocBLAS_WK%transb,    &
     &      nkr_c, n_jk_c, nl_rtm_c, one_c,                             &
     &      V_kl, nkr_c, P_lj, nl_rtm_c, zero_c, S_kj, nkr_c)
      else if(iflag_matmul .eq. iflag_rocBLAS) then
        call calypso_hip_rocBLAS_dgemm(rocBLAS_WK%handle,               &
     &      rocBLAS_WK%Nabytes, rocBLAS_WK%Nbbytes, rocBLAS_WK%Ncbytes, &
     &      rocBLAS_WK%transa, rocBLAS_WK%transb,                       &
     &      nkr_c, n_jk_c, nl_rtm_c, one_c,                             &
     &      V_kl, nkr_c, P_lj, nl_rtm_c, zero_c, S_kj, nkr_c,           &
     &      rocBLAS_WK%A_cptr, rocBLAS_WK%B_cptr, rocBLAS_WK%C_cptr)
#endif
      else
        call matmul_fwd_leg_trans(iflag_matmul, nkr, n_jk, nl_rtm,      &
     &                            V_kl, P_lj, S_kj)
      end if
!
      end subroutine ROCm_matmul_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine ROCm_matmul_bwd_leg_trans(iflag_matmul,                &
     &          nl_rtm, nkr, n_jk, P_lj, S_jk, V_lk, rocBLAS_WK)
!
#ifdef _AMD_ROCM_
      use calypso_rocBLAS_DGEMM
#endif
!
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(c_int) :: n_jk_c, nkr_c, nl_rtm_c
      real(c_double), parameter :: one_c = 1.0d0, zero_c = 0.0d0
!
!
      if(nkr .eq. 0) return
      nl_rtm_c = int(nl_rtm,kind=KIND(nl_rtm_c))
      nkr_c =    int(nkr,   kind=KIND(nkr_c))
      n_jk_c =   int(n_jk,  kind=KIND(n_jk_c))
      rocBLAS_WK%Nabytes = kreal * nl_rtm * n_jk
      rocBLAS_WK%Nbbytes = kreal * n_jk *   nkr
      rocBLAS_WK%Ncbytes = kreal * nl_rtm * nkr
!
      if(n_jk .eq. 0) then
!$omp parallel workshare
        V_lk = 0.0d0
!$omp end parallel workshare
!
#ifdef _AMD_ROCM_
      else if(iflag_matmul .eq. iflag_OMP_offload) then
        call calypso_OpenMP_target_DGEMM                                &
     &     (nl_rtm_c, nkr_c, n_jk_c, one_c,                             &
     &      P_lj, nl_rtm_c, S_jk, n_jk_c, zero_c, V_lk, nl_rtm_c)
      else if(iflag_matmul .eq. iflag_OMP_rocBLAS) then
        call calypso_OpenMP_rocBLAS_dgemm                               &
     &     (rocBLAS_WK%handle, rocBLAS_WK%transa, rocBLAS_WK%transb,    &
     &      nl_rtm_c, nkr_c, n_jk_c, one_c,                             &
     &      P_lj, nl_rtm_c, S_jk, n_jk_c, zero_c, V_lk, nl_rtm_c)
      else if(iflag_matmul .eq. iflag_rocBLAS) then
        call calypso_hip_rocBLAS_dgemm(rocBLAS_WK%handle,               &
     &      rocBLAS_WK%Nabytes, rocBLAS_WK%Nbbytes, rocBLAS_WK%Ncbytes, &
     &      rocBLAS_WK%transa, rocBLAS_WK%transb,                       &
     &      nl_rtm_c, nkr_c, n_jk_c, one_c,                             &
     &      P_lj, nl_rtm_c, S_jk, n_jk_c, zero_c, V_lk, nl_rtm_c,       &
     &      rocBLAS_WK%A_cptr, rocBLAS_WK%B_cptr, rocBLAS_WK%C_cptr)
#endif
      else
        call matmul_bwd_leg_trans(iflag_matmul, nl_rtm, nkr, n_jk,      &
     &                            P_lj, S_jk, V_lk)
      end if
!
      end subroutine ROCm_matmul_bwd_leg_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ROCm_DGEMM_fwd_leg_trans_Pjl(iflag_matmul,             &
     &          n_jk, nkr, nl_rtm, P_jl, V_lk, S_jk, rocBLAS_WK)
!
#ifdef _AMD_ROCM_
      use calypso_rocBLAS_DGEMM
#endif
!
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_jl(n_jk,nl_rtm)
      real(kind = kreal), intent(in) :: V_lk(nl_rtm,nkr)
!
      real(kind = kreal), intent(inout) :: S_jk(n_jk,nkr)
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(c_int) :: n_jk_c, nkr_c, nl_rtm_c
      real(c_double), parameter :: one_c = 1.0d0, zero_c = 0.0d0
!
!
      if(n_jk*nkr .eq. 0) return
      nkr_c =    int(nkr,   kind=KIND(nkr_c))
      n_jk_c =   int(n_jk,  kind=KIND(n_jk_c))
      nl_rtm_c = int(nl_rtm,kind=KIND(nl_rtm_c))
      rocBLAS_WK%Nabytes = kreal * n_jk *   nl_rtm
      rocBLAS_WK%Nbbytes = kreal * nl_rtm * nkr
      rocBLAS_WK%Ncbytes = kreal * n_jk *   nkr
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_jk = matmul(P_jl,V_lk)
!
#ifdef _AMD_ROCM_
      else if(iflag_matmul .eq. iflag_OMP_offload) then
        call calypso_OpenMP_target_DGEMM                                &
     &     (n_jk_c, nkr_c, nl_rtm_c, one_c,                             &
     &      P_jl, n_jk_c, V_lk, nl_rtm_c, zero_c, S_jk, n_jk_c)
      else if(iflag_matmul .eq. iflag_OMP_rocBLAS) then
        call calypso_OpenMP_rocBLAS_dgemm                               &
     &     (rocBLAS_WK%handle, rocBLAS_WK%transa, rocBLAS_WK%transb,    &
     &      n_jk_c, nkr_c, nl_rtm_c, one_c,                             &
     &      P_jl, n_jk_c, V_lk, nl_rtm_c, zero_c, S_jk, n_jk_c)
      else if(iflag_matmul .eq. iflag_rocBLAS) then
        call calypso_hip_rocBLAS_dgemm(rocBLAS_WK%handle,               &
     &      rocBLAS_WK%Nabytes, rocBLAS_WK%Nbbytes, rocBLAS_WK%Ncbytes, &
     &      rocBLAS_WK%transa, rocBLAS_WK%transb,                       &
     &      n_jk_c, nkr_c, nl_rtm_c, one_c,                             &
     &      P_jl, n_jk_c, V_lk, nl_rtm_c, zero_c, S_jk, n_jk_c,         &
     &      rocBLAS_WK%A_cptr, rocBLAS_WK%B_cptr, rocBLAS_WK%C_cptr)
#endif
      else
        call matmul_fwd_leg_trans_Pjl(iflag_matmul, n_jk, nkr, nl_rtm,  &
     &                                P_jl, V_lk, S_jk)
      end if
!
      end subroutine ROCm_DGEMM_fwd_leg_trans_Pjl
!
! ----------------------------------------------------------------------
!
      subroutine ROCm_DGEMM_bwd_leg_trans_Pjl(iflag_matmul,             &
     &          nkr, nl_rtm, n_jk, S_kj, P_jl, V_kl, rocBLAS_WK)
!
#ifdef _AMD_ROCM_
      use calypso_rocBLAS_DGEMM
#endif
!
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: S_kj(nkr,n_jk)
      real(kind = kreal), intent(in) :: P_jl(n_jk,nl_rtm)
!
      real(kind = kreal), intent(inout) :: V_kl(nkr,nl_rtm)
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(c_int) :: n_jk_c, nkr_c, nl_rtm_c
      real(c_double), parameter :: one_c = 1.0d0, zero_c = 0.0d0
!
!
      if(nkr .eq. 0) return
      nl_rtm_c = int(nl_rtm,kind=KIND(nl_rtm_c))
      nkr_c =    int(nkr,   kind=KIND(nkr_c))
      n_jk_c =   int(n_jk,  kind=KIND(n_jk_c))
      rocBLAS_WK%Nabytes = kreal * nkr *  n_jk
      rocBLAS_WK%Nbbytes = kreal * n_jk * nl_rtm
      rocBLAS_WK%Ncbytes = kreal * nkr *  nl_rtm
!
      if(n_jk .eq. 0) then
!$omp parallel workshare
        V_kl = 0.0d0
!$omp end parallel workshare
!
#ifdef _AMD_ROCM_
      else if(iflag_matmul .eq. iflag_OMP_offload) then
        call calypso_OpenMP_target_DGEMM                                &
     &     (nkr_c, nl_rtm_c, n_jk_c, one_c,                             &
     &      S_kj, nkr_c, P_jl, n_jk_c, zero_c, V_kl, nkr_c)
      else if(iflag_matmul .eq. iflag_OMP_rocBLAS) then
        call calypso_OpenMP_rocBLAS_dgemm                               &
     &     (rocBLAS_WK%handle, rocBLAS_WK%transa, rocBLAS_WK%transb,    &
     &      nkr_c, nl_rtm_c, n_jk_c, one_c,                             &
     &      S_kj, nkr_c, P_jl, n_jk_c, zero_c, V_kl, nkr_c)
      else if(iflag_matmul .eq. iflag_rocBLAS) then
        call calypso_hip_rocBLAS_dgemm(rocBLAS_WK%handle,               &
     &      rocBLAS_WK%Nabytes, rocBLAS_WK%Nbbytes, rocBLAS_WK%Ncbytes, &
     &      rocBLAS_WK%transa, rocBLAS_WK%transb,                       &
     &      nkr_c, nl_rtm_c, n_jk_c, one_c,                             &
     &      S_kj, nkr_c, P_jl, n_jk_c, zero_c, V_kl, nkr_c,             &
     &      rocBLAS_WK%A_cptr, rocBLAS_WK%B_cptr, rocBLAS_WK%C_cptr)
#endif
      else
        call matmul_bwd_leg_trans_Pjl(iflag_matmul, nkr, nl_rtm, n_jk,  &
     &                                S_kj, P_jl, V_kl)
      end if
!
      end subroutine ROCm_DGEMM_bwd_leg_trans_Pjl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ROCm_DGEMM_fwd_leg_trans(iflag_matmul,                 &
     &          nkr, n_jk, nl_rtm, V_kl, P_lj, coef, S_kj, rocBLAS_WK)
!
#ifdef _AMD_ROCM_
      use calypso_rocBLAS_DGEMM
#endif
!
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(c_int) :: n_jk_c, nkr_c, nl_rtm_c
      real(c_double), parameter :: one_c = 1.0d0
      real(c_double) :: coef_c
!
!
      if(n_jk*nkr .eq. 0) return
      coef_c =   real(coef,kind=KIND(coef_c))
      nkr_c =    int(nkr,   kind=KIND(nkr_c))
      n_jk_c =   int(n_jk,  kind=KIND(n_jk_c))
      nl_rtm_c = int(nl_rtm,kind=KIND(nl_rtm_c))
      rocBLAS_WK%Nabytes = kreal * nkr *    nl_rtm
      rocBLAS_WK%Nbbytes = kreal * nl_rtm * n_jk
      rocBLAS_WK%Ncbytes = kreal * nkr *    n_jk
!
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj(1:nkr,1:n_jk) = coef * S_kj(1:nkr,1:n_jk)                  &
     &         +  matmul(V_kl(1:nkr,1:nl_rtm), P_lj(1:nl_rtm,1:n_jk))
!
#ifdef _AMD_ROCM_
      else if(iflag_matmul .eq. iflag_OMP_offload) then
        call calypso_OpenMP_target_DGEMM(nkr_c, n_jk_c, nl_rtm_c,       &
     &      one_c, V_kl, nkr_c, P_lj, nl_rtm_c, coef_c, S_kj, nkr_c)
      else if(iflag_matmul .eq. iflag_OMP_rocBLAS) then
        call calypso_OpenMP_rocBLAS_dgemm                               &
     &     (rocBLAS_WK%handle, rocBLAS_WK%transa, rocBLAS_WK%transb,    &
     &      nkr_c, n_jk_c, nl_rtm_c, one_c,                             &
     &      V_kl, nkr_c, P_lj, nl_rtm_c, coef_c, S_kj, nkr_c)
      else if(iflag_matmul .eq. iflag_rocBLAS) then
        call calypso_hip_rocBLAS_dgemm(rocBLAS_WK%handle,               &
     &      rocBLAS_WK%Nabytes, rocBLAS_WK%Nbbytes, rocBLAS_WK%Ncbytes, &
     &      rocBLAS_WK%transa, rocBLAS_WK%transb,                       &
     &      nkr_c, n_jk_c, nl_rtm_c, one_c,                             &
     &      V_kl, nkr_c, P_lj, nl_rtm_c, coef_c, S_kj, nkr_c,           &
     &      rocBLAS_WK%A_cptr, rocBLAS_WK%B_cptr, rocBLAS_WK%C_cptr)
#endif
      else
        call add_matmul_fwd_leg_trans(iflag_matmul, nkr, n_jk, nl_rtm,  &
     &                                V_kl, P_lj, coef, S_kj)
      end if
!
      end subroutine ROCm_DGEMM_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine ROCm_DGEMM_bwd_leg_trans(iflag_matmul,                 &
     &          nl_rtm, nkr, n_jk, P_lj, S_jk, coef, V_lk, rocBLAS_WK)
!
#ifdef _AMD_ROCM_
      use calypso_rocBLAS_DGEMM
#endif
!
      use matmul_for_legendre_trans
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
      type(rocBLAS_work), intent(inout) :: rocBLAS_WK
!
      integer(c_int) :: n_jk_c, nkr_c, nl_rtm_c
      real(c_double), parameter :: one_c = 1.0d0
      real(c_double) :: coef_c
!
!
      if(nkr .eq. 0) return
      coef_c =   real(coef,kind=KIND(coef_c))
      nl_rtm_c = int(nl_rtm,kind=KIND(nl_rtm_c))
      nkr_c =    int(nkr,   kind=KIND(nkr_c))
      n_jk_c =   int(n_jk,  kind=KIND(n_jk_c))
      rocBLAS_WK%Nabytes = kreal * nl_rtm * n_jk
      rocBLAS_WK%Nbbytes = kreal * n_jk *   nkr
      rocBLAS_WK%Ncbytes = kreal * nl_rtm * nkr
!
      if(n_jk .eq. 0) then
!$omp parallel workshare
        V_lk = 0.0d0
!$omp end parallel workshare
!
#ifdef _AMD_ROCM_
      else if(iflag_matmul .eq. iflag_OMP_offload) then
        call calypso_OpenMP_target_DGEMM                                &
     &     (nl_rtm_c, nkr_c, n_jk_c, one_c,                             &
     &      P_lj, nl_rtm_c, S_jk, n_jk_c, coef_c, V_lk, nl_rtm_c)
      else if(iflag_matmul .eq. iflag_OMP_rocBLAS) then
        call calypso_OpenMP_rocBLAS_dgemm                               &
     &     (rocBLAS_WK%handle, rocBLAS_WK%transa, rocBLAS_WK%transb,    &
     &      nl_rtm_c, nkr_c, n_jk_c, one_c,                             &
     &      P_lj, nl_rtm_c, S_jk, n_jk_c, coef_c, V_lk, nl_rtm_c)
      else if(iflag_matmul .eq. iflag_rocBLAS) then
        call calypso_hip_rocBLAS_dgemm(rocBLAS_WK%handle,               &
     &      rocBLAS_WK%Nabytes, rocBLAS_WK%Nbbytes, rocBLAS_WK%Ncbytes, &
     &      rocBLAS_WK%transa, rocBLAS_WK%transb,                       &
     &      nl_rtm_c, nkr_c, n_jk_c, one_c,                             &
     &      P_lj, nl_rtm_c, S_jk, n_jk_c, coef_c, V_lk, nl_rtm_c,       &
     &      rocBLAS_WK%A_cptr, rocBLAS_WK%B_cptr, rocBLAS_WK%C_cptr)
#endif
      else
        call add_matmul_bwd_leg_trans(iflag_matmul, nl_rtm, nkr, n_jk,  &
     &                                P_lj, S_jk, coef, V_lk)
      end if
!
      end subroutine ROCm_DGEMM_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      end module rocBLAS_for_legendre_trans
