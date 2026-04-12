!>@file   t_ispack3_FFT_wrapper.f90
!!@brief  module t_ispack3_FFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed on Apr., 2013
!
!
!>@brief  Fourier transform with work structures for ISPACK-3
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine finalize_wk_ispack3_t(WK)
!!      subroutine verify_wk_ispack3_t(Nsmp, Nstacksmp, Nfft, WK)
!!        integer(kind = kint_gl), intent(in) ::  Nfft
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        type(working_ISPACK3), intent(inout) :: WK
!!
!!      subroutine alloc_work_ispack3_t(Nsmp, nmax_comp, Nfft, WK)
!!      subroutine alloc_const_ispack3_t(nfft, WK)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nfft, nmax_comp
!!        type(working_ISPACK3), intent(inout) :: WK
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK-3
!! ------------------------------------------------------------------
!!
!! i = 1:     a_{0}
!! i = 2:     a_{Nfft/2}
!! i = 3:     a_{1}
!! i = 4:     b_{1}
!! ...
!! i = 2*k+1: a_{k}
!! i = 2*k+2: b_{k}
!! ...
!! i = Nfft-1:   a_{Nfft/2-1}
!! i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!@n @param WK          Work structure for ISPACK-3
!
      module t_ispack3_FFT_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      structure for working data for ISPACK-3
      type working_ISPACK3
!>        Maximum nuber of components for each SMP process
        integer(kind = kint_gl) :: Mmax_smp
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X_ispack(:,:)
!>        Work area for ISPACK-3
        integer(kind = kint_gl), allocatable :: IT_ispack(:)
!>        Work constants for ISPACK-3
        real(kind = 8), allocatable :: T_ispack(:)
!>        flag for length of Fourier transform
        integer(kind = kint_gl) :: iflag_fft_len = -1
!>        flag for number of components for Fourier transform
        integer(kind = kint) :: iflag_fft_comp = -1
      end type working_ISPACK3
!
!
      private :: dealloc_work_ispack3_t, dealloc_const_ispack3_t
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine finalize_wk_ispack3_t(WK)
!
      type(working_ISPACK3), intent(inout) :: WK
!
!
      call dealloc_const_ispack3_t(WK)
      call dealloc_work_ispack3_t(WK)
!
      end subroutine finalize_wk_ispack3_t
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_ispack3_t(Nsmp, Nstacksmp, Nfft, WK)
!
      use multi_pout_ISPACK3_smp
!
      integer(kind = kint_gl), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK3), intent(inout) :: WK
!
      integer(kind = kint) :: ip
!
!
      WK%Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        WK%Mmax_smp                                                     &
     &      = max(WK%Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      if( WK%iflag_fft_len .ne. Nfft) then
!
        if( WK%iflag_fft_len .lt. 0) then
          call alloc_const_ispack3_t(Nfft, WK)
        else if( Nfft .gt. WK%iflag_fft_comp ) then
          call dealloc_const_ispack3_t(WK)
          call alloc_const_ispack3_t(Nfft, WK)
        end if
!
        call FXRINI_kemo(Nfft, WK%IT_ispack, WK%T_ispack)
      end if
!
      if( WK%iflag_fft_comp .lt. 0) then
        call alloc_work_ispack3_t(Nsmp, WK%Mmax_smp, Nfft, WK)
      else if( (WK%Mmax_smp*Nfft) .gt. WK%iflag_fft_comp ) then
        call dealloc_work_ispack3_t(WK)
        call alloc_work_ispack3_t(Nsmp, WK%Mmax_smp, Nfft, WK)
      end if
!
      end subroutine verify_wk_ispack3_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_ispack3_t(Nsmp, nmax_comp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nfft, nmax_comp
      type(working_ISPACK3), intent(inout) :: WK
!
!
      WK%iflag_fft_comp = nmax_comp * Nfft
      allocate( WK%X_ispack(WK%iflag_fft_comp,Nsmp) )
!
      end subroutine alloc_work_ispack3_t
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_ispack3_t(nfft, WK)
!
      integer(kind = kint_gl), intent(in) :: nfft
      type(working_ISPACK3), intent(inout) :: WK
!
      WK%iflag_fft_len = nfft
      allocate( WK%iT_ispack(nfft/2) )
      allocate( WK%T_ispack(nfft+nfft/2) )
      WK%T_ispack = 0.0d0
      WK%iT_ispack = 0
!
      end subroutine alloc_const_ispack3_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_ispack3_t(WK)
!
      type(working_ISPACK3), intent(inout) :: WK
!
!
      deallocate(WK%X_ispack)
      WK%iflag_fft_comp = 0
!
      end subroutine dealloc_work_ispack3_t
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_ispack3_t(WK)
!
      type(working_ISPACK3), intent(inout) :: WK
!
!
      deallocate(WK%T_ispack, WK%iT_ispack)
      WK%iflag_fft_len = 0
!
      end subroutine dealloc_const_ispack3_t
!
! ------------------------------------------------------------------
!
      end module t_ispack3_FFT_wrapper
