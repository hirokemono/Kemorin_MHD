!>@file   t_ispack_FFT_wrapper.f90
!!@brief  module t_ispack_FFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed on Apr., 2013
!
!
!>@brief  Fourier transform with work structures for ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine FTTRUI_kemo(Nfft, IT_ispack, T_ispack)
!!        integer(kind = kint), intent(in) :: Nfft
!!        integer(kind = 4), intent(inout) :: IT_ispack(5)
!!        real(kind = 8), intent(inout) :: T_ispack(itwo*Nfft)
!!
!!      subroutine finalize_wk_ispack_t(WK)
!!      subroutine verify_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!!      subroutine alloc_work_ispack_t(Nsmp, nmax_comp, Nfft, WK)
!!      subroutine alloc_const_ispack_t(Nsmp, nfft, WK)
!!        integer(kind = kint), intent(in) :: Nsmp, nmax_comp, Nfft
!!        type(working_ISPACK), intent(inout) :: WK
!!
!!      subroutine count_ispack_smp(Nsmp, Nstacksmp, WK)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        type(working_ISPACK), intent(inout) :: WK
!!
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
!!@n @param WK          Work structure for ISPACK
!
      module t_ispack_FFT_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      structure for working data for ISPACK
      type working_ISPACK
!>        Number of FFT call (generally number of SMP threads)
        integer(kind = kint) :: Nplan_ISPACK = 1
!>        Stack for FFT
        integer(kind = kint), allocatable :: istack_ISPACK(:)
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X_ispack(:,:)
!>        Work area for ISPACK
        integer(kind = 4) :: IT_ispack(5)
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T_ispack(:)
!>        Work area for ISPACK
        real(kind = 8), allocatable :: WORK_ispack(:,:)
!>        flag for length of Fourier transform
        integer(kind = kint_gl) :: iflag_fft_len = -1
!>        flag for number of components for Fourier transform
        integer(kind = kint_gl) :: iflag_fft_comp = -1
      end type working_ISPACK
!
      private :: dealloc_work_ispack_t, dealloc_const_ispack_t
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine FTTRUI_kemo(Nfft, IT_ispack, T_ispack)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = 4), intent(inout) :: IT_ispack(5)
      real(kind = 8), intent(inout) :: T_ispack(itwo*Nfft)
!
!
      call FTTRUI( Nfft, IT_ispack, T_ispack(1) )
!
      end subroutine FTTRUI_kemo
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine finalize_wk_ispack_t(WK)
!
      type(working_ISPACK), intent(inout) :: WK
!
!
      call dealloc_const_ispack_t(WK)
      call dealloc_work_ispack_t(WK)
!
      end subroutine finalize_wk_ispack_t
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK), intent(inout) :: WK
!
!
      if(WK%iflag_fft_len .ne. Nfft) then
!
        if(WK%iflag_fft_len .lt. 0) then
          call alloc_const_ispack_t(Nsmp, Nfft, WK)
        else if( Nfft .gt. WK%iflag_fft_comp ) then
          call dealloc_const_ispack_t(WK)
          call alloc_const_ispack_t(Nsmp, Nfft, WK)
        end if
        call count_ispack_smp(Nsmp, Nstacksmp, WK)
!
        call FTTRUI_kemo( Nfft, WK%IT_ispack, WK%T_ispack )
      end if
!
      if(WK%iflag_fft_comp .lt. 0) then
        call alloc_work_ispack_t(Nsmp, WK%Mmax_smp, Nfft, WK)
      else if( (WK%Mmax_smp*Nfft) .gt. WK%iflag_fft_comp ) then
        call dealloc_work_ispack_t(WK)
        call alloc_work_ispack_t(Nsmp, WK%Mmax_smp, Nfft, WK)
      end if
!
      end subroutine verify_wk_ispack_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_ispack_t(Nsmp, nmax_comp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, nmax_comp, Nfft
      type(working_ISPACK), intent(inout) :: WK
!
!
      WK%iflag_fft_comp = nmax_comp * Nfft
      allocate(WK%X_ispack(WK%iflag_fft_comp,Nsmp))
      allocate(WK%WORK_ispack(WK%iflag_fft_comp,Nsmp))
      WK%WORK_ispack = 0.0d0
!
      end subroutine alloc_work_ispack_t
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_ispack_t(Nsmp, nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, nfft
      type(working_ISPACK), intent(inout) :: WK
!
      WK%Nplan_ISPACK = Nsmp
      WK%iflag_fft_len = nfft
!
      allocate(WK%istack_ISPACK(0:Nsmp))
      WK%istack_ISPACK(0:Nsmp) = 0
!
      allocate(WK%T_ispack(2*nfft))
      WK%T_ispack = 0.0d0
!
      end subroutine alloc_const_ispack_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_ispack_t(WK)
!
      type(working_ISPACK), intent(inout) :: WK
!
!
      deallocate(WK%X_ispack, WK%WORK_ispack )
      WK%iflag_fft_comp = 0
!
      end subroutine dealloc_work_ispack_t
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_ispack_t(WK)
!
      type(working_ISPACK), intent(inout) :: WK
!
!
      deallocate( WK%T_ispack )
      WK%iflag_fft_len = 0
!
      end subroutine dealloc_const_ispack_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine count_ispack_smp(Nsmp, Nstacksmp, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      type(working_ISPACK), intent(inout) :: WK
!
      integer(kind = kint) :: ip
!
      WK%istack_ISPACK(0:Nsmp) = Nstacksmp(0:Nsmp)
      WK%Mmax_smp = 0
      do ip = 1, Nsmp
        WK%Mmax_smp                                                     &
     &        = max(WK%Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)))
      end do
!
      end subroutine count_ispack_smp
!
! ------------------------------------------------------------------
!
      end module t_ispack_FFT_wrapper
