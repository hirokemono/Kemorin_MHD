!>@file   m_ispack_FFT_wrapper.f90
!!@brief  module m_ispack_FFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Fourier transform using ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_4_ispack(Nsmp, Nstacksmp, Nfft)
!!      subroutine finalize_4_ispack
!!      subroutine verify_work_4_ispack(Nsmp, Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine FTTRUF_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine FTTRUB_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ISPACK
!!
!! x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!! (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
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
!
      module m_ispack_FFT_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Maximum nuber of components for each SMP process
      integer(kind = kint) :: Mmax_smp
!>      Data for multiple Fourier transform
      real(kind = 8), allocatable :: X_ispack(:,:)
!>      Work area for ISPACK
      integer(kind = 4) :: IT_ispack(5)
!>      Work constants for ISPACK
      real(kind = 8), allocatable :: T_ispack(:)
!>      Work area for ISPACK
      real(kind = 8), allocatable :: WORK_ispack(:,:)
!>      flag for length of Fourier transform
      integer(kind = kint) :: iflag_fft_len = -1
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_comp = -1
!
      private :: Mmax_smp, X_ispack
      private :: IT_ispack, T_ispack, WORK_ispack
      private :: iflag_fft_len, iflag_fft_comp
!
      private :: allocate_work_4_ispack, allocate_const_4_ispack
      private :: deallocate_work_4_ispack, deallocate_const_4_ispack
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_4_ispack(Nsmp, Nstacksmp, Nfft)
!
      use ispack_FFT_wrapper
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      integer(kind = kint) :: ip
!
!
      Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        Mmax_smp = max(Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      call allocate_const_4_ispack(Nfft)
      call FTTRUI_kemo( Nfft, IT_ispack, T_ispack(1) )
!
      call allocate_work_4_ispack(Nsmp, Nfft)
!
      end subroutine init_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine finalize_4_ispack
!
!
      call deallocate_const_4_ispack
      call deallocate_work_4_ispack
!
      end subroutine finalize_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine verify_work_4_ispack(Nsmp, Nstacksmp, Nfft)
!
      use ispack_FFT_wrapper
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      integer(kind = kint) :: ip
!
!
      Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        Mmax_smp = max(Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      if( iflag_fft_len .ne. Nfft) then
!
        if( iflag_fft_len .lt. 0) then
          call allocate_const_4_ispack(Nfft)
        else if( Nfft .gt. iflag_fft_comp ) then
          call deallocate_const_4_ispack
          call allocate_const_4_ispack(Nfft)
        end if
!
        call FTTRUI_kemo( Nfft, IT_ispack, T_ispack(1) )
      end if
!
      if( iflag_fft_comp .lt. 0) then
        call allocate_work_4_ispack(Nsmp, Nfft)
      else if( (Mmax_smp*Nfft) .gt. iflag_fft_comp ) then
        call deallocate_work_4_ispack
        call allocate_work_4_ispack(Nsmp, Nfft)
      end if
!
      end subroutine verify_work_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FTTRUF_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!
      use ispack_FFT_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
!
      call FTTRUF_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,                 &
     &    X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack)
!
      end subroutine FTTRUF_kemo
!
! ------------------------------------------------------------------
!
      subroutine FTTRUB_kemo(Nsmp, Nstacksmp, M, Nfft, X)
!
      use ispack_FFT_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
!
      call FTTRUB_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,                 &
     &    X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack)
!
      end subroutine FTTRUB_kemo
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_work_4_ispack(Nsmp, Nfft)
!
      integer(kind = kint), intent(in) :: Nsmp, Nfft
!
!
      iflag_fft_comp = Mmax_smp*Nfft
      allocate( X_ispack(iflag_fft_comp,Nsmp) )
      allocate( WORK_ispack(iflag_fft_comp,Nsmp) )
      WORK_ispack = 0.0d0
!
      end subroutine allocate_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine allocate_const_4_ispack(nfft)
!
      integer(kind = kint), intent(in) :: nfft
!
      iflag_fft_len = nfft
      allocate( T_ispack(2*nfft) )
      T_ispack = 0.0d0
!
      end subroutine allocate_const_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine deallocate_work_4_ispack
!
      deallocate(X_ispack, WORK_ispack )
      iflag_fft_comp = 0
!
      end subroutine deallocate_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine deallocate_const_4_ispack
!
      deallocate( T_ispack )
      iflag_fft_len = 0
!
      end subroutine deallocate_const_4_ispack
!
! ------------------------------------------------------------------
!
      end module m_ispack_FFT_wrapper
