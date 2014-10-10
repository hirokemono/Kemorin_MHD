!>@file   FFT_selector.f90
!!@brief  module FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine set_fft_library_ctl(FFT_library_ctl)
!!
!!      subroutine initialize_FFT_select(my_rank, Nsmp, Nstacksmp, Nfft)
!!      subroutine finalize_FFT_select(Nsmp, Nstacksmp)
!!      subroutine verify_FFT_select(Nsmp, Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine forward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine backward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for backward FFT
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param my_rank     Procdess ID
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module FFT_selector
!
      use m_precision
      use m_machine_parameter
      use m_FFTPACK5_wrapper
      use m_ispack_FFT_wrapper
!
#ifdef FFTW3
      use m_FFTW_wrapper
      use m_multi_FFTW_wrapper
#endif
!
      implicit none
!
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter :: hd_FFTPACK = 'fftpack'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW =     'fftw'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW3 =    'fftw3'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW_S =  'fftw_single'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW3_S = 'fftw3_single'
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK =  'ispack'
!
!>      integer flag for undefined
      integer(kind = kint), parameter :: iflag_UNDEFINED_FFT =   0
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =     1
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW =        2
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_SINGLE = 3
!>      integer flag to use ISPACK
      integer(kind = kint), parameter :: iflag_ISPACK =      4
!
      integer(kind = kint) :: iflag_FFT = iflag_UNDEFINED_FFT
!
      private :: hd_FFTPACK, hd_FFTW, hd_FFTW3, hd_FFTW3_S
      private :: hd_ISPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine set_fft_library_ctl(FFT_library_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: FFT_library_ctl
!
!
      if     (cmp_no_case(FFT_library_ctl, hd_FFTPACK) .gt. 0) then
        iflag_FFT = iflag_FFTPACK
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK) .gt. 0) then
        iflag_FFT = iflag_ISPACK
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW) .gt. 0              &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3) .gt. 0) then
        iflag_FFT = iflag_FFTW
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_S) .gt. 0            &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_S) .gt. 0)        &
     &   then
        iflag_FFT = iflag_FFTW_SINGLE
      end if
!
      end subroutine set_fft_library_ctl
!
! ------------------------------------------------------------------
!
      subroutine initialize_FFT_select(my_rank, Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  my_rank, Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        if(my_rank .eq. 0) write(*,*) 'Use ISPACK'
        call init_4_ispack(Nsmp, Nstacksmp, Nfft)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(my_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_FFTW_mul(Nsmp, Nstacksmp, Nfft)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(my_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_4_FFTW(Nsmp, Nstacksmp, Nfft)
#endif
      else
        if(my_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call init_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
      end if
!
!
      end subroutine initialize_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFT_select(Nsmp, Nstacksmp)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK'
        call finalize_4_ispack
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_FFTW_mul(Nsmp)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_4_FFTW(Nsmp, Nstacksmp)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_4_FFTPACK
      end if
!
      end subroutine finalize_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_FFT_select(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK'
        call verify_work_4_ispack(Nsmp, Nstacksmp, Nfft)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_work_FFTW_mul(Nsmp, Nstacksmp, Nfft)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single transforms in FFTW'
        call verify_work_4_FFTW(Nsmp, Nstacksmp, Nfft)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call verify_work_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
      end if
!
      end subroutine verify_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine forward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        call FTTRUF_kemo(Nsmp, Nstacksmp, M, Nfft, X)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_mul_forward(Nsmp, Nstacksmp, M, Nfft, X)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call FFTW_forward(Nsmp, Nstacksmp, M, Nfft, X)
#endif
      else
        call CALYPSO_RFFTMF(Nsmp, Nstacksmp, M, Nfft, X)
      end if
!
      end subroutine forward_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine backward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        call FTTRUB_kemo(Nsmp, Nstacksmp, M, Nfft, X)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_mul_backward(Nsmp, Nstacksmp, M, Nfft, X)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call FFTW_backward(Nsmp, Nstacksmp, M, Nfft, X)
#endif
      else
        call CALYPSO_RFFTMB(Nsmp, Nstacksmp, M, Nfft, X)
      end if
!
      end subroutine backward_FFT_select
!
! ------------------------------------------------------------------
!
      end module FFT_selector
