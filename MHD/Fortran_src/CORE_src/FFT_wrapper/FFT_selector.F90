!
!      module FFT_selector
!
!
!      subroutine initialize_FFT_select(Nsmp, Nstacksmp, Nfft)
!      subroutine verify_FFT_select(Nsmp, Nstacksmp, Nfft)
! ------------------------------------------------------------------
!   wrapper subroutine for initierize FFT for ISPACK
! ------------------------------------------------------------------
!
!      subroutine forward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
! ------------------------------------------------------------------
!
!   wrapper subroutine for FFT in ISPACK
!
!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!
!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!    K = Nfft/2....
!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!
! ------------------------------------------------------------------
!
!      subroutine backward_FFT_select(Nsmp, Nstacksmp, M, Nfft, X)
! ------------------------------------------------------------------
!
!   wrapper subroutine for inverse FFT for ISPACK
!
!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!
! ------------------------------------------------------------------
!
!       i = 1:     a_{0}
!       i = 2:     a_{Nfft/2}
!       i = 3:     a_{1}
!       i = 4:     b_{1}
!       ...
!       i = 2*k+1: a_{k}
!       i = 2*k+2: b_{k}
!       ...
!       i = Nfft-1:   a_{Nfft/2-1}
!       i = Nfft:     b_{Nfft/2-1}
!
! ------------------------------------------------------------------
!
      module FFT_selector
!
      use m_precision
      use m_machine_parameter
      use FFTPACK5_wrapper
      use ispack_FFT_wrapper
!
      use FFTW_wrapper
!      use FFTW_kemo_wrapper
!
      implicit none
!
      integer(kind = kint), parameter :: iflag_ISPACK =    0
      integer(kind = kint), parameter :: iflag_FFTPACK =   1
      integer(kind = kint), parameter :: iflag_FFTW =      2
!      integer(kind = kint), parameter :: iflag_FFTW = 3
!
      integer(kind = kint) :: iflag_FFT = iflag_ISPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine initialize_FFT_select(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK'
        call init_4_ispack(Nsmp, Nstacksmp, Nfft)
#ifdef FFTW3
!      else if(iflag_FFT .eq. iflag_FFTW) then
!        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW by kemo_wrapper'
!        call init_4_FFTW_kemo(Nsmp, Nstacksmp, Nfft)
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call init_4_FFTW(Nsmp, Nstacksmp, Nfft)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call init_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
      end if
!
!
      end subroutine initialize_FFT_select
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
!      else if(iflag_FFT .eq. iflag_FFTW) then
!        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW by kemo_wrapper'
!        call verify_work_4_FFTW_kemo(Nsmp, Nstacksmp, Nfft)
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
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
!      else if(iflag_FFT .eq. iflag_FFTW) then
!        call FFTW_forward_kemo(Nsmp, Nstacksmp, M, Nfft, X)
      else if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_forward(Nsmp, Nstacksmp, M, Nfft, X)
#endif
      else
        call RFFTMF_norm(Nsmp, Nstacksmp, M, Nfft, X)
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
!      else if(iflag_FFT .eq. iflag_FFTW) then
!        call FFTW_backward_kemo(Nsmp, Nstacksmp, M, Nfft, X)
      else if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_backward(Nsmp, Nstacksmp, M, Nfft, X)
#endif
      else
        call RFFTMB_norm(Nsmp, Nstacksmp, M, Nfft, X)
      end if
!
      end subroutine backward_FFT_select
!
! ------------------------------------------------------------------
!
      end module FFT_selector
