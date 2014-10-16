!>@file   sph_FFT_selector.F90
!!@brief  module sph_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine init_sph_FFT_select(my_rank, ncomp)
!!      subroutine finalize_sph_FFT_select
!!      subroutine verify_sph_FFT_select(ncomp)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine fwd_FFT_select_to_send(ncomp, n_WS, vr_rtp, WS)
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
!!      subroutine back_FFT_select_from_recv(ncomp, n_WR, WR, vr_rtp)
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
      module sph_FFT_selector
!
      use m_precision
      use m_machine_parameter
      use m_sph_FFTPACK5
      use m_sph_ISPACK_FFT
      use FFT_selector
!
#ifdef FFTW3
      use m_sph_single_FFTW
      use m_sph_multi_FFTW
#endif
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFT_select(my_rank, ncomp)
!
      integer(kind = kint), intent(in) ::  my_rank, ncomp
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        if(my_rank .eq. 0) write(*,*) 'Use ISPACK'
        call init_sph_ISPACK(ncomp)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(my_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_sph_multi_FFTW
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(my_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_sph_single_FFTW
#endif
      else
        if(my_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call init_sph_FFTPACK5(ncomp)
      end if
!
      end subroutine init_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFT_select
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK'
        call finalize_sph_ISPACK
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_sph_multi_FFTW
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_sph_single_FFTW
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_sph_FFTPACK5
      end if
!
      end subroutine finalize_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFT_select(ncomp)
!
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) ::  ncomp
      integer(kind = kint) :: Nstacksmp(0:np_smp)
!
!
      Nstacksmp(0:np_smp) = ncomp*irt_rtp_smp_stack(0:np_smp)
      if(iflag_FFT .eq. iflag_ISPACK) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK'
        call verify_sph_ISPACK(ncomp)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_sph_multi_FFTW
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single transforms in FFTW'
        call verify_sph_single_FFTW
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call verify_sph_FFTPACK5(ncomp)
      end if
!
      end subroutine verify_sph_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine fwd_FFT_select_to_send(ncomp, n_WS, vr_rtp, WS)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ncomp, n_WS
      real (kind=kreal), intent(inout):: vr_rtp(nnod_rtp,ncomp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        call sph_FTTRUF_to_send                                         &
     &     (ncomp, n_WS, irev_sr_rtp, vr_rtp(1,1), WS(1))
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        call sph_multi_fwd_FFTW_to_send                                 &
     &     (ncomp, n_WS, irev_sr_rtp, vr_rtp(1,1), WS(1))
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_fwd_FFTW_to_send                                &
     &     (ncomp, n_WS, irev_sr_rtp, vr_rtp(1,1), WS(1))
#endif
      else
        call sph_RFFTMF_to_send                                         &
     &     (ncomp, n_WS, irev_sr_rtp, vr_rtp(1,1), WS(1))
      end if
!
      end subroutine fwd_FFT_select_to_send
!
! ------------------------------------------------------------------
!
      subroutine back_FFT_select_from_recv(ncomp, n_WR, WR, vr_rtp)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ncomp, n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: vr_rtp(nnod_rtp,ncomp)
!
!
      if(iflag_FFT .eq. iflag_ISPACK) then
        call sph_FTTRUB_from_recv                                       &
     &    (ncomp, n_WR, irev_sr_rtp, WR(1), vr_rtp(1,1))
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW) then
        call sph_multi_back_FFTW_from_recv                              &
     &    (ncomp, n_WR, irev_sr_rtp, WR(1), vr_rtp(1,1))
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_back_FFTW_from_recv                             &
     &    (ncomp, n_WR, irev_sr_rtp, WR(1), vr_rtp(1,1))
#endif
      else
        call sph_RFFTMB_from_recv                                       &
     &     (ncomp, n_WR, irev_sr_rtp, WR, vr_rtp(1,1))
      end if
!
      end subroutine back_FFT_select_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_FFT_selector
