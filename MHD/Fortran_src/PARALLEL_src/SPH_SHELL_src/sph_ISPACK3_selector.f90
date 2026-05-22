!>@file   sph_ISPACK3_selector.f90
!!@brief  module sph_ISPACK3_selector
!!
!!@author H. Matsui
!!@date Programmed in 2026
!
!
!>@brief  Fourier transform selector for spherical harmonic transform
!!@n      using ISPACK3
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!      subroutine sel_sph_fwd_ISPACK3_to_send(iflag_size,              &
!!     &          sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp, WS,        &
!!     &          WKs_ISPACK3, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
!!        real(kind = kreal), intent(in)                                &
!!     &             :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!!        real(kind = kreal), intent(inout):: WS(n_WS)
!!        type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sel_sph_bwd_ISPACK3_from_recv(iflag_size,            &
!!     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,        &
!!     &          WKs_ISPACK3, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &                  :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!!        type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
!!        logical, intent(inout) :: flag_FFT
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
!
      module sph_ISPACK3_selector
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_selector
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use t_sph_ISPACK3_selector
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_sph_fwd_ISPACK3_to_send(iflag_size,                &
     &          sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp, WS,          &
     &          WKs_ISPACK3, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real(kind = kreal), intent(in)                                    &
     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
      real(kind = kreal), intent(inout):: WS(n_WS)
      type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        call sph_FXRTFA_to_send                                         &
     &     (sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                &
     &      WKs_ISPACK3%sph_full_ISPACK3, flag_FFT)
      else if(iflag_size .eq. iflag_domain_once) then
        call sph_domain_FXRTFA_to_send                                  &
     &     (sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                &
     &      WKs_ISPACK3%sph_domain_ispack3, flag_FFT)
      else if(iflag_size .eq. iflag_component_once) then
        call sph_comp_FXRTFA_to_send                                    &
     &     (sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WKs_ISPACK3%sph_comp_ispack3, flag_FFT)
      else if(iflag_size .eq. iflag_single_fft) then
        call sph_single_FXRTFA_to_send                                  &
     &     (sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WKs_ISPACK3%sph_sgl_ispack3, flag_FFT)
      end if
!
      end subroutine sel_sph_fwd_ISPACK3_to_send
!
! ------------------------------------------------------------------
!
      subroutine sel_sph_bwd_ISPACK3_from_recv(iflag_size,              &
     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,          &
     &          WKs_ISPACK3, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                  :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        call sph_FXRTBA_from_recv                                       &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WKs_ISPACK3%sph_full_ISPACK3, flag_FFT)
      else if(iflag_size .eq. iflag_domain_once) then
        call sph_domain_FXRTBA_from_recv                                &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WKs_ISPACK3%sph_domain_ispack3, flag_FFT)
      else if(iflag_size .eq. iflag_component_once) then
        call sph_comp_FXRTBA_from_recv                                  &
     &     (sph_rtp,comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),       &
     &      WKs_ISPACK3%sph_comp_ispack3, flag_FFT)
      else if(iflag_size .eq. iflag_single_fft) then
        call sph_single_FXRTBA_from_recv                                &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WKs_ISPACK3%sph_sgl_ispack3, flag_FFT)
      end if
!
      end subroutine sel_sph_bwd_ISPACK3_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_ISPACK3_selector
