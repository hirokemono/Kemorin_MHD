!>@file   t_sph_ISPACK0_selector.f90
!!@brief  module t_sph_ISPACK0_selector
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
!!
!!      subroutine sel_init_sph_ISPACK0(id_rank, iflag_size,            &
!!     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,              &
!!     &          WKs_ISPACK, flag_FFT)
!!      subroutine sel_finalize_sph_ISPACK0                             &
!!     &         (iflag_size, WKs_ISPACK, flag_FFT)
!!      subroutine sel_verify_sph_ISPACK0                               &
!!     &         (iflag_size, sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,  &
!!     &          WKs_ISPACK, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(works_sph_ispack), intent(inout) :: WKs_ISPACK
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sel_sph_fwd_ISPACK0_to_send(iflag_size, sph_rtp,     &
!!     &          ncomp_fwd, n_WS, v_rtp, WS, WKs_ISPACK, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
!!        real(kind = kreal), intent(in)                                &
!!     &             :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!!        real(kind = kreal), intent(inout):: WS(n_WS)
!!        type(works_sph_ispack), intent(inout) :: WKs_ISPACK
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
!!      subroutine sel_sph_bwd_ISPACK0_from_recv(iflag_size,            &
!!     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,        &
!!     &          WKs_ISPACK, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &                  :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!!        type(works_sph_ispack), intent(inout) :: WKs_ISPACK
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
      module t_sph_ISPACK0_selector
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_selector
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use t_sph_ISPACK_FFT
      use t_sph_domain_ISPACK_FFT
!
      implicit none
!
!>      Structure for work area of ISPACK
      type works_sph_ispack
!>        Structure to use ISPACK
        type(work_for_ispack) :: sph_ISPACK
!>        Structure to use ISPACK for domain
        type(work_for_domain_ispack) :: sph_domain_ISPACK
      end type works_sph_ispack
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_init_sph_ISPACK0(id_rank, iflag_size,              &
     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                &
     &          WKs_ISPACK, flag_FFT)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(works_sph_ispack), intent(inout) :: WKs_ISPACK
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V0.93'
        call init_sph_ISPACK(sph_rtp, comm_rtp,                         &
     &      ncomp_bwd, ncomp_fwd, WKs_ISPACK%sph_ISPACK, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V0.93 for domain'
        call init_sph_domain_ISPACK                                     &
     &     (sph_rtp, comm_rtp, WKs_ISPACK%sph_domain_ISPACK, flag_fft)
      end if
!
      end subroutine sel_init_sph_ISPACK0
!
! ------------------------------------------------------------------
!
      subroutine sel_finalize_sph_ISPACK0(iflag_size,                   &
     &                                    WKs_ISPACK, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
!
      type(works_sph_ispack), intent(inout) :: WKs_ISPACK
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK V0.93'
        call finalize_sph_ISPACK(WKs_ISPACK%sph_ISPACK, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'Finalize ISPACK V0.93 for domain'
        call finalize_sph_domain_ISPACK(WKs_ISPACK%sph_domain_ISPACK,   &
     &                                  flag_fft)
      end if
!
      end subroutine sel_finalize_sph_ISPACK0
!
! ------------------------------------------------------------------
!
      subroutine sel_verify_sph_ISPACK0(iflag_size, sph_rtp, comm_rtp,  &
     &          ncomp_bwd, ncomp_fwd, WKs_ISPACK, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(works_sph_ispack), intent(inout) :: WKs_ISPACK
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V0.93'
        call verify_sph_ISPACK(sph_rtp, comm_rtp,                       &
     &      ncomp_bwd, ncomp_fwd, WKs_ISPACK%sph_ISPACK, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V0.93 for domain'
        call verify_sph_domain_ISPACK                                   &
     &     (sph_rtp, comm_rtp, WKs_ISPACK%sph_domain_ISPACK, flag_fft)
      end if
!
      end subroutine sel_verify_sph_ISPACK0
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_sph_fwd_ISPACK0_to_send(iflag_size, sph_rtp,       &
     &          ncomp_fwd, n_WS, v_rtp, WS, WKs_ISPACK, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real(kind = kreal), intent(in)                                    &
     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      real(kind = kreal), intent(inout):: WS(n_WS)
      type(works_sph_ispack), intent(inout) :: WKs_ISPACK
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        call sph_FTTRUF_to_send(sph_rtp, ncomp_fwd,                     &
     &      n_WS, v_rtp(1,1), WS(1), WKs_ISPACK%sph_ISPACK, flag_FFT)
      else if(iflag_size .eq. iflag_domain_once) then
        call sph_domain_FTTRUF_to_send                                  &
     &     (sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                &
     &      WKs_ISPACK%sph_domain_ISPACK, flag_FFT)
      end if
!
      end subroutine sel_sph_fwd_ISPACK0_to_send
!
! ------------------------------------------------------------------
!
      subroutine sel_sph_bwd_ISPACK0_from_recv(iflag_size,              &
     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,          &
     &          WKs_ISPACK, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(works_sph_ispack), intent(inout) :: WKs_ISPACK
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        call sph_FTTRUB_from_recv(sph_rtp, comm_rtp, ncomp_bwd,         &
     &      n_WR, WR(1), v_rtp(1,1), WKs_ISPACK%sph_ISPACK, flag_FFT)
      else if(iflag_size .eq. iflag_domain_once) then
        call sph_domain_FTTRUB_from_recv                                &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WKs_ISPACK%sph_domain_ISPACK, flag_FFT)
      end if
!
      end subroutine sel_sph_bwd_ISPACK0_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_ISPACK0_selector
