!>@file   sph_prt_FFTW_selector.F90
!!@brief  module sph_prt_FFTW_selector
!!
!!@author H. Matsui
!!@date Programmed in MAy, 2026
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine sel_init_prt_FFTW_smp(id_rank, iflag_size,           &
!!     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,              &
!!     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!!      subroutine sel_finalize_sph_FFTW_smp(iflag_size,                &
!!     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!!      subroutine sel_verify_prt_FFTW_smp                              &
!!     &         (iflag_size, sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,  &
!!     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
!!        type(work_for_sgl_FFTW), intent(inout) :: sph_sgl_FFTW
!!        type(work_for_comp_FFTW), intent(inout) :: sph_comp_FFTW
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for FFTW with OpoenMP
!! ------------------------------------------------------------------
!!
!!      subroutine sel_prt_fwd_FFTW_to_send(iflag_size,                 &
!!     &          sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp, WS,        &
!!     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
!!        real(kind = kreal), intent(in)                                &
!!     &           :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!!        real(kind = kreal), intent(inout) :: WS(n_WS)
!!        type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
!!        type(work_for_comp_FFTW), intent(inout) :: sph_comp_FFTW
!!        type(work_for_sgl_FFTW), intent(inout) :: sph_sgl_FFTW
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sel_prt_bwd_FFTW_from_recv(iflag_size,               &
!!     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,        &
!!     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
!!        real(kind = kreal), intent(inout) :: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!!        type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
!!        type(work_for_comp_FFTW), intent(inout) ::  sph_comp_FFTW
!!        type(work_for_sgl_FFTW), intent(inout) ::   sph_sgl_FFTW
!!        logical, intent(inout) :: flag_FFT
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
!!@n @param id_rank     Procdess ID
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module sph_prt_FFTW_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector

      use t_sph_field_FFTW
      use t_sph_component_FFTW
      use t_sph_single_FFTW
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_init_prt_FFTW_smp(id_rank, iflag_size,             &
     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                &
     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!
      use sph_prt_FFTW
      use sph_prt_domain_FFTW
!
      integer, intent(in) :: id_rank
      integer(kind = kint) :: iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
      type(work_for_comp_FFTW), intent(inout) :: sph_comp_FFTW
      type(work_for_sgl_FFTW), intent(inout) :: sph_sgl_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(id_rank .eq. 0) write(*,*) 'Use prt FFTW'
        call init_prt_FFTW_smp(sph_rtp, comm_rtp,                       &
     &      ncomp_bwd, ncomp_fwd, sph_fld_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(id_rank .eq. 0) write(*,*) 'Use prt FFTW for domain'
        call init_prt_field_FFTW(sph_rtp, comm_rtp,                     &
     &                           sph_fld_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTW for all compontnent'
        call init_sph_component_FFTW(sph_rtp, ncomp_bwd, ncomp_fwd,     &
     &                               sph_comp_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(id_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_sph_single_FFTW(sph_rtp, sph_sgl_FFTW, flag_fft)
      end if
!
      end subroutine sel_init_prt_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine sel_finalize_sph_FFTW_smp(iflag_size,                  &
     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!
      use sph_prt_FFTW
      use sph_prt_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_size
!
      type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
      type(work_for_comp_FFTW), intent(inout) :: sph_comp_FFTW
      type(work_for_sgl_FFTW), intent(inout) :: sph_sgl_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if     ((iflag_size .eq. iflag_once_fft)                          &
     &   .or. (iflag_size .eq. iflag_domain_once))  then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_sph_field_FFTW(sph_fld_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW for all comps'
        call finalize_sph_component_FFTW(sph_comp_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_sph_single_FFTW(sph_sgl_FFTW, flag_fft)
      end if
!
      end subroutine sel_finalize_sph_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine sel_verify_prt_FFTW_smp                                &
     &         (iflag_size, sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,    &
     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!
      use sph_prt_FFTW
      use sph_prt_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
      type(work_for_sgl_FFTW), intent(inout) :: sph_sgl_FFTW
      type(work_for_comp_FFTW), intent(inout) :: sph_comp_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Use prt FFTW'
        call verify_prt_FFTW_smp(sph_rtp, comm_rtp,                     &
     &      ncomp_bwd, ncomp_fwd, sph_fld_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(iflag_debug .gt. 0) write(*,*) 'Use prt FFTW for field'
        call verify_prt_field_FFTW(sph_rtp, comm_rtp,                   &
     &                             sph_fld_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW for all comp.'
        call verify_sph_component_FFTW(sph_rtp, ncomp_bwd, ncomp_fwd,   &
     &                                 sph_comp_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTW'
        call verify_sph_single_FFTW(sph_rtp, sph_sgl_FFTW, flag_fft)
      end if
!
      end subroutine sel_verify_prt_FFTW_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_prt_fwd_FFTW_to_send(iflag_size,                   &
     &          sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp, WS,          &
     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!
      use sph_prt_FFTW
      use sph_prt_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real (kind=kreal), intent(in):: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
      type(work_for_comp_FFTW), intent(inout) :: sph_comp_FFTW
      type(work_for_sgl_FFTW), intent(inout) :: sph_sgl_FFTW
!
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        call prt_fwd_FFTW_smp_to_send(sph_rtp, comm_rtp, ncomp_fwd,     &
     &      n_WS, v_rtp(1,1), WS(1), sph_fld_FFTW, flag_FFT)
      else if(iflag_size .eq. iflag_domain_once) then
        call prt_field_fwd_FFTW_to_send(sph_rtp, ncomp_fwd, n_WS,       &
     &      v_rtp(1,1), WS(1), sph_fld_FFTW, flag_FFT)
      else if(iflag_size .eq. iflag_component_once) then
        call sph_comp_fwd_FFTW_to_send(sph_rtp, comm_rtp, ncomp_fwd,    &
     &      n_WS, v_rtp(1,1), WS(1), sph_comp_FFTW, flag_FFT)
      else if(iflag_size .eq. iflag_single_fft) then
        call sph_single_fwd_FFTW_to_send(sph_rtp, comm_rtp, ncomp_fwd,  &
     &      n_WS, v_rtp(1,1), WS(1), sph_sgl_FFTW, flag_FFT)
      end if
!
      end subroutine sel_prt_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sel_prt_bwd_FFTW_from_recv(iflag_size,                 &
     &          sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp,          &
     &          sph_fld_FFTW, sph_comp_FFTW, sph_sgl_FFTW, flag_FFT)
!
      use sph_prt_FFTW
      use sph_prt_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
!
      real(kind = kreal), intent(inout) :: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_field_FFTW), intent(inout) :: sph_fld_FFTW
      type(work_for_comp_FFTW), intent(inout) :: sph_comp_FFTW
      type(work_for_sgl_FFTW), intent(inout) :: sph_sgl_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        call prt_back_FFTW_smp_from_recv                                &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      sph_fld_FFTW, flag_FFT)
      else if(iflag_size .eq. iflag_domain_once) then
        call prt_field_back_FFTW_from_recv                              &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      sph_fld_FFTW, flag_FFT)
      else if(iflag_size .eq. iflag_component_once) then
        call sph_comp_back_FFTW_from_recv                               &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      sph_comp_FFTW, flag_FFT)
      else if(iflag_size .eq. iflag_single_fft) then
        call sph_single_back_FFTW_from_recv                             &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      sph_sgl_FFTW, flag_FFT)
      end if
!
      end subroutine sel_prt_bwd_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_prt_FFTW_selector
