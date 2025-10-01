!>@file   MHD_FFT_selector.F90
!!@brief  module MHD_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine init_MHD_FFT_select(id_rank, sph_rtp,                &
!!     &          ncomp, ncomp_fwd, ncomp_bwd, MHD_mul_FFTW)
!!      subroutine verify_MHD_FFT_select(id_rank, sph_rtp,              &
!!     &          ncomp, ncomp_fwd, ncomp_bwd, MHD_mul_FFTW)
!!      subroutine finalize_MHD_FFT_select(MHD_mul_FFTW)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine fwd_MHD_FFT_sel_to_send(sph_rtp, comm_rtp,           &
!!     &          ncomp_fwd, n_WS, fld_rtp, WS, WK_FFTs, MHD_mul_FFTW)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
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
!!      subroutine back_MHD_FFT_sel_from_recv(sph_rtp, comm_rtp,        &
!!     &         ncomp_bwd, n_WR, WR, fld_rtp, WK_FFTs, MHD_mul_FFTW)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
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
      module MHD_FFT_selector
!
      use m_precision
      use m_machine_parameter
      use t_sph_FFTPACK5
      use sph_rtp_FFTPACK5
      use sph_prt_FFTPACK5
      use t_sph_ISPACK_FFT
      use t_sph_ISPACK3_FFT
      use t_addresses_sph_transform
      use m_FFT_selector
!
#ifdef FFTW3
      use t_sph_single_FFTW
      use t_sph_field_FFTW
      use t_sph_multi_FFTW
      use sph_rtp_domain_FFTW
      use sph_prt_domain_FFTW
#endif
!
      use t_sph_single_FFTW
      use t_sph_FFT_selector
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_MHD_FFT_select(id_rank, sph_rtp,                  &
     &          ncomp, ncomp_fwd, ncomp_bwd, MHD_mul_FFTW)
!
      use t_spheric_rtp_data
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ncomp, ncomp_fwd, ncomp_bwd
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTW with prefixed recipi'
        call init_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd,           &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, MHD_mul_FFTW)
      end if
#endif
      return
!
      end subroutine init_MHD_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_MHD_FFT_select(MHD_mul_FFTW)
!
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_MHD_multi_FFTW(MHD_mul_FFTW)
      end if
#endif
      return
!
      end subroutine finalize_MHD_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_MHD_FFT_select(id_rank, sph_rtp,                &
     &          ncomp, ncomp_fwd, ncomp_bwd, MHD_mul_FFTW)
!
      use t_spheric_rtp_data
!
      integer, intent(in) :: id_rank
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp, ncomp_fwd, ncomp_bwd
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTW with prefixed recipi'
        call verify_MHD_multi_FFTW(ncomp, ncomp_fwd, ncomp_bwd,         &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, MHD_mul_FFTW)
      end if
#endif
      return
!
      end subroutine verify_MHD_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine fwd_MHD_FFT_sel_to_send(sph_rtp, comm_rtp,             &
     &          ncomp_fwd, n_WS, fld_rtp, WS, WK_FFTs, MHD_mul_FFTW)
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use swap_phi_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real (kind=kreal), intent(inout) :: WS(n_WS)
      real(kind=kreal), intent(inout)                                   &
     &                 :: fld_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!
!
      if(iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        call sph_FTTRUF_to_send(sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,     &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_fwd, n_WS,                 &
     &      comm_rtp%irev_sr, fld_rtp, WS(1), WK_FFTs%sph_ispack)
      else if(iflag_FFT .eq. iflag_ISPACK3_ONCE) then
        call sph_FXRTFA_to_send                                         &
     &    (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                          &
     &     sph_rtp%istack_rtp_rt_smp, ncomp_fwd, n_WS,                  &
     &     comm_rtp%irev_sr, fld_rtp, WS(1), WK_FFTs%sph_ISPACK3)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        call swap_phi_order_to_trans(ncomp_fwd,                         &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      MHD_mul_FFTW%v_tmp, fld_rtp)
        call MHD_multi_fwd_FFTW_to_send(ncomp_fwd, sph_rtp%nnod_rtp,    &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      n_WS, comm_rtp%irev_sr, fld_rtp, WS(1),  MHD_mul_FFTW)
        call swap_phi_order_from_trans(ncomp_fwd,                       &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      MHD_mul_FFTW%v_tmp, fld_rtp)
      else if(iflag_FFT .eq. iflag_FFTW_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_field_fwd_FFTW_to_send(sph_rtp, comm_rtp,            &
     &        ncomp_fwd, n_WS, fld_rtp, WS(1), WK_FFTs%sph_fld_FFTW)
        else
          call rtp_field_fwd_FFTW_to_send(sph_rtp, comm_rtp,            &
     &        ncomp_fwd, n_WS, fld_rtp, WS(1), WK_FFTs%sph_fld_FFTW)
        end if
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_fwd_FFTW_to_send(sph_rtp, comm_rtp,             &
     &      ncomp_fwd, n_WS, fld_rtp, WS(1), WK_FFTs%sph_sgl_FFTW)
#endif
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), &
     &                            WS(1), WK_FFTs%sph_FFTPACK)
        else
          call rtp_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), &
     &                            WS(1), WK_FFTs%sph_FFTPACK)
        end if
      end if
!
      end subroutine fwd_MHD_FFT_sel_to_send
!
! ------------------------------------------------------------------
!
      subroutine back_MHD_FFT_sel_from_recv(sph_rtp, comm_rtp,          &
     &         ncomp_bwd, n_WR, WR, fld_rtp, WK_FFTs, MHD_mul_FFTW)
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use swap_phi_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real(kind=kreal), intent(inout) :: WR(n_WR)
      real(kind=kreal), intent(inout)                                   &
     &                 :: fld_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!
!
      if(iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        call sph_FTTRUB_from_recv                                       &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_bwd, n_WR,                 &
     &      comm_rtp%irev_sr, WR(1), fld_rtp, WK_FFTs%sph_ispack)
      else if(iflag_FFT .eq. iflag_ISPACK3_ONCE) then
        call sph_FXRTBA_from_recv                                       &
     &    (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                          &
     &     sph_rtp%istack_rtp_rt_smp, ncomp_bwd, n_WR,                  &
     &     comm_rtp%irev_sr, WR(1), fld_rtp, WK_FFTs%sph_ispack3)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        call MHD_multi_back_FFTW_from_recv(ncomp_bwd, sph_rtp%nnod_rtp, &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      n_WR, comm_rtp%irev_sr, WR(1), fld_rtp, MHD_mul_FFTW)
        call swap_phi_order_from_trans(ncomp_bwd,                       &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      MHD_mul_FFTW%v_tmp, fld_rtp)
      else if(iflag_FFT .eq. iflag_FFTW_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_field_back_FFTW_from_recv(sph_rtp, comm_rtp,         &
     &        ncomp_bwd, n_WR, WR(1), fld_rtp, WK_FFTs%sph_fld_FFTW)
        else
          call rtp_field_back_FFTW_from_recv(sph_rtp, comm_rtp,         &
     &        ncomp_bwd, n_WR, WR(1), fld_rtp, WK_FFTs%sph_fld_FFTW)
        end if
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call sph_single_back_FFTW_from_recv(sph_rtp, comm_rtp,          &
     &      ncomp_bwd, n_WR, WR(1), fld_rtp, WK_FFTs%sph_sgl_FFTW)
#endif
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd, n_WR, &
     &         WR, v_rtp(1,1), WK_FFTs%sph_FFTPACK)
        else
          call rtp_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd, n_WR, &
     &        WR, v_rtp(1,1), WK_FFTs%sph_FFTPACK)
        end if
      end if
!
      end subroutine back_MHD_FFT_sel_from_recv
!
! ------------------------------------------------------------------
!
      end module MHD_FFT_selector
