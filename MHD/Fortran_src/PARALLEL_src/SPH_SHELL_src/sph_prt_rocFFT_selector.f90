!>@file   sph_prt_rocFFT_selector.F90
!!@brief  module sph_prt_rocFFT_selector
!!
!!@author H. Matsui
!!@date Programmed in May, 2026
!
!>@brief  Selector of rocFFT routines
!!
!!@verbatim
!!      subroutine sel_init_prt_rocFFT                                  &
!!     &         (id_rank, iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,&
!!     &          ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
!!      subroutine sel_verify_prt_rocFFT                                &
!!     &         (iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,         &
!!     &          ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_field_rocFFT), intent(inout) :: sph_rocFFT
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sel_prt_fwd_rocFFT_to_send                           &
!!     &         (iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,         &
!!     &          ncomp_fwd, n_WS, v_rtp, WS, sph_rocFFT, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!!        real(kind = kreal), intent(inout):: WS(n_WS)
!!        type(work_for_field_rocFFT), intent(inout) :: sph_rocFFT
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
!!      subroutine sel_prt_bwd_rocFFT_from_recv                         &
!!     &         (iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,         &
!!     &          ncomp_bwd, n_WR, WR, v_rtp, sph_rocFFT, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!!        type(work_for_field_rocFFT), intent(inout) :: sph_rocFFT
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
      module sph_prt_rocFFT_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector
!
      use t_sph_field_rocFFT
!
      implicit none
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_init_prt_rocFFT                                    &
     &         (id_rank, iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,  &
     &          ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
!
      use sph_field_real_rocFFT
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_field_rocFFT), intent(inout) :: sph_rocFFT
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if     ((iflag_sph_FFT .eq. iflag_real_rocFFT)                  &
     &     .or. (iflag_sph_FFT .eq. iflag_OMP_rocFFT)) then
          call init_prt_real_rocFFT(sph_rtp, comm_rtp,                  &
     &        ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          call init_prt_complex_rocFFT(sph_rtp, comm_rtp,               &
     &        ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
        end if
!
      else if(iflag_size .eq. iflag_domain_once) then
        if     ((iflag_sph_FFT .eq. iflag_real_rocFFT)                  &
     &     .or. (iflag_sph_FFT .eq. iflag_OMP_rocFFT)) then
          if(id_rank .eq. 0) write(*,*) 'Use prt real rocFFT'
          call init_prt_real_rocFFT(sph_rtp, comm_rtp,                  &
     &        ione, ione, sph_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          if(id_rank .eq. 0) write(*,*) 'Use prt complex rocFFT'
          call init_prt_complex_rocFFT(sph_rtp, comm_rtp,               &
     &        ione, ione, sph_rocFFT, flag_FFT)
        end if
      end if
!
      end subroutine sel_init_prt_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine sel_verify_prt_rocFFT                                  &
     &         (iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,           &
     &          ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
!
      use sph_field_real_rocFFT
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_field_rocFFT), intent(inout) :: sph_rocFFT
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if     ((iflag_sph_FFT .eq. iflag_real_rocFFT)                  &
     &     .or. (iflag_sph_FFT .eq. iflag_OMP_rocFFT)) then
          call verify_prt_real_rocFFT(sph_rtp, comm_rtp,                &
     &        ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          call verify_prt_complex_rocFFT(sph_rtp, comm_rtp,             &
     &        ncomp_bwd, ncomp_fwd, sph_rocFFT, flag_FFT)
        end if
!
      else if(iflag_size .eq. iflag_domain_once) then
        if     ((iflag_sph_FFT .eq. iflag_real_rocFFT)                  &
     &     .or. (iflag_sph_FFT .eq. iflag_OMP_rocFFT)) then
          call verify_prt_real_rocFFT(sph_rtp, comm_rtp,                &
     &        ione, ione, sph_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          call verify_prt_complex_rocFFT(sph_rtp, comm_rtp,             &
     &        ione, ione, sph_rocFFT, flag_FFT)
        end if
      end if
!
      end subroutine sel_verify_prt_rocFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_prt_fwd_rocFFT_to_send                             &
     &         (iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,           &
     &          ncomp_fwd, n_WS, v_rtp, WS, sph_rocFFT, flag_FFT)
!
      use sph_prt_complex_rocFFT
      use sph_prt_real_rocFFT
      use sph_prt_OpenMP_rocFFT
      use sph_prt_domain_cplx_rocFFT
      use sph_prt_domain_real_rocFFT
      use sph_prt_domain_OMP_rocFFT
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real(kind = kreal), intent(in)                                    &
     &                   :: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      real(kind = kreal), intent(inout):: WS(n_WS)
      type(work_for_field_rocFFT), intent(inout) :: sph_rocFFT
!
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if     (iflag_sph_FFT .eq. iflag_real_rocFFT) then
          call prt_fwd_real_rocFFT_to_send(sph_rtp,                     &
     &        sph_rocFFT%comm_sph_rocFFT, sph_rocFFT%rocFFT_fwd,        &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                       &
     &        sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_OMP_rocFFT) then
          call prt_fwd_OMP_rocFFT_to_send(sph_rtp,                      &
     &        sph_rocFFT%comm_sph_rocFFT, sph_rocFFT%rocFFT_fwd,        &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                       &
     &        sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          call prt_fwd_cplx_rocFFT_to_send(sph_rtp,                     &
     &        sph_rocFFT%comm_sph_rocFFT, sph_rocFFT%rocFFT_fwd,        &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                       &
     &        sph_rocFFT%WK_rocFFT, flag_FFT)
        end if
!
      else if(iflag_size .eq. iflag_domain_once) then
        if     (iflag_sph_FFT .eq. iflag_real_rocFFT) then
          call prt_dmn_fwd_real_rocFFT_to_send(sph_rtp,                 &
     &        sph_rocFFT%comm_sph_rocFFT, sph_rocFFT%rocFFT_fwd,        &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                       &
     &        sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_OMP_rocFFT) then
          call prt_dmn_fwd_OMP_rocFFT_to_send(sph_rtp,                  &
     &        sph_rocFFT%comm_sph_rocFFT, sph_rocFFT%rocFFT_fwd,        &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                       &
     &        sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          call prt_dmn_fwd_cplx_rocFFT_to_send(sph_rtp,                 &
     &        sph_rocFFT%comm_sph_rocFFT, sph_rocFFT%rocFFT_fwd,        &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                       &
     &        sph_rocFFT%WK_rocFFT, flag_FFT)
        end if
      end if
!
      end subroutine sel_prt_fwd_rocFFT_to_send
!
! ------------------------------------------------------------------
!
      subroutine sel_prt_bwd_rocFFT_from_recv                           &
     &         (iflag_sph_FFT, iflag_size, sph_rtp, comm_rtp,           &
     &          ncomp_bwd, n_WR, WR, v_rtp, sph_rocFFT, flag_FFT)
!
      use sph_prt_complex_rocFFT
      use sph_prt_real_rocFFT
      use sph_prt_OpenMP_rocFFT
      use sph_prt_domain_cplx_rocFFT
      use sph_prt_domain_real_rocFFT
      use sph_prt_domain_OMP_rocFFT
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                  :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_field_rocFFT), intent(inout) :: sph_rocFFT
!
      logical, intent(inout) :: flag_FFT
!
      if     (iflag_size .eq. iflag_once_fft) then
        if     (iflag_sph_FFT .eq. iflag_real_rocFFT) then
          call prt_bwd_real_rocFFT_from_recv                            &
     &       (sph_rtp, comm_rtp, sph_rocFFT%rocFFT_bwd, ncomp_bwd,      &
     &        n_WR, WR(1), v_rtp(1,1), sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_OMP_rocFFT) then
          call prt_bwd_OMP_rocFFT_from_recv                             &
     &       (sph_rtp, comm_rtp, sph_rocFFT%rocFFT_bwd, ncomp_bwd,      &
     &        n_WR, WR(1), v_rtp(1,1), sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          call prt_bwd_cplx_rocFFT_from_recv                            &
     &       (sph_rtp, comm_rtp, sph_rocFFT%rocFFT_bwd, ncomp_bwd,      &
     &        n_WR, WR(1), v_rtp(1,1), sph_rocFFT%WK_rocFFT, flag_FFT)
        end if
!
      else if(iflag_size .eq. iflag_domain_once) then
        if     (iflag_sph_FFT .eq. iflag_real_rocFFT) then
          call prt_dmn_bwd_real_rocFFT_fm_recv                          &
     &       (sph_rtp, comm_rtp, sph_rocFFT%rocFFT_bwd, ncomp_bwd,      &
     &        n_WR, WR(1), v_rtp(1,1), sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_OMP_rocFFT) then
          call prt_dmn_bwd_OMP_rocFFT_fm_recv                           &
     &       (sph_rtp, comm_rtp, sph_rocFFT%rocFFT_bwd, ncomp_bwd,      &
     &        n_WR, WR(1), v_rtp(1,1), sph_rocFFT%WK_rocFFT, flag_FFT)
        else if(iflag_sph_FFT .eq. iflag_rocFFT) then
          call prt_dmn_bwd_c_rocFFT_from_recv                           &
     &       (sph_rtp, comm_rtp, sph_rocFFT%rocFFT_bwd, ncomp_bwd,      &
     &        n_WR, WR(1), v_rtp(1,1), sph_rocFFT%WK_rocFFT, flag_FFT)
        end if
      end if
!
      end subroutine sel_prt_bwd_rocFFT_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_prt_rocFFT_selector
