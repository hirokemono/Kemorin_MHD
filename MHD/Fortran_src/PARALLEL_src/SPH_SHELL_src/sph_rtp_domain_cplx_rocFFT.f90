!>@file   sph_rtp_domain_cplx_rocFFT.f90
!!@brief  module sph_rtp_domain_cplx_rocFFT
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Fourier transform using AMD rocFFT
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine rtp_dmn_fwd_cplx_rocFFT_to_send                      &
!!     &         (sph_rtp, comm_sph_rocFFT, rocFFT_fwd, ncomp_fwd,      &
!!     &          n_WS, X_rtp, WS, WK_rocFFT, flag_FFT)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(comm_tbl_from_FFTW), intent(in) :: comm_sph_rocFFT
!!        type(calypso_rocFFT_params), intent(in), target :: rocFFT_fwd
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        logical, intent(inout) :: flag_FFT
!!      subroutine rtp_dmn_bwd_c_rocFFT_from_recv                       &
!!     &         (sph_rtp, comm_rtp, rocFFT_bwd, ncomp_bwd,             &
!!     &          n_WR, WR, X_rtp, WK_rocFFT, flag_FFT)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(calypso_rocFFT_params), intent(in), target :: rocFFT_bwd
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        real (kind=kreal), intent(in):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform
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
      module sph_rtp_domain_cplx_rocFFT
!
      use iso_c_binding
!
      use m_precision
      use m_constants
!
      use t_sph_field_rocFFT
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_multi_rocFFT_wrapper
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine rtp_dmn_fwd_cplx_rocFFT_to_send                        &
     &         (sph_rtp, comm_sph_rocFFT, rocFFT_fwd, ncomp_fwd,        &
     &          n_WS, X_rtp, WS, WK_rocFFT, flag_FFT)
!
      use m_elapsed_labels_SPH_TRNS
      use calypso_multi_rocFFT
      use multi_pin_complex_rocFFT
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_rocFFT
      type(calypso_rocFFT_params), intent(in), target :: rocFFT_fwd
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout):: WS(n_WS)
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      logical, intent(inout) :: flag_FFT
!
      integer(kind = kint) :: nd
!
!
      flag_FFT = .TRUE.
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
        call copy_rtp_field_to_OMP_FFTW(ione,                           &
     &      sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),     &
     &      X_rtp(1,nd), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
        call calypso_forward_rocFFT_r2c(rocFFT_fwd%rocFFT_plan,         &
     &      rocFFT_fwd%rocFFT_wk_info, rocFFT_fwd%Ncomp,                &
     &      WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT,                       &
     &      WK_rocFFT%Nfft_c, WK_rocFFT%C_rocFFT,                       &
     &      rocFFT_fwd%Nbytes, WK_rocFFT%data_ptr)
!$omp parallel workshare
        WK_rocFFT%C_rocFFT(1:sph_rtp%istack_rtp_rt_smp(np_smp)*WK_rocFFT%Nfft_c)   &
     &   = WK_rocFFT%aNfft                                                &
     &  * WK_rocFFT%C_rocFFT(1:sph_rtp%istack_rtp_rt_smp(np_smp)*WK_rocFFT%Nfft_c)
!$omp end parallel workshare
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
!   normalization
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
        call copy_1comp_rtp_FFTW_to_send                                &
     &     (nd, sph_rtp%istack_rtp_rt_smp(np_smp),                      &
     &      int(WK_rocFFT%Nfft_c), ncomp_fwd, WK_rocFFT%C_rocFFT(1),    &
     &      comm_sph_rocFFT, n_WS, WS(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
!
      end subroutine rtp_dmn_fwd_cplx_rocFFT_to_send
!
! ------------------------------------------------------------------
!
      subroutine rtp_dmn_bwd_c_rocFFT_from_recv                         &
     &         (sph_rtp, comm_rtp, rocFFT_bwd, ncomp_bwd,               &
     &          n_WR, WR, X_rtp, WK_rocFFT, flag_FFT)
!
      use m_elapsed_labels_SPH_TRNS
      use calypso_multi_rocFFT
      use multi_pin_complex_rocFFT
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      type(calypso_rocFFT_params), intent(in), target :: rocFFT_bwd
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      logical, intent(inout) :: flag_FFT
!
      integer(kind = kint) :: nd
!
!
      flag_FFT = .TRUE.
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
        call set_OMP_FFTW_comp_from_recv                                &
     &     (nd, sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nnod_rtp,    &
     &      ncomp_bwd, n_WR, comm_rtp%irev_sr, WR,                      &
     &      int(WK_rocFFT%Nfft_c), WK_rocFFT%C_rocFFT(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
        call calypso_backward_rocFFT_c2r(rocFFT_bwd%rocFFT_plan,        &
     &      rocFFT_bwd%rocFFT_wk_info, rocFFT_bwd%Ncomp,                &
     &      WK_rocFFT%Nfft_c, WK_rocFFT%C_rocFFT(1),                    &
     &      WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),                    &
     &      rocFFT_bwd%Nbytes, WK_rocFFT%data_ptr)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
        call copy_rtp_field_from_OMP_FFTW                               &
     &     (ione, sph_rtp%istack_rtp_rt_smp(np_smp),                    &
     &      int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),               &
     &      sph_rtp%nidx_rtp(3), X_rtp(1,nd))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
!
      end subroutine rtp_dmn_bwd_c_rocFFT_from_recv
!
! ------------------------------------------------------------------
!
      end module sph_rtp_domain_cplx_rocFFT
