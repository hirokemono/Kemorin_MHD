!>@file   t_multi_rocFFT_wrapper.F90
!!@brief  module t_multi_rocFFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Fourier transform using AMD rocFFT
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine init_prt_complex_rocFFT                              &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, rocFFT_f)
!!      subroutine finalize_prt_complex_rocFFT(rocFFT_f)
!!      subroutine verify_prt_complex_rocFFT                            &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, rocFFT_f)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        type(work_for_field_rocFFT), intent(inout) :: rocFFT_f
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
      module t_sph_field_rocFFT
!
      use m_precision
      use m_constants
!
      use iso_c_binding
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_multi_rocFFT_wrapper
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
!>      Structure to use rocFFT
      type work_for_field_rocFFT
!>        Flag and lengh
        integer(kind = kint) :: iflag_rocFFT_len = -1
!>        Structure of paramters for forward transform
        type(calypso_rocFFT_params) :: rocFFT_fwd
!>        Structure of paramters for backward transform
        type(calypso_rocFFT_params) :: rocFFT_bwd
!>        Structure of work area for rocFFT
        type(calypso_rocFFT_work) :: WK_rocFFT
!>        Structure of communication table from FFT to send buffer
        type(comm_tbl_from_FFTW) :: comm_sph_rocFFT
      end type work_for_field_rocFFT
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_prt_complex_rocFFT                                &
     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, rocFFT_f)
!
      use set_comm_table_prt_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_field_rocFFT), intent(inout) :: rocFFT_f
!
      integer(kind = kint) :: howmany_bwd, howmany_fwd, nnod_rt
      integer(kind = kint) :: Nfft_c4
      real(kind = kreal) :: aNfft_d
!
!
      nnod_rt = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
!
      howmany_bwd = ncomp_bwd * nnod_rt
      howmany_fwd = ncomp_fwd * nnod_rt
!
      call calypso_pin_rocFFT_init                                      &
     &   (howmany_fwd, howmany_bwd, sph_rtp%nidx_rtp(3),                &
     &    rocFFT_f%rocFFT_fwd, rocFFT_f%rocFFT_bwd, rocFFT_f%WK_rocFFT)
      rocFFT_f%WK_rocFFT%aNfft = one / dble(sph_rtp%nidx_rtp(3))
!
      call alloc_comm_table_sph_FFTW                                    &
     &   (comm_rtp%ntot_item_sr, rocFFT_f%comm_sph_rocFFT)
!
      Nfft_c4 = int(rocFFT_f%WK_rocFFT%Nfft_c,kind=KIND(Nfft_c4))
      aNfft_d = real(rocFFT_f%WK_rocFFT%aNfft,kind=KIND(aNfft_d))
      call set_comm_item_prt_4_FFTW                                     &
     &   (sph_rtp%nnod_rtp, sph_rtp%istep_rtp, nnod_rt,                 &
     &    comm_rtp%ntot_item_sr, comm_rtp%irev_sr, Nfft_c4, aNfft_d,    &
     &    rocFFT_f%comm_sph_rocFFT)
      rocFFT_f%iflag_rocFFT_len = nnod_rt * sph_rtp%nidx_rtp(3)         &
     &                           *  max(ncomp_bwd,ncomp_fwd)
!
      end subroutine init_prt_complex_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine finalize_prt_complex_rocFFT(rocFFT_f)
!
      type(work_for_field_rocFFT), intent(inout) :: rocFFT_f
!
!
      call dealloc_comm_table_sph_FFTW(rocFFT_f%comm_sph_rocFFT)
      call calypso_rocFFT_fin(rocFFT_f%rocFFT_fwd, rocFFT_f%rocFFT_bwd, &
     &                        rocFFT_f%WK_rocFFT)
      rocFFT_f%iflag_rocFFT_len = -1
!
      end subroutine finalize_prt_complex_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine verify_prt_complex_rocFFT                              &
     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, rocFFT_f)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_field_rocFFT), intent(inout) :: rocFFT_f
!
      integer(kind = kint) :: num, nnod_rt
!
!
      if(rocFFT_f%iflag_rocFFT_len .lt. 0) then
        call init_prt_complex_rocFFT(sph_rtp, comm_rtp,                 &
     &                               ncomp_bwd, ncomp_fwd, rocFFT_f)
        return
      end if
!
      nnod_rt = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
      num = nnod_rt * sph_rtp%nidx_rtp(3) * max(ncomp_bwd,ncomp_fwd)
      if(rocFFT_f%iflag_rocFFT_len .ne. num) then
        call finalize_prt_complex_rocFFT(rocFFT_f)
        call init_prt_complex_rocFFT(sph_rtp, comm_rtp,                 &
     &                               ncomp_bwd, ncomp_fwd, rocFFT_f)
      end if
!
      end subroutine verify_prt_complex_rocFFT
!
! ------------------------------------------------------------------
!
      end module t_sph_field_rocFFT
