!>@file   t_sph_FFT_selector.F90
!!@brief  module t_sph_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine init_sph_FFT_select(id_rank, iflag_FFT_in,           &
!!     &         sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!      subroutine finalize_sph_FFT_select(sph_rtp, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!      subroutine verify_sph_FFT_select                                &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine fwd_FFT_select_to_send(sph_rtp, comm_rtp, ncomp_fwd, &
!!     &                                  n_WS, v_rtp, WS, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
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
!!      subroutine back_FFT_select_from_recv                            &
!!     &        (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
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
      module t_sph_FFT_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector

      use t_sph_FFTPACK5
      use t_sph_component_FFTPACK5
      use t_sph_domain_FFTPACK5
      use t_sph_single_FFTPACK5
      use sph_rtp_domain_FFTPACK5
      use sph_prt_domain_FFTPACK5
      use sph_rtp_FFTPACK5
      use sph_prt_FFTPACK5
!
      use t_sph_ISPACK_FFT
      use t_sph_domain_ISPACK_FFT
!
      use t_sph_ISPACK3_FFT
      use t_sph_domain_ISPACK3_FFT
      use t_sph_component_ISPACK3_FFT
      use t_sph_single_ISPACK3_FFT
!
      use t_sph_test_FFT
!
#ifdef FFTW3
      use t_sph_single_FFTW
      use t_sph_field_FFTW
      use t_sph_component_FFTW
      use sph_rtp_domain_FFTW
      use sph_prt_domain_FFTW
      use sph_rtp_FFTW
      use sph_prt_FFTW
#endif
#ifdef OMP_FFTW3
      use t_sph_OMP_FFTW
      use t_sph_field_OMP_FFTW
#endif
#ifdef _AMD_ROCM_
      use t_sph_field_rocFFT
#endif
!
      implicit none
!
!>      Structure for work area of FFTs
      type work_for_FFTs
!>        Integer flag for FFT type
        integer(kind = kint) :: iflag_FFT
!
!>        Structure to use FFTPACK
        type(work_for_fftpack) :: sph_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_sgl_fftpack) :: sph_sgl_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_comp_fftpack) :: sph_comp_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_domain_fftpack) :: sph_domain_FFTPACK
!
!>        Structure to use ISPACK
        type(work_for_ispack) :: sph_ISPACK
!>        Structure to use ISPACK for domain
        type(work_for_domain_ispack) :: sph_domain_ISPACK
!
!>        Structure to use ISPACK3
        type(work_for_ispack3) :: sph_ISPACK3
!>        Structure to use ISPACK3 for domain
        type(work_for_domain_ispack3) :: sph_domain_ispack3
!>        Structure to use ISPACK3 for component
        type(work_for_comp_ispack3) :: sph_comp_ispack3
!>        Structure to use single ISPACK3
        type(work_for_single_ispack3) :: sph_sgl_ispack3
!
!>        Structure to use FFT test
        type(work_for_test_FFT) :: sph_test_FFT
!
#ifdef FFTW3
!>        Structure to use FFTW
        type(work_for_field_FFTW) :: sph_fld_FFTW
!>        Structure to use FFTW for each component and meridinal point
        type(work_for_sgl_FFTW) :: sph_sgl_FFTW
!>        Structure to use FFTW for each component
        type(work_for_comp_FFTW) :: sph_comp_FFTW
#endif
!
#ifdef OMP_FFTW3
!>        Structure to use FFTW with OpenMP
        type(work_for_domain_OMP_FFTW) :: sph_domain_OMP_FFTW
!>        Structure to use FFTW with OpenMP
        type(work_for_OpenMP_FFTW) :: sph_OMP_FFTW
#endif
!
#ifdef _AMD_ROCM_
!>        Structure to use rocFFT
        type(work_for_field_rocFFT) :: sph_rocFFT
#endif
!
      end type work_for_FFTs
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFT_select(id_rank, iflag_FFT_in,             &
     &         sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!
      use sph_ISPACK3_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
#ifdef OMP_FFTW3
      use sph_prt_OMP_FFTW_selector
      use sph_rtp_OMP_FFTW_selector
#endif
!
#ifdef _AMD_ROCM_
      use sph_prt_rocFFT_selector
      use sph_rtp_rocFFT_selector
#endif
!
      integer, intent(in) :: id_rank
      integer(kind = kint) :: iflag_FFT_in
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
!
#ifdef _AMD_ROCM_
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_init_prt_rocFFT(id_rank, iflag_sph_FFT, iflag_size,    &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      else
        call sel_init_rtp_rocFFT(id_rank, iflag_sph_FFT, iflag_size,    &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      end if
#endif
!
#ifdef OMP_FFTW3
      if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_init_prt_OMP_FFTW(id_rank, iflag_size,               &
     &        sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                  &
     &        WK_FFTs%sph_fld_FFTW, flag_FFT)
        else
          call sel_init_rtp_OMP_FFTW(id_rank, iflag_size,               &
     &        sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                  &
     &        WK_FFTs%sph_OMP_FFTW, WK_FFTs%sph_domain_OMP_FFTW,        &
     &        flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
#ifdef FFTW3
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_init_prt_FFTW_smp(id_rank, iflag_size,               &
     &        sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                  &
     &        WK_FFTs%sph_fld_FFTW, WK_FFTs%sph_comp_FFTW,              &
     &        WK_FFTs%sph_sgl_FFTW, flag_FFT)
        else
          call sel_init_rtp_FFTW_smp(id_rank, iflag_size,               &
     &        sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                  &
     &        WK_FFTs%sph_fld_FFTW, WK_FFTs%sph_comp_FFTW,              &
     &        WK_FFTs%sph_sgl_FFTW, flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_ISPACK3) then
        call sel_init_sph_ISPACK3(id_rank, iflag_size,                  &
     &     sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                     &
     &     WK_FFTs%sph_ISPACK3, WK_FFTs%sph_domain_ispack3,             &
     &     WK_FFTs%sph_comp_ispack3, WK_FFTs%sph_sgl_ispack3, flag_FFT)
      end if
      if(flag_fft) return
!
      WK_FFTs%iflag_FFT = iflag_FFT_in
      if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V0.93'
        call init_sph_ISPACK(sph_rtp, comm_rtp,                         &
     &      ncomp_bwd, ncomp_fwd, WK_FFTs%sph_ISPACK, flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_DOMAIN) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V0.93 for domain'
        call init_sph_domain_ISPACK                                     &
     &     (sph_rtp, comm_rtp, WK_FFTs%sph_domain_ISPACK, flag_fft)
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        if(id_rank .eq. 0) write(*,*) 'Use single FFTPACK'
        call init_sph_single_FFTPACK5(sph_rtp, WK_FFTs%sph_sgl_FFTPACK, &
     &                                flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTPACK for all comp'
        call init_sph_comp_FFTPACK5(sph_rtp, ncomp_bwd, ncomp_fwd,      &
     &                              WK_FFTs%sph_comp_FFTPACK, flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(id_rank .eq. 0) write(*,*) 'Use prt FFTPACK for domaikn'
          call init_prt_domain_FFTPACK5(sph_rtp, comm_rtp,              &
     &        WK_FFTs%sph_domain_FFTPACK, flag_fft)
        else
          if(id_rank .eq. 0) write(*,*) 'Use rtp FFTPACK for domaikn'
          call init_rtp_domain_FFTPACK5(sph_rtp, comm_rtp,              &
     &        WK_FFTs%sph_domain_FFTPACK, flag_fft)
        end if
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFT_TEST) then
        if(id_rank .eq. 0) write(*,*) 'Use Test FFT routine'
        call init_sph_test_FFT(sph_rtp%nidx_rtp,                        &
     &      ncomp_bwd, ncomp_fwd, WK_FFTs%sph_test_FFT, flag_fft)
!
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(id_rank .eq. 0) write(*,*) 'Use prt FFTPACK'
          call init_prt_FFTPACK5(sph_rtp, comm_rtp,                     &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK, flag_fft)
        else
          if(id_rank .eq. 0) write(*,*) 'Use rtp FFTPACK'
          call init_rtp_FFTPACK5(sph_rtp, comm_rtp,                     &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK, flag_fft)
        end if
      end if
!
      end subroutine init_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFT_select(sph_rtp, WK_FFTs)
!
      use sph_ISPACK3_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
#endif
!
#ifdef OMP_FFTW3
      use sph_prt_OMP_FFTW_selector
      use sph_rtp_OMP_FFTW_selector
#endif
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
      flag_fft = .FALSE.
!
#ifdef _AMD_ROCM_
      if     ((WK_FFTs%iflag_FFT/10 .eq. iflag_OMP_rocFFT)              &
     &  .or.  (WK_FFTs%iflag_FFT/10 .eq. iflag_real_rocFFT)             &
     &  .or.  (WK_FFTs%iflag_FFT/10 .eq. iflag_rocFFT)) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize rocFFT'
        call finalize_sph_rocFFT(WK_FFTs%sph_rocFFT, flag_fft)
      end if
      if(flag_fft) return
#endif
!
#ifdef OMP_FFTW3
      if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_finalize_prt_OMP_FFTW                                &
     &       (iflag_size, WK_FFTs%sph_fld_FFTW, flag_FFT)
        else
          call sel_finalize_rtp_OMP_FFTW(iflag_size,                    &
     &        WK_FFTs%sph_OMP_FFTW, WK_FFTs%sph_domain_OMP_FFTW,        &
     &        flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
#ifdef FFTW3
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        call sel_finalize_sph_FFTW_smp(iflag_size,                      &
     &      WK_FFTs%sph_fld_FFTW, WK_FFTs%sph_comp_FFTW,                &
     &      WK_FFTs%sph_sgl_FFTW, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_ISPACK3) then
        call sel_finalize_sph_ISPACK3(iflag_size, WK_FFTs%sph_ISPACK3,  &
     &      WK_FFTs%sph_domain_ispack3, WK_FFTs%sph_comp_ispack3,       &
     &      WK_FFTs%sph_sgl_ispack3, flag_FFT)
      end if
      if(flag_fft) return
!
      if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK V0.93'
        call finalize_sph_ISPACK(WK_FFTs%sph_ISPACK, flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_DOMAIN) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'Finalize ISPACK V0.93 for domain'
        call finalize_sph_domain_ISPACK(WK_FFTs%sph_domain_ISPACK,      &
     &                                  flag_fft)
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTPACK'
        call finalize_sph_single_FFTPACK5(WK_FFTs%sph_sgl_FFTPACK,      &
     &                                    flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                     'Finalize FFTPACK for all comp'
        call finalize_sph_comp_FFTPACK5(WK_FFTs%sph_comp_FFTPACK,       &
     &                                  flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                     'Finalize FFTPACK for domain'
        call finalize_sph_domain_FFTPACK5(WK_FFTs%sph_domain_FFTPACK,   &
     &                                    flag_fft)
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFT_TEST) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize Test FFT'
        call finalize_sph_test_FFT(WK_FFTs%sph_test_FFT, flag_FFT)
!
      else
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_sph_FFTPACK5(WK_FFTs%sph_FFTPACK, flag_fft)
      end if
!
      end subroutine finalize_sph_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFT_select                                  &
     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, WK_FFTs)
!
!
      use sph_ISPACK3_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
#ifdef OMP_FFTW3
      use sph_prt_OMP_FFTW_selector
      use sph_rtp_OMP_FFTW_selector
#endif
!
#ifdef _AMD_ROCM_
      use sph_prt_rocFFT_selector
      use sph_rtp_rocFFT_selector
#endif
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
!
#ifdef _AMD_ROCM_
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_verify_prt_rocFFT(iflag_sph_FFT, iflag_size,           &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      else
        call sel_verify_rtp_rocFFT(iflag_sph_FFT, iflag_size,           &
     &      sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
#ifdef OMP_FFTW3
      if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_verify_prt_OMP_FFTW(iflag_size, sph_rtp, comm_rtp,   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_fld_FFTW, flag_FFT)
        else
          call sel_verify_rtp_OMP_FFTW(iflag_size, sph_rtp, comm_rtp,   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_OMP_FFTW,               &
     &        WK_FFTs%sph_domain_OMP_FFTW, flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
#ifdef FFTW3
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_verify_prt_FFTW_smp(iflag_size, sph_rtp, comm_rtp,   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_fld_FFTW,               &
     &        WK_FFTs%sph_comp_FFTW, WK_FFTs%sph_sgl_FFTW, flag_fft)
        else
          call sel_verify_rtp_FFTW_smp(iflag_size, sph_rtp, comm_rtp,   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_fld_FFTW,               &
     &        WK_FFTs%sph_comp_FFTW, WK_FFTs%sph_sgl_FFTW, flag_fft)
        end if
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_ISPACK3) then
        call sel_verify_sph_ISPACK3                                     &
     &    (iflag_size, sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,         &
     &     WK_FFTs%sph_ISPACK3, WK_FFTs%sph_domain_ispack3,             &
     &     WK_FFTs%sph_comp_ispack3, WK_FFTs%sph_sgl_ispack3, flag_FFT)
      end if
      if(flag_fft) return
!
      if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V0.93'
        call verify_sph_ISPACK(sph_rtp, comm_rtp,                       &
     &      ncomp_bwd, ncomp_fwd, WK_FFTs%sph_ISPACK, flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_DOMAIN) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V0.93 for domain'
        call verify_sph_domain_ISPACK                                   &
     &     (sph_rtp, comm_rtp, WK_FFTs%sph_domain_ISPACK, flag_fft)
!
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTPACK'
        call verify_sph_single_FFTPACK5                                 &
     &     (sph_rtp, WK_FFTs%sph_sgl_FFTPACK, flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK for component'
        call verify_sph_comp_FFTPACK5(sph_rtp, ncomp_bwd, ncomp_fwd,    &
     &      WK_FFTs%sph_comp_FFTPACK, flag_fft)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                         'Use prt FFTPACK for domain'
          call verify_prt_domain_FFTPACK5(sph_rtp, comm_rtp,            &
     &        WK_FFTs%sph_domain_FFTPACK, flag_fft)
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                         'Use rtp FFTPACK for domain'
          call verify_rtp_domain_FFTPACK5(sph_rtp, comm_rtp,            &
     &        WK_FFTs%sph_domain_FFTPACK, flag_fft)
        end if
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFT_TEST) then
        if(iflag_debug .gt. 0) write(*,*) 'Use Test FFT routine'
        call verify_sph_test_FFT(sph_rtp%nidx_rtp,                      &
     &      ncomp_bwd, ncomp_fwd, WK_FFTs%sph_test_FFT, flag_fft)
!
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          if(iflag_debug .gt. 0) write(*,*) 'Use prt FFTPACK'
          call verify_prt_FFTPACK5(sph_rtp, comm_rtp,                   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK, flag_fft)
        else
          if(iflag_debug .gt. 0) write(*,*) 'Use rtp FFTPACK'
          call verify_rtp_FFTPACK5(sph_rtp, comm_rtp,                   &
     &        ncomp_bwd, ncomp_fwd, WK_FFTs%sph_FFTPACK, flag_fft)
        end if
      end if
!
      end subroutine verify_sph_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine fwd_FFT_select_to_send(sph_rtp, comm_rtp, ncomp_fwd,   &
     &                                  n_WS, v_rtp, WS, WK_FFTs)
!
      use calypso_mpi
      use sph_ISPACK3_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
#ifdef OMP_FFTW3
      use sph_prt_OMP_FFTW_selector
      use sph_rtp_OMP_FFTW_selector
#endif
!
#ifdef _AMD_ROCM_
      use sph_prt_rocFFT_selector
      use sph_rtp_rocFFT_selector
#endif
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd, n_WS
      real (kind=kreal), intent(in):: v_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
!
#ifdef _AMD_ROCM_
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_prt_fwd_rocFFT_to_send(iflag_sph_FFT, iflag_size,      &
     &      sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      else
        call sel_rtp_fwd_rocFFT_to_send(iflag_sph_FFT, iflag_size,      &
     &      sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
#ifdef OMP_FFTW3
      if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_prt_fwd_OMP_FFTW_to_send(iflag_size,                 &
     &        sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),    &
     &        sph_fld_FFTW, flag_FFT)
        else
          call sel_rtp_fwd_OMP_FFTW_to_send                             &
     &       (iflag_size, sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),  &
     &        sph_OMP_FFTW, sph_domain_OMP_FFTW, flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
#ifdef FFTW3
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_prt_fwd_FFTW_to_send(iflag_size, sph_rtp, comm_rtp,  &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_fld_FFTW, &
     &        WK_FFTs%sph_comp_FFTW, WK_FFTs%sph_sgl_FFTW, flag_FFT)
        else
          call sel_rtp_fwd_FFTW_to_send(iflag_size, sph_rtp, comm_rtp,  &
     &        ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_fld_FFTW, &
     &        WK_FFTs%sph_comp_FFTW, WK_FFTs%sph_sgl_FFTW, flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_ISPACK3) then
        call sel_sph_fwd_ISPACK3_to_send(iflag_size, sph_rtp, comm_rtp, &
     &      ncomp_fwd, n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_ISPACK3,    &
     &      WK_FFTs%sph_domain_ispack3, WK_FFTs%sph_comp_ispack3,       &
     &      WK_FFTs%sph_sgl_ispack3, flag_FFT)
      end if
      if(flag_fft) return
!
!      if(my_rank .eq. 0) write(*,*) sph_rtp%istep_rtp(3),              &
!     &                  'fwd_FFT_select_to_send', WK_FFTs%iflag_FFT
!
      if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        call sph_FTTRUF_to_send(sph_rtp, ncomp_fwd,                     &
     &      n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_ISPACK, flag_FFT)
      else if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_DOMAIN) then
        call sph_domain_FTTRUF_to_send                                  &
     &     (sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),                &
     &      WK_FFTs%sph_domain_ISPACK, flag_FFT)
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        call sph_single_RFFTMF_to_send(sph_rtp, comm_rtp, ncomp_fwd,    &
     &      n_WS, v_rtp(1,1), WS(1), WK_FFTs%sph_sgl_FFTPACK, flag_FFT)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        call sph_comp_RFFTMF_to_send                                    &
     &     (sph_rtp, comm_rtp, ncomp_fwd, n_WS, v_rtp(1,1), WS(1),      &
     &      WK_FFTs%sph_comp_FFTPACK, flag_FFT)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_domain_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS,      &
     &        v_rtp(1,1), WS(1), WK_FFTs%sph_domain_FFTPACK, flag_FFT)
        else
          call rtp_domain_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS,      &
     &        v_rtp(1,1), WS(1), WK_FFTs%sph_domain_FFTPACK, flag_FFT)
        end if
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFT_TEST) then
        call sph_test_fwd_FFT_to_send                                   &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_fwd, n_WS,                 &
     &      comm_rtp%irev_sr, v_rtp(1,1), WS(1),                        &
     &      WK_FFTs%sph_test_FFT, flag_FFT)
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), &
     &                            WS(1), WK_FFTs%sph_FFTPACK, flag_FFT)
        else
          call rtp_RFFTMF_to_send(sph_rtp, ncomp_fwd, n_WS, v_rtp(1,1), &
     &                            WS(1), WK_FFTs%sph_FFTPACK, flag_FFT)
        end if
      end if
!
      end subroutine fwd_FFT_select_to_send
!
! ------------------------------------------------------------------
!
      subroutine back_FFT_select_from_recv                              &
     &        (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp, WK_FFTs)
!
      use calypso_mpi
      use sph_ISPACK3_selector
!
#ifdef FFTW3
      use sph_prt_FFTW_selector
      use sph_rtp_FFTW_selector
#endif
!
#ifdef OMP_FFTW3
      use sph_prt_OMP_FFTW_selector
      use sph_rtp_OMP_FFTW_selector
#endif
!
#ifdef _AMD_ROCM_
      use sph_prt_rocFFT_selector
      use sph_rtp_rocFFT_selector
#endif
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, n_WR
      real (kind=kreal), intent(inout) :: WR(n_WR)
      real (kind=kreal), intent(inout)                                  &
     &                  :: v_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      logical :: flag_FFT
      integer(kind = kint) :: iflag_sph_FFT, iflag_size
!
!
      iflag_size =    mod(WK_FFTs%iflag_FFT,10)
      iflag_sph_FFT = WK_FFTs%iflag_FFT - iflag_size
!
      flag_fft = .FALSE.
#ifdef _AMD_ROCM_
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call sel_prt_bwd_rocFFT_from_recv(iflag_sph_FFT, iflag_size,    &
     &      sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      else
        call sel_rtp_bwd_rocFFT_from_recv(iflag_sph_FFT, iflag_size,    &
     &      sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WK_FFTs%sph_rocFFT, flag_FFT)
      end if
      if(flag_fft) return
#endif
!
#ifdef OMP_FFTW3
      if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_prt_bwd_OMP_FFTW_from_recv(iflag_size,               &
     &        sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WK_FFTs%sph_fld_FFTW, flag_FFT)
        else
          call sel_rtp_bwd_OMP_FFTW_from_recv(iflag_size,               &
     &        sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WK_FFTs%sph_OMP_FFTW, WK_FFTs%sph_domain_OMP_FFTW,        &
     &        flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
#ifdef FFTW3
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call sel_prt_bwd_FFTW_from_recv(iflag_size,                   &
     &        sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WK_FFTs%sph_fld_FFTW, WK_FFTs%sph_comp_FFTW,              &
     &        WK_FFTs%sph_sgl_FFTW, flag_FFT)
        else
          call sel_rtp_bwd_FFTW_from_recv(iflag_size,                   &
     &        sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),    &
     &        WK_FFTs%sph_fld_FFTW, WK_FFTs%sph_comp_FFTW,              &
     &        WK_FFTs%sph_sgl_FFTW, flag_FFT)
        end if
      end if
      if(flag_fft) return
#endif
!
      if(iflag_sph_FFT .eq. iflag_ISPACK3) then
        call sel_sph_bwd_ISPACK3_from_recv(iflag_size,                  &
     &      sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WK_FFTs%sph_ISPACK3, WK_FFTs%sph_domain_ispack3,            &
     &      WK_FFTs%sph_comp_ispack3, WK_FFTs%sph_sgl_ispack3,          &
     &      flag_FFT)
      end if
      if(flag_fft) return
!
!      if(my_rank .eq. 0) write(*,*) sph_rtp%istep_rtp(3),              &
!     &                  'back_FFT_select_from_recv', WK_FFTs%iflag_FFT
!
      if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        call sph_FTTRUB_from_recv(sph_rtp, comm_rtp, ncomp_bwd,         &
     &      n_WR, WR(1), v_rtp(1,1), WK_FFTs%sph_ISPACK, flag_FFT)
      else if(WK_FFTs%iflag_FFT .eq. iflag_ISPACK1_DOMAIN) then
        call sph_domain_FTTRUB_from_recv                                &
     &     (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR(1), v_rtp(1,1),      &
     &      WK_FFTs%sph_domain_ISPACK, flag_FFT)
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_SINGLE) then
        call sph_single_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd,  &
     &      n_WR, WR, v_rtp(1,1), WK_FFTs%sph_sgl_FFTPACK, flag_FFT)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_COMPONENT) then
        call sph_comp_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd,    &
     &      n_WR, WR, v_rtp(1,1), WK_FFTs%sph_comp_FFTPACK, flag_FFT)
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFTPACK_DOMAIN) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_domain_RFFTMB_from_recv                              &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp(1,1),       &
     &        WK_FFTs%sph_domain_FFTPACK, flag_FFT)
        else
          call rtp_domain_RFFTMB_from_recv                              &
     &       (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, v_rtp(1,1),       &
     &        WK_FFTs%sph_domain_FFTPACK, flag_FFT)
        end if
!
      else if(WK_FFTs%iflag_FFT .eq. iflag_FFT_TEST) then
        call sph_test_back_FFT_from_recv                                &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_bwd, n_WR,                 &
     &      comm_rtp%irev_sr, WR, v_rtp(1,1),                           &
     &      WK_FFTs%sph_test_FFT, flag_FFT)
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call prt_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd, n_WR, &
     &         WR, v_rtp(1,1), WK_FFTs%sph_FFTPACK, flag_FFT)
        else
          call rtp_RFFTMB_from_recv(sph_rtp, comm_rtp, ncomp_bwd, n_WR, &
     &        WR, v_rtp(1,1), WK_FFTs%sph_FFTPACK, flag_FFT)
        end if
      end if
!
      end subroutine back_FFT_select_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      end module t_sph_FFT_selector
