!>@file   t_sph_ISPACK3_selector.f90
!!@brief  module t_sph_ISPACK3_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine sel_init_sph_ISPACK3(id_rank, iflag_size,            &
!!     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,              &
!!     &          WKs_ISPACK3, flag_FFT)
!!      subroutine sel_finalize_sph_ISPACK3(iflag_size, WKs_ISPACK3,    &
!!     &                                    flag_FFT)
!!      subroutine sel_verify_sph_ISPACK3(iflag_size, sph_rtp, comm_rtp,&
!!     &          ncomp_bwd, ncomp_fwd, WKs_ISPACK3, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
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
!
      module t_sph_ISPACK3_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector
!
      use t_sph_ISPACK3_FFT
      use t_sph_domain_ISPACK3_FFT
      use t_sph_component_ISPACK3_FFT
      use t_sph_single_ISPACK3_FFT
!
      implicit none
!
!>      Structure for work area of ISPACK3
      type works_sph_ispack3
!>        Structure to use ISPACK3
        type(work_for_ispack3) :: sph_full_ISPACK3
!>        Structure to use ISPACK3 for domain
        type(work_for_domain_ispack3) :: sph_domain_ispack3
!>        Structure to use ISPACK3 for component
        type(work_for_comp_ispack3) :: sph_comp_ispack3
!>        Structure to use single ISPACK3
        type(work_for_single_ispack3) :: sph_sgl_ispack3
      end type works_sph_ispack3
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_init_sph_ISPACK3(id_rank, iflag_size,              &
     &          sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                &
     &          WKs_ISPACK3, flag_FFT)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V3.0.1'
        call init_sph_ISPACK3(sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,  &
     &      WKs_ISPACK3%sph_full_ISPACK3, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V3.0.1 for domain'
        call init_sph_domain_ISPACK3(sph_rtp, comm_rtp,                 &
     &      WKs_ISPACK3%sph_domain_ispack3, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V3.0.1 for component'
        call init_sph_comp_ISPACK3(sph_rtp, ncomp_bwd, ncomp_fwd,       &
     &      WKs_ISPACK3%sph_comp_ispack3, flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(id_rank .eq. 0) write(*,*) 'Use single ISPACK V3.0.1'
        call init_sph_single_ISPACK3(sph_rtp,                           &
     &      WKs_ISPACK3%sph_sgl_ispack3, flag_fft)
      end if
!
      end subroutine sel_init_sph_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine sel_finalize_sph_ISPACK3(iflag_size, WKs_ISPACK3,      &
     &                                    flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
!
      type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK V3.0.1'
        call finalize_sph_ISPACK3(WKs_ISPACK3%sph_full_ISPACK3,         &
     &                            flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'Finalize ISPACK V3.0.1 for domain'
        call finalize_sph_domain_ISPACK3                                &
     &     (WKs_ISPACK3%sph_domain_ispack3, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'Finalize ISPACK V3.0.1 for component'
        call finalize_sph_comp_ISPACK3(WKs_ISPACK3%sph_comp_ispack3,    &
     &                                 flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'Finalize single ISPACK V3.0.1'
        call finalize_sph_single_ISPACK3(WKs_ISPACK3%sph_sgl_ispack3,   &
     &                                   flag_fft)
      end if
!
      end subroutine sel_finalize_sph_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine sel_verify_sph_ISPACK3(iflag_size, sph_rtp, comm_rtp,  &
     &          ncomp_bwd, ncomp_fwd, WKs_ISPACK3, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(works_sph_ispack3), intent(inout) :: WKs_ISPACK3
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V3.0.1'
        call verify_sph_ISPACK3                                         &
     &     (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd,                    &
     &      WKs_ISPACK3%sph_full_ISPACK3, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'Use ISPACK V3.0.1 for domain'
        call verify_sph_domain_ISPACK3(sph_rtp, comm_rtp,               &
     &      WKs_ISPACK3%sph_domain_ispack3, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'Use ISPACK V3.0.1 for component'
        call verify_sph_comp_ISPACK3(sph_rtp, ncomp_bwd, ncomp_fwd,     &
     &      WKs_ISPACK3%sph_comp_ispack3, flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single ISPACK V3.0.1'
        call verify_sph_single_ISPACK3                                  &
     &     (sph_rtp, WKs_ISPACK3%sph_sgl_ispack3, flag_fft)
      end if
!
      end subroutine sel_verify_sph_ISPACK3
!
! ------------------------------------------------------------------
!
      end module t_sph_ISPACK3_selector
