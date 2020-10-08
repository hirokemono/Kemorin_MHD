!>@file   t_sph_field_FFTW.F90
!!@brief  module t_sph_field_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_field_FFTW                                  &
!!     &         (nidx_rtp, irt_rtp_smp_stack, FFTW_f)
!!      subroutine finalize_sph_field_FFTW(FFTW_f)
!!      subroutine verify_sph_field_FFTW(nnod_rtp, nidx_rtp,            &
!!     &          irt_rtp_smp_stack, FFTW_f)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_field_fwd_FFTW_to_send                           &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,     &
!!     &          n_WS, irev_sr_rtp, X_rtp, WS, FFTW_f)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
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
!!      subroutine sph_field_back_FFTW_from_recv                        &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_bwd,     &
!!     &          n_WR, irev_sr_rtp, WR, X_rtp, FFTW_f)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTW3
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
!!@n @param Nstacksmp(0:np_smp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!
      module t_sph_field_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      implicit none
!
!>      Structure to use SNGLE FFTW
      type work_for_field_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_bwd(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_fwd(:)
!
!>        length of FFT for real
        integer(kind = kint) :: Nfft_r
!>        length of FFT for complex
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X(:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C(:)
!
!>        temporal area for ordering
        real(kind = kreal), allocatable :: v_tmp(:)
      end type work_for_field_FFTW
!
      private :: alloc_fld_FFTW_plan, dealloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_field_FFTW                                    &
     &         (nidx_rtp, irt_rtp_smp_stack, FFTW_f)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint_gl) :: ist_r, ist_c
      integer(kind = kint) :: ip
      integer(kind = 4) :: howmany, idist_r, idist_c
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
      integer, parameter :: istride = 1
!
!
!
      call alloc_fld_FFTW_plan(nidx_rtp(3), irt_rtp_smp_stack, FFTW_f)
!
      do ip = 1, np_smp
        howmany = int(irt_rtp_smp_stack(ip  )                           &
     &           - irt_rtp_smp_stack(ip-1))
        idist_r = int(FFTW_f%Nfft_r)
        idist_c = int(FFTW_f%Nfft_c)
        ist_r = FFTW_f%Nfft_r * irt_rtp_smp_stack(ip-1)
        ist_c = FFTW_f%Nfft_c * irt_rtp_smp_stack(ip-1)
!
        call dfftw_plan_many_dft_r2c                                    &
     &     (FFTW_f%plan_fwd(ip), IONE_4, int(FFTW_f%Nfft_r), howmany,   &
     &      FFTW_f%X(ist_r+1), inembed, howmany, IONE_4,                &
     &      FFTW_f%C(ist_c+1), inembed, howmany, IONE_4,                &
     &      FFTW_ESTIMATE)
        call dfftw_plan_many_dft_c2r                                    &
     &     (FFTW_f%plan_bwd(ip), IONE_4, int(FFTW_f%Nfft_r), howmany,   &
     &      FFTW_f%C(ist_c+1), inembed, howmany, IONE_4,                &
     &      FFTW_f%X(ist_r+1), inembed, howmany, IONE_4,                &
     &      FFTW_ESTIMATE)
      end do
      FFTW_f%aNfft = one / dble(nidx_rtp(3))
!
      end subroutine init_sph_field_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_field_FFTW(FFTW_f)
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: j
!
!
      do j = 1, np_smp
        call dfftw_destroy_plan(FFTW_f%plan_fwd(j))
        call dfftw_destroy_plan(FFTW_f%plan_bwd(j))
        call dfftw_cleanup
      end do
!
      call dealloc_fld_FFTW_plan(FFTW_f)
!
      end subroutine finalize_sph_field_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_field_FFTW(nnod_rtp, nidx_rtp,              &
     &          irt_rtp_smp_stack, FFTW_f)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      if(allocated(FFTW_f%X) .eqv. .false.) then
        call init_sph_field_FFTW(nidx_rtp, irt_rtp_smp_stack, FFTW_f)
        return
      end if
!
      if(size(FFTW_f%X) .ne. nnod_rtp) then
        call finalize_sph_field_FFTW(FFTW_f)
        call init_sph_field_FFTW(nidx_rtp, irt_rtp_smp_stack, FFTW_f)
      end if
!
      end subroutine verify_sph_field_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_field_fwd_FFTW_to_send                             &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,       &
     &          n_WS, irev_sr_rtp, X_rtp, WS, FFTW_f)
!
      use set_comm_table_rtp_FFTW
      use copy_rtp_data_to_FFTPACK
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) ::  ip, nd
      integer(kind = kint_gl) :: ist_r, ist_c
!
!
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
          call copy_FFTPACK_from_rtp_comp                               &
     &       (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                    &
     &        X_rtp(1,nd), FFTW_f%X)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
!$omp parallel do private(ip,ist_r,ist_c)
        do ip = 1, np_smp
          ist_r = FFTW_f%Nfft_r * irt_rtp_smp_stack(ip-1)
          ist_c = FFTW_f%Nfft_c * irt_rtp_smp_stack(ip-1)
          call dfftw_execute_dft_r2c(FFTW_f%plan_fwd(ip),               &
     &        FFTW_f%X(ist_r+1), FFTW_f%C(ist_c+1))
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
!   normalization
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
        call copy_rtp_comp_FFTW_to_send                                 &
     &     (nd, irt_rtp_smp_stack(np_smp), irt_rtp_smp_stack,           &
     &      nnod_rtp, ncomp_fwd, n_WS, irev_sr_rtp, WS,                 &
     &      FFTW_f%Nfft_c, FFTW_f%aNfft, FFTW_f%C)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
!
      end subroutine sph_field_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_field_back_FFTW_from_recv                          &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_bwd,       &
     &          n_WR, irev_sr_rtp, WR, X_rtp, FFTW_f)
!
      use set_comm_table_rtp_FFTW
      use copy_rtp_data_to_FFTPACK
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: X_rtp(nnod_rtp,ncomp_bwd)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint_gl) :: ist_r, ist_c
      integer(kind = kint) :: nd, ip
!
!
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
          call copy_FFTW_comp_from_recv                                 &
     &       (nd, irt_rtp_smp_stack(np_smp), irt_rtp_smp_stack,         &
     &        nnod_rtp, ncomp_bwd, n_WR, irev_sr_rtp, WR,               &
     &        FFTW_f%Nfft_c, FFTW_f%C)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do private(ip,ist_r,ist_c)
        do ip = 1, np_smp
          ist_r = FFTW_f%Nfft_r * irt_rtp_smp_stack(ip-1)
          ist_c = FFTW_f%Nfft_c * irt_rtp_smp_stack(ip-1)
          call dfftw_execute_dft_c2r(FFTW_f%plan_bwd(ip),               &
     &        FFTW_f%C(ist_c+1), FFTW_f%X(ist_r+1))
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
        call copy_FFTPACK_to_rtp_comp                                   &
     &     (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                      &
     &      FFTW_f%X, X_rtp(1,nd))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
!
      end subroutine sph_field_back_FFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_fld_FFTW_plan(Nfft, irt_rtp_smp_stack, FFTW_f)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint_gl) :: nnod_rt
!
!
      FFTW_f%Nfft_r = Nfft
      FFTW_f%Nfft_c = Nfft/2 + 1
!
      allocate(FFTW_f%plan_bwd(np_smp))
      allocate(FFTW_f%plan_fwd(np_smp))
!
      nnod_rt = irt_rtp_smp_stack(np_smp)
      allocate(FFTW_f%X(FFTW_f%Nfft_r*nnod_rt))
      allocate(FFTW_f%C(FFTW_f%Nfft_c*nnod_rt))
      FFTW_f%X = 0.0d0
      FFTW_f%C = 0.0d0
!
      end subroutine alloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_fld_FFTW_plan(FFTW_f)
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      deallocate(FFTW_f%plan_fwd, FFTW_f%plan_bwd)
      deallocate(FFTW_f%X, FFTW_f%C)
!
      end subroutine dealloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module t_sph_field_FFTW
