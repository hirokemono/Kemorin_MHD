!>@file   t_sph_OMP_FFTW.F90
!!@brief  module t_sph_OMP_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_OMP_FFTW                                    &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, OFFTW)
!!      subroutine finalize_sph_OMP_FFTW(OFFTW)
!!      subroutine verify_sph_OMP_FFTW                                  &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, OFFTW)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_forward_OFFTW_to_send                            &
!!     &         (sph_rtp, ncomp_fwd, n_WS, X_rtp, WS, OFFTW)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
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
!!      subroutine sph_backward_OFFTW_from_recv                         &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, X_rtp, OFFTW)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
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
      module t_sph_OMP_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_OMP_FFTW3_counter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
!>      Structure to use SNGLE FFTW
      type work_for_OpenMP_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan) :: plan_bwd
!>        plan ID for forward transform
        integer(kind = fftw_plan) :: plan_fwd
!
        type(comm_tbl_from_FFTW) :: comm_FFTW
!
!>        number of backward FFT
        integer(kind = kint_4b) :: howmany_bwd
!>        number of forward FFT
        integer(kind = kint_4b) :: howmany_fwd
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
      end type work_for_OpenMP_FFTW
!
      private :: alloc_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_OMP_FFTW                                      &
     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, OFFTW)
!
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_OpenMP_FFTW), intent(inout) :: OFFTW
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
!
!
!
      call alloc_OMP_FFTW_plan                                          &
     &   (sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp,               &
     &    ncomp_bwd, ncomp_fwd, OFFTW)
!
      call chack_init_OMP_FFTW()
!
      call dfftw_plan_many_dft_r2c(OFFTW%plan_fwd, IONE_4,              &
     &    int(OFFTW%Nfft_r), OFFTW%howmany_fwd,                         &
     &    OFFTW%X(1), inembed, OFFTW%howmany_fwd, IONE_4,               &
     &    OFFTW%C(1), inembed, OFFTW%howmany_fwd, IONE_4,               &
     &    FFTW_KEMO_EST)
      call dfftw_plan_many_dft_c2r(OFFTW%plan_bwd, IONE_4,              &
     &    int(OFFTW%Nfft_r), OFFTW%howmany_bwd,                         &
     &    OFFTW%C(1), inembed, OFFTW%howmany_bwd, IONE_4,               &
     &    OFFTW%X(1), inembed, OFFTW%howmany_bwd, IONE_4,               &
     &    FFTW_KEMO_EST)
      OFFTW%aNfft = one / dble(sph_rtp%nidx_rtp(3))
!
      call alloc_comm_table_sph_FFTW                                    &
     &   (comm_rtp%ntot_item_sr, OFFTW%comm_FFTW)
      call set_comm_item_rtp_OMP_FFTW                                   &
     &   (sph_rtp%nnod_rtp, sph_rtp%istack_rtp_rt_smp(np_smp),          &
     &    comm_rtp%irev_sr, OFFTW%Nfft_c, OFFTW%aNfft,                  &
     &    OFFTW%comm_FFTW)
!
      end subroutine init_sph_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_OMP_FFTW(OFFTW)
!
      type(work_for_OpenMP_FFTW), intent(inout) :: OFFTW
!
!
      call dealloc_comm_table_sph_FFTW(OFFTW%comm_FFTW)
      call dfftw_destroy_plan(OFFTW%plan_fwd)
      call dfftw_destroy_plan(OFFTW%plan_bwd)
      call dfftw_cleanup
      call chack_clean_OMP_FFTW()
!
      call dealloc_OMP_FFTW_plan(OFFTW)
!
      end subroutine finalize_sph_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_OMP_FFTW                                    &
     &         (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, OFFTW)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_OpenMP_FFTW), intent(inout) :: OFFTW
!
      integer(kind = kint) :: hm_bw, hm_fw
!
!
      if(allocated(OFFTW%X) .eqv. .false.) then
        call init_sph_OMP_FFTW                                          &
     &     (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, OFFTW)
        return
      end if
!
      hm_bw = ncomp_bwd*sph_rtp%istack_rtp_rt_smp(np_smp)
      hm_fw = ncomp_fwd*sph_rtp%istack_rtp_rt_smp(np_smp)
      if(     int(hm_bw) .ne. OFFTW%howmany_bwd                         &
     &   .or. int(hm_fw) .ne. OFFTW%howmany_fwd                         &
     &   .or. sph_rtp%nidx_rtp(3) .ne. OFFTW%Nfft_r) then
        call finalize_sph_OMP_FFTW(OFFTW)
        call init_sph_OMP_FFTW                                          &
     &     (sph_rtp, comm_rtp, ncomp_bwd, ncomp_fwd, OFFTW)
      end if
!
      end subroutine verify_sph_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_forward_OFFTW_to_send                              &
     &         (sph_rtp, ncomp_fwd, n_WS, X_rtp, WS, OFFTW)
!
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &        :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_OpenMP_FFTW), intent(inout) :: OFFTW
!
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
      call copy_rtp_field_to_OMP_FFTW                                   &
     &   (sph_rtp%nnod_rtp, ncomp_fwd, X_rtp(1,1), OFFTW%X(1))
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
      call dfftw_execute_dft_r2c(OFFTW%plan_fwd,                        &
     &                            OFFTW%X, OFFTW%C)
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
!   normalization
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
      call copy_all_rtp_FFTW_to_send                                    &
     &   (sph_rtp%istack_rtp_rt_smp(np_smp), OFFTW%Nfft_c,              &
     &    ncomp_fwd, OFFTW%C, OFFTW%comm_FFTW, n_WS, WS)
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
!
      end subroutine sph_forward_OFFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_backward_OFFTW_from_recv                           &
     &         (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, X_rtp, OFFTW)
!
      use set_comm_table_rtp_OMP_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &        :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_OpenMP_FFTW), intent(inout) :: OFFTW
!
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
      call set_OMP_FFTW_field_from_recv                                 &
     &   (sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nnod_rtp,          &
     &    ncomp_bwd, n_WR, comm_rtp%irev_sr, WR, OFFTW%Nfft_c,          &
     &    OFFTW%C(1))
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
      call dfftw_execute_dft_c2r(OFFTW%plan_bwd,                        &
     &                           OFFTW%C, OFFTW%X)
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
      call copy_rtp_field_from_OMP_FFTW                                 &
     &   (sph_rtp%nnod_rtp, ncomp_bwd, OFFTW%X(1), X_rtp(1,1))
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
!
      end subroutine sph_backward_OFFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_OMP_FFTW_plan                                    &
     &        (Nfft, irt_rtp_smp_stack, ncomp_bwd, ncomp_fwd, OFFTW)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_OpenMP_FFTW), intent(inout) :: OFFTW
!
      integer(kind = kint_gl) :: nnod_rt
      integer(kind = kint) :: Ncomp
!
      nnod_rt = irt_rtp_smp_stack(np_smp)
      Ncomp = max(ncomp_bwd, ncomp_fwd)
      OFFTW%howmany_bwd = int(ncomp_bwd)*nnod_rt
      OFFTW%howmany_fwd = int(ncomp_fwd)*nnod_rt
      OFFTW%Nfft_r = Nfft
      OFFTW%Nfft_c = Nfft/2 + 1
!
      allocate(OFFTW%X(OFFTW%Nfft_r*nnod_rt*Ncomp))
      allocate(OFFTW%C(OFFTW%Nfft_c*nnod_rt*Ncomp))
      OFFTW%X = 0.0d0
      OFFTW%C = 0.0d0
!
      end subroutine alloc_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_OMP_FFTW_plan(OFFTW)
!
      type(work_for_OpenMP_FFTW), intent(inout) :: OFFTW
!
!
      deallocate(OFFTW%X, OFFTW%C)
!
      end subroutine dealloc_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module t_sph_OMP_FFTW
