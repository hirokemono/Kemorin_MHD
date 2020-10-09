!>@file   sph_rtp_FFTW.F90
!!@brief  module sph_rtp_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_rtp_FFTW(sph_rtp, comm_rtp,                     &
!!     &                         ncomp_bwd, ncomp_fwd, FFTW_f)
!!      subroutine verify_rtp_FFTW(sph_rtp, comm_rtp,                   &
!!     &                           ncomp_bwd, ncomp_fwd, FFTW_f)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine rtp_fwd_FFTW_to_send                                 &
!!     &         (sph_rtp, comm_rtp, ncomp_fwd, n_WS, X_rtp, WS, FFTW_f)
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
!!      subroutine rtp_back_FFTW_from_recv                              &
!!     &         (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, X_rtp, FFTW_f)
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
      module sph_rtp_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_elapsed_labels_SPH_TRNS
!
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFTW
      use t_sph_field_FFTW
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_rtp_FFTW(sph_rtp, comm_rtp,                       &
     &                         ncomp_bwd, ncomp_fwd, FFTW_f)
!
      use set_comm_table_rtp_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
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
      call alloc_whole_FFTW_plan                                        &
     &  (sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp,                &
     &   ncomp_bwd, ncomp_fwd, FFTW_f)
!
      do ip = 1, np_smp
        howmany = sph_rtp%istack_rtp_rt_smp(ip  )                       &
     &           - sph_rtp%istack_rtp_rt_smp(ip-1)
        FFTW_f%howmany_bwd = ncomp_bwd
        FFTW_f%howmany_fwd = ncomp_fwd
!
        idist_r = int(FFTW_f%Nfft_r)
        idist_c = int(FFTW_f%Nfft_c)
        ist_r = ncomp_bwd*FFTW_f%Nfft_r
        ist_c = ncomp_fwd*FFTW_f%Nfft_c
!
        call dfftw_plan_many_dft_r2c(FFTW_f%plan_fwd(ip),               &
     &     IONE_4, int(FFTW_f%Nfft_r), int(FFTW_f%howmany_fwd),         &
     &     FFTW_f%X(ist_r+1), inembed, int(FFTW_f%howmany_fwd), IONE_4, &
     &     FFTW_f%C(ist_c+1), inembed, int(FFTW_f%howmany_fwd), IONE_4, &
     &     FFTW_ESTIMATE)
        call dfftw_plan_many_dft_c2r(FFTW_f%plan_bwd(ip),               &
     &     IONE_4, int(FFTW_f%Nfft_r), int(FFTW_f%howmany_bwd),         &
     &     FFTW_f%C(ist_c+1), inembed, int(FFTW_f%howmany_bwd), IONE_4, &
     &     FFTW_f%X(ist_r+1), inembed, int(FFTW_f%howmany_bwd), IONE_4, &
     &     FFTW_ESTIMATE)
      end do
      FFTW_f%aNfft = one / dble(sph_rtp%nidx_rtp(3))
!
      call alloc_comm_table_sph_FFTW                                    &
     &   (comm_rtp%ntot_item_sr, FFTW_f%comm_sph_FFTW)
      call set_comm_item_rtp_4_FFTW                                     &
     &   (sph_rtp%nnod_rtp, comm_rtp%ntot_item_sr, comm_rtp%irev_sr,    &
     &    sph_rtp%istack_rtp_rt_smp, FFTW_f%Nfft_c, FFTW_f%aNfft,       &
     &    FFTW_f%comm_sph_FFTW)
!
      end subroutine init_rtp_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_rtp_FFTW(sph_rtp, comm_rtp,                     &
     &                           ncomp_bwd, ncomp_fwd, FFTW_f)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      if(allocated(FFTW_f%X) .eqv. .false.) then
        call init_rtp_FFTW(sph_rtp, comm_rtp,                           &
     &                     ncomp_bwd, ncomp_fwd, FFTW_f)
        return
      end if
!
      if(size(FFTW_f%X) .ne. sph_rtp%nnod_rtp) then
        call finalize_sph_field_FFTW(FFTW_f)
        call init_rtp_FFTW(sph_rtp, comm_rtp,                           &
     &                     ncomp_bwd, ncomp_fwd, FFTW_f)
      end if
!
      end subroutine verify_rtp_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine rtp_fwd_FFTW_to_send                                   &
     &         (sph_rtp, comm_rtp, ncomp_fwd, n_WS, X_rtp, WS, FFTW_f)
!
      use set_comm_table_rtp_FFTW
      use copy_rtp_data_to_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) ::  j, ip, ist, num
      integer(kind = kint) :: ist_r, ist_c, ntot
!
!
!$omp parallel do private(j,ip,ist,num,ist_r, ist_c)
      do ip = 1, np_smp
        ist_r = (ip-1) * FFTW_f%Nfft_r * ncomp_fwd
        ist_c = (ip-1) * FFTW_f%Nfft_c * ncomp_fwd
        ist = sph_rtp%istack_rtp_rt_smp(ip-1)
        num = sph_rtp%istack_rtp_rt_smp(ip)   &
     &       - sph_rtp%istack_rtp_rt_smp(ip-1)
        do j = 1, num
          call sel_copy_comp_rtp_to_FFT2                                &
     &       (j+ist, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),            &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_fwd, X_rtp(1,1), FFTW_f%X(ist_r+1))
!
          call dfftw_execute_dft_r2c(FFTW_f%plan_fwd(ip),               &
     &        FFTW_f%X(ist_r+1), FFTW_f%C(ist_c+1))
!
!   normalization
          call copy_comp_FFTW_to_send2                                  &
     &       (j+ist, sph_rtp%nnod_rtp, comm_rtp%irev_sr,                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), ncomp_fwd,             &
     &        FFTW_f%Nfft_c, FFTW_f%C(ist_c+1), FFTW_f%aNfft, n_WS, WS)
        end do
      end do
!$omp end parallel do
!
      end subroutine rtp_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine rtp_back_FFTW_from_recv                                &
     &         (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, X_rtp, FFTW_f)
!
      use set_comm_table_rtp_FFTW
      use copy_rtp_data_to_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: j, ip, ist, num
      integer(kind = kint) :: ist_r, ist_c, ntot
!
!
!      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
!      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!$omp parallel do private(j,ip,ist,num,ist_r, ist_c)
      do ip = 1, np_smp
        ist_r = (ip-1) * FFTW_f%Nfft_r * ncomp_bwd
        ist_c = (ip-1) * FFTW_f%Nfft_c * ncomp_bwd
        ist = sph_rtp%istack_rtp_rt_smp(ip-1)
        num = sph_rtp%istack_rtp_rt_smp(ip)   &
     &       - sph_rtp%istack_rtp_rt_smp(ip-1)
        do j = 1, num
          call copy_comp_FFTW_from_recv2                                &
     &       (j+ist, sph_rtp%nnod_rtp, comm_rtp%irev_sr,                    &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), ncomp_bwd,             &
     &        n_WR, WR, FFTW_f%Nfft_c, FFTW_f%C(ist_c+1))
!
          call dfftw_execute_dft_c2r(FFTW_f%plan_bwd(ip),               &
     &        FFTW_f%C(ist_c+1), FFTW_f%X(ist_r+1))
!
          call sel_copy_comp_FFT_to_rtp2                                &
     &       (j+ist, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_bwd, FFTW_f%X(ist_r+1), X_rtp(1,1))
        end do
      end do
!$omp end parallel do
!
      end subroutine rtp_back_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_comp_rtp_to_FFT2                              &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r,                &
     &          ncomp_fwd, X_rtp, X_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_rtp(nnod_rtp,ncomp_fwd)
!
      real(kind = kreal), intent(inout) :: X_fft(ncomp_fwd,Nfft_r)
!
      integer(kind = kint) :: ist, m
!
!
        do m = 1, Nfft_r
          ist = j + (m-1) * nnod_rt
          X_fft(1:ncomp_fwd,m) = X_rtp(ist,1:ncomp_fwd)
        end do
!
      end subroutine sel_copy_comp_rtp_to_FFT2
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_comp_FFT_to_rtp2                              &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r,                &
     &          ncomp_bwd, X_fft, X_rtp)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      integer(kind = kint), intent(in) :: ncomp_bwd
      real(kind = kreal), intent(in) :: X_fft(ncomp_bwd,Nfft_r)
!
      real(kind = kreal), intent(inout) :: X_rtp(nnod_rtp,ncomp_bwd)
!
      integer(kind = kint) :: ist, m, nd
!
!
        do m = 1, Nfft_r
          ist = j + (m-1) * nnod_rt
          X_rtp(ist,1:ncomp_bwd) = X_fft(1:ncomp_bwd,m)
        end do
!
      end subroutine sel_copy_comp_FFT_to_rtp2
!
! ------------------------------------------------------------------
!
      subroutine copy_comp_FFTW_to_send2(j, nnod_rtp, irev_sr_rtp,      &
     &          nnod_rt, ncomp_fwd, Nfft_c, C_fft, aNfft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_fft(ncomp_fwd*Nfft_c)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, ms, ic_rtp, is_rtp, ic_send, is_send
!
!
      ic_send = ncomp_fwd * (irev_sr_rtp(j) - 1)
      WS(ic_send+1:ic_send+ncomp_fwd)                                   &
     &          = aNfft * real(C_fft(1:ncomp_fwd))
      do m = 2, Nfft_c-1
        ic_rtp = j + (2*m-2) * nnod_rt
        is_rtp = j + (2*m-1) * nnod_rt
        ic_send = ncomp_fwd * (irev_sr_rtp(ic_rtp) - 1)
        is_send = ncomp_fwd * (irev_sr_rtp(is_rtp) - 1)
        ms =      ncomp_fwd * (m-1)
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &          = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd))
        WS(is_send+1:is_send+ncomp_fwd)                                 &
     &          = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd)*iu)
      end do 
      ic_rtp = j + nnod_rt
      ic_send = ncomp_fwd * (irev_sr_rtp(ic_rtp)-1)
      ms =      ncomp_fwd * (Nfft_c-1)
      WS(ic_send+1:ic_send+ncomp_fwd)                                   &
     &        = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd))
!
      end subroutine copy_comp_FFTW_to_send2
!
! ------------------------------------------------------------------
!
      subroutine copy_comp_FFTW_from_recv2                            &
     &         (j, nnod_rtp, irev_sr_rtp, nnod_rt,            &
     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: Nfft_c
      complex(kind = fftw_complex), intent(inout)                       &
     &                  :: C_fft(ncomp_bwd*Nfft_c)
!
      integer(kind = kint) :: m, ms, ic_rtp, is_rtp, ic_recv, is_recv
!
!
      ic_recv = ncomp_bwd * (irev_sr_rtp(j) - 1)
      C_fft(1:ncomp_bwd)                                                &
     &        = cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd), zero, kind(0d0))
      do m = 2, Nfft_c-1
        ic_rtp = j + (2*m-2) * nnod_rt
        is_rtp = j + (2*m-1) * nnod_rt
        ic_recv = ncomp_bwd * (irev_sr_rtp(ic_rtp) - 1)
        is_recv = ncomp_bwd * (irev_sr_rtp(is_rtp) - 1)
        ms =      ncomp_bwd * (m - 1)
        C_fft(ms+1:ms+ncomp_bwd)                                        &
     &        = half*cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),             &
     &                    -WR(is_recv+1:is_recv+ncomp_bwd),kind(0d0))
      end do
      ic_rtp = j + nnod_rt
      ic_recv = ncomp_bwd * (irev_sr_rtp(ic_rtp) - 1)
      ms =      ncomp_bwd * (Nfft_c - 1)
      C_fft(ms+1:ms+ncomp_bwd)                                          &
     &  = half * cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),zero, kind(0d0))
!
      end subroutine copy_comp_FFTW_from_recv2
!
! ------------------------------------------------------------------
!
      end module sph_rtp_FFTW
