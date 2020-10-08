!>@file   sph_prt_domain_FFTW.F90
!!@brief  module sph_prt_domain_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_prt_field_FFTW(sph_rtp, comm_rtp, FFTW_f)
!!      subroutine verify_prt_field_FFTW(sph_rtp, comm_rtp, FFTW_f)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine prt_field_fwd_FFTW_to_send                           &
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
!!      subroutine prt_field_back_FFTW_from_recv                        &
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
      module sph_prt_domain_FFTW
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
      subroutine init_prt_field_FFTW(sph_rtp, comm_rtp, FFTW_f)
!
      use set_comm_table_prt_FFTW
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: ist_r, ist_c
      integer(kind = kint) :: ip
      integer(kind = 4) :: howmany, idist_r, idist_c
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
      integer, parameter :: istride = 1
!
!
!
      call alloc_fld_FFTW_plan                                          &
     &  (sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp, FFTW_f)
!
      do ip = 1, np_smp
        howmany = int(sph_rtp%istack_rtp_rt_smp(ip  )                   &
     &           - sph_rtp%istack_rtp_rt_smp(ip-1))
        idist_r = int(FFTW_f%Nfft_r)
        idist_c = int(FFTW_f%Nfft_c)
        ist_r = FFTW_f%Nfft_r * sph_rtp%istack_rtp_rt_smp(ip-1)
        ist_c = FFTW_f%Nfft_c * sph_rtp%istack_rtp_rt_smp(ip-1)
!
        call dfftw_plan_many_dft_r2c                                    &
     &     (FFTW_f%plan_fwd(ip), IONE_4, int(FFTW_f%Nfft_r), howmany,   &
     &      FFTW_f%X(ist_r+1), inembed, istride, idist_r,               &
     &      FFTW_f%C(ist_c+1), inembed, istride, idist_c,               &
     &      FFTW_ESTIMATE)
        call dfftw_plan_many_dft_c2r                                    &
     &     (FFTW_f%plan_bwd(ip), IONE_4, int(FFTW_f%Nfft_r), howmany,   &
     &      FFTW_f%C(ist_c+1), inembed, istride, idist_c,               &
     &      FFTW_f%X(ist_r+1), inembed, istride, idist_r,               &
     &      FFTW_ESTIMATE)
      end do
      FFTW_f%aNfft = one / dble(sph_rtp%nidx_rtp(3))
!
      call alloc_comm_table_sph_FFTW                                    &
     &   (comm_rtp%ntot_item_sr, FFTW_f%comm_sph_FFTW)
      call set_comm_item_prt_4_FFTW                                     &
     &   (sph_rtp%nnod_rtp, comm_rtp%ntot_item_sr, comm_rtp%irev_sr,    &
     &    sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),       &
     &    FFTW_f%Nfft_c, FFTW_f%aNfft, FFTW_f%comm_sph_FFTW)
!
      end subroutine init_prt_field_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_prt_field_FFTW(sph_rtp, comm_rtp, FFTW_f)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      if(allocated(FFTW_f%X) .eqv. .false.) then
        call init_prt_field_FFTW(sph_rtp, comm_rtp, FFTW_f)
        return
      end if
!
      if(size(FFTW_f%X) .ne. sph_rtp%nnod_rtp) then
        call finalize_sph_field_FFTW(FFTW_f)
        call init_prt_field_FFTW(sph_rtp, comm_rtp, FFTW_f)
      end if
!
      end subroutine verify_prt_field_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine prt_field_fwd_FFTW_to_send                             &
     &         (sph_rtp, comm_rtp, ncomp_fwd, n_WS, X_rtp, WS, FFTW_f)
!
      use set_comm_table_prt_FFTW
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
      integer(kind = kint) :: ist_r, ist_c
      integer(kind = kint) :: j, ip, ist, ied, nd
!
!
      do nd = 1, ncomp_fwd
!$omp parallel do  private(j,ip,ist,ied,ist_r, ist_c)
        do ip = 1, np_smp
          ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
          ied = sph_rtp%istack_rtp_rt_smp(ip) 
!
          do j = ist, ied
            ist_r = FFTW_f%Nfft_r * (j-1)
            ist_c = FFTW_f%Nfft_c * (j-1)
            call sel_copy_single_rtp_to_FFT2                            &
     &         (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),              &
     &          sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3), &
     &          X_rtp(1,nd), FFTW_f%X(ist_r+1))
          end do
        end do
!$omp end parallel do
!
!$omp parallel do  private(j,ip,ist,ist_r, ist_c)
        do ip = 1, np_smp
          ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
          ist_r = FFTW_f%Nfft_r * (ist-1)
          ist_c = FFTW_f%Nfft_c * (ist-1)
          call dfftw_execute_dft_r2c(FFTW_f%plan_fwd(ip),               &
     &        FFTW_f%X(ist_r+1), FFTW_f%C(ist_c+1))
        end do
!$omp end parallel do
!
!$omp parallel do  private(j,ip,ist,ied,ist_r, ist_c)
        do ip = 1, np_smp
          ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
          ied = sph_rtp%istack_rtp_rt_smp(ip) 
!
          do j = ist, ied
            ist_r = FFTW_f%Nfft_r * (j-1)
            ist_c = FFTW_f%Nfft_c * (j-1)
            call copy_single_FFTW_to_send2                              &
     &         (nd, j, sph_rtp%nnod_rtp, comm_rtp%irev_sr,              &
     &          sph_rtp%istack_rtp_rt_smp(np_smp), ncomp_fwd,           &
     &          FFTW_f%Nfft_c, FFTW_f%C(ist_c+1), FFTW_f%aNfft, n_WS, WS)
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine prt_field_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine prt_field_back_FFTW_from_recv                          &
     &         (sph_rtp, comm_rtp, ncomp_bwd, n_WR, WR, X_rtp, FFTW_f)
!
      use set_comm_table_prt_FFTW
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
      integer(kind = kint) :: ist_r, ist_c
      integer(kind = kint) :: j, ip, ist, ied, nd
!
!
      do nd = 1, ncomp_bwd
!$omp parallel do private(j,ip,ist,ied,ist_r, ist_c)
        do ip = 1, np_smp
          ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
          ied = sph_rtp%istack_rtp_rt_smp(ip)
!
          do j = ist, ied
            ist_r = FFTW_f%Nfft_r * (j-1)
            ist_c = FFTW_f%Nfft_c * (j-1)
!
            call copy_single_FFTW_from_recv2                            &
     &         (nd, j, sph_rtp%nnod_rtp, comm_rtp%irev_sr,              &
     &          sph_rtp%istack_rtp_rt_smp(np_smp), ncomp_bwd,           &
     &          n_WR, WR, FFTW_f%Nfft_c, FFTW_f%C(ist_c+1))
          end do
        end do
!$omp end parallel do
!
!$omp parallel do private(j,ip,ist,ist_r,ist_c)
        do ip = 1, np_smp
          ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
          ist_r = FFTW_f%Nfft_r * (ist-1)
          ist_c = FFTW_f%Nfft_c * (ist-1)
          call dfftw_execute_dft_c2r(FFTW_f%plan_bwd(ip),               &
     &        FFTW_f%C(ist_c+1), FFTW_f%X(ist_r+1))
        end do
!$omp end parallel do
!
!$omp parallel do private(j,ip,ist,ied,ist_r, ist_c)
        do ip = 1, np_smp
          ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
          ied = sph_rtp%istack_rtp_rt_smp(ip)
!
          do j = ist, ied
            ist_r = FFTW_f%Nfft_r * (j-1)
            ist_c = FFTW_f%Nfft_c * (j-1)
            call sel_copy_single_FFT_to_rtp2                            &
     &         (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),              &
     &          sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3), &
     &          FFTW_f%X(ist_r+1), X_rtp(1,nd))
          end do
        end do
!$omp end parallel do
      end do
!
!
      end subroutine prt_field_back_FFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_single_FFTW_to_send2                              &
     &         (nd, j, nnod_rtp, irev_sr_rtp, nnod_rt, &
     &          ncomp_fwd, Nfft_c, C_fft, aNfft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nd, j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      integer(kind = kint), intent(in) :: Nfft_c
      complex(kind = fftw_complex), intent(in) :: C_fft(Nfft_c)
      real(kind = kreal), intent(in) :: aNfft
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind = kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, ic_rtp, is_rtp, ic_send, is_send
!
!
      ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp_fwd
      WS(ic_send) = aNfft * real(C_fft(1))
      do m = 2, Nfft_c-1
        ic_rtp = j + (2*m-2) * nnod_rt
        is_rtp = j + (2*m-1) * nnod_rt
        ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        WS(ic_send) = two*aNfft * real(C_fft(m))
        WS(is_send) = two*aNfft * real(C_fft(m)*iu)
      end do 
      ic_rtp = j + nnod_rt
      ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
      WS(ic_send) = two*aNfft * real(C_fft(Nfft_c))
!
      end subroutine copy_single_FFTW_to_send2
!
! ------------------------------------------------------------------
!
      subroutine copy_single_FFTW_from_recv2                          &
     &         (nd, j, nnod_rtp, irev_sr_rtp, nnod_rt, &
     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: nd, j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: Nfft_c
      complex(kind = fftw_complex), intent(inout) :: C_fft(Nfft_c)
!
      integer(kind = kint) :: m, ic_rtp, is_rtp, ic_recv, is_recv
!
!
      ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp_bwd
      C_fft(1) = cmplx(WR(ic_recv), zero, kind(0d0))
      do m = 2, Nfft_c-1
        ic_rtp = j + (2*m-2) * nnod_rt
        is_rtp = j + (2*m-1) * nnod_rt
        ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
        C_fft(m) = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
      end do
      ic_rtp = j + nnod_rt
      ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
      C_fft(Nfft_c) = half * cmplx(WR(ic_recv), zero, kind(0d0))
!
      end subroutine copy_single_FFTW_from_recv2
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_single_rtp_to_FFT2                            &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r, X_rtp, X_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      real(kind = kreal), intent(in) :: X_rtp(nnod_rtp)
!
      real(kind = kreal), intent(inout) :: X_fft(Nfft_r)
!
      integer(kind = kint) :: ist
!
        ist = (j-1) * Nfft_r
        X_fft(1:Nfft_r) = X_rtp(ist+1:ist+Nfft_r)
!
      end subroutine sel_copy_single_rtp_to_FFT2
!
! ------------------------------------------------------------------
!
      subroutine sel_copy_single_FFT_to_rtp2                            &
     &         (j, nnod_rtp, istep_phi, nnod_rt, Nfft_r, X_fft, X_rtp)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt, Nfft_r
      integer(kind = kint), intent(in) :: istep_phi
      real(kind = kreal), intent(in) :: X_fft(Nfft_r)
      real(kind = kreal), intent(inout) :: X_rtp(nnod_rtp)
!
      integer(kind = kint) :: ist
!
!
        ist = (j-1) * Nfft_r
        X_rtp(ist+1:ist+Nfft_r) = X_fft(1:Nfft_r)
!
      end subroutine sel_copy_single_FFT_to_rtp2
!
! ------------------------------------------------------------------
!
      end module sph_prt_domain_FFTW
