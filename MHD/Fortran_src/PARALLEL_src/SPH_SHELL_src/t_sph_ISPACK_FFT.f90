!>@file   t_sph_ISPACK_FFT.f90
!!@brief  module t_sph_ISPACK_FFT
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Fourier transform for spherical harmonics transform 
!!@n      using ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_ISPACK(nnod_rtp, nphi_rtp,                  &
!!     &                           ncomp_bwd, ncomp_fwd, ispack_t)
!!      subroutine finalize_sph_ISPACK(ispack_t)
!!      subroutine verify_sph_ISPACK(sph_rtp, comm_rtp,                 &
!!     &                             ncomp_bwd, ncomp_fwd, ispack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sph_FTTRUF_to_send(sph_rtp, comm_rtp,                &
!!     &          ncomp_fwd, n_WS, X_rtp, WS, ispack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sph_FTTRUB_from_recv(sph_rtp, comm_rtp,              &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, ispack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ISPACK
!!
!! x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!! (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!! i = 1:     a_{0}
!! i = 2:     a_{Nfft/2}
!! i = 3:     a_{1}
!! i = 4:     b_{1}
!! ...
!! i = 2*k+1: a_{k}
!! i = 2*k+2: b_{k}
!! ...
!! i = Nfft-1:   a_{Nfft/2-1}
!! i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module t_sph_ISPACK_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFT
!
      implicit none
!
!
!>      Structure to use ISPACK
      type work_for_ispack
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = 4) :: IT(5)
!
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for ISPACK
        real(kind = 8), allocatable :: WK(:)
!
!>      Structure of communication table from FFT to send buffer
        type(comm_tbl_from_FFT) :: comm_sph_ISPACK
      end type work_for_ispack
!
      private :: alloc_work_4_ispack, alloc_const_4_ispack
      private :: dealloc_work_4_ispack, dealloc_const_4_ispack
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_ISPACK(sph_rtp, comm_rtp,                     &
     &                           ncomp_bwd, ncomp_fwd, ispack_t)
!
      use ispack_0931
      use set_comm_table_rtp_ISPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: ncomp
!
!
      ncomp = max(ncomp_bwd, ncomp_fwd)
      call alloc_const_4_ispack(sph_rtp%nidx_rtp(3), ispack_t)
      call FTTRUI(sph_rtp%nidx_rtp(3), ispack_t%IT, ispack_t%T)
!
      call alloc_work_4_ispack(sph_rtp%nnod_rtp, ncomp, ispack_t)
!
      call alloc_comm_table_sph_FFT                                     &
     &   (comm_rtp%ntot_item_sr, ispack_t%comm_sph_ISPACK)
      call set_comm_item_rtp_4_ISPACK                                   &
     &   (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(3),                        &
     &    sph_rtp%istep_rtp, sph_rtp%istack_rtp_rt_smp,                 &
     &    comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                      &
     &    ispack_t%comm_sph_ISPACK)
!
      end subroutine init_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_ISPACK(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      call dealloc_comm_table_sph_FFT(ispack_t%comm_sph_ISPACK)
      call dealloc_const_4_ispack(ispack_t)
      call dealloc_work_4_ispack(ispack_t)
!
      end subroutine finalize_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_ISPACK(sph_rtp, comm_rtp,                   &
     &                             ncomp_bwd, ncomp_fwd, ispack_t)
!
      use ispack_0931
      use set_comm_table_rtp_ISPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: ncomp
!
!
      ncomp = max(ncomp_bwd, ncomp_fwd)
!
      if((2*sph_rtp%nidx_rtp(3)) .ne. size(ispack_t%T)) then
!
        if(allocated(ispack_t%T) .eqv. .false.) then
          call alloc_const_4_ispack(sph_rtp%nidx_rtp(3), ispack_t)
        else if( (2*sph_rtp%nidx_rtp(3)) .gt. size(ispack_t%T) ) then
          call dealloc_const_4_ispack(ispack_t)
          call alloc_const_4_ispack(sph_rtp%nidx_rtp(3), ispack_t)
        end if
!
        call FTTRUI(sph_rtp%nidx_rtp(3), ispack_t%IT, ispack_t%T )
!
        call dealloc_comm_table_sph_FFT(ispack_t%comm_sph_ISPACK)
        call alloc_comm_table_sph_FFT                                   &
     &     (comm_rtp%ntot_item_sr, ispack_t%comm_sph_ISPACK)
        call set_comm_item_rtp_4_ISPACK                                 &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(3),                      &
     &      sph_rtp%istep_rtp, sph_rtp%istack_rtp_rt_smp,               &
     &      comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                    &
     &      ispack_t%comm_sph_ISPACK)
      end if
!
      if(ALLOCATED(ispack_t%X) .eqv. .false.) then
        call alloc_work_4_ispack(sph_rtp%nnod_rtp, ncomp, ispack_t)
      else if( (ncomp*sph_rtp%nnod_rtp) .gt. size(ispack_t%X,1) ) then
        call dealloc_work_4_ispack(ispack_t)
        call alloc_work_4_ispack(sph_rtp%nnod_rtp, ncomp, ispack_t)
      end if
!
      end subroutine verify_sph_ISPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUF_to_send(sph_rtp, comm_rtp,                  &
     &          ncomp_fwd, n_WS, X_rtp, WS, ispack_t)
!
      use ispack_0931
      use set_comm_table_rtp_ISPACK
      use copy_rtp_data_to_ISPACK
      use copy_rtp_data_to_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: ip, num, ntot, ist_fft
!
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call copy_ISPACK_from_prt_field(sph_rtp%nnod_rtp,               &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      ncomp_fwd, X_rtp(1,1), ispack_t%X)
      else
        call copy_FFTPACK_from_rtp_field(sph_rtp%nnod_rtp,              &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      ncomp_fwd, X_rtp(1,1), ispack_t%X)
      end if
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
!$omp parallel do private(ip,num,ntot,ist_fft)
      do ip = 1, np_smp
        num = sph_rtp%istack_rtp_rt_smp(ip)                             &
     &       - sph_rtp%istack_rtp_rt_smp(ip-1)
        ntot = ncomp_fwd * num
        ist_fft = ncomp_fwd*sph_rtp%nidx_rtp(3)                         &
     &           *sph_rtp%istack_rtp_rt_smp(ip-1)
        call FTTRUF(ntot, sph_rtp%nidx_rtp(3), ispack_t%X(ist_fft+1),   &
     &      ispack_t%WK(ist_fft+1), ispack_t%IT, ispack_t%T)
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
!      call copy_ISPACK_field_to_send(sph_rtp%nnod_rtp,                 &
!   &      sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp,              &
!   &      ncomp_fwd, comm_rtp%irev_sr, ispack_t%X, n_WS, WS)
      call copy_all_rtp_FFT_to_send_smp(sph_rtp%nnod_rtp,               &
     &    sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                  &
     &    ncomp_fwd, ispack_t%X, ispack_t%comm_sph_ISPACK, n_WS, WS)
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
!
      end subroutine sph_FTTRUF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUB_from_recv(sph_rtp, comm_rtp,                &
     &          ncomp_bwd, n_WR, WR, X_rtp, ispack_t)
!
      use ispack_0931
      use set_comm_table_rtp_ISPACK
      use copy_rtp_data_to_ISPACK
      use copy_rtp_data_to_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: ip, num, ntot, ist_fft
!
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
      call copy_ISPACK_field_from_recv(sph_rtp%nnod_rtp,                &
     &    sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp,               &
     &    ncomp_bwd, comm_rtp%irev_sr, n_WR, WR, ispack_t%X)
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do private(ip,num,ntot,ist_fft)
      do ip = 1, np_smp
        num = sph_rtp%istack_rtp_rt_smp(ip)                             &
     &       - sph_rtp%istack_rtp_rt_smp(ip-1)
        ntot = ncomp_bwd * num
        ist_fft = ncomp_bwd*sph_rtp%nidx_rtp(3)                         &
     &           *sph_rtp%istack_rtp_rt_smp(ip-1)
        call FTTRUB(ntot, sph_rtp%nidx_rtp(3), ispack_t%X(ist_fft+1),   &
     &      ispack_t%WK(ist_fft+1), ispack_t%IT, ispack_t%T)
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call copy_ISPACK_to_prt_field(sph_rtp%nnod_rtp,                 &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp, ncomp_bwd,     &
     &      ispack_t%X, X_rtp(1,1))
      else
        call copy_FFTPACK_to_rtp_field(sph_rtp%nnod_rtp,                &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      ncomp_bwd, ispack_t%X, X_rtp(1,1))
      end if
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
!
      end subroutine sph_FTTRUB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_ispack(nnod_rtp, ncomp, ispack_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      allocate( ispack_t%X(nnod_rtp*ncomp) )
      allocate( ispack_t%WK(nnod_rtp*ncomp) )
!
!$omp parallel workshare
      ispack_t%X =  0.0d0
      ispack_t%WK = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_ispack(nfft, ispack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      allocate( ispack_t%T(2*nfft) )
      ispack_t%T = 0.0d0
!
      end subroutine alloc_const_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_ispack(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      deallocate(ispack_t%X, ispack_t%WK)
!
      end subroutine dealloc_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_ispack(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      deallocate(ispack_t%T)
!
      end subroutine dealloc_const_4_ispack
!
! ------------------------------------------------------------------
!
      end module t_sph_ISPACK_FFT
