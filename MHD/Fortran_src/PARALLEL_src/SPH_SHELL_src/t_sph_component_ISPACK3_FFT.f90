!>@file   t_sph_component_ISPACK3_FFT.f90
!!@brief  module t_sph_component_ISPACK3_FFT
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
!!      subroutine init_sph_comp_ISPACK3                                &
!!     &         (sph_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!!      subroutine finalize_sph_comp_ISPACK3(ispack3_c)
!!      subroutine verify_sph_comp_ISPACK3                              &
!!     &         (sph_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        integer(kind = kint), intent(in) :: maxirt_rtp_smp
!!        type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sph_comp_FXRTFA_to_send(sph_rtp, comm_rtp,           &
!!     &          ncomp_fwd, n_WS, X_rtp, WS, ispack3_c)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        real(kind = kreal), intent(in)                                &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!!        integer(kind = kint), intent(in) :: n_WS
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!        type(work_for_comp_ispack3), intent(inout) :: ispack3_c
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
!!      subroutine sph_comp_FXRTBA_from_recv(sph_rtp, comm_rtp,         &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, ispack3_c)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in)  :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        real (kind=kreal), intent(inout):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_bwd)
!!        type(work_for_comp_ispack3), intent(inout) :: ispack3_c
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
      module t_sph_component_ISPACK3_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      implicit none
!
!
!>      Structure to use ISPACK
      type work_for_comp_ispack3
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:,:)
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = kint_gl), allocatable :: IT(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_comp_ispack3
!
      private :: alloc_work_comp_ispack3, alloc_const_comp_ispack3
      private :: dealloc_work_comp_ispack3, dealloc_const_comp_ispack3
      private :: copy_comp_ISPACK3_to_send, copy_comp_ISPACK3_from_recv
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_comp_ISPACK3                                  &
     &         (sph_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!
      use transfer_to_long_integers
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      call alloc_const_comp_ispack3(sph_rtp%nidx_rtp(3), ispack3_c)
      call FXRINI(cast_long(sph_rtp%nidx_rtp(3)),                       &
     &            ispack3_c%IT, ispack3_c%T)
!
      call alloc_work_comp_ispack3                                      &
     &   (sph_rtp%nidx_rtp(3), ncomp_bwd, ncomp_fwd, ispack3_c)
!
      allocate(ispack3_c%t_omp(np_smp,0:3))
      ispack3_c%t_omp = 0.0d0
!
      end subroutine init_sph_comp_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_comp_ISPACK3(ispack3_c)
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      call dealloc_const_comp_ispack3(ispack3_c)
      call dealloc_work_comp_ispack3(ispack3_c)
      deallocate(ispack3_c%t_omp)
!
      end subroutine finalize_sph_comp_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_comp_ISPACK3                                &
     &         (sph_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!
      use transfer_to_long_integers
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      if((2*sph_rtp%nidx_rtp(3)) .ne. size(ispack3_c%T)) then
!
        if(allocated(ispack3_c%T) .eqv. .false.) then
          call alloc_const_comp_ispack3(sph_rtp%nidx_rtp(3), ispack3_c)
        else if( (2*sph_rtp%nidx_rtp(3)) .gt. size(ispack3_c%T) ) then
          call dealloc_const_comp_ispack3(ispack3_c)
          call alloc_const_comp_ispack3(sph_rtp%nidx_rtp(3), ispack3_c)
        end if
!
        call FXRINI(cast_long(sph_rtp%nidx_rtp(3)),                     &
     &              ispack3_c%IT(1), ispack3_c%T(1))
      end if
!
      if(ALLOCATED(ispack3_c%X) .eqv. .false.) then
        call alloc_work_comp_ispack3                                    &
     &     (sph_rtp%nidx_rtp(3), ncomp_bwd, ncomp_fwd, ispack3_c)
      else if( (sph_rtp%nidx_rtp(3)*max(ncomp_bwd, ncomp_fwd))          &
     &       .gt. size(ispack3_c%X,1) ) then
        call dealloc_work_comp_ispack3(ispack3_c)
        call alloc_work_comp_ispack3                                    &
     &     (sph_rtp%nidx_rtp(3), ncomp_bwd, ncomp_fwd, ispack3_c)
      end if
!
      end subroutine verify_sph_comp_ISPACK3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_comp_FXRTFA_to_send(sph_rtp, comm_rtp,             &
     &          ncomp_fwd, n_WS, X_rtp, WS, ispack3_c)
!
      use transfer_to_long_integers
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
      integer(kind = kint) :: m, j, ip, ist, ied
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,m,j,ist,ied)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip)
        do j = ist, ied
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call sel_copy_comp_rtp_to_FFT                                 &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_fwd, X_rtp(1,1), ispack3_c%X(1,ip))
          if(iflag_FFT_time) ispack3_c%t_omp(ip,1)                      &
     &                      = ispack3_c%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call FXRTFA                                                   &
     &       (cast_long(ncomp_fwd), cast_long(sph_rtp%nidx_rtp(3)),     &
     &        ispack3_c%X(1,ip), ispack3_c%IT(1), ispack3_c%T(1))
          if(iflag_FFT_time) ispack3_c%t_omp(ip,2)                      &
     &                      = ispack3_c%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call copy_comp_ISPACK3_to_send(j, sph_rtp%nnod_rtp,           &
     &        sph_rtp%nidx_rtp(3), sph_rtp%istep_rtp, comm_rtp%irev_sr, &
     &        ncomp_fwd, ispack3_c%X(1,ip), n_WS, WS)
          if(iflag_FFT_time) ispack3_c%t_omp(ip,3)                      &
     &                      = ispack3_c%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, ispack3_c%t_omp(1,1),        &
     &      elps1%elapsed(ist_elapsed_FFT+4))
      end if
!
      end subroutine sph_comp_FXRTFA_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_comp_FXRTBA_from_recv(sph_rtp, comm_rtp,           &
     &          ncomp_bwd, n_WR, WR, X_rtp, ispack3_c)
!
      use transfer_to_long_integers
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in)  :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
!
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
      integer(kind = kint) ::  m, j, ip, ist, ied
      integer(kind = kint) ::  inod_s, inod_c
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,m,j,ist,ied,inod_s,inod_c)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip)
        do j = ist, ied
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call copy_comp_ISPACK3_from_recv(j, sph_rtp%nnod_rtp,         &
     &        sph_rtp%nidx_rtp(3), sph_rtp%istep_rtp, comm_rtp%irev_sr, &
     &        ncomp_bwd, n_WR, WR, ispack3_c%X(1,ip))
          if(iflag_FFT_time) ispack3_c%t_omp(ip,1)                      &
     &                      = ispack3_c%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call FXRTBA                                                   &
     &       (cast_long(ncomp_bwd), cast_long(sph_rtp%nidx_rtp(3)),     &
     &        ispack3_c%X(1,ip), ispack3_c%IT(1), ispack3_c%T(1))
          if(iflag_FFT_time) ispack3_c%t_omp(ip,2)                      &
     &                      = ispack3_c%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call sel_copy_comp_FFT_to_rtp                                 &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_bwd, ispack3_c%X(1,ip), X_rtp(1,1))
          if(iflag_FFT_time) ispack3_c%t_omp(ip,3)                      &
     &                      = ispack3_c%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, ispack3_c%t_omp(1,1),        &
     &      elps1%elapsed(ist_elapsed_FFT+1))
      end if
!
      end subroutine sph_comp_FXRTBA_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_comp_ispack3                                &
     &         (Nfft, ncomp_bwd, ncomp_fwd, ispack3_c)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
      integer(kind = kint) :: nsize
!
!
      nsize = Nfft * max(ncomp_bwd, ncomp_fwd)
      allocate( ispack3_c%X(nsize,np_smp) )
!
      end subroutine alloc_work_comp_ispack3
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_comp_ispack3(nfft, ispack3_c)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      allocate( ispack3_c%T(nfft+nfft/2) )
      allocate( ispack3_c%IT(nfft/2) )
      ispack3_c%T =  0.0d0
      ispack3_c%IT = 0
!
      end subroutine alloc_const_comp_ispack3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_comp_ispack3(ispack3_c)
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      deallocate(ispack3_c%X)
!
      end subroutine dealloc_work_comp_ispack3
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_comp_ispack3(ispack3_c)
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      deallocate(ispack3_c%T, ispack3_c%IT)
!
      end subroutine dealloc_const_comp_ispack3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_comp_ISPACK3_to_send                              &
     &         (j, nnod_rtp, nphi_rtp, istep_rtp, irev_sr_rtp,          &
     &          ncomp_fwd, X_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nphi_rtp, istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_fft(ncomp_fwd*nphi_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: j0_rtp, inod_s, inod_c
      integer(kind = kint) :: m, ic_rtp, is_rtp, ic_send, is_send
!
!
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      ic_rtp = j0_rtp
      is_rtp = j0_rtp + istep_rtp(3)
      ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
      is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
      WS(ic_send+1:ic_send+ncomp_fwd) = X_fft(1:ncomp_fwd)
      WS(is_send+1:is_send+ncomp_fwd)                               &
     &        = X_fft(ncomp_fwd+1:ncomp_fwd+ncomp_fwd)
      do m = 2, nphi_rtp/2
        ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
        ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        inod_c = (2*m-2) * ncomp_fwd
        inod_s = (2*m-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                             &
     &         =   two * X_fft(inod_c+1:inod_c+ncomp_fwd)
        WS(is_send+1:is_send+ncomp_fwd)                             &
     &         = - two * X_fft(inod_s+1:inod_s+ncomp_fwd)
      end do
!
      end subroutine copy_comp_ISPACK3_to_send
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_comp_ISPACK3_from_recv                            &
     &         (j, nnod_rtp, nphi_rtp, istep_rtp, irev_sr_rtp,          &
     &          ncomp_bwd, n_WR, WR, X_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nphi_rtp, istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_fft(ncomp_bwd*nphi_rtp)
!
      integer(kind = kint) :: j0_rtp, inod_s, inod_c
      integer(kind = kint) :: m, ic_rtp, is_rtp, ic_recv, is_recv
!
!
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      ic_rtp = j0_rtp
      is_rtp = j0_rtp + istep_rtp(3)
      ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
      is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
      X_fft(1:ncomp_bwd) = WR(ic_recv+1:ic_recv+ncomp_bwd)
      X_fft(ncomp_bwd+1:ncomp_bwd+ncomp_bwd)                            &
     &           = WR(is_recv+1:is_recv+ncomp_bwd)
      do m = 2, nphi_rtp/2
        inod_c = (2*m-2) * ncomp_bwd
        inod_s = (2*m-1) * ncomp_bwd
        ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
        ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
        X_fft(inod_c+1:inod_c+ncomp_bwd)                                &
     &                =  half * WR(ic_recv+1:ic_recv+ncomp_bwd)
        X_fft(inod_s+1:inod_s+ncomp_bwd)                                &
     &                = -half * WR(is_recv+1:is_recv+ncomp_bwd)
      end do
!
      end subroutine copy_comp_ISPACK3_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_component_ISPACK3_FFT
