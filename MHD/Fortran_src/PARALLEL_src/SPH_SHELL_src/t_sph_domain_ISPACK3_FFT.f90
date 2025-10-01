!>@file   t_sph_domain_ISPACK3_FFT.f90
!!@brief  module t_sph_domain_ISPACK3_FFT
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
!!      subroutine init_sph_domain_ISPACK3                              &
!!     &         (sph_rtp, comm_rtp, ispack3_d)
!!      subroutine finalize_sph_domain_ISPACK3(ispack3_d)
!!      subroutine verify_sph_domain_ISPACK3                            &
!!     &         (sph_rtp, comm_rtp, ispack3_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sph_domain_FXRTFA_to_send                            &
!!     &         (sph_rtp, ncomp_fwd, n_WS, X_rtp, WS, ispack3_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        integer(kind = kint_gl), intent(in) :: ncomp_fwd
!!        real(kind = kreal), intent(in)                                &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!!        integer(kind = kint), intent(in) :: n_WS
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!        type(work_for_domain_ispack3), intent(inout) :: ispack3_d
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
!!      subroutine sph_domain_FXRTBA_from_recv(sph_rtp, comm_rtp,       &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, ispack3_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        integer(kind = kint_gl), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        real (kind=kreal), intent(inout):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_bwd)
!!        type(work_for_domain_ispack3), intent(inout) :: ispack3_d
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
      module t_sph_domain_ISPACK3_FFT
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
      type work_for_domain_ispack3
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = kint_gl), allocatable :: IT(:)
!
!>      Structure of communication table from FFT to send buffer
        type(comm_tbl_from_FFT) :: comm_sph_ISPACK3
      end type work_for_domain_ispack3
!
      private :: alloc_work_domain_ispack3, alloc_const_domain_ispack3
      private :: dealloc_work_domain_ispack3
      private :: dealloc_const_domain_ispack3
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_domain_ISPACK3(sph_rtp, comm_rtp, ispack3_d)
!
      use transfer_to_long_integers
      use set_comm_table_rtp_ISPACK
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      call alloc_const_domain_ispack3(sph_rtp%nidx_rtp(3), ispack3_d)
      call FXRINI(cast_long(sph_rtp%nidx_rtp(3)),                       &
     &            ispack3_d%IT, ispack3_d%T)
!
      call alloc_work_domain_ispack3(sph_rtp%nnod_rtp, ispack3_d)
!
      call alloc_comm_table_sph_FFT                                     &
     &   (comm_rtp%ntot_item_sr, ispack3_d%comm_sph_ISPACK3)
      call set_comm_item_rtp_4_ISPACK                                   &
     &   (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(3),                        &
     &    sph_rtp%istep_rtp, sph_rtp%istack_rtp_rt_smp,                 &
     &    comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                      &
     &    ispack3_d%comm_sph_ISPACK3)
!
      end subroutine init_sph_domain_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_domain_ISPACK3(ispack3_d)
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      call dealloc_comm_table_sph_FFT(ispack3_d%comm_sph_ISPACK3)
      call dealloc_const_domain_ispack3(ispack3_d)
      call dealloc_work_domain_ispack3(ispack3_d)
!
      end subroutine finalize_sph_domain_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_domain_ISPACK3                              &
     &         (sph_rtp, comm_rtp, ispack3_d)
!
      use transfer_to_long_integers
      use set_comm_table_rtp_ISPACK
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      if((2*sph_rtp%nidx_rtp(3)) .ne. size(ispack3_d%T)) then
!
        if(allocated(ispack3_d%T) .eqv. .false.) then
          call alloc_const_domain_ispack3(sph_rtp%nidx_rtp(3),          &
     &                                    ispack3_d)
        else if( (2*sph_rtp%nidx_rtp(3)) .gt. size(ispack3_d%T) ) then
          call dealloc_const_domain_ispack3(ispack3_d)
          call alloc_const_domain_ispack3(sph_rtp%nidx_rtp(3),          &
     &                                    ispack3_d)
        end if
!
        call FXRINI(cast_long(sph_rtp%nidx_rtp(3)),                     &
     &              ispack3_d%IT(1), ispack3_d%T(1))
!
        call dealloc_comm_table_sph_FFT(ispack3_d%comm_sph_ISPACK3)
        call alloc_comm_table_sph_FFT                                   &
     &     (comm_rtp%ntot_item_sr, ispack3_d%comm_sph_ISPACK3)
        call set_comm_item_rtp_4_ISPACK                                 &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(3),                      &
     &      sph_rtp%istep_rtp, sph_rtp%istack_rtp_rt_smp,               &
     &      comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                    &
     &      ispack3_d%comm_sph_ISPACK3)
      end if
!
      if(ALLOCATED(ispack3_d%X) .eqv. .false.) then
        call alloc_work_domain_ispack3(sph_rtp%nnod_rtp, ispack3_d)
      else if(sph_rtp%nnod_rtp .gt. size(ispack3_d%X,1)) then
        call dealloc_work_domain_ispack3(ispack3_d)
        call alloc_work_domain_ispack3(sph_rtp%nnod_rtp, ispack3_d)
      end if
!
      end subroutine verify_sph_domain_ISPACK3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_domain_FXRTFA_to_send                              &
     &         (sph_rtp, ncomp_fwd, n_WS, X_rtp, WS, ispack3_d)
!
      use transfer_to_long_integers
      use set_comm_table_rtp_ISPACK
      use copy_rtp_data_to_ISPACK
      use copy_rtp_data_to_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
      integer(kind = kint) :: ip, nd, num, ist_fft
!
!
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call copy_ISPACK_from_prt_comp                                &
     &       (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                       &
     &        sph_rtp%istack_rtp_rt_smp, X_rtp(1,nd), ispack3_d%X(1))
        else
          call copy_FFTPACK_from_rtp_comp                               &
     &       (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                       &
     &        sph_rtp%istack_rtp_rt_smp, X_rtp(1,nd), ispack3_d%X(1))
        end if
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
!$omp parallel do private(ip,num,ist_fft)
        do ip = 1, np_smp
          num = sph_rtp%istack_rtp_rt_smp(ip)                           &
     &         - sph_rtp%istack_rtp_rt_smp(ip-1)
          ist_fft = sph_rtp%istack_rtp_rt_smp(ip-1)                     &
     &             * sph_rtp%nidx_rtp(3)
          call FXRTFA(cast_long(num), cast_long(sph_rtp%nidx_rtp(3)),   &
     &        ispack3_d%X(ist_fft+1), ispack3_d%IT(1), ispack3_d%T(1))
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
        call copy_1comp_rtp_FFT_to_send_smp                             &
     &     (nd, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                     &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_fwd,                       &
     &      ispack3_d%X, ispack3_d%comm_sph_ISPACK3, n_WS, WS)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
!
      end subroutine sph_domain_FXRTFA_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_domain_FXRTBA_from_recv(sph_rtp, comm_rtp,         &
     &          ncomp_bwd, n_WR, WR, X_rtp, ispack3_d)
!
      use transfer_to_long_integers
      use set_comm_table_rtp_ISPACK
      use copy_rtp_data_to_ISPACK
      use copy_rtp_data_to_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd
!
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
      integer(kind = kint) :: ip, nd, num, ist_fft
!
!
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
        call copy_ISPACK_comp_from_recv                                 &
     &     (nd, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(3),                  &
     &      sph_rtp%istep_rtp, sph_rtp%istack_rtp_rt_smp,               &
     &      ncomp_bwd, comm_rtp%irev_sr, n_WR, WR, ispack3_d%X)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do private(ip,num,ist_fft)
        do ip = 1, np_smp
          num = sph_rtp%istack_rtp_rt_smp(ip)                           &
     &         - sph_rtp%istack_rtp_rt_smp(ip-1)
          ist_fft = sph_rtp%istack_rtp_rt_smp(ip-1)                     &
     &             * sph_rtp%nidx_rtp(3)
          call FXRTBA(cast_long(num), cast_long(sph_rtp%nidx_rtp(3)),   &
     &        ispack3_d%X(ist_fft+1), ispack3_d%IT(1), ispack3_d%T(1))
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call copy_ISPACK_to_prt_comp                                  &
     &       (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                       &
     &        sph_rtp%istack_rtp_rt_smp, ispack3_d%X, X_rtp(1,nd))
        else
          call copy_FFTPACK_to_rtp_comp                                 &
     &       (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                       &
     &        sph_rtp%istack_rtp_rt_smp, ispack3_d%X, X_rtp(1,nd))
        end if
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
!
      end subroutine sph_domain_FXRTBA_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_domain_ispack3(nnod_rtp, ispack3_d)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      allocate( ispack3_d%X(nnod_rtp) )
!
      end subroutine alloc_work_domain_ispack3
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_domain_ispack3(nfft, ispack3_d)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      allocate( ispack3_d%T(nfft+nfft/2) )
      allocate( ispack3_d%IT(nfft/2) )
      ispack3_d%T =  0.0d0
      ispack3_d%IT = 0
!
      end subroutine alloc_const_domain_ispack3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_domain_ispack3(ispack3_d)
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      deallocate(ispack3_d%X)
!
      end subroutine dealloc_work_domain_ispack3
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_domain_ispack3(ispack3_d)
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      deallocate(ispack3_d%T, ispack3_d%IT)
!
      end subroutine dealloc_const_domain_ispack3
!
! ------------------------------------------------------------------
!
      end module t_sph_domain_ISPACK3_FFT
