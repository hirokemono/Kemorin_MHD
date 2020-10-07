!>@file   t_sph_domain_FFTPACK5.f90
!!@brief  module t_sph_domain_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_domain_FFTPACK5                             &
!!     &         (sph_rtp, comm_rtp, fftpack_d)
!!      subroutine finalize_sph_domain_FFTPACK5(fftpack_d)
!!      subroutine verify_sph_domain_FFTPACK5                           &
!!     &         (sph_rtp, comm_rtp, fftpack_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine rtp_domain_RFFTMF_to_send(sph_rtp, comm_rtp,         &
!!     &          ncomp_fwd, n_WS, X_rtp, WS, fftpack_d)
!!      subroutine prt_domain_RFFTMF_to_send(sph_rtp, comm_rtp,         &
!!     &          ncomp_fwd, n_WS, X_rtp, WS, fftpack_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTPACK5
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine rtp_domain_RFFTMB_from_recv(sph_rtp, comm_rtp,       &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_d)
!!      subroutine prt_domain_RFFTMB_from_recv(sph_rtp, comm_rtp,       &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_d)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTPACK5
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
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module t_sph_domain_FFTPACK5
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
!>      Structure to use ISPACK
      type work_for_domain_fftpack
!>        Size of work constant for FFTPACK
        integer(kind = kint) :: NSV
!>        Work constatnts for FFTPACK
        real(kind = 8), allocatable :: WSV(:)
!>        flag for length of Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
!
!>      Structure of communication table from FFT to send buffer
        type(comm_tbl_from_FFT) :: comm_sph_FFTPACK
!
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for FFTPACK
        real(kind = 8), allocatable :: WK(:)
      end type work_for_domain_fftpack
!
      private :: alloc_work_domain_FFTPACK, alloc_const_domain_FFTPACK
      private :: dealloc_work_domain_FFTPACK
      private :: dealloc_const_domain_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_domain_FFTPACK5                               &
     &         (sph_rtp, comm_rtp, fftpack_d)
!
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: ierr
!
!
      call alloc_const_domain_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_d)
      call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_d%WSV,                   &
     &            fftpack_d%NSV, ierr)
!
      call alloc_work_domain_FFTPACK                                    &
     &   (sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),       &
     &    fftpack_d)
!
      call alloc_comm_table_sph_FFT                                     &
     &   (comm_rtp%ntot_item_sr, fftpack_d%comm_sph_FFTPACK)
      call set_comm_item_rtp_4_FFTPACK(sph_rtp%nnod_rtp,                &
     &    sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                  &
     &    comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                      &
     &    fftpack_d%comm_sph_FFTPACK)
!
      end subroutine init_sph_domain_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_domain_FFTPACK5(fftpack_d)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
!
      call dealloc_comm_table_sph_FFT(fftpack_d%comm_sph_FFTPACK)
      call dealloc_const_domain_FFTPACK(fftpack_d)
      call dealloc_work_domain_FFTPACK(fftpack_d)
!
      end subroutine finalize_sph_domain_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_domain_FFTPACK5                             &
     &         (sph_rtp, comm_rtp, fftpack_d)
!
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: ierr
!
!
      if(fftpack_d%iflag_fft_len .ne. sph_rtp%nidx_rtp(3)) then
        call dealloc_comm_table_sph_FFT(fftpack_d%comm_sph_FFTPACK)
        call alloc_comm_table_sph_FFT                                   &
     &     (comm_rtp%ntot_item_sr, fftpack_d%comm_sph_FFTPACK)
        call set_comm_item_rtp_4_FFTPACK(sph_rtp%nnod_rtp,              &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      comm_rtp%ntot_item_sr, comm_rtp%irev_sr,                    &
     &      fftpack_d%comm_sph_FFTPACK)
!
        if(fftpack_d%iflag_fft_len .lt. 0) then
          call alloc_const_domain_FFTPACK                               &
     &       (sph_rtp%nidx_rtp(3), fftpack_d)
        else if(sph_rtp%nnod_rtp .gt. size(fftpack_d%X)) then
          call dealloc_const_domain_FFTPACK(fftpack_d)
          call alloc_const_domain_FFTPACK                               &
     &       (sph_rtp%nidx_rtp(3), fftpack_d)
        end if
!
        call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_d%WSV,                 &
     &              fftpack_d%NSV, ierr)
      end if
!
      if(allocated(fftpack_d%X) .eqv. .false.) then
        call alloc_work_domain_FFTPACK                                  &
     &     (sph_rtp%istack_rtp_rt_smp(np_smp),                          &
     &      sph_rtp%nidx_rtp(3), fftpack_d)
      else if(sph_rtp%nnod_rtp .gt. size(fftpack_d%X)) then
        call dealloc_work_domain_FFTPACK(fftpack_d)
        call alloc_work_domain_FFTPACK                                  &
     &     (sph_rtp%istack_rtp_rt_smp(np_smp),                          &
     &      sph_rtp%nidx_rtp(3), fftpack_d)
      end if
!
      end subroutine verify_sph_domain_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine rtp_domain_RFFTMF_to_send(sph_rtp, comm_rtp,           &
     &          ncomp_fwd, n_WS, X_rtp, WS, fftpack_d)
!
      use copy_rtp_data_to_FFTPACK
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: ip, num, nsize, nd
      integer(kind = kint) :: ist_fft, ierr
!
!
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
        call copy_FFTPACK_from_rtp_comp                                 &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                         &
     &      sph_rtp%istack_rtp_rt_smp, X_rtp(1,nd), fftpack_d%X)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
!$omp parallel do private(num,nsize,ist_fft)
        do ip = 1, np_smp
          num = sph_rtp%istack_rtp_rt_smp(ip)                           &
     &         - sph_rtp%istack_rtp_rt_smp(ip-1)
          nsize = num * sph_rtp%nidx_rtp(3)
          ist_fft = sph_rtp%istack_rtp_rt_smp(ip-1)                     &
     &             * sph_rtp%nidx_rtp(3)
          call RFFTMF(num, ione, sph_rtp%nidx_rtp(3), num,              &
     &        fftpack_d%X(ist_fft+1), nsize,                            &
     &        fftpack_d%WSV, fftpack_d%NSV,                             &
     &        fftpack_d%WK(ist_fft+1), nsize, ierr)
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
!        call copy_rtp_comp_FFTPACK_to_send(nd, sph_rtp%nnod_rtp,       &
!     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,               &
!     &      ncomp_fwd, comm_rtp%irev_sr, fftpack_d%X(1), n_WS, WS)
        call copy_1comp_rtp_FFT_to_send_smp                             &
     &     (nd, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                     &
     &      sph_rtp%istack_rtp_rt_smp, ncomp_fwd,                       &
     &      fftpack_d%X(1), fftpack_d%comm_sph_FFTPACK, n_WS, WS)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
!
      end subroutine rtp_domain_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine rtp_domain_RFFTMB_from_recv(sph_rtp, comm_rtp,         &
     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_d)
!
      use copy_rtp_data_to_FFTPACK
      use set_comm_table_rtp_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: ip, num, nsize, nd
      integer(kind = kint) :: ierr, ist_fft
!
!
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
        call copy_FFTPACK_comp_from_recv(nd, sph_rtp%nnod_rtp,          &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp, ncomp_bwd,     &
     &      comm_rtp%irev_sr, n_WR, WR, fftpack_d%X(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do private(num,nsize,ist_fft)
        do ip = 1, np_smp
          num = sph_rtp%istack_rtp_rt_smp(ip)                           &
     &         - sph_rtp%istack_rtp_rt_smp(ip-1)
          nsize = num * sph_rtp%nidx_rtp(3)
          ist_fft = sph_rtp%istack_rtp_rt_smp(ip-1)                     &
     &             * sph_rtp%nidx_rtp(3)
          call RFFTMB(num, ione, sph_rtp%nidx_rtp(3), num,              &
     &        fftpack_d%X(ist_fft+1), nsize,                            &
     &        fftpack_d%WSV, fftpack_d%NSV,                             &
     &        fftpack_d%WK(ist_fft+1), nsize, ierr)
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
        call copy_FFTPACK_to_rtp_comp(sph_rtp%nnod_rtp,                 &
     &      sph_rtp%nidx_rtp, sph_rtp%istack_rtp_rt_smp,                &
     &      fftpack_d%X, X_rtp(1,nd))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
!
      end subroutine rtp_domain_RFFTMB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine prt_domain_RFFTMF_to_send(sph_rtp, comm_rtp,           &
     &          ncomp_fwd, n_WS, X_rtp, WS, fftpack_d)
!
      use copy_rtp_data_to_FFTPACK
      use set_comm_table_prt_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: num, nsize, j, nd
      integer(kind = kint) :: ist_fft, ierr
!
!      write(*,*) 'tako'
!
      do nd = 1, ncomp_fwd
          call sel_copy_single_FFT_to_rtp2                            &
     &       (sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                 &
     &        X_rtp(1,nd), fftpack_d%X(1))
!
!$omp parallel do private(j)
        do j = 1, sph_rtp%istack_rtp_rt_smp(np_smp)
          call RFFTMF(ione, ione, sph_rtp%nidx_rtp(3), ione,          &
     &          fftpack_d%X( 1+(j-1)*sph_rtp%nidx_rtp(3) ),  sph_rtp%nidx_rtp(3),                &
     &          fftpack_d%WSV, fftpack_d%NSV, fftpack_d%WK( 1+(j-1)*sph_rtp%nidx_rtp(3) ),       &
     &          sph_rtp%nidx_rtp(3), ierr)
        end do
!$omp end parallel do
!
          call copy_single_RFFTMF_to_send2                            &
     &         (nd, sph_rtp%nnod_rtp, comm_rtp%irev_sr,              &
     &          sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp(np_smp), &
     &          ncomp_fwd, fftpack_d%X(1), n_WS, WS)
      end do
!
      end subroutine prt_domain_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine prt_domain_RFFTMB_from_recv(sph_rtp, comm_rtp,         &
     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_d)
!
      use copy_rtp_data_to_FFTPACK
      use set_comm_table_prt_FFTPACK
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: num, nsize, nd, j
      integer(kind = kint) :: ierr, ist_fft
!
!      write(*,*) 'aho'
!
      do nd = 1, ncomp_bwd
          call copy_single_RFFTMB_from_recv2                          &
     &         (nd, sph_rtp%nnod_rtp, comm_rtp%irev_sr,              &
     &          sph_rtp%nidx_rtp(3), sph_rtp%istack_rtp_rt_smp(np_smp), &
     &          ncomp_bwd, n_WR, WR, fftpack_d%X(1))
!
!$omp parallel do private(j)
        do j = 1, sph_rtp%istack_rtp_rt_smp(np_smp)
          call RFFTMB(ione, ione, sph_rtp%nidx_rtp(3), ione,          &
     &          fftpack_d%X( 1+(j-1)*sph_rtp%nidx_rtp(3) ), sph_rtp%nidx_rtp(3),                 &
     &          fftpack_d%WSV, fftpack_d%NSV, fftpack_d%WK( 1+(j-1)*sph_rtp%nidx_rtp(3) ),       &
     &          sph_rtp%nidx_rtp(3), ierr)
        end do
!$omp end parallel do
!
          call sel_copy_single_FFT_to_rtp2                            &
     &       (sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                 &
     &        fftpack_d%X(1), X_rtp(1,nd))
      end do
!
      end subroutine prt_domain_RFFTMB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_domain_FFTPACK(Ncomp, Nfft, fftpack_d)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
!
      allocate( fftpack_d%X(Ncomp*Nfft) )
      allocate( fftpack_d%WK(Ncomp*Nfft) )
      fftpack_d%WK = 0.0d0
!
      end subroutine alloc_work_domain_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_domain_FFTPACK(nfft, fftpack_d)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      integer(kind = kint) :: itmp
!
      fftpack_d%iflag_fft_len = nfft
      itmp = int(log(real(Nfft)) / log(two),KIND(itmp))
      fftpack_d%NSV = nfft + itmp + ifour
      allocate(fftpack_d%WSV(fftpack_d%NSV) )
      fftpack_d%WSV = 0.0d0
!
      end subroutine alloc_const_domain_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_domain_FFTPACK(fftpack_d)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
!
      deallocate(fftpack_d%X, fftpack_d%WK)
!
      end subroutine dealloc_work_domain_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_domain_FFTPACK(fftpack_d)
!
      type(work_for_domain_fftpack), intent(inout) :: fftpack_d
!
      deallocate( fftpack_d%WSV )
      fftpack_d%iflag_fft_len = 0
!
      end subroutine dealloc_const_domain_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_single_RFFTMF_to_send2                            &
     &         (nd, nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,           &
     &          ncomp_fwd, X_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_fft(nnod_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: j, m, jst
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do private(j,m,ic_rtp,is_rtp,ic_send,is_send,jst)
      do j = 1, nnod_rt
        jst = (j-1)*mphi_rtp
!
        is_rtp = j + nnod_rt
        ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp_fwd
        is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        WS(ic_send) = X_fft(1+jst)
        WS(is_send) = X_fft(mphi_rtp+jst)
        do m = 1, mphi_rtp/2 - 1
          ic_rtp = j + (2*m  ) * nnod_rt
          is_rtp = j + (2*m+1) * nnod_rt
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send) = X_fft(2*m  +jst)
          WS(is_send) = X_fft(2*m+1+jst)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_single_RFFTMF_to_send2
!
! ------------------------------------------------------------------
!
      subroutine copy_single_RFFTMB_from_recv2                          &
     &         (nd, nnod_rtp, irev_sr_rtp, mphi_rtp, nnod_rt,        &
     &          ncomp_bwd, n_WR, WR, X_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_fft(mphi_rtp)
!
      integer(kind = kint) :: j, m, jst
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(j,m,ic_rtp,is_rtp,ic_recv,is_recv,jst)
      do j = 1, nnod_rt
        jst = (j-1)*mphi_rtp
!
        is_rtp = j + nnod_rt
        ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp_bwd
        is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
        X_fft(1+jst) =           WR(ic_recv)
        X_fft(mphi_rtp+jst) = WR(is_recv)
        do m = 1, mphi_rtp/2 - 1
          ic_rtp = j + (2*m  ) * nnod_rt
          is_rtp = j + (2*m+1) * nnod_rt
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          X_fft(2*m  +jst) = WR(ic_recv)
          X_fft(2*m+1+jst) = WR(is_recv)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_single_RFFTMB_from_recv2
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_copy_single_FFT_to_rtp2                            &
     &         (nnod_rtp, istep_phi, X_fft, X_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: istep_phi
      real(kind = kreal), intent(in) :: X_fft(nnod_rtp)
!
      real(kind = kreal), intent(inout) :: X_rtp(nnod_rtp)
!
!
!$omp parallel workshare
      X_rtp(1:nnod_rtp) = X_fft(1:nnod_rtp)
!$omp end parallel workshare
!
      end subroutine sel_copy_single_FFT_to_rtp2
!
! ------------------------------------------------------------------
!
      end module t_sph_domain_FFTPACK5
