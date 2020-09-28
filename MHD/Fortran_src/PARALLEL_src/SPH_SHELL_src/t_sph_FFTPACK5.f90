!>@file   t_sph_FFTPACK5.f90
!!@brief  module t_sph_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_FFTPACK5(sph_rtp, comm_rtp,                 &
!!     &                             ncomp_bwd, ncomp_fwd, fftpack_t)
!!      subroutine finalize_sph_FFTPACK5(fftpack_t)
!!      subroutine verify_sph_FFTPACK5(sph_rtp, comm_rtp,               &
!!     &                               ncomp_bwd, ncomp_fwd, fftpack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_fftpack), intent(inout) :: fftpack_t
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine sph_RFFTMF_to_send(nnod_rtp, nidx_rtp,               &
!!     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,      &
!!     &          X_rtp, WS, fftpack_t)
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
!!      subroutine sph_RFFTMB_from_recv(nnod_rtp, nidx_rtp,             &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,      &
!!     &          WR, X_rtp, fftpack_t)
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
      module t_sph_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use calypso_mpi
!
      implicit none
!
!>      Structure for each FFTPACK
      type work_each_fftpack
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for FFTPACK
        real(kind = 8), allocatable :: WK(:)
      end type work_each_fftpack
!
!>      Structure to use ISPACK
      type work_for_fftpack
!>      Structure for each thread
        type(work_each_fftpack), allocatable :: smp(:)
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
!>        Size of work constant for FFTPACK
        integer(kind = kint) :: NSV
!>        Work constatnts for FFTPACK
        real(kind = 8), allocatable :: WSV(:)
!>        flag for length of Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
!
!         radial-latitude address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: kl_fftw(:)
!         longitudinal address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: m_fftw(:)
!         Normalization on FFTPACK data from SEND buffer 
        real(kind = kreal), allocatable :: rnorm_sr_rtp(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_fftpack
!
      private :: alloc_work_4_FFTPACK, alloc_const_4_FFTPACK
      private :: dealloc_work_4_FFTPACK, dealloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFTPACK5(sph_rtp, comm_rtp,                   &
     &                             ncomp_bwd, ncomp_fwd, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      fftpack_t%Mmax_smp                                                &
     &    = sph_rtp%maxirt_rtp_smp * max(ncomp_bwd, ncomp_fwd)
!
      call alloc_const_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV,    &
     &    ierr)
!
      call alloc_work_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
!
      call alloc_comm_table_sph_FFTPACK                                 &
     &   (comm_rtp%ntot_item_sr, fftpack_t)
      call set_comm_item_rtp_4_FFTPACK                                  &
     &   (sph_rtp%nidx_rtp, sph_rtp%istep_rtp,                          &
     &    comm_rtp%ntot_item_sr, comm_rtp%item_sr,                      &
     &    fftpack_t%kl_fftw, fftpack_t%m_fftw, fftpack_t%rnorm_sr_rtp)
!
      allocate(fftpack_t%t_omp(np_smp,0:3))
      fftpack_t%t_omp = 0.0d0
!
      end subroutine init_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFTPACK5(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
!
      call dealloc_comm_table_sph_FFTPACK(fftpack_t)
      call dealloc_const_4_FFTPACK(fftpack_t)
      call dealloc_work_4_FFTPACK(fftpack_t)
      deallocate(fftpack_t%t_omp)
!
      end subroutine finalize_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFTPACK5(sph_rtp, comm_rtp,                 &
     &                               ncomp_bwd, ncomp_fwd, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      fftpack_t%Mmax_smp                                                &
     &    = sph_rtp%maxirt_rtp_smp * max(ncomp_bwd, ncomp_fwd)
!
      if(fftpack_t%iflag_fft_len .ne. sph_rtp%nidx_rtp(3)) then
        call dealloc_comm_table_sph_FFTPACK(fftpack_t)
        call alloc_comm_table_sph_FFTPACK                               &
     &    (comm_rtp%ntot_item_sr, fftpack_t)
        call set_comm_item_rtp_4_FFTPACK                                &
     &    (sph_rtp%nidx_rtp, sph_rtp%istep_rtp,                         &
     &     comm_rtp%ntot_item_sr, comm_rtp%item_sr,                     &
     &     fftpack_t%kl_fftw, fftpack_t%m_fftw, fftpack_t%rnorm_sr_rtp)
!
        if(fftpack_t%iflag_fft_len .lt. 0) then
          call alloc_const_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        else if((fftpack_t%Mmax_smp*sph_rtp%nidx_rtp(3))                &
     &          .gt. size(fftpack_t%smp(1)%WK) ) then
          call dealloc_const_4_FFTPACK(fftpack_t)
          call alloc_const_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        end if
!
        call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV,  &
     &              ierr)
      end if
!
      if(allocated(fftpack_t%smp(1)%WK) .eqv. .false.) then
        call alloc_work_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      else if( (fftpack_t%Mmax_smp*sph_rtp%nidx_rtp(3))                 &
     &      .gt. size(fftpack_t%smp(1)%WK,1)  ) then
        call dealloc_work_4_FFTPACK(fftpack_t)
        call alloc_work_4_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      end if
!
      end subroutine verify_sph_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMF_to_send(nnod_rtp, nidx_rtp,                 &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, fftpack_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: m, j, ip, ist, num, ntot, inum, nsize, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c, ierr
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        fftpack_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(m,j,nd,ist,num,ntot,inum,nsize,inod_s,inod_c, &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num =  irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_fwd * num
        nsize = ntot*nidx_rtp(3)
!
        if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
        do m = 1, nidx_rtp(3)
          do j = 1, num
            do nd = 1, ncomp_fwd
              inum = nd + (j-1) * ncomp_fwd
              inod_c = inum + (m-1) * ncomp_fwd*num
              fftpack_t%smp(ip)%X(inod_c) = X_rtp(j+ist,m,nd)
            end do
          end do
        end do
        if(iflag_FFT_time) fftpack_t%t_omp(ip,1)= fftpack_t%t_omp(ip,1) &
     &                    + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
        if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
        call RFFTMF(ntot, ione, nidx_rtp(3), ntot, fftpack_t%smp(ip)%X, &
     &      nsize, fftpack_t%WSV, fftpack_t%NSV, fftpack_t%smp(ip)%WK,  &
     &      nsize, ierr)
        if(iflag_FFT_time) fftpack_t%t_omp(ip,2)= fftpack_t%t_omp(ip,2) &
     &                    + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
        if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
        call copy_FFTPACK_to_send_smp_1(nnod_rtp, nidx_rtp, ist, num,   &
     &      ncomp_fwd, irev_sr_rtp, fftpack_t%smp(ip), n_WS, WS)
        if(iflag_FFT_time) fftpack_t%t_omp(ip,3)= fftpack_t%t_omp(ip,3) &
     &                    + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          fftpack_t%t_omp(1,1) = fftpack_t%t_omp(1,1)                   &
     &                          + fftpack_t%t_omp(ip,1)
          fftpack_t%t_omp(1,2) = fftpack_t%t_omp(1,2)                   &
     &                          + fftpack_t%t_omp(ip,2)
          fftpack_t%t_omp(1,3) = fftpack_t%t_omp(1,3)                   &
     &                          + fftpack_t%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + fftpack_t%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + fftpack_t%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + fftpack_t%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMB_from_recv(nnod_rtp, nidx_rtp,               &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,        &
     &          WR, X_rtp, fftpack_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_bwd)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: m, j, ip, ist, inum, num, ntot, nsize, nd
      integer(kind = kint) :: inod_s, inod_c, ierr
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        fftpack_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(m,j,nd,ist,num,ntot,inum,nsize,inod_s,inod_c, &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd*num
        nsize = ntot*nidx_rtp(3)
!
!   normalization
        if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
        do j = 1, num
          do nd = 1, ncomp_bwd
            inum = nd + (j-1) * ncomp_bwd
            inod_s = inum + (nidx_rtp(3)-1) * ncomp_bwd*num
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            fftpack_t%smp(ip)%X(inum) =   WR(ic_recv)
            fftpack_t%smp(ip)%X(inod_s) = WR(is_recv)
          end do
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (2*m-1) * ncomp_bwd*num
              inod_s = inum + (2*m  ) * ncomp_bwd*num
              ic_rtp = j+ist + (2*m  ) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m+1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              fftpack_t%smp(ip)%X(inod_c) = WR(ic_recv)
              fftpack_t%smp(ip)%X(inod_s) = WR(is_recv)
            end do
          end do
        end do
        if(iflag_FFT_time) fftpack_t%t_omp(ip,1)= fftpack_t%t_omp(ip,1) &
     &                    + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
        if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
        call RFFTMB(ntot, ione, nidx_rtp(3), ntot, fftpack_t%smp(ip)%X, &
     &      nsize, fftpack_t%WSV, fftpack_t%NSV, fftpack_t%smp(ip)%WK,  &
     &      nsize, ierr)
        if(iflag_FFT_time) fftpack_t%t_omp(ip,2)= fftpack_t%t_omp(ip,2) &
     &                    + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
        if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
        do m = 1, nidx_rtp(3)
          do j = 1, num
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (m-1) * ncomp_bwd*num
              X_rtp(j+ist,m,nd) = fftpack_t%smp(ip)%X(inod_c)
            end do
          end do
        end do
        if(iflag_FFT_time) fftpack_t%t_omp(ip,3)= fftpack_t%t_omp(ip,3) &
     &                    + MPI_WTIME() - fftpack_t%t_omp(ip,0)
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          fftpack_t%t_omp(1,1) = fftpack_t%t_omp(1,1)                   &
     &                          + fftpack_t%t_omp(ip,1)
          fftpack_t%t_omp(1,2) = fftpack_t%t_omp(1,2)                   &
     &                          + fftpack_t%t_omp(ip,2)
          fftpack_t%t_omp(1,3) = fftpack_t%t_omp(1,3)                   &
     &                          + fftpack_t%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + fftpack_t%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + fftpack_t%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + fftpack_t%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_RFFTMB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTPACK(Nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: Nfft
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: iflag_fft_comp, ip
!
!
      allocate( fftpack_t%smp(np_smp) )
!
      iflag_fft_comp = fftpack_t%Mmax_smp*Nfft
      do ip = 1, np_smp
        allocate( fftpack_t%smp(ip)%X(iflag_fft_comp) )
        allocate( fftpack_t%smp(ip)%WK(iflag_fft_comp) )
        fftpack_t%smp(ip)%WK = 0.0d0
      end do
!
      end subroutine alloc_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_FFTPACK(nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: itmp
!
      fftpack_t%iflag_fft_len = nfft
      itmp = int(log(real(Nfft)) / log(two),KIND(itmp))
      fftpack_t%NSV = nfft + itmp + ifour
      allocate(fftpack_t%WSV(fftpack_t%NSV) )
      fftpack_t%WSV = 0.0d0
!
      end subroutine alloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_FFTPACK(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(fftpack_t%smp(ip)%X, fftpack_t%smp(ip)%WK)
      end do
!
      deallocate(fftpack_t%smp)
!
      end subroutine dealloc_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_FFTPACK(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      deallocate( fftpack_t%WSV )
      fftpack_t%iflag_fft_len = 0
!
      end subroutine dealloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine set_comm_item_rtp_4_FFTPACK                            &
     &         (nidx_rtp, istep_rtp, ntot_sr_rtp, item_sr_rtp,          &
     &          kl_fftw, m_fftw, rnorm_sr_rtp)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: item_sr_rtp(ntot_sr_rtp)
!
      integer(kind = kint), intent(inout) :: kl_fftw(ntot_sr_rtp)
      integer(kind = kint), intent(inout) :: m_fftw(ntot_sr_rtp)
      real(kind = kreal), intent(inout) :: rnorm_sr_rtp(ntot_sr_rtp)
!
      integer(kind = kint) :: inum, inod
      integer(kind = kint) :: k_rtp, l_rtp, m_rtp
!
!
!$omp parallel do private(inum,inod,k_rtp,l_rtp,m_rtp)
      do inum = 1, ntot_sr_rtp
        inod = item_sr_rtp(inum)
        k_rtp = 1 + mod(((inod-1)/istep_rtp(1)), nidx_rtp(1))
        l_rtp = 1 + mod(((inod-1)/istep_rtp(2)), nidx_rtp(2))
        m_rtp = 1 + mod(((inod-1)/istep_rtp(3)), nidx_rtp(3))
        kl_fftw(inum) = 1 + (k_rtp-1)*istep_rtp(1)                      &
     &                    + (l_rtp-1)*istep_rtp(2)
        rnorm_sr_rtp(inum) = one
        if(m_rtp .eq. 1) then
          m_fftw(inum) = 1
        else if(m_rtp .eq. 2) then
          m_fftw(inum) = nidx_rtp(3)
        else
          m_fftw(inum) = m_rtp-1
        end if
      end do
!$omp end parallel do
!
      end subroutine set_comm_item_rtp_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_to_send_smp_1(nnod_rtp, nidx_rtp,         &
     &          ist, num, ncomp_fwd, irev_sr_rtp, WK_smp, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: ist, num
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      type(work_each_fftpack), intent(in) :: WK_smp
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, j, inum, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c
!
!
        do j = 1, num
          do nd = 1, ncomp_fwd
            inum = nd + (j-1) * ncomp_fwd
            inod_s = inum + (nidx_rtp(3)-1) * ncomp_fwd*num
            ic_rtp = j+ist
            is_rtp = j+ist + nidx_rtp(1)*nidx_rtp(2)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            WS(ic_send) = WK_smp%X(inum)
            WS(is_send) = WK_smp%X(inod_s)
          end do
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
            do nd = 1, ncomp_fwd
              inum = nd + (j-1) * ncomp_fwd
              inod_c = inum + (2*m-1) * ncomp_fwd*num
              inod_s = inum + (2*m  ) * ncomp_fwd*num
              ic_rtp = j+ist + (2*m  ) * nidx_rtp(1)*nidx_rtp(2)
              is_rtp = j+ist + (2*m+1) * nidx_rtp(1)*nidx_rtp(2)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
              WS(ic_send) = WK_smp%X(inod_c)
              WS(is_send) = WK_smp%X(inod_s)
            end do
          end do
        end do
!
      end subroutine copy_FFTPACK_to_send_smp_1
!
! ------------------------------------------------------------------
!
      subroutine alloc_comm_table_sph_FFTPACK(ntot_sr_rtp, fftpack_t)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      allocate(fftpack_t%kl_fftw(ntot_sr_rtp))
      allocate(fftpack_t%m_fftw(ntot_sr_rtp))
      allocate(fftpack_t%rnorm_sr_rtp(ntot_sr_rtp))
!
      end subroutine alloc_comm_table_sph_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_comm_table_sph_FFTPACK(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      deallocate(fftpack_t%kl_fftw, fftpack_t%m_fftw)
      deallocate(fftpack_t%rnorm_sr_rtp)
!
      end subroutine dealloc_comm_table_sph_FFTPACK
!
! ------------------------------------------------------------------
!
      end module t_sph_FFTPACK5
