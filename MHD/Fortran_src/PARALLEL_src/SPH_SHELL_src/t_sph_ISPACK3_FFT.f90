!>@file   t_sph_ISPACK3_FFT.f90
!!@brief  module t_sph_ISPACK3_FFT
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
!!      subroutine init_sph_ISPACK3                                     &
!!     &         (nphi_rtp, maxirt_rtp_smp, ncomp, ispack3_t)
!!      subroutine finalize_sph_ISPACK3(ispack3_t)
!!      subroutine verify_sph_ISPACK3                                   &
!!     &         (nphi_rtp, maxirt_rtp_smp, ncomp, ispack3_t)
!!        integer(kind = kint_gl), intent(in) :: ncomp
!!        integer(kind = kint_gl), intent(in) :: nphi_rtp
!!        integer(kind = kint), intent(in) :: maxirt_rtp_smp
!!        type(work_for_ispack3), intent(inout) :: ispack3_t
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!        subroutine sph_FXRTFA_to_send(nnod_rtp, nphi_rtp,             &
!!       &          irt_rtp_smp_stack, ncomp, n_WS, irev_sr_rtp,        &
!!       &          X_rtp, WS, ispack3_t)
!!        integer(kind = kint_gl), intent(in) :: ncomp
!!        integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        real(kind = kreal), intent(in)                                &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp)
!!        integer(kind = kint), intent(in) :: n_WS
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!        type(work_for_ispack3), intent(inout) :: ispack3_t
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
!!      subroutine sph_FXRTBA_from_recv(nnod_rtp, nidx_rtp,             &
!!     &          irt_rtp_smp_stack, ncomp, n_WR, irev_sr_rtp,          &
!!     &          WR, X_rtp, ispack3_t)
!!        integer(kind = kint_gl), intent(in) :: ncomp
!!        integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real (kind=kreal), intent(inout):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp)
!!        type(work_for_ispack3), intent(inout) :: ispack3_t
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
      module t_sph_ISPACK3_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      implicit none
!
!
!>      Structure for each thread
      type work_each_ispack3
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
      end type work_each_ispack3
!
!>      Structure to use ISPACK
      type work_for_ispack3
!>      Structure for each thread
        type(work_each_ispack3), pointer :: smp(:)
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = kint_gl), allocatable :: IT(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_ispack3
!
      private :: alloc_work_4_ispack3, alloc_const_4_ispack3
      private :: dealloc_work_4_ispack3, dealloc_const_4_ispack3
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_ISPACK3                                       &
     &         (nphi_rtp, maxirt_rtp_smp, ncomp, ispack3_t)
!
      integer(kind = kint_gl), intent(in) :: ncomp
      integer(kind = kint_gl), intent(in) :: nphi_rtp
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      ispack3_t%Mmax_smp = ncomp*maxirt_rtp_smp
      call alloc_const_4_ispack3(nphi_rtp, ispack3_t)
      call FXRINI(nphi_rtp, ispack3_t%IT, ispack3_t%T)
!
      call alloc_work_4_ispack3(nphi_rtp, ispack3_t)
!
      allocate(ispack3_t%t_omp(np_smp,0:3))
      ispack3_t%t_omp = 0.0d0
!
      end subroutine init_sph_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_ISPACK3(ispack3_t)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      call dealloc_const_4_ispack3(ispack3_t)
      call dealloc_work_4_ispack3(ispack3_t)
      deallocate(ispack3_t%t_omp)
!
      end subroutine finalize_sph_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_ISPACK3                                     &
     &         (nphi_rtp, maxirt_rtp_smp, ncomp, ispack3_t)
!
      integer(kind = kint_gl), intent(in) :: ncomp
      integer(kind = kint_gl), intent(in) :: nphi_rtp
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      ispack3_t%Mmax_smp = ncomp*maxirt_rtp_smp
!
      if((2*nphi_rtp) .ne. size(ispack3_t%T)) then
!
        if(allocated(ispack3_t%T) .eqv. .false.) then
          call alloc_const_4_ispack3(nphi_rtp, ispack3_t)
        else if( (2*nphi_rtp) .gt. size(ispack3_t%T) ) then
          call dealloc_const_4_ispack3(ispack3_t)
          call alloc_const_4_ispack3(nphi_rtp, ispack3_t)
        end if
!
        call FXRINI(nphi_rtp, ispack3_t%IT(1), ispack3_t%T(1))
      end if
!
      if(ASSOCIATED(ispack3_t%smp) .eqv. .false.) then
        call alloc_work_4_ispack3(nphi_rtp, ispack3_t)
      else if( (ispack3_t%Mmax_smp*nphi_rtp)                            &
     &       .gt. size(ispack3_t%smp(1)%X,1) ) then
        call dealloc_work_4_ispack3(ispack3_t)
        call alloc_work_4_ispack3(nphi_rtp, ispack3_t)
      end if
!
      end subroutine verify_sph_ISPACK3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_FXRTFA_to_send(nnod_rtp, nphi_rtp,                 &
     &          irt_rtp_smp_stack, ncomp, n_WS, irev_sr_rtp,            &
     &          X_rtp, WS, ispack3_t)
!
      integer(kind = kint_gl), intent(in) :: ncomp
      integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
      integer(kind = kint_gl) :: m, j, ip, ist, nd
      integer(kind = kint_gl) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint_gl) :: inod_s, inod_c
      integer(kind = kint_gl) :: num8, inum
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,nd,ist,num8,inum,inod_s,inod_c,           &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num8 = ncomp * (irt_rtp_smp_stack(ip)                           &
     &                 - irt_rtp_smp_stack(ip-1))
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do m = 1, nphi_rtp/2
          do inum = 1, num8
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num8
            inod_s = inum + (2*m-1) * num8
            ispack3_t%smp(ip)%X(inod_c) = X_rtp(j,2*m-1,nd)
            ispack3_t%smp(ip)%X(inod_s) = X_rtp(j,2*m,  nd)
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,1)= ispack3_t%t_omp(ip,1) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        call FXRTFA(num8, nphi_rtp, ispack3_t%smp(ip)%X(1),             &
     &              ispack3_t%IT(1), ispack3_t%T(1))
        if(iflag_FFT_time) ispack3_t%t_omp(ip,2)= ispack3_t%t_omp(ip,2) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do inum = 1, num8
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          inod_c = inum
          inod_s = inum + num8
          WS(ic_send) = ispack3_t%smp(ip)%X(inod_c)
          WS(is_send) = ispack3_t%smp(ip)%X(inod_s)
        end do
        do m = 2, nphi_rtp/2
          do inum = 1, num8
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            inod_c = inum + (2*m-2) * num8
            inod_s = inum + (2*m-1) * num8
            WS(ic_send) =   two * ispack3_t%smp(ip)%X(inod_c)
            WS(is_send) = - two * ispack3_t%smp(ip)%X(inod_s)
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,2)= ispack3_t%t_omp(ip,2) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
!
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_t%t_omp(1,1) = ispack3_t%t_omp(1,1)                   &
     &                         + ispack3_t%t_omp(ip,1)
          ispack3_t%t_omp(1,2) = ispack3_t%t_omp(1,2)                   &
     &                         + ispack3_t%t_omp(ip,2)
          ispack3_t%t_omp(1,3) = ispack3_t%t_omp(1,3)                   &
     &                         + ispack3_t%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + ispack3_t%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + ispack3_t%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + ispack3_t%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_FXRTFA_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_FXRTBA_from_recv(nnod_rtp, nphi_rtp,               &
     &          irt_rtp_smp_stack, ncomp, n_WR, irev_sr_rtp,            &
     &          WR, X_rtp, ispack3_t)
!
      integer(kind = kint_gl), intent(in) :: ncomp
      integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
      integer(kind = kint_gl) ::  m, j, ip, ist, nd
      integer(kind = kint_gl) ::  inod_s, inod_c
      integer(kind = kint_gl) :: ic_rtp, is_rtp, ic_recv, is_recv
      integer(kind = kint_gl) :: num8, inum
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num8,inum,nd,inod_s,inod_c,           &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num8 = ncomp * (irt_rtp_smp_stack(ip)                           &
     &                 - irt_rtp_smp_stack(ip-1))
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do inum = 1, num8
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          inod_c = inum
          inod_s = inum + num8
          ispack3_t%smp(ip)%X(inod_c) = WR(ic_recv)
          ispack3_t%smp(ip)%X(inod_s) = WR(is_recv)
        end do
        do m = 2, nphi_rtp/2
          do inum = 1, num8
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num8
            inod_s = inum + (2*m-1) * num8
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            ispack3_t%smp(ip)%X(inod_c) =  half * WR(ic_recv)
            ispack3_t%smp(ip)%X(inod_s) = -half * WR(is_recv)
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,1)= ispack3_t%t_omp(ip,1) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        call FXRTBA(num8, nphi_rtp, ispack3_t%smp(ip)%X(1),             &
     &              ispack3_t%IT(1), ispack3_t%T(1))
        if(iflag_FFT_time) ispack3_t%t_omp(ip,2)= ispack3_t%t_omp(ip,2) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do m = 1, nphi_rtp/2
          do inum = 1, num8
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num8
            inod_s = inum + (2*m-1) * num8
            X_rtp(j,2*m-1,nd) = ispack3_t%smp(ip)%X(inod_c)
            X_rtp(j,2*m,  nd) = ispack3_t%smp(ip)%X(inod_s)
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,3)= ispack3_t%t_omp(ip,3) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_t%t_omp(1,1) = ispack3_t%t_omp(1,1)                   &
     &                         + ispack3_t%t_omp(ip,1)
          ispack3_t%t_omp(1,2) = ispack3_t%t_omp(1,2)                   &
     &                         + ispack3_t%t_omp(ip,2)
          ispack3_t%t_omp(1,3) = ispack3_t%t_omp(1,3)                   &
     &                         + ispack3_t%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + ispack3_t%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + ispack3_t%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + ispack3_t%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_FXRTBA_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_ispack3(Nfft, ispack3_t)
!
      integer(kind = kint_gl), intent(in) :: Nfft
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
      integer(kind = kint_gl) :: iflag_fft_comp, ip
!
!
      allocate( ispack3_t%smp(np_smp) )
!
      iflag_fft_comp = ispack3_t%Mmax_smp*Nfft
      do ip = 1, np_smp
        allocate( ispack3_t%smp(ip)%X(iflag_fft_comp) )
      end do
!
      end subroutine alloc_work_4_ispack3
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_ispack3(nfft, ispack3_t)
!
      integer(kind = kint_gl), intent(in) :: nfft
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      allocate( ispack3_t%T(nfft+nfft/2) )
      allocate( ispack3_t%IT(nfft/2) )
      ispack3_t%T =  0.0d0
      ispack3_t%IT = 0
!
      end subroutine alloc_const_4_ispack3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_ispack3(ispack3_t)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      deallocate(ispack3_t%smp)
!
      end subroutine dealloc_work_4_ispack3
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_ispack3(ispack3_t)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      deallocate(ispack3_t%T, ispack3_t%IT)
!
      end subroutine dealloc_const_4_ispack3
!
! ------------------------------------------------------------------
!
      end module t_sph_ISPACK3_FFT
