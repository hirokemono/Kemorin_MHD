!>@file   t_sph_domain_ISPACK_FFT.f90
!!@brief  module t_sph_domain_ISPACK_FFT
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
!!      subroutine init_sph_domain_ISPACK                               &
!!     &         (nidx_rtp, maxirt_rtp_smp, ispack_d)
!!      subroutine finalize_sph_domain_ISPACK(ispack_d)
!!      subroutine verify_sph_domain_ISPACK                             &
!!     &         (nidx_rtp, maxirt_rtp_smp, ispack_d)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sph_domain_FTTRUF_to_send(nnod_rtp, nidx_rtp,        &
!!     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,      &
!!     &          X_rtp, WS, ispack_d)
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
!!      subroutine sph_domain_FTTRUB_from_recv(nnod_rtp, nidx_rtp,      &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,      &
!!     &          WR, X_rtp, ispack_d)
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
      module t_sph_domain_ISPACK_FFT
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
      type work_each_domain_ispack
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for ISPACK
        real(kind = 8), allocatable :: WK(:)
      end type work_each_domain_ispack
!
!>      Structure to use ISPACK
      type work_for_domain_ispack
!>      Structure for each thread
        type(work_each_domain_ispack), pointer :: smp(:)
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = 4) :: IT(5)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_domain_ispack
!
      private :: alloc_work_domain_ispack, alloc_const_domain_ispack
      private :: dealloc_work_domain_ispack
      private :: dealloc_const_domain_ispack
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_domain_ISPACK                                 &
     &         (nidx_rtp, maxirt_rtp_smp, ispack_d)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
!
      call alloc_const_domain_ispack(nidx_rtp(3), ispack_d)
      call FTTRUI(nidx_rtp(3), ispack_d%IT, ispack_d%T)
!
      call alloc_work_domain_ispack                                     &
     &   (nidx_rtp(3), maxirt_rtp_smp, ispack_d)
!
      allocate(ispack_d%t_omp(np_smp,0:3))
      ispack_d%t_omp = 0.0d0
!
      end subroutine init_sph_domain_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_domain_ISPACK(ispack_d)
!
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
!
      call dealloc_const_domain_ispack(ispack_d)
      call dealloc_work_domain_ispack(ispack_d)
      deallocate(ispack_d%t_omp)
!
      end subroutine finalize_sph_domain_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_domain_ISPACK                               &
     &         (nidx_rtp, maxirt_rtp_smp, ispack_d)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
!
      if((2*nidx_rtp(3)) .ne. size(ispack_d%T)) then
!
        if(allocated(ispack_d%T) .eqv. .false.) then
          call alloc_const_domain_ispack(nidx_rtp(3), ispack_d)
        else if( (2*nidx_rtp(3)) .gt. size(ispack_d%T) ) then
          call dealloc_const_domain_ispack(ispack_d)
          call alloc_const_domain_ispack(nidx_rtp(3), ispack_d)
        end if
!
        call FTTRUI( nidx_rtp(3), ispack_d%IT, ispack_d%T )
      end if
!
      if(ASSOCIATED(ispack_d%smp) .eqv. .false.) then
        call alloc_work_domain_ispack                                   &
     &     (nidx_rtp(3), maxirt_rtp_smp, ispack_d)
      else if( (maxirt_rtp_smp*nidx_rtp(3))                             &
     &       .gt. size(ispack_d%smp(1)%X,1) ) then
        call dealloc_work_domain_ispack(ispack_d)
        call alloc_work_domain_ispack                                   &
     &    (nidx_rtp(3), maxirt_rtp_smp, ispack_d)
      end if
!
      end subroutine verify_sph_domain_ISPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_domain_FTTRUF_to_send(nnod_rtp, nidx_rtp,          &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, ispack_d)
!
      use ispack_0931
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
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
      integer(kind = kint) :: m, j, ip, ist, num, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inum, inod_s, inod_c
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack_d%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
      do nd = 1, ncomp_fwd
!$omp parallel do private(ip,m,j,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          if(iflag_FFT_time) ispack_d%t_omp(ip,0) = MPI_WTIME()
          do m = 1, nidx_rtp(3)/2
            inod_c = (2*m-2) * num
            inod_s = (2*m-1) * num
            ispack_d%smp(ip)%X(inod_c+1:inod_c+num)                     &
     &             = X_rtp(ist+1:ist+num,2*m-1,nd)
            ispack_d%smp(ip)%X(inod_s+1:inod_s+num)                     &
     &             = X_rtp(ist+1:ist+num,2*m,  nd)
          end do
          if(iflag_FFT_time) ispack_d%t_omp(ip,1)                       &
     &                      = ispack_d%t_omp(ip,1)                      &
     &                       + MPI_WTIME() - ispack_d%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack_d%t_omp(ip,0) = MPI_WTIME()
          call FTTRUF(num, nidx_rtp(3), ispack_d%smp(ip)%X,             &
     &      ispack_d%smp(ip)%WK, ispack_d%IT, ispack_d%T)
          if(iflag_FFT_time) ispack_d%t_omp(ip,2)                       &
     &                      = ispack_d%t_omp(ip,2)                      &
     &                       + MPI_WTIME() - ispack_d%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack_d%t_omp(ip,0) = MPI_WTIME()
          do j = 1, num
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            WS(ic_send) = ispack_d%smp(ip)%X(j)
            WS(is_send) = ispack_d%smp(ip)%X(j+num)
          end do
          do m = 2, nidx_rtp(3)/2
            do j = 1, num
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
              inod_c = j + (2*m-2) * num
              inod_s = j + (2*m-1) * num
              WS(ic_send) =   two * ispack_d%smp(ip)%X(inod_c)
              WS(is_send) = - two * ispack_d%smp(ip)%X(inod_s)
            end do
          end do
          if(iflag_FFT_time) ispack_d%t_omp(ip,3)                       &
     &                      = ispack_d%t_omp(ip,3)                      &
     &                       + MPI_WTIME() - ispack_d%t_omp(ip,0)
!
        end do
!$omp end parallel do
      end do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack_d%t_omp(1,1) = ispack_d%t_omp(1,1)                     &
     &                         + ispack_d%t_omp(ip,1)
          ispack_d%t_omp(1,2) = ispack_d%t_omp(1,2)                     &
     &                         + ispack_d%t_omp(ip,2)
          ispack_d%t_omp(1,3) = ispack_d%t_omp(1,3)                     &
     &                         + ispack_d%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + ispack_d%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + ispack_d%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + ispack_d%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_domain_FTTRUF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_domain_FTTRUB_from_recv(nnod_rtp, nidx_rtp,        &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,        &
     &          WR, X_rtp, ispack_d)
!
      use ispack_0931
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
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
      integer(kind = kint) ::  m, j, ip, ist, num, nd
      integer(kind = kint) :: inum, inod_s, inod_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack_d%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
      do nd = 1, ncomp_bwd
!$omp parallel do private(ip,m,j,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          if(iflag_FFT_time) ispack_d%t_omp(ip,0) = MPI_WTIME()
          do j = 1, num
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            ispack_d%smp(ip)%X(j) = WR(ic_recv)
            ispack_d%smp(ip)%X(j+num) = WR(is_recv)
          end do
          do m = 2, nidx_rtp(3)/2
            do j = 1, num
              inod_c = j + (2*m-2) * num
              inod_s = j + (2*m-1) * num
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              ispack_d%smp(ip)%X(inod_c) =  half * WR(ic_recv)
              ispack_d%smp(ip)%X(inod_s) = -half * WR(is_recv)
            end do
          end do
          if(iflag_FFT_time) ispack_d%t_omp(ip,1)                       &
     &                      = ispack_d%t_omp(ip,1)                      &
     &                       + MPI_WTIME() - ispack_d%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack_d%t_omp(ip,0) = MPI_WTIME()
          call FTTRUB(num, nidx_rtp(3), ispack_d%smp(ip)%X,             &
     &      ispack_d%smp(ip)%WK, ispack_d%IT, ispack_d%T)
          if(iflag_FFT_time) ispack_d%t_omp(ip,2)                       &
     &                      = ispack_d%t_omp(ip,2)                      &
     &                       + MPI_WTIME() - ispack_d%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack_d%t_omp(ip,0) = MPI_WTIME()
          do m = 1, nidx_rtp(3)/2
            inod_c = (2*m-2) * num
            inod_s = (2*m-1) * num
            X_rtp(ist+1:ist+num,2*m-1,nd)                               &
     &             = ispack_d%smp(ip)%X(inod_c+1:inod_c+num)
            X_rtp(ist+1:ist+num,2*m,  nd)                               &
     &             = ispack_d%smp(ip)%X(inod_s+1:inod_s+num)
          end do
          if(iflag_FFT_time) ispack_d%t_omp(ip,3)                       &
     &                      = ispack_d%t_omp(ip,3)                      &
     &                       + MPI_WTIME() - ispack_d%t_omp(ip,0)
        end do
!$omp end parallel do
      end do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack_d%t_omp(1,1) = ispack_d%t_omp(1,1)                     &
     &                         + ispack_d%t_omp(ip,1)
          ispack_d%t_omp(1,2) = ispack_d%t_omp(1,2)                     &
     &                         + ispack_d%t_omp(ip,2)
          ispack_d%t_omp(1,3) = ispack_d%t_omp(1,3)                     &
     &                         + ispack_d%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + ispack_d%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + ispack_d%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + ispack_d%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_domain_FTTRUB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_domain_ispack                               &
     &         (Nfft, maxirt_rtp_smp, ispack_d)
!
      integer(kind = kint), intent(in) :: Nfft, maxirt_rtp_smp
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
      integer(kind = kint) :: iflag_fft_comp, ip
!
!
      allocate( ispack_d%smp(np_smp) )
!
      iflag_fft_comp = maxirt_rtp_smp*Nfft
      do ip = 1, np_smp
        allocate( ispack_d%smp(ip)%X(iflag_fft_comp) )
        allocate( ispack_d%smp(ip)%WK(iflag_fft_comp) )
        ispack_d%smp(ip)%WK = 0.0d0
      end do
!
      end subroutine alloc_work_domain_ispack
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_domain_ispack(nfft, ispack_d)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
!
      allocate( ispack_d%T(2*nfft) )
      ispack_d%T = 0.0d0
!
      end subroutine alloc_const_domain_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_domain_ispack(ispack_d)
!
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(ispack_d%smp(ip)%X, ispack_d%smp(ip)%WK)
      end do
!
      deallocate(ispack_d%smp)
!
      end subroutine dealloc_work_domain_ispack
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_domain_ispack(ispack_d)
!
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
!
      deallocate(ispack_d%T)
!
      end subroutine dealloc_const_domain_ispack
!
! ------------------------------------------------------------------
!
      end module t_sph_domain_ISPACK_FFT
