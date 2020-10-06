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
!!     &         (nphi_rtp, maxirt_rtp_smp, ispack3_d)
!!      subroutine finalize_sph_domain_ISPACK3(ispack3_d)
!!      subroutine verify_sph_domain_ISPACK3                            &
!!     &         (nphi_rtp, maxirt_rtp_smp, ispack3_d)
!!        integer(kind = kint_gl), intent(in) :: ncomp_bwd, ncomp_fwd
!!        integer(kind = kint_gl), intent(in) :: nphi_rtp
!!        integer(kind = kint), intent(in) :: maxirt_rtp_smp
!!        type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!        subroutine sph_domain_FXRTFA_to_send(nnod_rtp, nphi_rtp,      &
!!       &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,    &
!!       &          X_rtp, WS, ispack3_d)
!!        integer(kind = kint_gl), intent(in) :: ncomp_fwd
!!        integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        real(kind = kreal), intent(in)                                &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!!        integer(kind = kint), intent(in) :: n_WS
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
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
!!      subroutine sph_domain_FXRTBA_from_recv(nnod_rtp, nidx_rtp,      &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,      &
!!     &          WR, X_rtp, ispack3_d)
!!        integer(kind = kint_gl), intent(in) :: ncomp_bwd
!!        integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
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
      implicit none
!
!
!>      Structure for each thread
      type work_each_domain_ispack3
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
      end type work_each_domain_ispack3
!
!>      Structure to use ISPACK
      type work_for_domain_ispack3
!>      Structure for each thread
        type(work_each_domain_ispack3), pointer :: smp(:)
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = kint_gl), allocatable :: IT(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
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
      subroutine init_sph_domain_ISPACK3                                &
     &         (nphi_rtp, maxirt_rtp_smp, ispack3_d)
!
      integer(kind = kint_gl), intent(in) :: nphi_rtp
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      call alloc_const_domain_ispack3(nphi_rtp, ispack3_d)
      call FXRINI(nphi_rtp, ispack3_d%IT, ispack3_d%T)
!
      call alloc_work_domain_ispack3                                    &
     &   (nphi_rtp, maxirt_rtp_smp, ispack3_d)
!
      allocate(ispack3_d%t_omp(np_smp,0:3))
      ispack3_d%t_omp = 0.0d0
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
      call dealloc_const_domain_ispack3(ispack3_d)
      call dealloc_work_domain_ispack3(ispack3_d)
      deallocate(ispack3_d%t_omp)
!
      end subroutine finalize_sph_domain_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_domain_ISPACK3                              &
     &         (nphi_rtp, maxirt_rtp_smp, ispack3_d)
!
      integer(kind = kint_gl), intent(in) :: nphi_rtp
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
!
      if((2*nphi_rtp) .ne. size(ispack3_d%T)) then
!
        if(allocated(ispack3_d%T) .eqv. .false.) then
          call alloc_const_domain_ispack3(nphi_rtp, ispack3_d)
        else if( (2*nphi_rtp) .gt. size(ispack3_d%T) ) then
          call dealloc_const_domain_ispack3(ispack3_d)
          call alloc_const_domain_ispack3(nphi_rtp, ispack3_d)
        end if
!
        call FXRINI(nphi_rtp, ispack3_d%IT(1), ispack3_d%T(1))
      end if
!
      if(ASSOCIATED(ispack3_d%smp) .eqv. .false.) then
        call alloc_work_domain_ispack3                                  &
     &     (nphi_rtp, maxirt_rtp_smp, ispack3_d)
      else if( (maxirt_rtp_smp*nphi_rtp)                                &
     &       .gt. size(ispack3_d%smp(1)%X,1) ) then
        call dealloc_work_domain_ispack3(ispack3_d)
        call alloc_work_domain_ispack3                                  &
     &     (nphi_rtp, maxirt_rtp_smp, ispack3_d)
      end if
!
      end subroutine verify_sph_domain_ISPACK3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_domain_FXRTFA_to_send(nnod_rtp, nphi_rtp,          &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, ispack3_d)
!
      integer(kind = kint_gl), intent(in) :: ncomp_fwd
      integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
      integer(kind = kint_gl) :: m, j, ip, ist, nd
      integer(kind = kint_gl) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint_gl) :: inod_s, inod_c
      integer(kind = kint_gl) :: num8
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_d%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
      do nd = 1, ncomp_fwd
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num8,inod_s,inod_c,                   &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          if(iflag_FFT_time) ispack3_d%t_omp(ip,0) = MPI_WTIME()
          do m = 1, nphi_rtp/2
            inod_c = (2*m-2) * num8
            inod_s = (2*m-1) * num8
            ispack3_d%smp(ip)%X(inod_c+1:inod_c+num8)                   &
     &             = X_rtp(ist+1:ist+num8,2*m-1,nd)
            ispack3_d%smp(ip)%X(inod_s+1:inod_s+num8)                   &
     &             = X_rtp(ist+1:ist+num8,2*m,  nd)
          end do
          if(iflag_FFT_time) ispack3_d%t_omp(ip,1)                      &
     &                      = ispack3_d%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - ispack3_d%t_omp(ip,0)
        end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num8,inod_s,inod_c,                   &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          if(iflag_FFT_time) ispack3_d%t_omp(ip,0) = MPI_WTIME()
          call FXRTFA(num8, nphi_rtp, ispack3_d%smp(ip)%X(1),           &
     &              ispack3_d%IT(1), ispack3_d%T(1))
          if(iflag_FFT_time) ispack3_d%t_omp(ip,2)                      &
     &                      = ispack3_d%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - ispack3_d%t_omp(ip,0)
        end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num8,inod_s,inod_c,                   &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          if(iflag_FFT_time) ispack3_d%t_omp(ip,0) = MPI_WTIME()
          do j = 1, num8
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            WS(ic_send) = ispack3_d%smp(ip)%X(j)
            WS(is_send) = ispack3_d%smp(ip)%X(j+num8)
          end do
          do m = 2, nphi_rtp/2
            do j = 1, num8
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
              inod_c = j + (2*m-2) * num8
              inod_s = j + (2*m-1) * num8
              WS(ic_send) =   two * ispack3_d%smp(ip)%X(inod_c)
              WS(is_send) = - two * ispack3_d%smp(ip)%X(inod_s)
            end do
          end do
          if(iflag_FFT_time) ispack3_d%t_omp(ip,3)                      &
     &                      = ispack3_d%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - ispack3_d%t_omp(ip,0)
!
        end do
!$omp end parallel do
      end do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_d%t_omp(1,1) = ispack3_d%t_omp(1,1)                   &
     &                         + ispack3_d%t_omp(ip,1)
          ispack3_d%t_omp(1,2) = ispack3_d%t_omp(1,2)                   &
     &                         + ispack3_d%t_omp(ip,2)
          ispack3_d%t_omp(1,3) = ispack3_d%t_omp(1,3)                   &
     &                         + ispack3_d%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + ispack3_d%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + ispack3_d%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + ispack3_d%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_domain_FXRTFA_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_domain_FXRTBA_from_recv(nnod_rtp, nphi_rtp,        &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,        &
     &          WR, X_rtp, ispack3_d)
!
      integer(kind = kint_gl), intent(in) :: ncomp_bwd
      integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_bwd)
!
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
      integer(kind = kint_gl) ::  m, j, ip, ist, nd
      integer(kind = kint_gl) ::  inod_s, inod_c
      integer(kind = kint_gl) :: ic_rtp, is_rtp, ic_recv, is_recv
      integer(kind = kint_gl) :: num8, inum
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_d%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
      do nd = 1, ncomp_bwd
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num8,inum,inod_s,inod_c,              &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          if(iflag_FFT_time) ispack3_d%t_omp(ip,0) = MPI_WTIME()
          do j = 1, num8
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            ispack3_d%smp(ip)%X(j) = WR(ic_recv)
            ispack3_d%smp(ip)%X(j+num8) = WR(is_recv)
          end do
          do m = 2, nphi_rtp/2
            do j = 1, num8
              inod_c = j + (2*m-2) * num8
              inod_s = j + (2*m-1) * num8
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              ispack3_d%smp(ip)%X(inod_c) =  half * WR(ic_recv)
              ispack3_d%smp(ip)%X(inod_s) = -half * WR(is_recv)
            end do
          end do
          if(iflag_FFT_time) ispack3_d%t_omp(ip,1)                      &
     &                      = ispack3_d%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - ispack3_d%t_omp(ip,0)
        end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num8,inum,inod_s,inod_c,              &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
          if(iflag_FFT_time) ispack3_d%t_omp(ip,0) = MPI_WTIME()
          call FXRTBA(num8, nphi_rtp, ispack3_d%smp(ip)%X(1),           &
     &                ispack3_d%IT(1), ispack3_d%T(1))
          if(iflag_FFT_time) ispack3_d%t_omp(ip,2)                      &
     &                      = ispack3_d%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - ispack3_d%t_omp(ip,0)
        end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num8,inum,inod_s,inod_c,              &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
          if(iflag_FFT_time) ispack3_d%t_omp(ip,0) = MPI_WTIME()
          do m = 1, nphi_rtp/2
            inod_c = (2*m-2) * num8
            inod_s = (2*m-1) * num8
            X_rtp(ist+1:ist+num8,2*m-1,nd)                              &
     &             = ispack3_d%smp(ip)%X(inod_c+1:inod_c+num8)
            X_rtp(ist+1:ist+num8,2*m,  nd)                              &
     &             = ispack3_d%smp(ip)%X(inod_s+1:inod_s+num8)
          end do
          if(iflag_FFT_time) ispack3_d%t_omp(ip,3)                      &
     &                      = ispack3_d%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - ispack3_d%t_omp(ip,0)
        end do
!$omp end parallel do
      end do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_d%t_omp(1,1) = ispack3_d%t_omp(1,1)                   &
     &                         + ispack3_d%t_omp(ip,1)
          ispack3_d%t_omp(1,2) = ispack3_d%t_omp(1,2)                   &
     &                         + ispack3_d%t_omp(ip,2)
          ispack3_d%t_omp(1,3) = ispack3_d%t_omp(1,3)                   &
     &                         + ispack3_d%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + ispack3_d%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + ispack3_d%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + ispack3_d%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_domain_FXRTBA_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_domain_ispack3                              &
     &         (Nfft, maxirt_rtp_smp, ispack3_d)
!
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
      type(work_for_domain_ispack3), intent(inout) :: ispack3_d
!
      integer(kind = kint_gl) :: iflag_fft_comp, ip
!
!
      allocate( ispack3_d%smp(np_smp) )
!
      iflag_fft_comp = maxirt_rtp_smp*Nfft
      do ip = 1, np_smp
        allocate( ispack3_d%smp(ip)%X(iflag_fft_comp) )
      end do
!
      end subroutine alloc_work_domain_ispack3
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_domain_ispack3(nfft, ispack3_d)
!
      integer(kind = kint_gl), intent(in) :: nfft
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
      deallocate(ispack3_d%smp)
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
