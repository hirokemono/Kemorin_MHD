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
!!     &         (nphi_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!!      subroutine finalize_sph_comp_ISPACK3(ispack3_c)
!!      subroutine verify_sph_comp_ISPACK3                              &
!!     &         (nphi_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!!        integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!!        integer(kind = kint), intent(in) :: nphi_rtp
!!        integer(kind = kint), intent(in) :: maxirt_rtp_smp
!!        type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!        subroutine sph_comp_FXRTFA_to_send(nnod_rtp, nphi_rtp,        &
!!       &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,    &
!!       &          X_rtp, WS, ispack3_c)
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        real(kind = kreal), intent(in)                                &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!!        integer(kind = kint), intent(in) :: n_WS
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
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
!!      subroutine sph_comp_FXRTBA_from_recv(nnod_rtp, nidx_rtp,        &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,      &
!!     &          WR, X_rtp, ispack3_c)
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
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
!>      Structure for each thread
      type work_each_comp_ispack3
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
      end type work_each_comp_ispack3
!
!>      Structure to use ISPACK
      type work_for_comp_ispack3
!>      Structure for each thread
        type(work_each_comp_ispack3), pointer :: smp(:)
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
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_comp_ISPACK3                                  &
     &         (nphi_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      integer(kind = kint), intent(in) :: nphi_rtp
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      call alloc_const_comp_ispack3(nphi_rtp, ispack3_c)
      call FXRINI(cast_long(nphi_rtp), ispack3_c%IT, ispack3_c%T)
!
      call alloc_work_comp_ispack3                                      &
     &   (nphi_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
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
     &         (nphi_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      integer(kind = kint), intent(in) :: nphi_rtp
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
!
      if((2*nphi_rtp) .ne. size(ispack3_c%T)) then
!
        if(allocated(ispack3_c%T) .eqv. .false.) then
          call alloc_const_comp_ispack3(nphi_rtp, ispack3_c)
        else if( (2*nphi_rtp) .gt. size(ispack3_c%T) ) then
          call dealloc_const_comp_ispack3(ispack3_c)
          call alloc_const_comp_ispack3(nphi_rtp, ispack3_c)
        end if
!
        call FXRINI(cast_long(nphi_rtp),                                &
     &              ispack3_c%IT(1), ispack3_c%T(1))
      end if
!
      if(ASSOCIATED(ispack3_c%smp) .eqv. .false.) then
        call alloc_work_comp_ispack3                                    &
     &     (nphi_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
      else if( (nphi_rtp*max(ncomp_bwd, ncomp_fwd))                     &
     &       .gt. size(ispack3_c%smp(1)%X,1) ) then
        call dealloc_work_comp_ispack3(ispack3_c)
        call alloc_work_comp_ispack3                                    &
     &     (nphi_rtp, ncomp_bwd, ncomp_fwd, ispack3_c)
      end if
!
      end subroutine verify_sph_comp_ISPACK3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_comp_FXRTFA_to_send(nnod_rtp, nphi_rtp,            &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, ispack3_c)
!
      use transfer_to_long_integers
      use copy_single_FFT_and_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
      integer(kind = kint) :: m, j, ip, ist
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c
      integer(kind = kint) :: num8
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,m,j,ist,num8,inod_s,inod_c,                &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        do j = 1, num8
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          do m = 1, nphi_rtp/2
            inod_c = (2*m-2) * ncomp_fwd
            inod_s = (2*m-1) * ncomp_fwd
            ispack3_c%smp(ip)%X(inod_c+1:inod_c+ncomp_fwd)              &
     &             = X_rtp(j+ist,2*m-1,1:ncomp_fwd)
            ispack3_c%smp(ip)%X(inod_s+1:inod_s+ncomp_fwd)              &
     &             = X_rtp(j+ist,2*m,  1:ncomp_fwd)
          end do
          if(iflag_FFT_time) ispack3_c%t_omp(ip,1)                      &
     &                      = ispack3_c%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call FXRTFA(cast_long(ncomp_fwd), cast_long(nphi_rtp),        &
     &        ispack3_c%smp(ip)%X(1), ispack3_c%IT(1), ispack3_c%T(1))
          if(iflag_FFT_time) ispack3_c%t_omp(ip,2)                      &
     &                      = ispack3_c%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          ic_rtp = j+ist
          is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
          ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send+1:ic_send+ncomp_fwd)                               &
     &        = ispack3_c%smp(ip)%X(1:ncomp_fwd)
          WS(is_send+1:is_send+ncomp_fwd)                               &
     &        = ispack3_c%smp(ip)%X(ncomp_fwd+1:ncomp_fwd+ncomp_fwd)
          do m = 2, nphi_rtp/2
            ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            inod_c = (2*m-2) * ncomp_fwd
            inod_s = (2*m-1) * ncomp_fwd
            WS(ic_send+1:ic_send+ncomp_fwd)                             &
     &         =   two * ispack3_c%smp(ip)%X(inod_c+1:inod_c+ncomp_fwd)
            WS(is_send+1:is_send+ncomp_fwd)                             &
     &         = - two * ispack3_c%smp(ip)%X(inod_s+1:inod_s+ncomp_fwd)
          end do
          if(iflag_FFT_time) ispack3_c%t_omp(ip,3)                      &
     &                      = ispack3_c%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_c%t_omp(1,1) = ispack3_c%t_omp(1,1)                   &
     &                         + ispack3_c%t_omp(ip,1)
          ispack3_c%t_omp(1,2) = ispack3_c%t_omp(1,2)                   &
     &                         + ispack3_c%t_omp(ip,2)
          ispack3_c%t_omp(1,3) = ispack3_c%t_omp(1,3)                   &
     &                         + ispack3_c%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + ispack3_c%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + ispack3_c%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + ispack3_c%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_comp_FXRTFA_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_comp_FXRTBA_from_recv(nnod_rtp, nphi_rtp,          &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,        &
     &          WR, X_rtp, ispack3_c)
!
      use transfer_to_long_integers
      use copy_single_FFT_and_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_bwd)
!
      type(work_for_comp_ispack3), intent(inout) :: ispack3_c
!
      integer(kind = kint) ::  m, j, ip, ist
      integer(kind = kint) ::  inod_s, inod_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
      integer(kind = kint) :: num8
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,m,j,ist,num8,inod_s,inod_c,                &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        do j = 1, num8
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          ic_rtp = j+ist
          is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
          ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          ispack3_c%smp(ip)%X(1:ncomp_bwd)                              &
       &           = WR(ic_recv+1:ic_recv+ncomp_bwd)
          ispack3_c%smp(ip)%X(ncomp_bwd+1:ncomp_bwd+ncomp_bwd)          &
       &           = WR(is_recv+1:is_recv+ncomp_bwd)
          do m = 2, nphi_rtp/2
            inod_c = (2*m-2) * ncomp_bwd
            inod_s = (2*m-1) * ncomp_bwd
            ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            ispack3_c%smp(ip)%X(inod_c+1:inod_c+ncomp_bwd)              &
     &                =  half * WR(ic_recv+1:ic_recv+ncomp_bwd)
            ispack3_c%smp(ip)%X(inod_s+1:inod_s+ncomp_bwd)              &
     &                = -half * WR(is_recv+1:is_recv+ncomp_bwd)
          end do
          if(iflag_FFT_time) ispack3_c%t_omp(ip,1)                      &
     &                      = ispack3_c%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          call FXRTBA(cast_long(ncomp_bwd), cast_long(nphi_rtp),        &
     &        ispack3_c%smp(ip)%X(1), ispack3_c%IT(1), ispack3_c%T(1))
          if(iflag_FFT_time) ispack3_c%t_omp(ip,2)                      &
     &                      = ispack3_c%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
          if(iflag_FFT_time) ispack3_c%t_omp(ip,0) = MPI_WTIME()
          do m = 1, nphi_rtp/2
            inod_c = (2*m-2) * ncomp_bwd
            inod_s = (2*m-1) * ncomp_bwd
            X_rtp(j+ist,2*m-1,1:ncomp_bwd)                              &
     &             = ispack3_c%smp(ip)%X(inod_c+1:inod_c+ncomp_bwd)
            X_rtp(j+ist,2*m,  1:ncomp_bwd)                              &
     &             = ispack3_c%smp(ip)%X(inod_s+1:inod_s+ncomp_bwd)
          end do
          if(iflag_FFT_time) ispack3_c%t_omp(ip,3)                      &
     &                      = ispack3_c%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - ispack3_c%t_omp(ip,0)
!
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_c%t_omp(1,1) = ispack3_c%t_omp(1,1)                   &
     &                         + ispack3_c%t_omp(ip,1)
          ispack3_c%t_omp(1,2) = ispack3_c%t_omp(1,2)                   &
     &                         + ispack3_c%t_omp(ip,2)
          ispack3_c%t_omp(1,3) = ispack3_c%t_omp(1,3)                   &
     &                         + ispack3_c%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + ispack3_c%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + ispack3_c%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + ispack3_c%t_omp(1,3) / dble(np_smp)
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
      integer(kind = kint) :: nsize, ip
!
!
      allocate( ispack3_c%smp(np_smp) )
!
      nsize = Nfft * max(ncomp_bwd, ncomp_fwd)
      do ip = 1, np_smp
        allocate( ispack3_c%smp(ip)%X(nsize) )
      end do
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
      deallocate(ispack3_c%smp)
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
!
      end module t_sph_component_ISPACK3_FFT
