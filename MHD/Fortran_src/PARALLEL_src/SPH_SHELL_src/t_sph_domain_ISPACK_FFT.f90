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
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_comm_table_from_FFT
!
      implicit none
!
!
!>      Structure to use ISPACK
      type work_for_domain_ispack
!>        Total size of X for each domain
        integer(kind = kint) :: ntot_X
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:,:)
!>        Work area for ISPACK
        real(kind = 8), allocatable :: WK(:,:)
!
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = 4) :: IT(5)
!
!>      Structure of communication table from FFT to send buffer
        type(comm_tbl_from_FFT) :: comm_sph_ISPACK
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
      if(ALLOCATED(ispack_d%X) .eqv. .false.) then
        call alloc_work_domain_ispack                                   &
     &     (nidx_rtp(3), maxirt_rtp_smp, ispack_d)
      else if( (maxirt_rtp_smp*nidx_rtp(3))                             &
     &       .gt. size(ispack_d%X,1) ) then
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
      use set_comm_table_rtp_ISPACK
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
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
!$omp parallel do private(ip,m,j,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          do m = 1, nidx_rtp(3)
            inod_c = (m-1) * num
            ispack_d%X(inod_c+1:inod_c+num,ip)                         &
     &             = X_rtp(ist+1:ist+num,m,nd)
          end do
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
!$omp parallel do private(ip,m,j,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
          call FTTRUF(num, nidx_rtp(3), ispack_d%X(1,ip),             &
     &        ispack_d%WK(1,ip), ispack_d%IT, ispack_d%T)
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
        call copy_rtp_comp_ISPACK_to_send(nd, nnod_rtp, nidx_rtp(3),    &
     &      irt_rtp_smp_stack, irev_sr_rtp, ncomp_fwd,                  &
     &      ispack_d%ntot_X, ispack_d%X, n_WS, WS)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
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
      use copy_single_FFT_and_rtp
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
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
!$omp parallel do private(ip,m,j,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
          do j = 1, num
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            ispack_d%X(j,ip) = WR(ic_recv)
            ispack_d%X(j+num,ip) = WR(is_recv)
          end do
          do m = 2, nidx_rtp(3)/2
            do j = 1, num
              inod_c = j + (2*m-2) * num
              inod_s = j + (2*m-1) * num
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              ispack_d%X(inod_c,ip) =  half * WR(ic_recv)
              ispack_d%X(inod_s,ip) = -half * WR(is_recv)
            end do
          end do
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do private(ip,m,j,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
          call FTTRUB(num, nidx_rtp(3), ispack_d%X(1,ip),             &
     &      ispack_d%WK(1,ip), ispack_d%IT, ispack_d%T)
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
!$omp parallel do private(ip,m,j,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1)
          num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
          do m = 1, nidx_rtp(3)
            inod_c = (m-1) * num
            X_rtp(ist+1:ist+num,m,nd)                                   &
     &             = ispack_d%X(inod_c+1:inod_c+num,ip)
          end do
        end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
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
!
      ispack_d%ntot_X = maxirt_rtp_smp*Nfft
      allocate( ispack_d%X(ispack_d%ntot_X,np_smp) )
      allocate( ispack_d%WK(ispack_d%ntot_X,np_smp) )
      ispack_d%WK = 0.0d0
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
      deallocate(ispack_d%X, ispack_d%WK)
!
      end subroutine dealloc_work_domain_ispack
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_domain_ispack(ispack_d)
!
      type(work_for_domain_ispack), intent(inout) :: ispack_d
!
      deallocate(ispack_d%T)
!
      end subroutine dealloc_const_domain_ispack
!
! ------------------------------------------------------------------
!
      end module t_sph_domain_ISPACK_FFT
