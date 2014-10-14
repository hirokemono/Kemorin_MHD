!>@file   m_sph_FFTPACK5.f90
!!@brief  module m_sph_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_FFTPACK5(ncomp)
!!      subroutine finalize_sph_FFTPACK5
!!      subroutine verify_sph_FFTPACK5(Nstacksmp, Nfft)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine sph_RFFTMF_to_send                                   &
!!     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
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
!!      subroutine sph_RFFTMB_from_recv                                 &
!!     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
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
      module m_sph_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!>      Maximum nuber of components for each SMP process
      integer(kind = kint) :: Mmax_smp
!>      Data for multiple Fourier transform
      real(kind = 8), allocatable :: X_FFTPACK5(:,:)
!
!>      Size of work constant for FFTPACK
      integer(kind = kint) :: lsave_FFTPACK
!>      Work constatnts for FFTPACK
      real(kind = 8), allocatable :: WSAVE_FFTPACK(:)
!>      Work area for FFTPACK
      real(kind = 8), allocatable :: WORK_FFTPACK(:,:)
!>      flag for length of Fourier transform
      integer(kind = kint) :: iflag_fft_len =  -1
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_comp = -1
!
      private :: X_FFTPACK5, Mmax_smp
      private :: lsave_FFTPACK, WSAVE_FFTPACK, WORK_FFTPACK
      private :: iflag_fft_len, iflag_fft_comp
!
      private :: allocate_work_4_FFTPACK, allocate_const_4_FFTPACK
      private :: deallocate_work_4_FFTPACK, deallocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFTPACK5(ncomp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint) :: ierr
!
      Mmax_smp = ncomp*maxirt_rtp_smp
!
      call allocate_const_4_FFTPACK(nidx_rtp(3))
      call RFFTMI(nidx_rtp(3), WSAVE_FFTPACK, lsave_FFTPACK, ierr)
!
      call allocate_work_4_FFTPACK(nidx_rtp(3))
!
      end subroutine init_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFTPACK5
!
!
      call deallocate_const_4_FFTPACK
      call deallocate_work_4_FFTPACK
!
      end subroutine finalize_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFTPACK5(ncomp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) ::  ncomp
!
      integer(kind = kint) :: ierr
!
!
      Mmax_smp = ncomp*maxirt_rtp_smp
!
      if( iflag_fft_len .ne. nidx_rtp(3)) then
!
        if( iflag_fft_len .lt. 0) then
          call allocate_const_4_FFTPACK(nidx_rtp(3))
        else if( nidx_rtp(3) .gt. iflag_fft_comp ) then
          call deallocate_const_4_FFTPACK
          call allocate_const_4_FFTPACK(nidx_rtp(3))
        end if
!
        call RFFTMI(nidx_rtp(3), WSAVE_FFTPACK, lsave_FFTPACK, ierr)
      end if
!
      if( iflag_fft_comp .lt. 0) then
        call allocate_work_4_FFTPACK(nidx_rtp(3))
      else if( (Mmax_smp*nidx_rtp(3)) .gt. iflag_fft_comp ) then
        call deallocate_work_4_FFTPACK
        call allocate_work_4_FFTPACK(nidx_rtp(3))
      end if
!
      end subroutine verify_sph_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMF_to_send                                     &
     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  m, j, ip, ist, num, inum, nsize, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c, ierr
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(m,j,nd,ist,num,inum,nsize,inod_s,inod_c,      &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
        nsize = num*nidx_rtp(3)
!
        do m = 1, nidx_rtp(3)
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (m-1) * num
            X_FFTPACK5(inod_c,ip) = X_rtp(j,m,nd)
          end do
        end do
!
        call RFFTMF(num, ione, nidx_rtp(3), num, X_FFTPACK5(1,ip),      &
     &      nsize, WSAVE_FFTPACK, lsave_FFTPACK, WORK_FFTPACK(1,ip),    &
     &      nsize, ierr)
!
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          inod_s = inum + (nidx_rtp(3)-1) * num
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          WS(ic_send) = X_FFTPACK5(inum,ip)
          WS(is_send) = X_FFTPACK5(inod_s,ip)
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-1) * num
            inod_s = inum + (2*m  ) * num
            ic_rtp = j + (2*m  ) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m+1) * irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            WS(ic_send) = X_FFTPACK5(inod_c,ip)
            WS(is_send) = X_FFTPACK5(inod_s,ip)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine sph_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMB_from_recv                                   &
     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint) ::  m, j, ip, ist, inum, num, nsize, nd
      integer(kind = kint) :: inod_s, inod_c, ierr
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(m,j,nd,ist,num,inum,nsize,inod_s,inod_c,      &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
        nsize = num*nidx_rtp(3)
!
!   normalization
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          inod_s = inum + (nidx_rtp(3)-1) * num
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          X_FFTPACK5(inum,ip) =   WR(ic_recv)
          X_FFTPACK5(inod_s,ip) = WR(is_recv)
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-1) * num
            inod_s = inum + (2*m  ) * num
            ic_rtp = j + (2*m  ) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m+1) * irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            X_FFTPACK5(inod_c,ip) = WR(ic_recv)
            X_FFTPACK5(inod_s,ip) = WR(is_recv)
          end do
        end do
!
        call RFFTMB (num, ione, nidx_rtp(3), num, X_FFTPACK5(1,ip),     &
     &      nsize, WSAVE_FFTPACK, lsave_FFTPACK, WORK_FFTPACK(1,ip),    &
     &      nsize, ierr)
!
        do m = 1, nidx_rtp(3)
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (m-1) * num
            X_rtp(j,m,nd) = X_FFTPACK5(inod_c,ip)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_RFFTMB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_work_4_FFTPACK(Nfft)
!
      integer(kind = kint), intent(in) :: Nfft
!
!
      iflag_fft_comp = Mmax_smp*Nfft
      allocate( X_FFTPACK5(iflag_fft_comp,np_smp) )
      allocate( WORK_FFTPACK(iflag_fft_comp,np_smp) )
      WORK_FFTPACK = 0.0d0
!
      end subroutine allocate_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine allocate_const_4_FFTPACK(nfft)
!
      integer(kind = kint), intent(in) :: nfft
!
      iflag_fft_len = nfft
      lsave_FFTPACK = Nfft                                              &
     &                + int ( log ( real(Nfft) ) / log(two) ) + ifour
      allocate(WSAVE_FFTPACK(lsave_FFTPACK) )
      WSAVE_FFTPACK = 0.0d0
!
      end subroutine allocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine deallocate_work_4_FFTPACK
!
      deallocate(X_FFTPACK5, WORK_FFTPACK)
      iflag_fft_comp = 0
!
      end subroutine deallocate_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine deallocate_const_4_FFTPACK
!
      deallocate( WSAVE_FFTPACK )
      iflag_fft_len = 0
!
      end subroutine deallocate_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      end module m_sph_FFTPACK5
