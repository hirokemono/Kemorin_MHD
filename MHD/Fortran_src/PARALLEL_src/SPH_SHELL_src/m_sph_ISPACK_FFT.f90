!>@file   m_sph_ISPACK_FFT.f90
!!@brief  module m_sph_ISPACK_FFT
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Fourier transform using ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_ISPACK(ncomp)
!!      subroutine finalize_sph_ISPACK
!!      subroutine verify_sph_ISPACK(ncomp)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sph_FTTRUF_to_send                                   &
!!     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
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
!!      subroutine sph_FTTRUB_from_recv                                 &
!!     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
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
      module m_sph_ISPACK_FFT
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
      real(kind = 8), allocatable :: X_ispack(:,:)
!>      Work area for ISPACK
      integer(kind = 4) :: IT_ispack(5)
!>      Work constants for ISPACK
      real(kind = 8), allocatable :: T_ispack(:)
!>      Work area for ISPACK
      real(kind = 8), allocatable :: WORK_ispack(:,:)
!>      flag for length of Fourier transform
      integer(kind = kint) :: iflag_fft_len = -1
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_comp = -1
!
      private :: Mmax_smp, X_ispack
      private :: IT_ispack, T_ispack, WORK_ispack
      private :: iflag_fft_len, iflag_fft_comp
!
      private :: allocate_work_4_ispack, allocate_const_4_ispack
      private :: deallocate_work_4_ispack, deallocate_const_4_ispack
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_ISPACK(ncomp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
!
!
      Mmax_smp = ncomp*maxirt_rtp_smp
      call allocate_const_4_ispack(nidx_rtp(3))
      call FTTRUI(nidx_rtp(3), IT_ispack, T_ispack(1))
!
      call allocate_work_4_ispack(nidx_rtp(3))
!
      end subroutine init_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_ISPACK
!
!
      call deallocate_const_4_ispack
      call deallocate_work_4_ispack
!
      end subroutine finalize_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_ISPACK(ncomp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
!
!
      Mmax_smp = ncomp*maxirt_rtp_smp
!
      if( iflag_fft_len .ne. nidx_rtp(3)) then
!
        if( iflag_fft_len .lt. 0) then
          call allocate_const_4_ispack(nidx_rtp(3))
        else if( nidx_rtp(3) .gt. iflag_fft_comp ) then
          call deallocate_const_4_ispack
          call allocate_const_4_ispack(nidx_rtp(3))
        end if
!
        call FTTRUI( nidx_rtp(3), IT_ispack, T_ispack(1) )
      end if
!
      if( iflag_fft_comp .lt. 0) then
        call allocate_work_4_ispack(nidx_rtp(3))
      else if( (Mmax_smp*nidx_rtp(3)) .gt. iflag_fft_comp ) then
        call deallocate_work_4_ispack
        call allocate_work_4_ispack(nidx_rtp(3))
      end if
!
      end subroutine verify_sph_ISPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUF_to_send                                     &
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
      integer(kind = kint) :: m, j, ip, ist, num, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inum, inod_s, inod_c
!
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,nd,ist,num,inum,inod_s,inod_c,            &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
!
        do m = 1, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            X_ispack(inod_c,ip) = X_rtp(j,2*m-1,nd)
            X_ispack(inod_s,ip) = X_rtp(j,2*m,  nd)
          end do
        end do
!
        call FTTRUF(num, nidx_rtp(3), X_ispack(1,ip),                   &
     &      WORK_ispack(1,ip), IT_ispack(1), T_ispack(1))
!
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          inod_c = inum
          inod_s = inum + num
          WS(ic_send) = X_ispack(inod_c,ip)
          WS(is_send) = X_ispack(inod_s,ip)
        end do
        do m = 2, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            WS(ic_send) =   two * X_ispack(inod_c,ip)
            WS(is_send) = - two * X_ispack(inod_s,ip)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine sph_FTTRUF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUB_from_recv                                   &
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
      integer(kind = kint) ::  m, j, ip, ist, num, nd
      integer(kind = kint) :: inum, inod_s, inod_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num,inum,nd,inod_s,inod_c,            &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
!
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          inod_c = inum
          inod_s = inum + num
          X_ispack(inod_c,ip) = WR(ic_recv)
          X_ispack(inod_s,ip) = WR(is_recv)
        end do
        do m = 2, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            X_ispack(inod_c,ip) =  half * WR(ic_recv)
            X_ispack(inod_s,ip) = -half * WR(is_recv)
          end do
        end do
!
        call FTTRUB(num, nidx_rtp(3), X_ispack(1,ip),                   &
     &      WORK_ispack(1,ip), IT_ispack(1), T_ispack(1) )
!
        do m = 1, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            X_rtp(j,2*m-1,nd) = X_ispack(inod_c,ip)
            X_rtp(j,2*m,  nd) = X_ispack(inod_s,ip)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_FTTRUB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_work_4_ispack(Nfft)
!
      integer(kind = kint), intent(in) :: Nfft
!
!
      iflag_fft_comp = Mmax_smp*Nfft
      allocate( X_ispack(iflag_fft_comp,np_smp) )
      allocate( WORK_ispack(iflag_fft_comp,np_smp) )
      WORK_ispack = 0.0d0
!
      end subroutine allocate_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine allocate_const_4_ispack(nfft)
!
      integer(kind = kint), intent(in) :: nfft
!
      iflag_fft_len = nfft
      allocate( T_ispack(2*nfft) )
      T_ispack = 0.0d0
!
      end subroutine allocate_const_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine deallocate_work_4_ispack
!
      deallocate(X_ispack, WORK_ispack )
      iflag_fft_comp = 0
!
      end subroutine deallocate_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine deallocate_const_4_ispack
!
      deallocate( T_ispack )
      iflag_fft_len = 0
!
      end subroutine deallocate_const_4_ispack
!
! ------------------------------------------------------------------
!
      end module m_sph_ISPACK_FFT
