!>@file   t_sph_comm_table_from_FFT.f90
!!@brief  module t_sph_comm_table_from_FFT
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
!!     &          ncomp_bwd, ncomp_fwd, comm_sph_FFT)
!!      subroutine finalize_sph_FFTPACK5(comm_sph_FFT)
!!      subroutine verify_sph_FFTPACK5(sph_rtp, comm_rtp,               &
!!     &          ncomp_bwd, ncomp_fwd, comm_sph_FFT)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine sph_RFFTMF_to_send(nnod_rtp, nidx_rtp,               &
!!     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,      &
!!     &          X_rtp, WS, comm_sph_FFT)
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
!!     &          WR, X_rtp, comm_sph_FFT)
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
      module t_sph_comm_table_from_FFT
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
!>      Structure to use ISPACK
      type comm_tbl_from_FFT
!         Number of table
        integer(kind = kint) :: ntot_item
!         radial-latitude address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: ip_smp_fft(:)
!         radial-latitude address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: kl_fft(:)
!         longitudinal address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: m_fft(:)
!         Normalization on FFTPACK data from SEND buffer 
        real(kind = kreal), allocatable :: rnorm_sr_rtp(:)
      end type comm_tbl_from_FFT
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
      subroutine alloc_comm_table_sph_FFTPACK                           &
     &         (ntot_sr_rtp, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      comm_sph_FFT%ntot_item = ntot_sr_rtp
      allocate(comm_sph_FFT%ip_smp_fft(ntot_sr_rtp))
      allocate(comm_sph_FFT%kl_fft(ntot_sr_rtp))
      allocate(comm_sph_FFT%m_fft(ntot_sr_rtp))
      allocate(comm_sph_FFT%rnorm_sr_rtp(ntot_sr_rtp))
!
      end subroutine alloc_comm_table_sph_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_comm_table_sph_FFTPACK(comm_sph_FFT)
!
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      deallocate(comm_sph_FFT%kl_fft, comm_sph_FFT%m_fft)
      deallocate(comm_sph_FFT%ip_smp_fft, comm_sph_FFT%rnorm_sr_rtp)
!
      end subroutine dealloc_comm_table_sph_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_from_rtp(nnod_rtp, nidx_rtp,              &
     &          irt_rtp_smp_stack, ncomp_bwd, X_FFT, X_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      real(kind = kreal), intent(in) :: X_FFT(ncomp_bwd*nnod_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_bwd)
!
!
      integer(kind = kint) :: m, j, ip, ist, inum, num, nd
      integer(kind = kint) :: inod_c, ist_fft
!
!
!$omp parallel do private(ip,m,j,nd,ist,num,inum,inod_c,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3) * ncomp_bwd
        do m = 1, nidx_rtp(3)
          do j = 1, num
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (m-1) * ncomp_bwd*num + ist_fft
              X_rtp(j+ist,m,nd) = X_FFT(inod_c)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTPACK_from_rtp
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_to_rtp(nnod_rtp, nidx_rtp,                &
     &          irt_rtp_smp_stack, ncomp_fwd, X_rtp, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_fwd)
!
      real(kind = kreal), intent(inout) :: X_FFT(ncomp_fwd*nnod_rtp)
!
      integer(kind = kint) :: m, j, ip, ist, inum, num, nd
      integer(kind = kint) :: inod_c, ist_fft
!
!
!$omp parallel do private(m,j,nd,ist,num,inum,inod_c,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num =  irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3) * ncomp_fwd
!
        do m = 1, nidx_rtp(3)
          do j = 1, num
            do nd = 1, ncomp_fwd
              inum = nd + (j-1) * ncomp_fwd
              inod_c = inum + (m-1) * ncomp_fwd*num + ist_fft
              X_FFT(inod_c) = X_rtp(j+ist,m,nd)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTPACK_to_rtp
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_from_recv(nnod_rtp, nidx_rtp,             &
     &          irt_rtp_smp_stack, ncomp_bwd, irev_sr_rtp,              &
     &          n_WR, WR, X_FFT)
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
      real(kind = kreal), intent(inout) :: X_FFT(ncomp_bwd*nnod_rtp)
!
      integer(kind = kint) :: m, j, ip, ist, inum, num, nd
      integer(kind = kint) :: inod_s, inod_c, ist_fft
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(m,j,nd,ist,num,inum,inod_s,inod_c,            &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3) * ncomp_bwd
!
!   normalization
        do j = 1, num
          do nd = 1, ncomp_bwd
            inum = nd + (j-1) * ncomp_bwd
            inod_c = inum + ist_fft
            inod_s = inum + (nidx_rtp(3)-1) * ncomp_bwd*num + ist_fft
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            X_FFT(inod_c) =   WR(ic_recv)
            X_FFT(inod_s) = WR(is_recv)
          end do
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (2*m-1) * ncomp_bwd*num + ist_fft
              inod_s = inum + (2*m  ) * ncomp_bwd*num + ist_fft
              ic_rtp = j+ist + (2*m  ) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m+1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              X_FFT(inod_c) = WR(ic_recv)
              X_FFT(inod_s) = WR(is_recv)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTPACK_from_recv
!
! ------------------------------------------------------------------
!
      subroutine set_comm_item_rtp_4_FFTPACK                            &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                  &
     &          ntot_sr_rtp, irev_sr_rtp, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      integer(kind = kint) :: ip, m, j, ist, num
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!!$omp parallel do private(m,j,ist,num,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        do j = 1, num
            ic_rtp = j+ist
            ic_send = irev_sr_rtp(ic_rtp)
            if(ic_send .le. ntot_sr_rtp) then
              comm_sph_FFT%ip_smp_fft(ic_send) = ip
              comm_sph_FFT%kl_fft(ic_send) = j
              comm_sph_FFT%m_fft(ic_send) =  1
              comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
            end if
!
            is_rtp = j+ist + nidx_rtp(1)*nidx_rtp(2)
            is_send = irev_sr_rtp(is_rtp)
            if(is_send .le. ntot_sr_rtp) then
              comm_sph_FFT%ip_smp_fft(is_send) = ip
              comm_sph_FFT%kl_fft(is_send) = j
              comm_sph_FFT%m_fft(is_send) =  nidx_rtp(3)
              comm_sph_FFT%rnorm_sr_rtp(is_send) = one
            end if
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
!              inod_c = j + (2*m-1)*num
              ic_rtp = j+ist + (2*m  ) * irt_rtp_smp_stack(np_smp)
              ic_send = irev_sr_rtp(ic_rtp)
              if(ic_send .le. ntot_sr_rtp) then
                comm_sph_FFT%ip_smp_fft(ic_send) = ip
                comm_sph_FFT%kl_fft(ic_send) = j
                comm_sph_FFT%m_fft(ic_send) = 2*m
                comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
              end if
!
!              inod_s = j + (2*m  )*num
              is_rtp = j+ist + (2*m+1) * irt_rtp_smp_stack(np_smp)
              is_send = irev_sr_rtp(is_rtp)
              if(is_send .le. ntot_sr_rtp) then
                comm_sph_FFT%ip_smp_fft(is_send) = ip
                comm_sph_FFT%kl_fft(is_send) = j
                comm_sph_FFT%m_fft(is_send) = 2*m+1
                comm_sph_FFT%rnorm_sr_rtp(is_send) = one
              end if
            end do
          end do
        end do
!!$omp end parallel do
!
      end subroutine set_comm_item_rtp_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_to_send_smp_1(nnod_rtp, nidx_rtp,         &
     &          irt_rtp_smp_stack, ncomp_fwd, irev_sr_rtp, X_FFT,       &
     &          n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: ip, m, j, inum, nd, ist, num
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c, ist_fft
!
!
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num =  irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3) * ncomp_fwd
        do j = 1, num
          do nd = 1, ncomp_fwd
            inum = nd + (j-1) * ncomp_fwd
            inod_c = inum + ist_fft
            inod_s = inum + (nidx_rtp(3)-1) * ncomp_fwd*num + ist_fft
            ic_rtp = j+ist
            is_rtp = j+ist + nidx_rtp(1)*nidx_rtp(2)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            WS(ic_send) = X_FFT(inum)
            WS(is_send) = X_FFT(inod_s)
          end do
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
            do nd = 1, ncomp_fwd
              inum = nd + (j-1) * ncomp_fwd
              inod_c = inum + (2*m-1) * ncomp_fwd*num + ist_fft
              inod_s = inum + (2*m  ) * ncomp_fwd*num + ist_fft
              ic_rtp = j+ist + (2*m  ) * nidx_rtp(1)*nidx_rtp(2)
              is_rtp = j+ist + (2*m+1) * nidx_rtp(1)*nidx_rtp(2)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
              WS(ic_send) = X_FFT(inod_c)
              WS(is_send) = X_FFT(inod_s)
            end do
          end do
        end do
      end do
!
      end subroutine copy_FFTPACK_to_send_smp_1
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_to_send_smp_2(nnod_rtp, nidx_rtp,         &
     &          irt_rtp_smp_stack, ncomp_fwd, X_FFT, comm_sph_FFT,      &
     &          X_FFT, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, nd, num
      integer(kind = kint) :: inod_c, ic_send, ist_fft, ip
!
!
!$omp parallel do private(inum,ip,num,ist_fft,inod_fft,                 &
!$omp&                    nd,ic_send,inod_c)
      do inum = 1, comm_sph_FFT%ntot_item
        ip =  comm_sph_FFT%ip_smp_fft(inum)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3) * ncomp_fwd
        inod_fft = comm_sph_FFT%kl_fft(inum)                            &
     &            + (comm_sph_FFT%m_fft(inum)-1)*num
        do nd = 1, ncomp_fwd
          ic_send = nd + (inum-1) * ncomp_fwd
          inod_c = nd + (inod_fft-1) * ncomp_fwd + ist_fft
          WS(ic_send) = comm_sph_FFT%rnorm_sr_rtp(inum) * X_FFT(inod_c)
        end do
      end do
!$end parallel do
!
      end subroutine copy_FFTPACK_to_send_smp_2
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_to_send_smp_3(nd, nnod_rtp, nidx_rtp,     &
     &          irt_rtp_smp_stack, ncomp_fwd, X_FFT, comm_sph_FFT,      &
     &          n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFT), intent(in) :: comm_sph_FFT
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, num
      integer(kind = kint) :: inod_c, ic_send, ist_fft, ip
!
!
!$omp parallel do private(inum,ip,num,ist_fft,inod_fft,                 &
!$omp&                    ic_send,inod_c)
      do inum = 1, comm_sph_FFT%ntot_item
        ip =  comm_sph_FFT%ip_smp_fft(inum)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3) * ncomp_fwd
        inod_fft = comm_sph_FFT%kl_fft(inum)                            &
     &            + (comm_sph_FFT%m_fft(inum)-1)*num
!
        ic_send = nd + (inum-1) * ncomp_fwd
        inod_c = nd + (inod_fft-1) * ncomp_fwd + ist_fft
        WS(ic_send) = comm_sph_FFT%rnorm_sr_rtp(inum) * X_FFT(inod_c)
      end do
!$end parallel do
!
      end subroutine copy_FFTPACK_to_send_smp_3
!
! ------------------------------------------------------------------
!
      end module t_sph_comm_table_from_FFT
