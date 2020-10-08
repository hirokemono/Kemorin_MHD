!>@file   t_sph_comm_table_from_FFTW.f90
!!@brief  module t_sph_comm_table_from_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!      subroutine alloc_comm_table_sph_FFTW(ntot_sr_rtp, comm_sph_FFTW)
!!      subroutine dealloc_comm_table_sph_FFTW(comm_sph_FFTW)
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!!
!!      subroutine copy_all_rtp_FFTW_to_send_smp                        &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,     &
!!     &          X_FFT, comm_sph_FFTW, n_WS, WS)
!!      subroutine copy_1comp_rtp_FFTW_to_send_smp                      &
!!     &         (nd, nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd, &
!!     &          X_FFT, comm_sph_FFTW, n_WS, WS)
!!      subroutine copy_all_rtp_FFTW_to_send                            &
!!     &         (nnod_rtp, irt_rtp_smp_stack, ncomp_fwd,               &
!!     &          X_FFT, comm_sph_FFTW, n_WS, WS)
!!      subroutine copy_1comp_rtp_FFTW_to_send(nd, nnod_rt, Nfft_c,     &
!!     &          ncomp_fwd, C_fft, comm_sph_FFTW, n_WS, WS)
!!        type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!!
!!      subroutine copy_all_prt_FFTW_to_send(nnod_rtp, nidx_rtp,        &
!!     &          ncomp_fwd, X_FFT, comm_sph_FFTW, n_WS, WS)
!!      subroutine copy_1comp_prt_FFTW_to_send(nd, nnod_rtp, nidx_rtp,  &
!!     &          ncomp_fwd, X_FFT, comm_sph_FFTW, n_WS, WS)
!!@endverbatim
!!
      module t_sph_comm_table_from_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use calypso_mpi
!
      implicit none
!
!>      Structure of communication table from FFT to send buffer
      type comm_tbl_from_FFTW
!         Number of table
        integer(kind = kint) :: ntot_item
!         radial-latitude address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: ip_smp_fftw(:)
!         radial-latitude address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: kl_fftw(:)
!         longitudinal address on FFTPACK data from SEND buffer 
        integer(kind = kint), allocatable :: m_fftw(:)
!         Normalization on FFTPACK data from SEND buffer 
        complex(kind = fftw_complex), allocatable :: cnrm_sr_rtp(:)
      end type comm_tbl_from_FFTW
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine alloc_comm_table_sph_FFTW(ntot_sr_rtp, comm_sph_FFTW)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!
      comm_sph_FFTW%ntot_item = ntot_sr_rtp
      allocate(comm_sph_FFTW%ip_smp_fftw(ntot_sr_rtp))
      allocate(comm_sph_FFTW%kl_fftw(ntot_sr_rtp))
      allocate(comm_sph_FFTW%m_fftw(ntot_sr_rtp))
      allocate(comm_sph_FFTW%cnrm_sr_rtp(ntot_sr_rtp))
!
      end subroutine alloc_comm_table_sph_FFTW
!
! ------------------------------------------------------------------
!
      subroutine dealloc_comm_table_sph_FFTW(comm_sph_FFTW)
!
      type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!
      deallocate(comm_sph_FFTW%kl_fftw, comm_sph_FFTW%m_fftw)
      deallocate(comm_sph_FFTW%ip_smp_fftw, comm_sph_FFTW%cnrm_sr_rtp)
!
      end subroutine dealloc_comm_table_sph_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_all_rtp_FFTW_to_send_smp                          &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,       &
     &          X_FFT, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
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
      do inum = 1, comm_sph_FFTW%ntot_item
        ip =  comm_sph_FFTW%ip_smp_fftw(inum)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
        inod_fft = comm_sph_FFTW%kl_fftw(inum)                          &
     &            + (comm_sph_FFTW%m_fftw(inum)-1)*num
!
        ic_send = (inum-1) * ncomp_fwd
        inod_c =  (inod_fft+ist_fft-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &         = comm_sph_FFTW%cnrm_sr_rtp(inum)                        &
     &            * X_FFT(inod_c+1:inod_c+ncomp_fwd)
      end do
!$end parallel do
!
      end subroutine copy_all_rtp_FFTW_to_send_smp
!
! ------------------------------------------------------------------
!
      subroutine copy_1comp_rtp_FFTW_to_send_smp                        &
     &         (nd, nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,   &
     &          X_FFT, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, num
      integer(kind = kint) :: ic_send, ist_fft, ip
!
!
!$omp parallel do private(inum,ip,num,ist_fft,inod_fft,ic_send)
      do inum = 1, comm_sph_FFTW%ntot_item
        ip =  comm_sph_FFTW%ip_smp_fftw(inum)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
        inod_fft = comm_sph_FFTW%kl_fftw(inum)                          &
     &            + (comm_sph_FFTW%m_fftw(inum)-1)*num + ist_fft
!
        ic_send = nd + (inum-1) * ncomp_fwd
        WS(ic_send) = comm_sph_FFTW%cnrm_sr_rtp(inum) * X_FFT(inod_fft)
      end do
!$end parallel do
!
      end subroutine copy_1comp_rtp_FFTW_to_send_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_all_rtp_FFTW_to_send(nnod_rt, Nfft_c,             &
     &          ncomp_fwd, C_fft, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rt, Nfft_c
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      complex(kind = fftw_complex), intent(in)                          &
     &                             :: C_fft(ncomp_fwd,nnod_rt,Nfft_c)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kl, m, inum, inod_c, ic_send
!
!
!$omp parallel do private(inum,kl,m,ic_send)
      do inum = 1, comm_sph_FFTW%ntot_item
        kl = comm_sph_FFTW%kl_fftw(inum)
        m =  comm_sph_FFTW%m_fftw(inum)
!
        ic_send = (inum-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &       = real(comm_sph_FFTW%cnrm_sr_rtp(inum)                     &
     &              * C_fft(1:ncomp_fwd,kl,m))
      end do
!$end parallel do
!
      end subroutine copy_all_rtp_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_1comp_rtp_FFTW_to_send(nd, nnod_rt, Nfft_c,       &
     &          ncomp_fwd, C_fft, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rt, Nfft_c
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      complex(kind = fftw_complex), intent(in) :: C_fft(nnod_rt,Nfft_c)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!
      integer(kind = kint), intent(in) :: n_WS
      real(kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kl, m, inum, ic_send
!
!
!$omp parallel do private(inum,kl,m,ic_send)
      do inum = 1, comm_sph_FFTW%ntot_item
        kl = comm_sph_FFTW%kl_fftw(inum)
        m =  comm_sph_FFTW%m_fftw(inum)
!
        ic_send = nd + (inum-1) * ncomp_fwd
        WS(ic_send) = real(comm_sph_FFTW%cnrm_sr_rtp(inum)              &
     &                     * C_fft(kl,m))
      end do
!$end parallel do
!
      end subroutine copy_1comp_rtp_FFTW_to_send
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_all_prt_FFTW_to_send(nnod_rtp, nidx_rtp,          &
     &          ncomp_fwd, X_FFT, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, inod_c, ic_send
!
!
!$omp parallel do private(inum,inod_fft,ic_send,inod_c)
      do inum = 1, comm_sph_FFTW%ntot_item
        inod_fft = comm_sph_FFTW%kl_fftw(inum)                          &
     &            + (comm_sph_FFTW%m_fftw(inum)-1) * nidx_rtp(3)
        ic_send = (inum-1) * ncomp_fwd
        inod_c = (inod_fft-1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &         = comm_sph_FFTW%cnrm_sr_rtp(inum)                        &
     &          * X_FFT(inod_c+1:inod_c+ncomp_fwd)
      end do
!$end parallel do
!
      end subroutine copy_all_prt_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_1comp_prt_FFTW_to_send(nd, nnod_rtp, nidx_rtp,    &
     &          ncomp_fwd, X_FFT, comm_sph_FFTW, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      real(kind = kreal), intent(in) :: X_FFT(ncomp_fwd*nnod_rtp)
      type(comm_tbl_from_FFTW), intent(in) :: comm_sph_FFTW
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: inod_fft, inum, ic_send
!
!
!$omp parallel do private(inum,inod_fft,ic_send)
      do inum = 1, comm_sph_FFTW%ntot_item
        inod_fft = comm_sph_FFTW%m_fftw(inum)                           &
     &            + (comm_sph_FFTW%kl_fftw(inum)-1) * nidx_rtp(3)
!
        ic_send = nd + (inum-1) * ncomp_fwd
        WS(ic_send) = comm_sph_FFTW%cnrm_sr_rtp(inum) * X_FFT(inod_fft)
      end do
!$end parallel do
!
      end subroutine copy_1comp_prt_FFTW_to_send
!
! ------------------------------------------------------------------
!
      end module t_sph_comm_table_from_FFTW
