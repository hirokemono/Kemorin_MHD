!>@file   set_comm_table_rtp_FFTW.f90
!!@brief  module set_comm_table_rtp_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTW
!!
!!@verbatim
!!      subroutine set_comm_item_rtp_4_FFTW                             &
!!     &         (nnod_rtp, ntot_sr_rtp, irev_sr_rtp,                   &
!!     &          irt_rtp_smp_stack, Nfft_c, aNfft, comm_sph_FFTW)
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!!
!!      subroutine copy_rtp_comp_FFTW_to_send                           &
!!     &         (nd, nnod_rt, irt_rtp_smp_stack, nnod_rtp, ncomp_fwd,  &
!!     &          n_WS, irev_sr_rtp, WS, Nfft_c, aNfft, C_fft)
!!      subroutine copy_FFTW_comp_from_recv                             &
!!     &         (nd, nnod_rt, irt_rtp_smp_stack, nnod_rtp, ncomp_bwd,  &
!!     &          n_WR, irev_sr_rtp, WR, Nfft_c, C_fft)
!!@endverbatim
!!
      module set_comm_table_rtp_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine set_comm_item_rtp_4_FFTW                               &
     &         (nnod_rtp, ntot_sr_rtp, irev_sr_rtp,                     &
     &          irt_rtp_smp_stack, Nfft_c, aNfft, comm_sph_FFTW)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
!
      type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!
      integer(kind = kint) ::  ip, ist, num
      integer(kind = kint) ::  m, j, ic_rtp, is_rtp, ic_send, is_send
!      integer(kind = kint) ::  i
!
!
!$omp parallel do private(ip,ist,num,m,j,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
!          i = j + (1-1)*num + Nfft_c*ist
          ic_rtp = j+ist
          ic_send = irev_sr_rtp(j+ist)
          if(ic_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%ip_smp_fft(ic_send) = ip
            comm_sph_FFTW%kl_fft(ic_send) = j
            comm_sph_FFTW%m_fft(ic_send) =  1
            comm_sph_FFTW%cnorm_sr_rtp(ic_send) = aNfft * ru
          end if
!          WS(ic_send) = aNfft * real(C_fft(i))
        end do
!
        do m = 2, Nfft_c-1
          do j = 1, num
!            i = j + (m-1)*num + Nfft_c*ist
            ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
            ic_send = irev_sr_rtp(ic_rtp)
            if(ic_send .le. ntot_sr_rtp) then
              comm_sph_FFTW%ip_smp_fft(ic_send) = ip
              comm_sph_FFTW%kl_fft(ic_send) = j
              comm_sph_FFTW%m_fft(ic_send) =  m
              comm_sph_FFTW%cnorm_sr_rtp(ic_send) = two * aNfft * ru
            end if
!            WS(ic_send) = two * aNfft * real(C_fft(i)*ru)
!
            is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
            is_send = irev_sr_rtp(is_rtp)
            if(is_send .le. ntot_sr_rtp) then
              comm_sph_FFTW%ip_smp_fft(is_send) = ip
              comm_sph_FFTW%kl_fft(is_send) = j
              comm_sph_FFTW%m_fft(is_send) =  m
              comm_sph_FFTW%cnorm_sr_rtp(is_send) = two * aNfft *iu
            end if
!            WS(is_send) = two * aNfft * real(C_fft(i)*iu)
          end do 
        end do
!
        do j = 1, num
!          i = j + (Nfft_c-1)*num + Nfft_c*ist
          ic_rtp = j+ist + irt_rtp_smp_stack(np_smp)
          ic_send = irev_sr_rtp(ic_rtp)
          if(ic_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%ip_smp_fft(ic_send) = ip
            comm_sph_FFTW%kl_fft(ic_send) = j
            comm_sph_FFTW%m_fft(ic_send) =  Nfft_c
            comm_sph_FFTW%cnorm_sr_rtp(ic_send) = two * aNfft * ru
          end if
!          WS(ic_send) = two * aNfft * real(C_fft(i)*ru)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_comm_item_rtp_4_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_comp_FFTW_to_send                             &
     &         (nd, nnod_rt, irt_rtp_smp_stack, nnod_rtp, ncomp_fwd,    &
     &          n_WS, irev_sr_rtp, WS, Nfft_c, aNfft, C_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c, nnod_rt
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in) :: C_fft(nnod_rt*Nfft_c)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  ip, ist, num, i
      integer(kind = kint) ::  m, j, ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do private(ip,ist,num,i,m,j,ic_rtp,is_rtp,               &
!$omp&                    ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
          ic_send = nd + (irev_sr_rtp(j+ist) - 1) * ncomp_fwd
          i = j + (1-1)*num + Nfft_c*ist
          WS(ic_send) = aNfft * real(C_fft(i))
        end do
!
        do m = 2, Nfft_c-1
          do j = 1, num
            ic_rtp = j+ist + (2*m-2) * nnod_rt
            is_rtp = j+ist + (2*m-1) * nnod_rt
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            i = j + (m-1)*num + Nfft_c*ist
            WS(ic_send) = two * aNfft * real(C_fft(i))
            WS(is_send) = two * aNfft * real(C_fft(i)*iu)
          end do 
        end do
!
        do j = 1, num
          ic_rtp = j+ist + nnod_rt
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          i = j + (Nfft_c-1)*num + Nfft_c*ist
          WS(ic_send) = two * aNfft * real(C_fft(i))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_comp_FFTW_to_send
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_FFTW_comp_from_recv                               &
     &         (nd, nnod_rt, irt_rtp_smp_stack, nnod_rtp, ncomp_bwd,    &
     &          n_WR, irev_sr_rtp, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: Nfft_c, nnod_rt
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &                             :: C_fft(nnod_rt*Nfft_c)
!
      integer(kind = kint) ::  ip, ist, num, i
      integer(kind = kint) :: m, j, ic_rtp, is_rtp, ic_recv, is_recv
!
!
!   normalization
!$omp parallel do private(ip,ist,num,i,m,j,ic_rtp,is_rtp,               &
!$omp&                    ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
          ic_recv = nd + (irev_sr_rtp(j+ist) - 1) * ncomp_bwd
          i = j + (1-1)*num + Nfft_c*ist
          C_fft(i) = cmplx(WR(ic_recv), zero, kind(0d0))
        end do
!
        do m = 2, Nfft_c-1
          do j = 1, num
            ic_rtp = j+ist + (2*m-2) * nnod_rt
            is_rtp = j+ist + (2*m-1) * nnod_rt
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            i = j + (m-1)*num + Nfft_c*ist
            C_fft(i)                                                   &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
          end do
        end do
!
        do j = 1, num
          ic_rtp = j+ist + nnod_rt
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          i = j + (Nfft_c-1)*num + Nfft_c*ist
          C_fft(i) = half * cmplx(WR(ic_recv), zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTW_comp_from_recv
!
! ------------------------------------------------------------------
!
      end module set_comm_table_rtp_FFTW
