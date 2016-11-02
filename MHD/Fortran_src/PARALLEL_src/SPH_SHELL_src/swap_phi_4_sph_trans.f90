!>@file   swap_phi_4_sph_trans.f90
!!@brief  module swap_phi_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2014
!
!>@brief  Swap array for phi componene
!!
!!@verbatim
!!      subroutine swap_phi_from_trans(numdir, nnod_rtp, nidx_rtp, d_sph)
!!
!!      subroutine swap_phi_scalar_from_trans(nidx_rtp, m_folding,      &
!!     &          inod_rtp_smp_stack, nnod, v_prt, d_sph)
!!      subroutine swap_phi_vector_from_trans(nidx_rtp, m_folding,      &
!!     &          inod_rtp_smp_stack, nnod, v_prt, d_sph)
!!      subroutine swap_phi_tensor_from_trans(nidx_rtp, m_folding,      &
!!     &          inod_rtp_smp_stack, nnod, v_prt, d_sph)
!!        integer(kind = kint), intent(in) :: nnod
!!        real(kind = kreal), intent(in)                                &
!!     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),6)
!!        real(kind = kreal), intent(inout) :: d_sph(nnod,6)
!!
!!      subroutine swap_phi_to_trans(numdir, nnod_rtp, nidx_rtp, v_prt)
!!
!!      subroutine swap_phi_scalar_to_trans                             &
!!     &        (nidx_rtp, nnod, d_sph, v_prt)
!!      subroutine swap_phi_vector_to_trans                             &
!!     &        (nidx_rtp, nnod, d_sph, v_prt)
!!      subroutine swap_phi_tensor_to_trans                             &
!!     &        (nidx_rtp, nnod, d_sph, v_prt)
!!      integer(kind = kint), intent(in) :: nnod
!!      real(kind = kreal), intent(in) :: d_sph(nnod,6)
!!      real(kind = kreal), intent(inout)                               &
!!     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),6)
!!@endverbatim
!
      module swap_phi_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine swap_phi_from_trans(numdir, nnod_rtp, nidx_rtp, d_sph)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir, nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(inout) :: d_sph(nnod_rtp,numdir)
!
      integer(kind = kint) :: i_mkl, i_klm, kr_lt, mphi, nd
      real(kind = kreal) :: v_tmp(nnod_rtp)
!
!
      if(iflag_FFT .ne. iflag_FFTW) return
!
!$omp parallel
      do nd = 1, numdir
!$omp workshare
        v_tmp(1:nnod_rtp) = d_sph(1:nnod_rtp,nd)
!$omp end workshare
!
!$omp do private(i_mkl,i_klm,mphi,kr_lt)
        do mphi = 1, nidx_rtp(3)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            i_mkl = mphi + (kr_lt-1)*nidx_rtp(3)
            d_sph(i_klm,nd) = v_tmp(i_mkl)
          end do
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine swap_phi_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_scalar_from_trans(nidx_rtp, m_folding,        &
     &          inod_rtp_smp_stack, nnod, v_prt, d_sph)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in)                                    &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2))
      real(kind = kreal), intent(inout) :: d_sph(nnod)
!
      integer(kind = kint) :: ist, ied, ip, inod, inum, m_sym
      integer(kind = kint) :: kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp parallel do private(ist,ied,inum,inod,mphi,kr_lt,m_sym)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do m_sym = 1, m_folding
          do inum = ist, ied
            kr_lt = 1 + mod(inum-1,nrl)
            mphi =  1 + (inum - kr_lt) / nrl
            inod = inum + (m_sym-1) * inod_rtp_smp_stack(np_smp)
            d_sph(inod) = v_prt(mphi,kr_lt)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine swap_phi_scalar_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_vector_from_trans(nidx_rtp, m_folding,        &
     &          inod_rtp_smp_stack, nnod, v_prt, d_sph)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in)                                    &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),3)
      real(kind = kreal), intent(inout) :: d_sph(nnod,3)
!
      integer(kind = kint) :: ist, ied, ip, inod, inum, m_sym
      integer(kind = kint) :: kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp parallel do private(ist,ied,inum,inod,mphi,kr_lt,m_sym)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do m_sym = 1, m_folding
          do inum = ist, ied
            kr_lt = 1 + mod(inum-1,nrl)
            mphi =  1 + (inum - kr_lt) / nrl
            inod = inum + (m_sym-1) * inod_rtp_smp_stack(np_smp)
            d_sph(inod,1) = v_prt(mphi,kr_lt,1)
            d_sph(inod,2) = v_prt(mphi,kr_lt,2)
            d_sph(inod,3) = v_prt(mphi,kr_lt,3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine swap_phi_vector_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_tensor_from_trans(nidx_rtp, m_folding,        &
     &          inod_rtp_smp_stack, nnod, v_prt, d_sph)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in)                                    &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),6)
      real(kind = kreal), intent(inout) :: d_sph(nnod,6)
!
      integer(kind = kint) :: ist, ied, ip, inod, inum, m_sym
      integer(kind = kint) :: kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp parallel do private(ist,ied,inum,inod,mphi,kr_lt,m_sym)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do m_sym = 1, m_folding
          do inum = ist, ied
            kr_lt = 1 + mod(inum-1,nrl)
            mphi =  1 + (inum - kr_lt) / nrl
            inod = inum + (m_sym-1) * inod_rtp_smp_stack(np_smp)
            d_sph(inod,1) = v_prt(mphi,kr_lt,1)
            d_sph(inod,2) = v_prt(mphi,kr_lt,2)
            d_sph(inod,3) = v_prt(mphi,kr_lt,3)
            d_sph(inod,4) = v_prt(mphi,kr_lt,4)
            d_sph(inod,5) = v_prt(mphi,kr_lt,5)
            d_sph(inod,6) = v_prt(mphi,kr_lt,6)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine swap_phi_tensor_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine swap_phi_to_trans(numdir, nnod_rtp, nidx_rtp, v_prt)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir, nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(inout) :: v_prt(nnod_rtp,numdir)
!
      integer(kind = kint) :: i_mkl, i_klm, kr_lt, mphi, nd
      real(kind = kreal) :: v_tmp(nnod_rtp)
!
!
      if(iflag_FFT .ne. iflag_FFTW) return
!
!$omp parallel
      do nd = 1, numdir
!$omp workshare
        v_tmp(1:nnod_rtp) = v_prt(1:nnod_rtp,nd)
!$omp end workshare
!
!$omp do private(i_mkl,i_klm,mphi,kr_lt)
        do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
          do mphi = 1, nidx_rtp(3)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            i_mkl = mphi + (kr_lt-1)*nidx_rtp(3)
            v_prt(i_mkl,nd) = v_tmp(i_klm)
          end do
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine swap_phi_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_scalar_to_trans                               &
     &        (nidx_rtp, nnod, d_sph, v_prt)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: d_sph(nnod)
      real(kind = kreal), intent(inout)                                 &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2))
!
      integer(kind = kint) :: inod, kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp parallel do private(inod,mphi,kr_lt)
      do kr_lt = 1, nrl
        do mphi = 1, nidx_rtp(3)
          inod = kr_lt + (mphi-1)*nrl
          v_prt(mphi,kr_lt) = d_sph(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine swap_phi_scalar_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_vector_to_trans                               &
     &        (nidx_rtp, nnod, d_sph, v_prt)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: d_sph(nnod,3)
      real(kind = kreal), intent(inout)                                 &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),3)
!
      integer(kind = kint) :: inod, kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp parallel do private(inod,mphi,kr_lt)
      do kr_lt = 1, nrl
        do mphi = 1, nidx_rtp(3)
          inod = kr_lt + (mphi-1)*nrl
          v_prt(mphi,kr_lt,1) = d_sph(inod,1)
          v_prt(mphi,kr_lt,2) = d_sph(inod,2)
          v_prt(mphi,kr_lt,3) = d_sph(inod,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine swap_phi_vector_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_tensor_to_trans                               &
     &        (nidx_rtp, nnod, d_sph, v_prt)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: d_sph(nnod,6)
      real(kind = kreal), intent(inout)                                 &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),6)
!
      integer(kind = kint) :: inod, kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp parallel do private(inod,mphi,kr_lt)
      do kr_lt = 1, nrl
        do mphi = 1, nidx_rtp(3)
          inod = kr_lt + (mphi-1)*nrl
          v_prt(mphi,kr_lt,1) = d_sph(inod,1)
          v_prt(mphi,kr_lt,2) = d_sph(inod,2)
          v_prt(mphi,kr_lt,3) = d_sph(inod,3)
          v_prt(mphi,kr_lt,4) = d_sph(inod,4)
          v_prt(mphi,kr_lt,5) = d_sph(inod,5)
          v_prt(mphi,kr_lt,6) = d_sph(inod,6)
        end do
      end do
!$omp end parallel do
!
      end subroutine swap_phi_tensor_to_trans
!
!-----------------------------------------------------------------------
!
      end module swap_phi_4_sph_trans
