!!@brief  module ordering_schmidt_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is radial ID)
!!
!!@verbatim
!!      subroutine order_b_trans_fields_krin                            &
!!     &         (nnod_rlm, nidx_rlm, istep_rlm, a_r_1d_rlm_r,          &
!!     &          istack_rlm_j_smp, ncomp, nvector, nscalar,            &
!!     &          irev_sr_rlm,  n_WR, WR, sp_rlm_krin)
!!      subroutine order_f_trans_fields_krin                            &
!!     &         (nnod_rlm, nidx_rtm, istep_rtm, istack_rtm_m_smp,      &
!!     &          ncomp, nvector, nscalar, irev_sr_rtm,                 &
!!     &          n_WR, WR, vr_rtm_krin)
!!
!!      subroutine back_f_trans_fields_krin                             &
!!     &         (nidx_rlm, ncomp, nvector, nscalar, sp_rlm_krin,       &
!!     &          nneib_domain_rlm, istack_sr_rlm, item_sr_rlm, WS)
!!      subroutine back_b_trans_fields_krin                             &
!!     &         (nidx_rtm, ncomp, nvector, nscalar, vr_rtm_krin,       &
!!     &          nneib_domain_rtm, istack_sr_rtm, item_sr_rtm, WS)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module ordering_schmidt_trans_krin
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_fields_krin                              &
     &         (nnod_rlm, nidx_rlm, istep_rlm, a_r_1d_rlm_r,            &
     &          istack_rlm_j_smp, ncomp, nvector, nscalar,              &
     &          irev_sr_rlm,  n_WR, WR, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in) :: istack_rlm_j_smp(0:np_smp)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &    :: sp_rlm_krin(nidx_rlm(1),ncomp,nidx_rlm(2))
!
      integer(kind = kint) :: ip, ist, ied, inum, nd, i_rlm
      integer(kind = kint) :: j_rlm, k_rlm, i_recv
      real(kind = kreal) :: a2r_1d_rlm_r
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,ist,ied,inum,nd,i_rlm,k_rlm,j_rlm,i_recv,  &
!$omp&                    a2r_1d_rlm_r)
      do ip = 1, np_smp
        ist = istack_rlm_j_smp(ip-1) + 1
        ied = istack_rlm_j_smp(ip  )
        do j_rlm = ist, ied
          do nd = 1, nvector
            do k_rlm = 1, nidx_rlm(1)
              a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_recv = 3*nd + (irev_sr_rlm(i_rlm)-1) * ncomp
!
              sp_rlm_krin(k_rlm,nd,          j_rlm)                     &
     &               = WR(i_recv-2) * a2r_1d_rlm_r
              sp_rlm_krin(k_rlm,nd+nvector,  j_rlm)                     &
     &               = WR(i_recv-1) * a_r_1d_rlm_r(k_rlm)
              sp_rlm_krin(k_rlm,nd+2*nvector,j_rlm)                     &
     &               = WR(i_recv  ) * a_r_1d_rlm_r(k_rlm)
            end do
          end do
        end do
!
        do j_rlm = ist, ied
          do nd = 1, nscalar
            do k_rlm = 1, nidx_rlm(1)
              i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                      &
     &                  + (k_rlm-1) * istep_rlm(1)
              i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm)-1) * ncomp
!
              sp_rlm_krin(k_rlm,nd+3*nvector,j_rlm) = WR(i_recv)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_fields_krin                              &
     &         (nnod_rlm, nidx_rtm, istep_rtm, istack_rtm_m_smp,        &
     &          ncomp, nvector, nscalar, irev_sr_rtm,                   &
     &          n_WR, WR, vr_rtm_krin)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: istack_rtm_m_smp(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_krin(nidx_rtm(1),ncomp,nidx_rtm(2),nidx_rtm(3))
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm, nd, i_recv
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,ist,ied,inum,i_rtm,nd,k_rtm,l_rtm,m_rtm,i_recv)
      do ip = 1, np_smp
        ist = istack_rtm_m_smp(ip-1) + 1
        ied = istack_rtm_m_smp(ip  )
        do m_rtm = ist, ied
          do l_rtm = 1, nidx_rtm(2)
!
            do nd = 1, nvector
              do k_rtm = 1, nidx_rtm(1)
                i_rtm = 1 + (l_rtm-1) * istep_rtm(2)                    &
     &                    + (k_rtm-1) * istep_rtm(1)                    &
     &                    + (m_rtm-1) * istep_rtm(3)
                i_recv = 3*nd + (irev_sr_rtm(i_rtm)-1) * ncomp
!
                vr_rtm_krin(k_rtm,nd,          l_rtm,m_rtm)             &
     &            = WR(i_recv-2)
                vr_rtm_krin(k_rtm,nd+nvector,  l_rtm,m_rtm)             &
     &            = WR(i_recv-1)
                vr_rtm_krin(k_rtm,nd+2*nvector,l_rtm,m_rtm)             &
     &            = WR(i_recv  )
              end do
            end do
          end do
        end do
!
        do m_rtm = ist, ied
          do l_rtm = 1, nidx_rtm(2)
            do nd = 1, nscalar
              do k_rtm = 1, nidx_rtm(1)
                i_rtm = 1 + (l_rtm-1) * istep_rtm(2)                    &
     &                    + (k_rtm-1) * istep_rtm(1)                    &
     &                    + (m_rtm-1) * istep_rtm(3)
                i_recv = nd + 3*nvector                                 &
     &                      + (irev_sr_rtm(i_rtm)-1) * ncomp
!
                vr_rtm_krin(k_rtm,nd+3*nvector,l_rtm,m_rtm)             &
     &            = WR(i_recv  )
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_fields_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_fields_krin                               &
     &         (nidx_rlm, ncomp, nvector, nscalar, sp_rlm_krin,         &
     &          nneib_domain_rlm, istack_sr_rlm, item_sr_rlm, WS)
!
      use sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: nneib_domain_rlm
      integer(kind = kint), intent(in)                                  &
     &           :: istack_sr_rlm(0:nneib_domain_rlm)
      integer(kind = kint), intent(in)                                  &
     &           :: item_sr_rlm(istack_sr_rlm(nneib_domain_rlm))
      real(kind = kreal), intent(in)                                    &
     &           :: sp_rlm_krin(nidx_rlm(1),ncomp,nidx_rlm(2))
      real (kind=kreal), intent(inout)                                  &
     &           :: WS(ncomp*istack_sr_rlm(nneib_domain_rlm))
!
      integer(kind = kint) :: ip, ist, inum, i, num, inod
      integer(kind = kint) :: i_rlm, nd, j_rlm, k_rlm
!
!
!$omp parallel private(nd)
      do nd = 1, nvector
!$omp do private(inum,i_rlm,j_rlm,k_rlm,i,inod)
        do inum = 1, istack_sr_rlm(nneib_domain_rlm)
          i = 3*nd + (inum-1) * ncomp
          inod = item_sr_rlm(inum)
          i_rlm = 3*nd + (inod-1) * ncomp
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          WS(i-2) = sp_rlm_krin(k_rlm,nd,          j_rlm)
          WS(i-1) = sp_rlm_krin(k_rlm,nd+nvector,  j_rlm)
          WS(i  ) = sp_rlm_krin(k_rlm,nd+2*nvector,j_rlm)
        end do
!$omp end do nowait
      end do
      do nd = 1, nscalar
!$omp do private(inum,i_rlm,j_rlm,k_rlm,i,inod)
        do inum = 1, istack_sr_rlm(nneib_domain_rlm)
          i = nd + 3*nvector + (inum-1) * ncomp
          inod = item_sr_rlm(inum)
          i_rlm = nd + 3*nvector + (inod-1) * ncomp
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          WS(i  ) = sp_rlm_krin(k_rlm,nd+3*nvector,j_rlm)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine back_f_trans_fields_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_fields_krin                               &
     &         (nidx_rtm, ncomp, nvector, nscalar, vr_rtm_krin,         &
     &          nneib_domain_rtm, istack_sr_rtm, item_sr_rtm, WS)
!
      use sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: nidx_rtm(3)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: nneib_domain_rtm
      integer(kind = kint), intent(in)                                  &
     &       :: istack_sr_rtm(0:nneib_domain_rtm)
      integer(kind = kint), intent(in)                                  &
     &       :: item_sr_rtm(istack_sr_rtm(nneib_domain_rtm))
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nidx_rtm(1),ncomp,nidx_rtm(3),nidx_rtm(2))
      real (kind=kreal), intent(inout)                                  &
     &       :: WS(ncomp*istack_sr_rtm(nneib_domain_rtm))
!
      integer(kind = kint) :: ip, ist, num, inum, nd, i, inod
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm, km_rtm
!
!
!$omp parallel private(nd)
      do nd = 1, nvector
!$omp do private(inum,i_rtm,l_rtm,km_rtm,k_rtm,m_rtm,i,inod)
        do inum = 1, istack_sr_rtm(nneib_domain_rtm)
          i = 3*nd + (inum-1) * ncomp
          inod = item_sr_rtm(inum)
          i_rtm = 3*nd + (inod-1) * ncomp
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          km_rtm = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((km_rtm-1),nidx_rtm(1))
          m_rtm = 1 + (km_rtm - k_rtm) / nidx_rtm(1)
!
          WS(i-2) = vr_rtm_krin(k_rtm,nd,          m_rtm,l_rtm)
          WS(i-1) = vr_rtm_krin(k_rtm,nd+nvector,  m_rtm,l_rtm)
          WS(i  ) = vr_rtm_krin(k_rtm,nd+2*nvector,m_rtm,l_rtm)
        end do
!$omp end do nowait
      end do
      do nd = 1, nscalar
!$omp do private(inum,i_rtm,l_rtm,km_rtm,k_rtm,m_rtm,i,inod)
        do inum = 1, istack_sr_rtm(nneib_domain_rtm)
          i = nd + 3*nvector + (inum-1) * ncomp
          inod = item_sr_rtm(inum)
          i_rtm = nd + 3*nvector + (inod-1) * ncomp
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          km_rtm = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((km_rtm-1),nidx_rtm(1))
          m_rtm = 1 + (km_rtm - k_rtm) / nidx_rtm(1)
!
          WS(i  ) = vr_rtm_krin(k_rtm,nd+3*nvector,m_rtm,l_rtm)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine back_b_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_krin
