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
!!      subroutine order_b_trans_fields_krin(ncomp, sp_rlm_krin)
!!      subroutine order_f_trans_fields_krin(ncomp, vr_rtm_krin)
!!
!!      subroutine back_f_trans_fields_krin(ncomp, sp_rlm_krin)
!!      subroutine back_b_trans_fields_krin(ncomp, vr_rtm_krin)
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
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_fields_krin(ncomp, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: sp_rlm_krin(nnod_rlm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: k_rlm, j_rlm, i_rlm_0
      integer(kind = kint) :: nd, kr_nd, kr_j
!
!
!$omp  parallel do                                                      &
!$omp& private(ip,ist,ied,inum,k_rlm,j_rlm,nd,kr_nd,i_rlm_0,kr_j)
      do ip = 1, np_smp
        ist = ncomp*inod_rlm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          kr_nd = 1 + mod( (inum-1),(ncomp*nidx_rlm(1)) )
          nd =    1 + mod( (inum-1),ncomp)
          k_rlm = 1 + (kr_nd - nd) / ncomp
          j_rlm = 1 + (inum - kr_nd) / (ncomp*nidx_rlm(1))
!
          i_rlm_0 = nd + (j_rlm-1) * ncomp                              &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
          kr_j = k_rlm + (j_rlm-1)*nidx_rtm(1)
!
          sp_rlm_krin(kr_j,nd) = sp_rlm(i_rlm_0)
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_fields_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_fields_krin(ncomp, vr_rtm_krin)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout)                                 &
     &           :: vr_rtm_krin(nnod_rtm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1
      integer(kind = kint) :: nd, kr_nd, ip_rtm
!
!
!$omp  parallel do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,nd,kr_nd,     &
!$omp&                     i_rtm_1,m_rtm,inum,ip_rtm)
      do ip = 1, np_smp
        ist = ncomp*inod_rtm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd =    1 + mod( (inum-1),ncomp)
          kr_nd = 1 + mod( (inum-1),(ncomp*nidx_rtm(1)) )
          i_rtm_1 = 1 + (inum - kr_nd) / (ncomp*nidx_rtm(1))
!
          k_rtm = 1 + (kr_nd - nd) / ncomp
          l_rtm = 1 + mod((i_rtm_1-1),nidx_rtm(2))
          m_rtm = 1 + (i_rtm_1 - l_rtm) / nidx_rtm(2)
!
          i_rtm_0 =   nd + (l_rtm-1) * ncomp                            &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (i_rtm_1 - l_rtm) * ncomp*nidx_rtm(1)
          ip_rtm =  k_rtm + (l_rtm-1)*nidx_rtm(1)                       &
     &                    + (m_rtm-1)*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm_krin(ip_rtm,nd) = vr_rtm(i_rtm_0  )
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
      subroutine back_f_trans_fields_krin(ncomp, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: sp_rlm_krin(nnod_rlm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, jnod
      integer(kind = kint) :: i_rlm_0, j_rlm, k_rlm, nd, kr_j
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,jnod,k_rlm,j_rlm,       &
!$omp&                     nd,i_rlm_0,kr_j)
      do ip = 1, np_smp
        ist = ncomp*inod_rlm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          nd =    1 + mod( (inum-1),ncomp)
          inod =  1 + (inum - nd) / ncomp
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          i_rlm_0 = nd + (j_rlm-1) * ncomp                              &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
          kr_j = k_rlm + (j_rlm-1)*nidx_rtm(1)
!
          sp_rlm(i_rlm_0  ) = sp_rlm_krin(kr_j,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_fields_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_fields_krin(ncomp, vr_rtm_krin)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: vr_rtm_krin(nnod_rtm,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: inod, lnod
      integer(kind = kint) :: nd, k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, ip_rtm
!
!
!$omp parallel do private(ip,ist,ied,i_rtm_0,k_rtm,l_rtm,nd,inod,lnod,  &
!$omp&                    ip_rtm,m_rtm,inum)
      do ip = 1, np_smp
        ist = ncomp*inod_rtm_smp_stack(ip-1) + 1
        ied = ncomp*inod_rtm_smp_stack(ip)
        do inum = ist, ied
          nd = 1 + mod(inum-1,ncomp)
          inod = 1 + (inum - nd) / ncomp
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          m_rtm = 1 + (lnod - k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = nd + (l_rtm-1) * ncomp                              &
     &                 + (k_rtm-1) * ncomp*nidx_rtm(2)                  &
     &                 + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
          ip_rtm =  k_rtm + (l_rtm-1)*nidx_rtm(1)                       &
     &                    + (m_rtm-1)*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm(i_rtm_0  ) = vr_rtm_krin(ip_rtm,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_krin

