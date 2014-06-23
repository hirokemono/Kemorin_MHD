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
!!      subroutine order_b_trans_fields_krin(ncomp, nvector, nscalar,   &
!!     &          sp_rlm, sp_rlm_spin)
!!      subroutine order_f_trans_fields_krin(ncomp, nvector, nscalar,   &
!!     &          vr_rtm, vr_rtm_spin)
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
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_fields_krin(ncomp, nvector, nscalar,     &
     &          sp_rlm, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &    :: sp_rlm_krin(nidx_rlm(1),ncomp,nidx_rlm(2))
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, nd, i_rlm_0
      integer(kind = kint) :: j_rlm, k_rlm
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,nd,i_rlm_0,k_rlm,j_rlm)
      do ip = 1, np_smp
        ist = nvector*inod_rlm_smp_stack(ip-1) + 1
        ied = nvector*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          k_rlm = 1 + mod( (inod-1),nidx_rlm(1))
          j_rlm = 1 + (inod - k_rlm) / nidx_rlm(1)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = 3*nd + (j_rlm-1) * ncomp                            &
     &                   + (k_rlm-1) * ncomp * nidx_rlm(2)
!
          sp_rlm_krin(k_rlm,nd,          j_rlm) = sp_rlm(i_rlm_0-2)
          sp_rlm_krin(k_rlm,nd+nvector,  j_rlm) = sp_rlm(i_rlm_0-1)
          sp_rlm_krin(k_rlm,nd+2*nvector,j_rlm) = sp_rlm(i_rlm_0  )
        end do
!
        ist = nscalar*inod_rlm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rlm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          inod = 1 + mod( (inum-1),nnod_rlm)
          k_rlm = 1 + mod( (inod-1),nidx_rlm(1))
          j_rlm = 1 + (inod - k_rlm) / nidx_rlm(1)
          nd =   1 + (inum - inod) / nnod_rlm
!
          i_rlm_0 = nd + 3*nvector + (j_rlm-1) * ncomp                  &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
!
          sp_rlm_krin(k_rlm,nd+3*nvector,j_rlm) = sp_rlm(i_rlm_0)
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_fields_krin(ncomp, nvector, nscalar,     &
     &          vr_rtm, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(1),ncomp,nidx_rtm(2),nidx_rtm(3))
!
      integer(kind = kint) :: ip, ist, ied, inum, inod
      integer(kind = kint) :: i_rtm_0, k_rtm, l_rtm, m_rtm, nd, kr_nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,inod,i_rtm_0,nd,kr_nd,       &
!$omp&                     k_rtm,l_rtm,m_rtm)
      do ip = 1, np_smp
        ist = nvector*inod_rtm_smp_stack(ip-1) + 1
        ied = nvector*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          l_rtm = 1 + mod((inum-1),nidx_rtm(2))
          inod =  1 + (inum - l_rtm) / nidx_rtm(2)
          m_rtm = 1 + mod( (inod-1),nidx_rtm(3))
          kr_nd = 1 + (inod - m_rtm) / nidx_rtm(3)
          k_rtm = 1 + mod( (kr_nd-1),nidx_rtm(1))
          nd =    1 + (kr_nd-k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = 3*nd + (l_rtm-1) * ncomp                            &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm_spin(k_rtm,nd,          l_rtm,m_rtm)                   &
     &            = vr_rtm(i_rtm_0-2)
          vr_rtm_spin(k_rtm,nd+nvector,  l_rtm,m_rtm)                   &
     &            = vr_rtm(i_rtm_0-1)
          vr_rtm_spin(k_rtm,nd+2*nvector,l_rtm,m_rtm)                   &
     &            = vr_rtm(i_rtm_0  )
        end do
!
        ist = nscalar*inod_rtm_smp_stack(ip-1) + 1
        ied = nscalar*inod_rtm_smp_stack(ip)
!cdir nodep
        do inum = ist, ied
          l_rtm = 1 + mod((inum-1),nidx_rtm(2))
          inod =  1 + (inum - l_rtm) / nidx_rtm(2)
          m_rtm = 1 + mod( (inod-1),nidx_rtm(3))
          kr_nd = 1 + (inod - m_rtm) / nidx_rtm(3)
          k_rtm = 1 + mod( (kr_nd-1),nidx_rtm(1))
          nd =    1 + (kr_nd-k_rtm) / nidx_rtm(1)
!
          i_rtm_0 = nd + 3*nvector + (l_rtm-1) * ncomp                  &
     &                   + (k_rtm-1) * ncomp*nidx_rtm(2)                &
     &                   + (m_rtm-1) * ncomp*nidx_rtm(1)*nidx_rtm(2)
!
          vr_rtm_spin(k_rtm,nd+3*nvector,l_rtm,m_rtm)                   &
     &            = vr_rtm(i_rtm_0  )
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_krin
