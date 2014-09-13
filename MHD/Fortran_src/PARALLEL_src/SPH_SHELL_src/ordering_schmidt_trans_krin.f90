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
!!     &          sp_rlm, sp_rlm_krin)
!!      subroutine order_f_trans_fields_krin(ncomp, nvector, nscalar,   &
!!     &          vr_rtm, vr_rtm_krin)
!!
!!      subroutine back_f_trans_fields_krin(ncomp, nvector, nscalar,    &
!!     &          sp_rlm_krin, sp_rlm)
!!      subroutine back_b_trans_fields_krin(ncomp, nvector, nscalar,    &
!!     &          vr_rtm_krin, vr_rtm)
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
      integer(kind = kint) :: ip, ist, ied, inum, nd, i_rlm
      integer(kind = kint) :: j_rlm, k_rlm
!
!
!$omp  parallel do private(ip,ist,ied,inum,nd,i_rlm,k_rlm,j_rlm)
      do ip = 1, np_smp
        ist = idx_rlm_smp_stack(ip-1,2) + 1
        ied = idx_rlm_smp_stack(ip,  2)
        do j_rlm = ist, ied
          do nd = 1, nvector
            do k_rlm = 1, nidx_rlm(1)
              i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm_krin(k_rlm,nd,          j_rlm) = sp_rlm(i_rlm-2)
              sp_rlm_krin(k_rlm,nd+nvector,  j_rlm) = sp_rlm(i_rlm-1)
              sp_rlm_krin(k_rlm,nd+2*nvector,j_rlm) = sp_rlm(i_rlm  )
            end do
          end do
        end do
!
        do j_rlm = ist, ied
          do nd = 1, nscalar
            do k_rlm = 1, nidx_rlm(1)
              i_rlm = nd + 3*nvector                                    &
     &                     + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm_krin(k_rlm,nd+3*nvector,j_rlm) = sp_rlm(i_rlm)
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
      subroutine order_f_trans_fields_krin(ncomp, nvector, nscalar,     &
     &          vr_rtm, vr_rtm_krin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_krin(nidx_rtm(1),ncomp,nidx_rtm(2),nidx_rtm(3))
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm, nd
!
!
!$omp  parallel do private(ip,ist,ied,inum,i_rtm,nd,k_rtm,l_rtm,m_rtm)
      do ip = 1, np_smp
        ist = idx_rtm_smp_stack(ip-1,3) + 1
        ied = idx_rtm_smp_stack(ip,  3)
        do m_rtm = ist, ied
          do l_rtm = 1, nidx_rtm(2)
!
            do nd = 1, nvector
              do k_rtm = 1, nidx_rtm(1)
                i_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)          &
     &                              + (k_rtm-1) * istep_rtm(1)          &
     &                              + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm_krin(k_rtm,nd,          l_rtm,m_rtm)             &
     &            = vr_rtm(i_rtm-2)
                vr_rtm_krin(k_rtm,nd+nvector,  l_rtm,m_rtm)             &
     &            = vr_rtm(i_rtm-1)
                vr_rtm_krin(k_rtm,nd+2*nvector,l_rtm,m_rtm)             &
     &            = vr_rtm(i_rtm  )
              end do
            end do
          end do
        end do
!
        do m_rtm = ist, ied
          do l_rtm = 1, nidx_rtm(2)
            do nd = 1, nscalar
              do k_rtm = 1, nidx_rtm(1)
                i_rtm =  nd + 3*nvector                                 &
     &                      + ncomp*((l_rtm-1) * istep_rtm(2)           &
     &                             + (k_rtm-1) * istep_rtm(1)           &
     &                             + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm_krin(k_rtm,nd+3*nvector,l_rtm,m_rtm)             &
     &            = vr_rtm(i_rtm  )
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
      subroutine back_f_trans_fields_krin(ncomp, nvector, nscalar,      &
     &          sp_rlm_krin, sp_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_krin(nidx_rlm(1),ncomp,nidx_rlm(2))
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rlm, nd, j_rlm, k_rlm
!
!
!$omp  parallel do private(ip,ist,ied,nd,i_rlm,j_rlm,k_rlm)
      do ip = 1, np_smp
        ist = idx_rlm_smp_stack(ip-1,1) + 1
        ied = idx_rlm_smp_stack(ip,  1)
        do k_rlm = ist, ied
          do j_rlm = 1, nidx_rlm(2)
            do nd = 1, nvector
              i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm(i_rlm-2) = sp_rlm_krin(k_rlm,nd,          j_rlm)
              sp_rlm(i_rlm-1) = sp_rlm_krin(k_rlm,nd+nvector,  j_rlm)
              sp_rlm(i_rlm  ) = sp_rlm_krin(k_rlm,nd+2*nvector,j_rlm)
            end do
!
            do nd = 1 ,nscalar
              i_rlm = nd + 3*nvector                                    &
     &                     + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm(i_rlm  ) = sp_rlm_krin(k_rlm,nd+3*nvector,j_rlm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine back_f_trans_fields_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_fields_krin(ncomp, nvector, nscalar,      &
     &          vr_rtm_krin, vr_rtm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nidx_rtm(1),ncomp,nidx_rtm(3),nidx_rtm(2))
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm, nd
!
!
!$omp parallel do private(ip,ist,ied,i_rtm,k_rtm,l_rtm,nd,m_rtm,inum)
      do ip = 1, np_smp
        ist = idx_rtm_smp_stack(ip-1,3) + 1
        ied = idx_rtm_smp_stack(ip,  3)
        do m_rtm = ist, ied
          do k_rtm = 1, nidx_rtm(1)
            do l_rtm = 1, nidx_rtm(2)
              do nd = 1, nvector
                i_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)          &
     &                              + (k_rtm-1) * istep_rtm(1)          &
     &                              + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm(i_rtm-2)                                         &
     &                = vr_rtm_krin(k_rtm,nd,          m_rtm,l_rtm)
                vr_rtm(i_rtm-1)                                         &
     &                = vr_rtm_krin(k_rtm,nd+nvector,  m_rtm,l_rtm)
                vr_rtm(i_rtm  )                                         &
     &                = vr_rtm_krin(k_rtm,nd+2*nvector,m_rtm,l_rtm)
              end do
!
              do nd = 1, nscalar
                i_rtm =  nd + 3*nvector                                 &
     &                      + ncomp*((l_rtm-1) * istep_rtm(2)           &
     &                             + (k_rtm-1) * istep_rtm(1)           &
     &                             + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm(i_rtm  )                                         &
     &               = vr_rtm_krin(k_rtm,nd+3*nvector,m_rtm,l_rtm)
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_krin
