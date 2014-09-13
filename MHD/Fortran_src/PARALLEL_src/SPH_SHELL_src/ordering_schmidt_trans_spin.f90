!>@file   ordering_schmidt_trans_spin.f90
!!@brief  module ordering_schmidt_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine order_b_trans_fields_spin(ncomp, nvector, nscalar,   &
!!     &          sp_rlm, sp_rlm_spin)
!!      subroutine order_f_trans_fields_spin(ncomp, nvector, nscalar,   &
!!     &          vr_rtm, vr_rtm_spin)
!!
!!      subroutine back_f_trans_fields_spin(ncomp, nvector, nscalar,    &
!!     &          sp_rlm_spin, sp_rlm)
!!      subroutine back_b_trans_fields_spin(ncomp, nvector, nscalar,    &
!!     &          vr_rtm_spin, vr_rtm)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module ordering_schmidt_trans_spin
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
      subroutine order_b_trans_fields_spin(ncomp, nvector, nscalar,     &
     &          sp_rlm, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
!
      integer(kind = kint) :: ip, ist, ied, nd, i_rlm, j_rlm, k_rlm
!
!
!$omp parallel do private(ip,ist,ied,nd,i_rlm,j_rlm,k_rlm)
      do ip = 1, np_smp
        ist = idx_rlm_smp_stack(ip-1,1) + 1
        ied = idx_rlm_smp_stack(ip,  1)
        do nd = 1, nvector
          do k_rlm = ist, ied
            do j_rlm = 1, nidx_rlm(2)
              i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm_spin(j_rlm,k_rlm,nd          ) = sp_rlm(i_rlm-2)
              sp_rlm_spin(j_rlm,k_rlm,nd+nvector  ) = sp_rlm(i_rlm-1)
              sp_rlm_spin(j_rlm,k_rlm,nd+2*nvector) = sp_rlm(i_rlm  )
            end do
          end do
        end do
!
        do nd = 1, nscalar
          do k_rlm = ist, ied
            do j_rlm = 1, nidx_rlm(2)
              i_rlm = nd + 3*nvector                                    &
     &                     + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm_spin(j_rlm,k_rlm,nd+3*nvector) = sp_rlm(i_rlm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_fields_spin(ncomp, nvector, nscalar,     &
     &          vr_rtm, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(1),ncomp,nidx_rtm(3))
!
      integer(kind = kint) :: ip, ist, ied, nd
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm
!
!
!$omp  parallel do private(ip,ist,ied,i_rtm,nd,k_rtm,l_rtm,m_rtm)
      do ip = 1, np_smp
        ist = idx_rtm_smp_stack(ip-1,3) + 1
        ied = idx_rtm_smp_stack(ip,3)
        do m_rtm = ist, ied
          do nd = 1, nvector
            do k_rtm = 1, nidx_rtm(1)
              do l_rtm = 1, nidx_rtm(2)
                i_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)          &
     &                              + (k_rtm-1) * istep_rtm(1)          &
     &                              + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm_spin(l_rtm,k_rtm,nd,       m_rtm)                &
     &              = vr_rtm(i_rtm-2)
                vr_rtm_spin(l_rtm,k_rtm,nd+nvector,m_rtm)               &
     &              = vr_rtm(i_rtm-1)
                vr_rtm_spin(l_rtm,k_rtm,nd+2*nvector,m_rtm)             &
     &              = vr_rtm(i_rtm  )
              end do
            end do
          end do
!
          do nd = 1, nscalar
            do k_rtm = 1, nidx_rtm(1)
              do l_rtm = 1, nidx_rtm(2)
                i_rtm =  nd + 3*nvector                                 &
     &                      + ncomp*((l_rtm-1) * istep_rtm(2)           &
     &                             + (k_rtm-1) * istep_rtm(1)           &
     &                             + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm_spin(l_rtm,k_rtm,nd+3*nvector,m_rtm)             &
     &              = vr_rtm(i_rtm  )
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_fields_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_fields_spin(ncomp, nvector, nscalar,      &
     &          sp_rlm_spin, sp_rlm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1),ncomp)
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: ip, ist, ied, i_rlm, nd, j_rlm, k_rlm
!
!
!$omp  parallel do private(ip,ist,ied,nd,i_rlm,j_rlm,k_rlm)
      do ip = 1, np_smp
        ist = idx_rlm_smp_stack(ip-1,1) + 1
        ied = idx_rlm_smp_stack(ip,  1)
        do k_rlm = ist, ied
          do j_rlm = 1, nvector
            do nd = 1, nvector
              i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
!
              sp_rlm(i_rlm-2) = sp_rlm_spin(j_rlm,k_rlm,nd          )
              sp_rlm(i_rlm-1) = sp_rlm_spin(j_rlm,k_rlm,nd+nvector  )
              sp_rlm(i_rlm  ) = sp_rlm_spin(j_rlm,k_rlm,nd+2*nvector)
            end do
!
            do nd = 1, nscalar
              i_rlm = nd + 3*nvector                                    &
     &                     + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
              sp_rlm(i_rlm  ) = sp_rlm_spin(j_rlm,k_rlm,nd+3*nvector)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine back_f_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_fields_spin(ncomp, nvector, nscalar,      &
     &          vr_rtm_spin, vr_rtm)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(1),ncomp,nidx_rtm(3))
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm, m_rtm, nd
!
!
!$omp parallel do private(ip,ist,ied,i_rtm,k_rtm,l_rtm,nd,m_rtm)
      do ip = 1, np_smp
        ist = idx_rtm_smp_stack(ip-1,3) + 1
        ied = idx_rtm_smp_stack(ip,3)
        do m_rtm = ist, ied
          do k_rtm = 1, nidx_rtm(1)
            do l_rtm = 1, nidx_rtm(2)
              do nd = 1, nvector
                i_rtm = 3*nd + ncomp*((l_rtm-1) * istep_rtm(2)          &
     &                              + (k_rtm-1) * istep_rtm(1)          &
     &                              + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm(i_rtm-2)                                         &
     &              = vr_rtm_spin(l_rtm,k_rtm,nd,          m_rtm)
                vr_rtm(i_rtm-1)                                         &
     &              = vr_rtm_spin(l_rtm,k_rtm,nd+nvector,  m_rtm)
                vr_rtm(i_rtm  )                                         &
     &              = vr_rtm_spin(l_rtm,k_rtm,nd+2*nvector,m_rtm)
              end do
!
              do nd = 1, nvector
                i_rtm =  nd + 3*nvector                                 &
     &                      + ncomp*((l_rtm-1) * istep_rtm(2)           &
     &                             + (k_rtm-1) * istep_rtm(1)           &
     &                             + (m_rtm-1) * istep_rtm(3))
!
                vr_rtm(i_rtm  )                                         &
     &              = vr_rtm_spin(l_rtm,k_rtm,nd+3*nvector,m_rtm)
              end do
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_fields_spin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_spin
