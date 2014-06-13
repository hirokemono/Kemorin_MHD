!>@file   legendre_fwd_trans_fdout.f90
!!@brief  module legendre_fwd_trans_fdout
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (outmost field loop version)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_fdout(nvector,               &
!!     &          vr_rtm_fdout, sp_rlm_fdout)
!!      subroutine legendre_f_trans_scalar_fdout(nscalar,               &
!!     &          vr_rtm_fdout, sp_rlm_fdout)
!!        Input:  vr_rtm_fdout
!!        Output: sp_rlm_fdout
!!@endverbatim
!!
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_fwd_trans_fdout
!
      use m_precision
!
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
      subroutine legendre_f_trans_vector_fdout(nvector,                 &
     &          vr_rtm_fdout, sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_fdout(nnod_rtm,3*nvector)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_fdout(nnod_rlm,3*nvector)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel private(nd)
      do nd = 1, nvector
!$omp do private(l_rtm,j_rlm,k_rlm,i_rlm,ip_rtm,in_rtm)
        do j_rlm = 1, nidx_rlm(2)
          do k_rlm = 1, nidx_rlm(1)
            i_rlm = j_rlm + (k_rlm-1) * nidx_rlm(2)
!
            do l_rtm = 1, nidx_rtm(2)
              ip_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                  &
     &                       + (mdx_p_rlm_rtm(j_rlm)-1)                 &
     &                        * nidx_rtm(1) * nidx_rtm(2)
              in_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                  &
     &                       + (mdx_n_rlm_rtm(j_rlm)-1)                 &
     &                        * nidx_rtm(1) * nidx_rtm(2)
!
              sp_rlm_fdout(i_rlm,3*nd-2) = sp_rlm_fdout(i_rlm,3*nd-2)   &
     &            + vr_rtm_fdout(ip_rtm,3*nd-2) * Pvw_lj(l_rtm,j_rlm)
              sp_rlm_fdout(i_rlm,3*nd-1) = sp_rlm_fdout(i_rlm,3*nd-1)   &
     &           + (vr_rtm_fdout(ip_rtm,3*nd-1) * dPvw_lj(l_rtm,j_rlm)  &
     &            - vr_rtm_fdout(in_rtm,3*nd  ) * Pgvw_lj(l_rtm,j_rlm))
              sp_rlm_fdout(i_rlm,3*nd  ) = sp_rlm_fdout(i_rlm,3*nd  )   &
     &           - (vr_rtm_fdout(in_rtm,3*nd-1) * Pgvw_lj(l_rtm,j_rlm)  &
     &            + vr_rtm_fdout(ip_rtm,3*nd  ) * dPvw_lj(l_rtm,j_rlm))
            end do
!
            sp_rlm_fdout(i_rlm,3*nd-2) = sp_rlm_fdout(i_rlm,3*nd-2)     &
     &            * radius_1d_rlm_r(k_rlm)*radius_1d_rlm_r(k_rlm)
            sp_rlm_fdout(i_rlm,3*nd-1) = sp_rlm_fdout(i_rlm,3*nd-1)     &
     &            * radius_1d_rlm_r(k_rlm)
            sp_rlm_fdout(i_rlm,3*nd  ) = sp_rlm_fdout(i_rlm,3*nd  )     &
     &            * radius_1d_rlm_r(k_rlm)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine legendre_f_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_fdout(nscalar,                 &
     &          vr_rtm_fdout, sp_rlm_fdout)
!
      integer(kind = kint), intent(in) :: nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_fdout(nnod_rtm,nscalar)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_fdout(nnod_rlm,nscalar)
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel private(nd)
      do nd = 1, nscalar
!$omp do private(j_rlm,k_rlm,i_rlm,l_rtm,ip_rtm)
        do j_rlm = 1, nidx_rlm(2)
          do k_rlm = 1, nidx_rlm(1)
            i_rlm = j_rlm + (k_rlm-1) * nidx_rlm(2)
!
            do l_rtm = 1, nidx_rtm(2)
              ip_rtm = l_rtm + (k_rlm-1) * nidx_rtm(2)                  &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                  * nidx_rtm(1) * nidx_rtm(2)
!
              sp_rlm_fdout(i_rlm,nd) = sp_rlm_fdout(i_rlm,nd)           &
     &                 + vr_rtm_fdout(ip_rtm,nd) * Pws_lj(l_rtm,j_rlm)
            end do
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine legendre_f_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_fdout
