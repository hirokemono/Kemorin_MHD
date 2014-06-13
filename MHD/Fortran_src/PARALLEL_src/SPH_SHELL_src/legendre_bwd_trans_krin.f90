!>@file   legendre_bwd_trans_krin.f90
!!@brief  module legendre_bwd_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_krin(nvector,                &
!!     &          sp_rlm_krin, vr_rtm_krin)
!!      subroutine legendre_b_trans_scalar_krin(nscalar,                &
!!     &          sp_rlm_krin, vr_rtm_krin)
!!        Input:  vr_rtm_krin
!!        Output: sp_rlm_krin
!!@endverbatim
!!
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of scalar to be transformed
!
      module legendre_bwd_trans_krin
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
      subroutine legendre_b_trans_vector_krin(nvector,                  &
     &          sp_rlm_krin, vr_rtm_krin)
!
      integer(kind = kint), intent(in) :: nvector
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_krin(nnod_rlm,3*nvector)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_krin(nnod_rtm,3*nvector)
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: k_rtm, nd, kr_j, kr_l, i_rtm
!
!
!$omp parallel do private(j_rlm,k_rtm,nd,kr_j)
      do j_rlm = 1, nidx_rlm(2)
        do nd = 1, nvector
          do k_rtm = 1, nidx_rtm(1)
            kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
!
            sp_rlm_krin(kr_j,3*nd-2) = sp_rlm_krin(kr_j,3*nd-2)         &
     &                       * a_r_1d_rlm_r(k_rtm)*a_r_1d_rlm_r(k_rtm)
            sp_rlm_krin(kr_j,3*nd-1) = sp_rlm_krin(kr_j,3*nd-1)         &
     &                       * a_r_1d_rlm_r(k_rtm)
            sp_rlm_krin(kr_j,3*nd  ) = sp_rlm_krin(kr_j,3*nd  )         &
     &                       * a_r_1d_rlm_r(k_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(mp_rlm,j_rlm,kr_l,l_rtm,k_rtm,nd,kr_j,i_rtm)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
!
        do nd = 1, nvector
          do kr_l = 1, nidx_rtm(1)*nidx_rtm(2)
            i_rtm = kr_l + (mp_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
            k_rtm = 1 + mod(kr_l-1,nidx_rtm(1))
            l_rtm = 1 + (kr_l - k_rtm) / nidx_rtm(1)
            kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
!
            vr_rtm_krin(i_rtm,3*nd-2) = vr_rtm_krin(i_rtm,3*nd-2)       &
     &                     + sp_rlm_krin(kr_j,3*nd-2)                   &
     &                      * Pg3_lj(l_rtm,j_rlm)
            vr_rtm_krin(i_rtm,3*nd-1) = vr_rtm_krin(i_rtm,3*nd-1)       &
     &                     + sp_rlm_krin(kr_j,3*nd-1)                   &
     &                      * dPdt_rtm(l_rtm,j_rlm)
            vr_rtm_krin(i_rtm,3*nd  ) = vr_rtm_krin(i_rtm,3*nd  )       &
     &                     - sp_rlm_krin(kr_j,3*nd  )                   &
     &                      * dPdt_rtm(l_rtm,j_rlm)
          end do
        end do
!
      end do
!$omp end parallel do
!
!$omp parallel do private(mn_rlm,j_rlm,kr_l,l_rtm,k_rtm,nd,kr_j,i_rtm)
      do j_rlm = 1, nidx_rlm(2)
        mn_rlm = mdx_n_rlm_rtm(j_rlm)
!
        do nd = 1, nvector
          do kr_l = 1, nidx_rtm(1)*nidx_rtm(2)
            i_rtm = kr_l + (mn_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
            k_rtm = 1 + mod(kr_l-1,nidx_rtm(1))
            l_rtm = 1 + (kr_l - k_rtm) / nidx_rtm(1)
            kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
!
            vr_rtm_krin(i_rtm,3*nd-1) = vr_rtm_krin(i_rtm,3*nd-1)       &
     &                + sp_rlm_krin(kr_j,3*nd  ) * Pgv_lj(l_rtm,j_rlm)
            vr_rtm_krin(i_rtm,3*nd  ) = vr_rtm_krin(i_rtm,3*nd  )       &
     &                + sp_rlm_krin(kr_j,3*nd-1) * Pgv_lj(l_rtm,j_rlm)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_krin(nscalar,                  &
     &          sp_rlm_krin, vr_rtm_krin)
!
      integer(kind = kint), intent(in) :: nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_krin(nnod_rlm,nscalar)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_krin(nnod_rtm,nscalar)
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm
      integer(kind = kint) :: k_rtm, nd, kr_j, kr_l, i_rtm
!
!
!$omp parallel do private(j_rlm,mp_rlm,k_rtm,nd,l_rtm,kr_j,i_rtm)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
!
        do nd = 1, nscalar
          do kr_l = 1, nidx_rtm(1)*nidx_rtm(2)
            i_rtm = kr_l + (mp_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
            k_rtm = 1 + mod(kr_l-1,nidx_rtm(1))
            l_rtm = 1 + (kr_l - k_rtm) / nidx_rtm(1)
            kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
!
            vr_rtm_krin(i_rtm,nd) = vr_rtm_krin(i_rtm,nd)               &
     &              + sp_rlm_krin(kr_j,nd) * P_rtm(l_rtm,j_rlm)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_krin
