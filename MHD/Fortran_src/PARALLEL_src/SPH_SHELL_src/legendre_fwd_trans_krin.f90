!>@file   legendre_fwd_trans_krin.f90
!!@brief  module legendre_fwd_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_krin(nvector,                &
!!     &          vr_rtm_krin, sp_rlm_krin)
!!      subroutine legendre_f_trans_scalar_krin(nscalar,                &
!!     &          vr_rtm_krin, sp_rlm_krin)
!!        Input:  vr_rtm_krin
!!        Output: sp_rlm_krin
!!@endverbatim
!!
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of scalar to be transformed
!
      module legendre_fwd_trans_krin
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
      subroutine legendre_f_trans_vector_krin(nvector,                  &
     &          vr_rtm_krin, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nnod_rtm,3*nvector)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_krin(nnod_rlm,3*nvector)
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: k_rtm, nd, kr_j, kr_l
!
!
!$omp parallel do private(j_rlm,l_rtm,kr_j,kr_l,ip_rtm,in_rtm,          &
!$omp&                    mp_rlm,mn_rlm,k_rtm,nd)
        do j_rlm = 1, nidx_rlm(2)
          mp_rlm = mdx_p_rlm_rtm(j_rlm)
          mn_rlm = mdx_n_rlm_rtm(j_rlm)
!
          do l_rtm = 1, nidx_rtm(2)
            do nd = 1, nvector
              do k_rtm = 1, nidx_rtm(1)
                kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
                kr_l = k_rtm + (l_rtm-1)*nidx_rtm(1)
                ip_rtm = kr_l + (mp_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
                in_rtm = kr_l + (mn_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
!
                sp_rlm_krin(kr_j,3*nd-2) = sp_rlm_krin(kr_j,3*nd-2)     &
     &              + vr_rtm_krin(ip_rtm,3*nd-2) * Pvw_lj(l_rtm,j_rlm)
                sp_rlm_krin(kr_j,3*nd-1) = sp_rlm_krin(kr_j,3*nd-1)     &
     &           + ( vr_rtm_krin(ip_rtm,3*nd-1) * dPvw_lj(l_rtm,j_rlm)  &
     &             - vr_rtm_krin(in_rtm,3*nd  ) * Pgvw_lj(l_rtm,j_rlm))
                sp_rlm_krin(kr_j,3*nd  ) = sp_rlm_krin(kr_j,3*nd  )     &
     &           - ( vr_rtm_krin(in_rtm,3*nd-1) * Pgvw_lj(l_rtm,j_rlm)  &
     &             + vr_rtm_krin(ip_rtm,3*nd  ) * dPvw_lj(l_rtm,j_rlm))
              end do
            end do
          end do
!
        do nd = 1, nvector
          do k_rtm = 1, nidx_rtm(1)
            kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
            sp_rlm_krin(kr_j,3*nd-2) = sp_rlm_krin(kr_j,3*nd-2)         &
     &            * radius_1d_rlm_r(k_rtm)*radius_1d_rlm_r(k_rtm)
            sp_rlm_krin(kr_j,3*nd-1) = sp_rlm_krin(kr_j,3*nd-1)         &
     &            * radius_1d_rlm_r(k_rtm)
            sp_rlm_krin(kr_j,3*nd  ) = sp_rlm_krin(kr_j,3*nd  )         &
     &            * radius_1d_rlm_r(k_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_krin(nscalar,                  &
     &          vr_rtm_krin, sp_rlm_krin)
!
      integer(kind = kint), intent(in) :: nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nnod_rtm,nscalar)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_krin(nnod_rlm,nscalar)
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm, ip_rtm
      integer(kind = kint) :: k_rtm, nd, kr_j, kr_l
!
!
!$omp parallel do private(j_rlm,k_rtm,kr_j,nd,kr_l,l_rtm,ip_rtm,mp_rlm)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
        do l_rtm = 1, nidx_rtm(2)
!
          do nd = 1, nscalar
            do k_rtm = 1, nidx_rtm(1)
              kr_j = k_rtm + (j_rlm-1)*nidx_rtm(1)
              kr_l = k_rtm + (l_rtm-1)*nidx_rtm(1)
              ip_rtm = kr_l + (mp_rlm-1)*nidx_rtm(1)*nidx_rtm(2)
!
              sp_rlm_krin(kr_j,nd) = sp_rlm_krin(kr_j,nd)             &
     &               + vr_rtm_krin(ip_rtm,nd) * Pws_lj(l_rtm,j_rlm)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_krin
