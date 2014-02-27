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
!!      subroutine legendre_b_trans_vector_krin(nvector)
!!        Input:  vr_rtm_krin   (Order: radius,theta,phi)
!!        Output: sp_rlm_krin   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_krin(nscalar)
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
      use m_work_4_sph_trans_krin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_vector_krin(nvector)
!
      integer(kind = kint), intent(in) :: nvector
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm
      integer(kind = kint) :: l_rtm, mst, med
      integer(kind = kint) :: ip_rtm_1, in_rtm_1
      integer(kind = kint) :: nb_nri, k_rtm, nd
      real(kind = kreal) :: pg_tmp, dp_tmp
!
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel do private(mp_rlm,j_rlm,ip_rtm_1,mst,med,                &
!$omp&                    in_rtm_1,pg_tmp,dp_tmp,l_rtm,k_rtm,nd)
      do mp_rlm = 1, nidx_rtm(3)
        mst = lstack_rlm(mp_rlm-1)+1
        med = lstack_rlm(mp_rlm)
        do nd = 1, nvector
          do j_rlm = mst, med
!
            do l_rtm = 1, nidx_rtm(2)
              ip_rtm_1 = l_rtm + (mp_rlm-1) * nidx_rtm(2)
!
              pg_tmp = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
              dp_tmp = dPdt_rtm(l_rtm,j_rlm)
              do k_rtm = 1, nidx_rtm(1)
                vr_rtm_krin(k_rtm,ip_rtm_1,3*nd-2)                      &
     &                    = vr_rtm_krin(k_rtm,ip_rtm_1,3*nd-2)          &
     &                     + sp_rlm_krin(k_rtm,j_rlm,3*nd-2) * pg_tmp
                vr_rtm_krin(k_rtm,ip_rtm_1,3*nd-1)                      &
     &                    = vr_rtm_krin(k_rtm,ip_rtm_1,3*nd-1)          &
     &                     + sp_rlm_krin(k_rtm,j_rlm,3*nd-1) * dp_tmp
                vr_rtm_krin(k_rtm,ip_rtm_1,3*nd  )                      &
     &                    = vr_rtm_krin(k_rtm,ip_rtm_1,3*nd  )          &
     &                     - sp_rlm_krin(k_rtm,j_rlm,3*nd  ) * dp_tmp
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(mp_rlm,mn_rlm,j_rlm,ip_rtm_1,mst,med,         &
!$omp&                    in_rtm_1,pg_tmp,l_rtm,k_rtm,nd)
      do mp_rlm = 1, nidx_rtm(3)
        mn_rlm = nidx_rtm(3) - mp_rlm + 1
        mst = lstack_rlm(mp_rlm-1)+1
        med = lstack_rlm(mp_rlm)
        do nd = 1, nvector
          do j_rlm = mst, med
!
            do l_rtm = 1, nidx_rtm(2)
              in_rtm_1 = l_rtm + (mn_rlm-1) * nidx_rtm(2)
              pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)    &
     &              * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
!
              do k_rtm = 1, nidx_rtm(1)
                vr_rtm_krin(k_rtm,in_rtm_1,3*nd-1)                      &
     &                      = vr_rtm_krin(k_rtm,in_rtm_1,3*nd-1)        &
     &                       + sp_rlm_krin(k_rtm,j_rlm,3*nd  ) * pg_tmp
                vr_rtm_krin(k_rtm,in_rtm_1,3*nd  )                      &
     &                      = vr_rtm_krin(k_rtm,in_rtm_1,3*nd  )        &
     &                       + sp_rlm_krin(k_rtm,j_rlm,3*nd-1) * pg_tmp
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_krin(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
      integer(kind = kint) :: j_rlm, l_rtm, mst, med, mp_rlm
      integer(kind = kint) :: ip_rtm_1
      integer(kind = kint) :: nb_nri, k_rtm, nd
!
!
      nb_nri = nscalar*nidx_rtm(1)
!$omp parallel do private(j_rlm,k_rtm,nd,ip_rtm_1,mst,med,l_rtm)
      do mp_rlm = 1, nidx_rtm(3)
        mst = lstack_rlm(mp_rlm-1)+1
        med = lstack_rlm(mp_rlm)
        do nd = 1, nscalar
          do j_rlm = mst, med
!
            do l_rtm = 1, nidx_rtm(2)
              ip_rtm_1 = l_rtm + (mp_rlm-1) * nidx_rtm(2)
              do k_rtm = 1, nidx_rtm(1)
                vr_rtm_krin(k_rtm,ip_rtm_1,nd)                          &
     &             = vr_rtm_krin(k_rtm,ip_rtm_1,nd)                     &
     &              + sp_rlm_krin(k_rtm,j_rlm,nd) * P_rtm(l_rtm,j_rlm)
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_krin
