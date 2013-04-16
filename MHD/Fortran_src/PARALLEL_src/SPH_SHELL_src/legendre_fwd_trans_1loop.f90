!>@file   legendre_fwd_trans_1loop.f90
!!@brief  module legendre_fwd_trans_1loop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (perform with single loop for r, l, theta)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_1loop(nb)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_f_trans_scalar_1loop(nb)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!      subroutine legendre_f_trans_grad_1loop(nb)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
!
      module legendre_fwd_trans_1loop
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
      subroutine legendre_f_trans_vector_1loop(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: m, nd, inod
!
!
!$omp parallel do private(ip,ist,ied,i_rlm,k_rlm,j_rlm,                 &
!$omp&                    ip_rtm,in_rtm,l_rtm,m,nd,inod)
      do ip = 1, np_smp
        ist = nb*inod_rlm_smp_stack(ip-1) + 1
        ied = nb*inod_rlm_smp_stack(ip)
        do l_rtm = 1, nidx_rtm(2)
!cdir nodep
          do i_rlm = ist, ied
            nd = 1 + mod(i_rlm-1,nb)
            inod = 1 + (i_rlm - nd) / nb
            j_rlm = 1 + mod((inod-1),nidx_rlm(2))
            k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
            m = idx_gl_1d_rlm_j(j_rlm,3)
!
            ip_rtm = nd + (l_rtm-1)  * nb                               &
     &                 + (k_rlm-1)  * nb * nidx_rtm(2)                  &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                  * nb * nidx_rtm(1) * nidx_rtm(2)
            in_rtm = nd + (l_rtm-1)  * nb                               &
     &                 + (k_rlm-1)  * nb * nidx_rtm(2)                  &
     &                 + (mdx_n_rlm_rtm(j_rlm)-1)                       &
     &                  * nb * nidx_rtm(1) * nidx_rtm(2)
!
!            sp_rlm(i_rlm) = sp_rlm(i_rlm)                              &
            sp_rlm(3*i_rlm-2) = sp_rlm(3*i_rlm-2)                       &
     &                     + vr_rtm(3*ip_rtm-2) * P_rtm(l_rtm,j_rlm)    &
     &                      * weight_rtm(l_rtm)
!
!            ds_rlm(i_rlm) = ds_rlm(i_rlm)                              &
            sp_rlm(3*i_rlm-1) = sp_rlm(3*i_rlm-1)                       &
     &                 + ( vr_rtm(3*ip_rtm-1) * dPdt_rtm(l_rtm,j_rlm)   &
     &                   - vr_rtm(3*in_rtm  ) * P_rtm(l_rtm,j_rlm)      &
     &                      * dble(m) * asin_theta_1d_rtm(l_rtm) )      &
     &                     * weight_rtm(l_rtm)
!
!            st_rlm(i_rlm) = st_rlm(i_rlm)                              &
            sp_rlm(3*i_rlm  ) = sp_rlm(3*i_rlm  )                       &
     &                 - ( vr_rtm(3*in_rtm-1) * P_rtm(l_rtm,j_rlm)      &
     &                      * dble(m) * asin_theta_1d_rtm(l_rtm)        &
     &                   + vr_rtm(3*ip_rtm  ) * dPdt_rtm(l_rtm,j_rlm) ) &
     &                     * weight_rtm(l_rtm)
          end do
        end do
!
!cdir nodep
        do i_rlm = ist, ied
          nd = 1 + mod(i_rlm-1,nb)
          inod = 1 + (i_rlm - nd) / nb
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
!
          sp_rlm(3*i_rlm-2) = sp_rlm(3*i_rlm-2) * g_sph_rlm(j_rlm,7)
          sp_rlm(3*i_rlm-1) = sp_rlm(3*i_rlm-1) * g_sph_rlm(j_rlm,7)
          sp_rlm(3*i_rlm  ) = sp_rlm(3*i_rlm  ) * g_sph_rlm(j_rlm,7)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_1loop
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_1loop(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: i_rtm, l_rtm
      integer(kind = kint) :: nd, inod
!
!
!$omp parallel do                                                       &
!$omp& private(ip,ist,ied,i_rlm,k_rlm,j_rlm,i_rtm,l_rtm,nd,inod)
      do ip = 1, np_smp
        ist = nb*inod_rlm_smp_stack(ip-1) + 1
        ied = nb*inod_rlm_smp_stack(ip)
        do l_rtm = 1, nidx_rtm(2)
!cdir nodep
          do i_rlm = ist, ied
            nd = 1 + mod(i_rlm-1,nb)
            inod = 1 + (i_rlm - nd) / nb
            j_rlm = 1 + mod((inod-1),nidx_rlm(2))
            k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
            i_rtm = nd + (l_rtm-1) * nb                                 &
     &                 + (k_rlm-1) * nb * nidx_rtm(2)                   &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                  * nb * nidx_rtm(1) * nidx_rtm(2)
!
            sp_rlm(i_rlm) = sp_rlm(i_rlm) + vr_rtm(i_rtm)               &
     &                 * P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
          end do
        end do
!
!cdir nodep
        do i_rlm = ist, ied
          nd = 1 + mod(i_rlm-1,nb)
          inod = 1 + (i_rlm - nd) / nb
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          sp_rlm(i_rlm) = sp_rlm(i_rlm) * g_sph_rlm(j_rlm,6)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_1loop
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_grad_1loop(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: m, nd, inod
!
!
!$omp parallel do private(ip,ist,ied,i_rlm,k_rlm,j_rlm,                 &
!$omp&                    ip_rtm,in_rtm,l_rtm,m,nd,inod)
      do ip = 1, np_smp
        ist = nb*inod_rlm_smp_stack(ip-1) + 1
        ied = nb*inod_rlm_smp_stack(ip)
        do l_rtm = 1, nidx_rtm(2)
!cdir nodep
          do i_rlm = ist, ied
            nd = 1 + mod(i_rlm-1,nb)
            inod = 1 + (i_rlm - nd) / nb
            j_rlm = 1 + mod((inod-1),nidx_rlm(2))
            k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
            m = idx_gl_1d_rlm_j(j_rlm,3)
!
            ip_rtm = nd + (l_rtm-1) * nb                                &
     &                  + (k_rlm-1) * nb * nidx_rtm(2)                  &
     &                  + (mdx_p_rlm_rtm(j_rlm)-1)                      &
     &                   * nb * nidx_rtm(1) * nidx_rtm(2)
            in_rtm = nd + (l_rtm-1) * nb                                &
     &                  + (k_rlm-1) * nb * nidx_rtm(2)                  &
     &                  + (mdx_n_rlm_rtm(j_rlm)-1)                      &
     &                   * nb * nidx_rtm(1) * nidx_rtm(2)
!
            sp_rlm(2*i_rlm  ) = sp_rlm(2*i_rlm  )                       &
     &                   + ( vr_rtm(3*in_rtm-2) * P_rtm(l_rtm,j_rlm) )  &
     &                    * weight_rtm(l_rtm)
!
            sp_rlm(2*i_rlm-1) = sp_rlm(2*i_rlm-1)                       &
     &                   + ( vr_rtm(3*ip_rtm-1) * dPdt_rtm(l_rtm,j_rlm) &
     &                     + vr_rtm(3*in_rtm  ) * P_rtm(l_rtm,j_rlm)    &
     &                      * dble(m) * asin_theta_1d_rtm(l_rtm) )      &
     &                    * weight_rtm(l_rtm)
          end do
        end do
!
!cdir nodep
        do i_rlm = ist, ied
          nd = 1 + mod(i_rlm-1,nb)
          inod = 1 + (i_rlm - nd) / nb
          j_rlm = 1 + mod((inod-1),nidx_rlm(2))
          k_rlm = 1 + (inod - j_rlm) / nidx_rlm(2)
!
          sp_rlm(2*i_rlm  ) = sp_rlm(2*i_rlm  ) * g_sph_rlm(j_rlm,6)
          sp_rlm(2*i_rlm-1) = sp_rlm(2*i_rlm-1) * g_sph_rlm(j_rlm,7)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_grad_1loop
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_1loop
