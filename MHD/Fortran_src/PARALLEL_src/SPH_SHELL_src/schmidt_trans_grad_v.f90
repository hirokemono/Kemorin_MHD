!
!      module schmidt_trans_grad_v
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine schmidt_b_trans_grad_v(nb)
!        Input:  sp_rlm   (Order: spectr,diff_spectr)
!        Output: vr_rtm   (Order: radius,theta,phi)
!
!      subroutine schmidt_f_trans_grad_v(nb)
!        INput:  vr_rtm   (Order: radius,theta,phi)
!        Output: sp_rlm   (Order: spectr,diff_spectr)
!
      module schmidt_trans_grad_v
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use clear_schmidt_trans
!
      implicit none
!
      private :: schmidt_b_trans_grad_vec, schmidt_f_trans_grad_vec
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_grad_v(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_b_trans_vector(nb)
      call schmidt_b_trans_grad_vec(nb)
!
      end subroutine schmidt_b_trans_grad_v
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_grad_v(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_f_trans_grad(nb)
      call schmidt_f_trans_grad_vec(nb)
!
      end subroutine schmidt_f_trans_grad_v
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_grad_vec(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied, irt, irt_b
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: l_rtm, k_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: m, nd
!
!
!$omp parallel do private(ip,ist,ied,i_rlm,j_rlm,ip_rtm,k_rtm,l_rtm,    &
!$omp&                    m,nd,irt,irt_b)
      do ip = 1, np_smp
        ist = nb*irt_rtm_smp_stack(ip-1) + 1
        ied = nb*irt_rtm_smp_stack(ip)
        do j_rlm = 1, nidx_rlm(2)
          m = idx_gl_1d_rlm_j(j_rlm,3)
!cdir nodep
            do irt_b = ist, ied
              nd = 1 + mod(irt_b-1,nb)
              irt = 1 + (irt_b - nd) / nb
              l_rtm = 1 + mod( (irt-1),nidx_rtm(2) )
              k_rtm = 1 + (irt - l_rtm) / nidx_rtm(2)
              ip_rtm = irt_b                                            &
     &               + (mdx_p_rlm_rtm(j_rlm)-1)                         &
     &                * nb * nidx_rtm(1) * nidx_rtm(2)
              in_rtm = irt_b                                            &
     &               + (mdx_n_rlm_rtm(j_rlm)-1)                         &
     &                * nb * nidx_rtm(1) * nidx_rtm(2)
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
!
              vr_rtm(3*ip_rtm-2) = vr_rtm(3*ip_rtm-2)                   &
     &                      + sp_rlm(2*i_rlm  ) * P_rtm(l_rtm,j_rlm)
              vr_rtm(3*ip_rtm-1) = vr_rtm(3*ip_rtm-1)                   &
     &                      + sp_rlm(2*i_rlm-1) * dPdt_rtm(l_rtm,j_rlm)
              vr_rtm(3*in_rtm  ) = vr_rtm(3*in_rtm  )                   &
     &                      + sp_rlm(2*i_rlm-1) * P_rtm(l_rtm,j_rlm)    &
     &                       * dble(-m) * asin_theta_1d_rtm(l_rtm)
            end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine schmidt_b_trans_grad_vec
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_grad_vec(nb)
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
      end subroutine schmidt_f_trans_grad_vec
!
! -----------------------------------------------------------------------
!
      end module schmidt_trans_grad_v
