!
!      module schmidt_trans_vector
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine schmidt_b_trans_vector(nb)
!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!        Output: vr_rtm   (Order: radius,theta,phi)
!
!      subroutine schmidt_f_trans_vector(nb)
!        INput:  vr_rtm   (Order: radius,theta,phi)
!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!
      module schmidt_trans_vector
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
      private :: schmidt_f_trans_vector_v, schmidt_b_trans_vector_v
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_vector(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_b_trans_vector(nb)
      call schmidt_b_trans_vector_v(nb)
!
      end subroutine schmidt_b_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_vector(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_f_trans_vector(nb)
      call schmidt_f_trans_vector_v(nb)
!
      end subroutine schmidt_f_trans_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_vector_v(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied, irt, irt_b
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: m, nd, mst_rtm
!
!
!$omp parallel do private(ip,ist,ied,i_rlm,j_rlm,ip_rtm,in_rtm,         &
!$omp&               k_rtm,l_rtm,m,nd,irt,irt_b,mst_rtm)
      do ip = 1, np_smp
        ist = nb*irt_rtm_smp_stack(ip-1) + 1
        ied = nb*irt_rtm_smp_stack(ip)
        do j_rlm = 1, nidx_rlm(2)
          m = idx_gl_1d_rlm_j(j_rlm,3)
          mst_rtm = (mdx_p_rlm_rtm(j_rlm)-1) * nb                       &
     &             * nidx_rtm(1)*nidx_rtm(2)
!
!cdir nodep
          do irt_b = ist, ied
              nd = 1 + mod(irt_b-1,nb)
              irt = 1 + (irt_b - nd) / nb
              l_rtm = 1 + mod( (irt-1),nidx_rtm(2) )
              k_rtm = 1 + (irt - l_rtm) / nidx_rtm(2)
              ip_rtm = irt_b + mst_rtm
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
!
!              vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                          &
              vr_rtm(3*ip_rtm-2) = vr_rtm(3*ip_rtm-2)                   &
     &                     + sp_rlm(3*i_rlm-2) * P_rtm(l_rtm,j_rlm)     &
     &                      * g_sph_rlm(j_rlm,3)
!
!              vt_rtm(ip_rtm) = vt_rtm(ip_rtm)                          &
              vr_rtm(3*ip_rtm-1) = vr_rtm(3*ip_rtm-1)                   &
     &                     + sp_rlm(3*i_rlm-1) * dPdt_rtm(l_rtm,j_rlm)
!
!              vp_rtm(ip_rtm) = vp_rtm(ip_rtm)                          &
              vr_rtm(3*ip_rtm  ) = vr_rtm(3*ip_rtm  )                   &
     &                     - sp_rlm(3*i_rlm  ) * dPdt_rtm(l_rtm,j_rlm)
!
          end do
!
!
          mst_rtm = (mdx_n_rlm_rtm(j_rlm)-1)                            &
     &             * nb * nidx_rtm(1)*nidx_rtm(2)
!cdir nodep
          do irt_b = ist, ied
              nd = 1 + mod(irt_b-1,nb)
              irt = 1 + (irt_b - nd) / nb
              l_rtm = 1 + mod( (irt-1),nidx_rtm(2) )
              k_rtm = 1 + (irt - l_rtm) / nidx_rtm(2)
              in_rtm = irt_b + mst_rtm
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
!
!              vt_rtm(in_rtm) = vt_rtm(in_rtm)                          &
              vr_rtm(3*in_rtm-1) = vr_rtm(3*in_rtm-1)                   &
     &                       + sp_rlm(3*i_rlm  ) * P_rtm(l_rtm,j_rlm)   &
     &                        * dble(-m) * asin_theta_1d_rtm(l_rtm)
!
!              vp_rtm(in_rtm) = vp_rtm(in_rtm)                          &
              vr_rtm(3*in_rtm  ) = vr_rtm(3*in_rtm  )                   &
     &                       + sp_rlm(3*i_rlm-1) * P_rtm(l_rtm,j_rlm)   &
     &                        * dble(-m) * asin_theta_1d_rtm(l_rtm)
!
           end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_b_trans_vector_v
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_vector_v(nb)
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
      end subroutine schmidt_f_trans_vector_v
!
! -----------------------------------------------------------------------
!
      end module schmidt_trans_vector

