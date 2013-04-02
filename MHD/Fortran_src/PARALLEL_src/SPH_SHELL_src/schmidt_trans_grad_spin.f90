!schmidt_trans_grad_spin.f90
!      module schmidt_trans_grad_spin
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine schmidt_b_trans_grad_spin(nb)
!      subroutine schmidt_b_trans_grad_2(nb)
!        Input:  sp_rlm_2   (Order: poloidal,diff_poloidal)
!        Output: vr_rtm_2   (Order: radius,theta,phi)
!
!      subroutine schmidt_f_trans_grad_spin(nb)
!      subroutine schmidt_f_trans_grad_2(nb)
!        INput:  vr_rtm_2   (Order: radius,theta,phi)
!        Output: sp_rlm_2   (Order: poloidal,diff_poloidal)
!
      module schmidt_trans_grad_spin
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use ordering_schmidt_trans_spin
!
      implicit none
!
      private :: schmidt_b_trans_grad_2, schmidt_f_trans_grad_2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_grad_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_b_trans_grad_2(nb)
      call clear_b_trans_vector_2(nb)
!
      call schmidt_b_trans_grad_2(nb)
!
      call back_b_trans_vector_2(nb)
!
      end subroutine schmidt_b_trans_grad_spin
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_grad_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_f_trans_vector_2(nb)
      call clear_f_trans_grad_2(nb)
!
      call schmidt_f_trans_grad_2(nb)
!
      call back_f_trans_grad_2(nb)
!
      end subroutine schmidt_f_trans_grad_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_grad_2(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, mst, med
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: nb_nri, kr_nd
      real(kind = kreal) :: pg_tmp, dp_tmp
!
!
      nb_nri = nb*nidx_rtm(1)
!$omp parallel do private(j_rlm,l_rtm,mp_rlm,mn_rlm,                    &
!$omp&               mst,med,pg_tmp,dp_tmp)
      do kr_nd = 1, nb_nri
!      do k_rtm = 1,  nidx_rtm(1)
!        do nd = 1, nb
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
!cdir nodep
            do l_rtm = 1, nidx_rtm(2)
              dp_tmp = dPdt_rtm(l_rtm,j_rlm)
              pg_tmp = P_rtm(l_rtm,j_rlm) * asin_theta_1d_rtm(l_rtm)    &
     &                * dble( -idx_gl_1d_rlm_j(j_rlm,3) )
!
              vr_rtm_2(l_rtm,mp_rlm,kr_nd,1)                            &
     &                = vr_rtm_2(l_rtm,mp_rlm,kr_nd,1)                  &
     &                 + sp_rlm_2(j_rlm,kr_nd,1) * P_rtm(l_rtm,j_rlm)
!
              vr_rtm_2(l_rtm,mp_rlm,kr_nd,2)                            &
     &                 = vr_rtm_2(l_rtm,mp_rlm,kr_nd,2)                 &
     &                 + sp_rlm_2(j_rlm,kr_nd,2) * dp_tmp
!
              vr_rtm_2(l_rtm,mn_rlm,kr_nd,3)                            &
     &                = vr_rtm_2(l_rtm,mn_rlm,kr_nd,3)                  &
     &                 + sp_rlm_2(j_rlm,kr_nd,2) * pg_tmp
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine schmidt_b_trans_grad_2
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_grad_2(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, mst, med, l_rtm
      integer(kind = kint) :: nb_nri, kr_nd, nd
      real(kind = kreal) :: pwt_tmp, dpwt_tmp, pgwt_tmp
!
!
      nb_nri = nb*nidx_rlm(1)
!$omp parallel do private(j_rlm,l_rtm,mp_rlm,mn_rlm,                    &
!$omp&               mst,med,pwt_tmp,dpwt_tmp,pgwt_tmp)
!cdir nodep
      do kr_nd = 1, nb_nri
!      do k_rlm = 1, nidx_rlm(1)
!        do nd = 1, nb
!
        do mp_rlm = 1, nidx_rtm(3)
          mn_rlm = nidx_rtm(3) - mp_rlm + 1
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
!cdir nodep
            do l_rtm = 1, nidx_rtm(2)
              pwt_tmp =  P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              dpwt_tmp = dPdt_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
              pgwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)         &
     &                 * dble( idx_gl_1d_rlm_j(j_rlm,3) )               &
     &                 * asin_theta_1d_rtm(l_rtm)
!
              sp_rlm_2(j_rlm,kr_nd,1) = sp_rlm_2(j_rlm,kr_nd,1)         &
     &                 + vr_rtm_2(l_rtm,mp_rlm,kr_nd,1) * pwt_tmp
!
              sp_rlm_2(j_rlm,kr_nd,2) = sp_rlm_2(j_rlm,kr_nd,2)         &
     &                 + ( vr_rtm_2(l_rtm,mp_rlm,kr_nd,2) * dpwt_tmp    &
     &                   + vr_rtm_2(l_rtm,mn_rlm,kr_nd,3) * pgwt_tmp)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j_rlm)
      do kr_nd = 1, nb_nri
!      do k_rlm = 1, nidx_rlm(1)
!        do nd = 1, nb
!cdir nodep
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_2(j_rlm,kr_nd,1) = sp_rlm_2(j_rlm,kr_nd,1)           &
     &                           * g_sph_rlm(j_rlm,7)
            sp_rlm_2(j_rlm,kr_nd,2) = sp_rlm_2(j_rlm,kr_nd,2)           &
     &                           * g_sph_rlm(j_rlm,6)
          end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_f_trans_grad_2
!
! -----------------------------------------------------------------------
!
      end module schmidt_trans_grad_spin

