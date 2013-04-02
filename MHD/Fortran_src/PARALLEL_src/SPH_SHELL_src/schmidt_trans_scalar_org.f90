!schmidt_trans_scalar_org.f90
!      module schmidt_trans_scalar_org
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine schmidt_b_trans_scalar_org(nb)
!      subroutine schmidt_b_trans_scalar_0(nb)
!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!        Output: vr_rtm   (Order: radius,theta,phi)
!
!      subroutine schmidt_f_trans_scalar_org(nb)
!      subroutine schmidt_f_trans_scalar_0(nb)
!        INput:  vr_rtm   (Order: radius,theta,phi)
!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!
      module schmidt_trans_scalar_org
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
      private :: schmidt_b_trans_scalar_0, schmidt_f_trans_scalar_0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar_org(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_b_trans_scalar(nb)
      call schmidt_b_trans_scalar_0(nb)
!
      end subroutine schmidt_b_trans_scalar_org
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar_org(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_f_trans_scalar(nb)
      call schmidt_f_trans_scalar_0(nb)
!
      end subroutine schmidt_f_trans_scalar_org
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar_0(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rtm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,l_rtm,nd,ip_rtm,in_rtm,i_rlm)
      do k_rtm = 1,  nidx_rtm(1)
        do j_rlm = 1, nidx_rlm(2)
!
          do l_rtm = 1, nidx_rtm(2)
!cdir nodep
            do nd = 1, nb
              ip_rtm = nd + (l_rtm-1) * nb                              &
     &                    + (k_rtm-1) * nb*nidx_rtm(2)                  &
     &                    + (mdx_p_rlm_rtm(j_rlm)-1) * nb               &
     &                     * nidx_rtm(1)*nidx_rtm(2)
!
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
!
!              vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                          &
              vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                           &
     &                        + sp_rlm(i_rlm) * P_rtm(l_rtm,j_rlm)
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_b_trans_scalar_0
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar_0(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm
      integer(kind = kint) :: l_rtm
      integer(kind = kint) :: ip_rtm
      integer(kind = kint) :: nd
      real(kind = kreal) :: pwt_tmp
!
!
!$omp parallel do private(j_rlm,k_rlm,nd,i_rlm,ip_rtm,pwt_tmp)
      do l_rtm = 1, nidx_rtm(2)
!
        do j_rlm = 1, nidx_rlm(2)
          pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
!
          do k_rlm = 1, nidx_rlm(1)
!cdir nodep
            do nd = 1, nb
              i_rlm = nd + (j_rlm-1) * nb                               &
     &                     + (k_rlm-1) * nb*nidx_rlm(2)
              ip_rtm = nd + (l_rtm-1)  * nb                             &
     &                 + (k_rlm-1)  * nb * nidx_rtm(2)                  &
     &                 + (mdx_p_rlm_rtm(j_rlm)-1)                       &
     &                  * nb * nidx_rtm(1) * nidx_rtm(2)
!
!              sp_rlm(i_rlm) = sp_rlm(i_rlm)                            &
              sp_rlm(i_rlm) = sp_rlm(i_rlm) + vr_rtm(ip_rtm) * pwt_tmp
            end do
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(k_rlm,nd,i_rlm)
      do j_rlm = 1, nidx_rlm(2)
        do k_rlm = 1, nidx_rlm(1)
!cdir nodep
          do nd = 1, nb
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                     + (k_rlm-1) * nb*nidx_rlm(2)
!
            sp_rlm(i_rlm) = sp_rlm(i_rlm) * g_sph_rlm(j_rlm,6)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_f_trans_scalar_0
!
! -----------------------------------------------------------------------
!
      end module schmidt_trans_scalar_org

