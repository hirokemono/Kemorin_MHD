!
!      module schmidt_trans_scalar
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine schmidt_b_trans_scalar(nb)
!         Input:  sp_rlm
!         Output: vr_rtm
!
!      subroutine schmidt_f_trans_scalar(nb)
!         Input:  vr_rtm
!         Output: sp_rlm
!
      module schmidt_trans_scalar
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
      use m_parallel_var_dof
!
      implicit none
!
      private :: schmidt_b_trans_scalar_v, schmidt_f_trans_scalar_v
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_b_trans_scalar(nb)
      call schmidt_b_trans_scalar_v(nb)
!
      end subroutine schmidt_b_trans_scalar
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_f_trans_scalar(nb)
      call schmidt_f_trans_scalar_v(nb)
!
      end subroutine schmidt_f_trans_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar_v(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied, irt, irt_b
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(ip,ist,ied,i_rlm,j_rlm,i_rtm,k_rtm,l_rtm,     &
!$omp&                    nd,irt,irt_b)
      do ip = 1, np_smp
        ist = nb*irt_rtm_smp_stack(ip-1) + 1
        ied = nb*irt_rtm_smp_stack(ip)
        do j_rlm = 1, nidx_rlm(2)
!cdir nodep
            do irt_b = ist, ied
              nd = 1 + mod(irt_b-1,nb)
              irt = 1 + (irt_b - nd) / nb
              l_rtm = 1 + mod( (irt-1),nidx_rtm(2) )
              k_rtm = 1 + (irt - l_rtm) / nidx_rtm(2)
              i_rtm = irt_b                                             &
     &               + (mdx_p_rlm_rtm(j_rlm)-1)                         &
     &                * nb * nidx_rtm(1) * nidx_rtm(2)
              i_rlm = nd                                                &
     &               + (j_rlm-1) * nb                                   &
     &               + (k_rtm-1) * nb * nidx_rlm(2)
              vr_rtm(i_rtm) = vr_rtm(i_rtm)                             &
     &                      + sp_rlm(i_rlm) * P_rtm(l_rtm,j_rlm)
            end do
        end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_b_trans_scalar_v
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar_v(nb)
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
      end subroutine schmidt_f_trans_scalar_v
!
! -----------------------------------------------------------------------
!
      end module schmidt_trans_scalar
