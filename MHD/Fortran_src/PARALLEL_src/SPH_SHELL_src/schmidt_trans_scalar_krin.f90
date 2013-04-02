!schmidt_trans_scalar_krin.f90
!      module schmidt_trans_scalar_krin
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine schmidt_b_trans_scalar_krin(nb)
!      subroutine schmidt_b_trans_scalar_1(nb)
!        Input:  sp_rlm_1   (Order: poloidal,diff_poloidal,toroidal)
!        Output: vr_rtm_1   (Order: radius,theta,phi)
!
!      subroutine schmidt_f_trans_scalar_krin(nb)
!      subroutine schmidt_f_trans_scalar_1(nb)
!        INput:  vr_rtm_1   (Order: radius,theta,phi)
!        Output: sp_rlm_1   (Order: poloidal,diff_poloidal,toroidal)
!
      module schmidt_trans_scalar_krin
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use ordering_schmidt_trans_krin
!
      implicit none
!
      private :: schmidt_b_trans_scalar_1, schmidt_f_trans_scalar_1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_b_trans_scalar_1(nb)
      call clear_b_trans_scalar_1(nb)
!
      call schmidt_b_trans_scalar_1(nb)
!
      call back_b_trans_scalar_1(nb)
!
      end subroutine schmidt_b_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_f_trans_scalar_1(nb)
      call clear_f_trans_scalar_1(nb)
!
      call schmidt_f_trans_scalar_1(nb)
!
      call back_f_trans_scalar_1(nb)
!
      end subroutine schmidt_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar_1(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, l_rtm, mst, med, mp_rlm
      integer(kind = kint) :: ip_rtm_1
      integer(kind = kint) :: nb_nri, kr_nd
!
!
      nb_nri = nb*nidx_rtm(1)
!$omp parallel do private(j_rlm,kr_nd,ip_rtm_1,mst,med,l_rtm)
      do mp_rlm = 1, nidx_rtm(3)
        mst = lstack_rlm(mp_rlm-1)+1
        med = lstack_rlm(mp_rlm)
        do j_rlm = mst, med
!
          do l_rtm = 1, nidx_rtm(2)
            ip_rtm_1 = l_rtm + (mp_rlm-1) * nidx_rtm(2)
!          do k_rtm = 1, nidx_rtm(1)
!            do nd = 1, nb
!cdir nodep
            do kr_nd = 1, nb_nri
              vr_rtm_1(kr_nd,ip_rtm_1,1) = vr_rtm_1(kr_nd,ip_rtm_1,1)   &
     &                  + sp_rlm_1(kr_nd,j_rlm,1) * P_rtm(l_rtm,j_rlm)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_b_trans_scalar_1
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar_1(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, l_rtm, mst, med, mp_rlm
      integer(kind = kint) :: ip_rtm_1
      integer(kind = kint) :: nb_nri, kr_nd
      real(kind = kreal) :: pwt_tmp
!
!
      nb_nri = nb*nidx_rlm(1)
!$omp parallel do private(j_rlm,kr_nd,ip_rtm_1,mst,med,l_rtm)
      do mp_rlm = 1, nidx_rtm(3)
        mst = lstack_rlm(mp_rlm-1)+1
        med = lstack_rlm(mp_rlm)
        do j_rlm = mst, med
!
          do l_rtm = 1, nidx_rtm(2)
            ip_rtm_1 = l_rtm + (mp_rlm-1) * nidx_rtm(2)
            pwt_tmp = P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
!
!          do k_rtm = 1, nidx_rtm(1)
!            do nd = 1, nb
!cdir nodep
            do kr_nd = 1, nb_nri
              sp_rlm_1(kr_nd,j_rlm,1) = sp_rlm_1(kr_nd,j_rlm,1)         &
     &                           + vr_rtm_1(kr_nd,ip_rtm_1,1) * pwt_tmp
            end do
          end do
!
!        do k_rtm = 1, nidx_rtm(1)
!          do nd = 1, nb
!cdir nodep
          do kr_nd = 1, nb_nri
            sp_rlm_1(kr_nd,j_rlm,1) = sp_rlm_1(kr_nd,j_rlm,1)           &
     &                               * g_sph_rlm(j_rlm,6)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_f_trans_scalar_1
!
! -----------------------------------------------------------------------
!
      end module schmidt_trans_scalar_krin

