!schmidt_trans_scalar_spin.f90
!      module schmidt_trans_scalar_spin
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine schmidt_b_trans_scalar_spin(nb)
!      subroutine schmidt_b_trans_scalar_2(nb)
!        Input:  sp_rlm_2   (Order: poloidal,diff_poloidal,toroidal)
!        Output: vr_rtm_2   (Order: radius,theta,phi)
!
!      subroutine schmidt_f_trans_scalar_spin(nb)
!      subroutine schmidt_f_trans_scalar_2(nb)
!        INput:  vr_rtm_2   (Order: radius,theta,phi)
!        Output: sp_rlm_2   (Order: poloidal,diff_poloidal,toroidal)
!
      module schmidt_trans_scalar_spin
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
      private :: schmidt_b_trans_scalar_2, schmidt_f_trans_scalar_2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_b_trans_scalar_2(nb)
      call clear_b_trans_scalar_2(nb)
!
      call schmidt_b_trans_scalar_2(nb)
!
      call back_b_trans_scalar_2(nb)
!
      end subroutine schmidt_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_f_trans_scalar_2(nb)
      call clear_f_trans_scalar_2(nb)
!
      call schmidt_f_trans_scalar_2(nb)
!
      call back_f_trans_scalar_2(nb)
!
      end subroutine schmidt_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine schmidt_b_trans_scalar_2(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm, mst, med
      integer(kind = kint) :: nb_nri, kr_nd
!
!
      nb_nri = nb*nidx_rtm(1)
!$omp parallel do private(j_rlm,l_rtm,mp_rlm,mst,med)
      do kr_nd = 1, nb_nri
!      do k_rtm = 1,  nidx_rtm(1)
!        do nd = 1, nb
!
        do mp_rlm = 1, nidx_rtm(3)
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
!cdir nodep
            do l_rtm = 1, nidx_rtm(2)
              vr_rtm_2(l_rtm,mp_rlm,kr_nd,1)                            &
     &                  = vr_rtm_2(l_rtm,mp_rlm,kr_nd,1)                &
     &                   + sp_rlm_2(j_rlm,kr_nd,1) * P_rtm(l_rtm,j_rlm)
!
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine schmidt_b_trans_scalar_2
!
! -----------------------------------------------------------------------
!
      subroutine schmidt_f_trans_scalar_2(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm, mst, med
      integer(kind = kint) :: nb_nri, kr_nd, nd
      real(kind = kreal) :: pwt_tmp
!
!
      nb_nri = nb*nidx_rlm(1)
!$omp parallel do private(j_rlm,l_rtm,pwt_tmp,mp_rlm,mst,med)
!cdir nodep
      do kr_nd = 1, nb_nri
!      do k_rlm = 1, nidx_rlm(1)
!        do nd = 1, nb
!
        do mp_rlm = 1, nidx_rtm(3)
          mst = lstack_rlm(mp_rlm-1)+1
          med = lstack_rlm(mp_rlm)
          do j_rlm = mst, med
!cdir nodep
            do l_rtm = 1, nidx_rtm(2)
              pwt_tmp =  P_rtm(l_rtm,j_rlm) * weight_rtm(l_rtm)
!
              sp_rlm_2(j_rlm,kr_nd,1) = sp_rlm_2(j_rlm,kr_nd,1)         &
     &                     + vr_rtm_2(l_rtm,mp_rlm,kr_nd,1) * pwt_tmp
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
     &                               * g_sph_rlm(j_rlm,6)
          end do
      end do
!$omp end parallel do
!
      end subroutine schmidt_f_trans_scalar_2
!
! -----------------------------------------------------------------------
!
      end module schmidt_trans_scalar_spin

