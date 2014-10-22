!>@file   legendre_bwd_trans_testloop.f90
!!@brief  module legendre_bwd_trans_testloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_test(ncomp, nvector,         &
!!     &          irev_sr_rlm, n_WR, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_test(ncomp, nvector, nscalar,&
!!     &          irev_sr_rlm, n_WR, WR, WS)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_testloop
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_legendre_work_sym_matmul
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_vector_test(ncomp, nvector,           &
     &          irev_sr_rlm, n_WR, WR, vr_rtm)
!
      use cal_vr_rtm_by_vecprod
      use set_sp_rlm_for_leg_vecprod
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: ip_send, in_send
      integer(kind = kint) :: k_rlm, l_rtm, nd
      integer(kind = kint) :: ip, kst, ked, lp, lst, led
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, nj_rlm
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,led,jst,nj_rlm,             &
!$omp&                    l_rtm,nd,ip_rtm,in_rtm,ip_send,in_send,       &
!$omp&                    mp_rlm,mn_rlm,a1r_1d_rlm_r,a2r_1d_rlm_r)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = lstack_block_rtm(lp-1) + 1
          led = lstack_block_rtm(lp  )
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            do k_rlm = kst, ked
              a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
              a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
              do l_rtm = lst, led
!
                do nd = 1, nvector
                  ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                &
     &                       + (k_rlm-1) *  istep_rtm(1)                &
     &                       + (mp_rlm-1) * istep_rtm(3)
                  in_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                &
     &                       + (k_rlm-1) *  istep_rtm(1)                &
     &                       + (mn_rlm-1) * istep_rtm(3)
                  ip_send = 3*nd-2 + (ip_rtm-1) * ncomp
                  in_send = 3*nd-2 + (in_rtm-1) * ncomp
!
                  call set_sp_rlm_vector_blocked                        &
     &               (jst, nd, k_rlm, a1r_1d_rlm_r, a2r_1d_rlm_r,       &
     &                ncomp, n_WR, irev_sr_rlm, WR, nj_rlm,             &
     &                pol_e(1,ip), dpoldt_e(1,ip), dpoldp_e(1,ip),      &
     &                dtordt_e(1,ip), dtordp_e(1,ip))
!
                  call cal_vr_rtm_dydtheta_vector                       &
     &               (nj_rlm, P_jl(jst+1,l_rtm), dPdt_jl(jst+1,l_rtm),  &
     &                pol_e(1,ip), dpoldt_e(1,ip), dtordt_e(1,ip),      &
     &                vr_rtm(ip_send))
                  call cal_vr_rtm_dydphi_vector(nj_rlm,                 &
     &                P_jl(jst+1,l_rtm), asin_theta_1d_rtm(l_rtm),      &
     &                dpoldp_e(1,ip), dtordp_e(1,ip), vr_rtm(in_send))
                end do
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_test
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_test(ncomp, nvector, nscalar,  &
     &          irev_sr_rlm, n_WR, WR, vr_rtm)
!
      use cal_vr_rtm_by_vecprod
      use set_sp_rlm_for_leg_vecprod
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: k_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, nd, ip, kst, ked, lp, lst, led
      integer(kind = kint) :: ip_send, i_recv
      integer(kind = kint) :: mp_rlm, jst, nj_rlm
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,lp,lst,led,l_rtm,nd,i_recv,        &
!$omp&                    ip_rtm,ip_send,mp_rlm,jst,nj_rlm)
      do ip = 1, np_smp
        kst = idx_rtm_smp_stack(ip-1,1) + 1
        ked = idx_rtm_smp_stack(ip,  1)
        do lp = 1, nblock_l_rtm
          lst = lstack_block_rtm(lp-1) + 1
          led = lstack_block_rtm(lp  )
!
          do mp_rlm = 1, nidx_rtm(3)
            jst = lstack_rlm(mp_rlm-1)
            nj_rlm = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            do k_rlm = kst, ked
!
              do l_rtm = lst, led
                do nd = 1, nscalar
                  ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                &
     &                       + (k_rlm-1) *  istep_rtm(1)                &
     &                       + (mp_rlm-1) * istep_rtm(3)
                  ip_send = nd + 3*nvector                              &
     &                         + (ip_rtm-1) * ncomp
!
                  call set_sp_rlm_scalar_blocked                        &
     &               (jst, nd, k_rlm, ncomp, nvector,                   &
     &                n_WR, irev_sr_rlm, WR, nj_rlm, scl_e(1,ip))
                  call cal_vr_rtm_scalar_blocked(nj_rlm,                &
     &               P_jl(jst+1,l_rtm), scl_e(1,ip), vr_rtm(ip_send))
                end do
              end do
!
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_test
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_testloop
