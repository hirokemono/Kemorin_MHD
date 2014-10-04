!>@file   set_legendre_for_matmul.f90
!!@brief  module set_legendre_for_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine set_fwd_leg_vector_matmul                            &
!!     &         (jst, nj_rlm, num_jl, Pvw_le, dPvw_le, Pgvw_le)
!!      subroutine set_fwd_leg_scalar_matmul                            &
!!     &         (jst, nj_rlm, num_jl, Pws_le)
!!
!!      subroutine set_fwd_leg_vector_sym_matmul                        &
!!     &         (jst, n_jk_e, n_jk_o, nle_rtm, nlo_rtm,                &
!!     &          num_jl, Pvw_le, dPvw_le, Pgvw_le,                     &
!!     &          Pvw_lo, dPvw_lo, Pgvw_lo)
!!      subroutine set_fwd_leg_scalar_sym_matmul                        &
!!     &         (jst, n_jk_e, n_jk_o, nle_rtm, nlo_rtm,                &
!!     &          num_jl, Pws_le, Pws_lo)
!!
!!
!!      subroutine set_bwd_leg_vector_matmul                            &
!!     &         (jst, nj_rlm, num_lj, Pg3_je, dPdt_je, Pgv_je)
!!      subroutine set_bwd_leg_scalar_matmul(jst, nj_rlm, num_lj, P_je)
!!
!!      subroutine set_bwd_leg_vector_sym_matmul                        &
!!     &         (jst, n_jk_e, n_jk_o, nl_rtm, num_lj,                  &
!!     &          Pg3_je, dPdt_je, Pg3_jo, dPdt_jo)
!!      subroutine set_bwd_leg_scalar_sym_matmul                        &
!!     &         (jst, n_jk_e, n_jk_o, nl_rtm, num_lj, P_je, P_jo)
!!@endverbatim
!!
!
      module set_legendre_for_matmul
!
      use m_precision
      use m_spheric_parameter
      use m_work_4_sph_trans
      use m_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_fwd_leg_vector_matmul                              &
     &         (jst, nj_rlm, num_jl, Pvw_le, dPvw_le, Pgvw_le)
!
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      integer(kind = kint), intent(in) :: num_jl
      real(kind = kreal), intent(inout) :: Pvw_le(num_jl)
      real(kind = kreal), intent(inout) :: dPvw_le(num_jl)
      real(kind = kreal), intent(inout) :: Pgvw_le(num_jl)
!
      integer(kind = kint) :: l_rtm, i_lj, jj, j_rlm
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do l_rtm = 1, nidx_rtm(2)
          i_lj = l_rtm + (jj-1) * nidx_rtm(2)
!
          Pvw_le(i_lj) = P_rtm(l_rtm,j_rlm)                             &
     &                  * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
          dPvw_le(i_lj) = dPdt_rtm(l_rtm,j_rlm)                         &
     &                  * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
          Pgvw_le(i_lj) = P_rtm(l_rtm,j_rlm)                            &
     &                  * dble(idx_gl_1d_rlm_j(j_rlm,3))                &
     &                  * asin_theta_1d_rtm(l_rtm)                      &
     &                  * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
        end do
      end do
!
      end subroutine set_fwd_leg_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_fwd_leg_scalar_matmul                              &
     &         (jst, nj_rlm, num_jl, Pws_le)
!
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      integer(kind = kint), intent(in) :: num_jl
      real(kind = kreal), intent(inout) :: Pws_le(num_jl)
!
      integer(kind = kint) :: l_rtm, i_lj, jj, j_rlm
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do l_rtm = 1, nidx_rtm(2)
          i_lj = l_rtm + (jj-1) * nidx_rtm(2)
!
          Pws_le(i_lj) = P_rtm(l_rtm,j_rlm)                             &
     &                  * g_sph_rlm(j_rlm,6) * weight_rtm(l_rtm)
        end do
      end do
!
      end subroutine set_fwd_leg_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fwd_leg_vector_sym_matmul                          &
     &         (jst, n_jk_e, n_jk_o, nle_rtm, nlo_rtm,                  &
     &          num_jl, Pvw_le, dPvw_le, Pgvw_le,                       &
     &          Pvw_lo, dPvw_lo, Pgvw_lo)
!
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: num_jl
      real(kind = kreal), intent(inout) :: Pvw_le(num_jl)
      real(kind = kreal), intent(inout) :: dPvw_le(num_jl)
      real(kind = kreal), intent(inout) :: Pgvw_le(num_jl)
      real(kind = kreal), intent(inout) :: Pvw_lo(num_jl)
      real(kind = kreal), intent(inout) :: dPvw_lo(num_jl)
      real(kind = kreal), intent(inout) :: Pgvw_lo(num_jl)
!
      integer(kind = kint) :: lp_rtm, i_lj, jj, je_rlm, jo_rlm
!
!
      do jj = 1, n_jk_o
        je_rlm = 2*jj + jst - 1
        jo_rlm = 2*jj + jst
! Symmetric component
        do lp_rtm = 1, nle_rtm
          i_lj = lp_rtm + (jj-1) * nle_rtm
!
          Pvw_le(i_lj) = P_rtm(lp_rtm,je_rlm)
          Pgvw_le(i_lj) = P_rtm(lp_rtm,je_rlm)
!
          dPvw_lo(i_lj) = dPdt_rtm(lp_rtm,jo_rlm)
        end do
! anti-Symmetric component
        do lp_rtm = 1, nlo_rtm
          i_lj = lp_rtm + (jj-1) * nlo_rtm
!
          dPvw_le(i_lj) = dPdt_rtm(lp_rtm,je_rlm)
!
          Pvw_lo(i_lj) = P_rtm(lp_rtm,jo_rlm)
          Pgvw_lo(i_lj) = P_rtm(lp_rtm,jo_rlm)
        end do
      end do
      do jj = n_jk_o+1, n_jk_e
        je_rlm = 2*jj + jst - 1
! Symmetric component
        do lp_rtm = 1, nle_rtm
          i_lj = lp_rtm + (jj-1) * nle_rtm
!
          Pvw_le(i_lj) = P_rtm(lp_rtm,je_rlm)
          Pgvw_le(i_lj) = P_rtm(lp_rtm,je_rlm)
        end do
! anti-Symmetric component
        do lp_rtm = 1, nlo_rtm
          i_lj = lp_rtm + (jj-1) * nlo_rtm
!
          dPvw_le(i_lj) = dPdt_rtm(lp_rtm,je_rlm)
        end do
      end do
!
      end subroutine set_fwd_leg_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_fwd_leg_scalar_sym_matmul                          &
     &         (jst, n_jk_e, n_jk_o, nle_rtm, nlo_rtm,                  &
     &          num_jl, Pws_le, Pws_lo)
!
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: num_jl
      real(kind = kreal), intent(inout) :: Pws_le(num_jl)
      real(kind = kreal), intent(inout) :: Pws_lo(num_jl)
!
      integer(kind = kint) :: lp_rtm, i_lj, jj, je_rlm, jo_rlm
!
!
      do jj = 1, n_jk_e
        je_rlm = 2*jj + jst - 1
        do lp_rtm = 1, nle_rtm
          i_lj = lp_rtm + (jj-1) * nle_rtm
          Pws_le(i_lj) = P_rtm(lp_rtm,je_rlm)
        end do
      end do
!
!    odd l-m
      do jj = 1, n_jk_o
        jo_rlm = 2*jj + jst
        do lp_rtm = 1, nlo_rtm
          i_lj = lp_rtm + (jj-1) * nlo_rtm
          Pws_lo(i_lj) = P_rtm(lp_rtm,jo_rlm)
        end do
      end do
!
      end subroutine set_fwd_leg_scalar_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_bwd_leg_vector_matmul                              &
     &         (jst, nj_rlm, num_lj, Pg3_je, dPdt_je, Pgv_je)
!
      integer(kind = kint), intent(in) :: jst, nj_rlm
      integer(kind = kint), intent(in) :: num_lj
      real(kind = kreal), intent(inout) :: Pg3_je(num_lj)
      real(kind = kreal), intent(inout) :: dPdt_je(num_lj)
      real(kind = kreal), intent(inout) :: Pgv_je(num_lj)
!
      integer(kind = kint) :: l_rtm, i_lj, jj, j_rlm
!
!
!   all hermonics
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do l_rtm = 1, nidx_rtm(2)
          i_lj = l_rtm + (jj-1) * nidx_rtm(2)
          Pg3_je(i_lj) =  P_jl(j_rlm,l_rtm) * g_sph_rlm(j_rlm,3)
          dPdt_je(i_lj) = dPdt_jl(j_rlm,l_rtm)
          Pgv_je(i_lj) = -P_jl(j_rlm,l_rtm)                             &
     &                     * dble(idx_gl_1d_rlm_j(j_rlm,3))             &
     &                      *asin_theta_1d_rtm(l_rtm)
        end do
      end do
!
      end subroutine set_bwd_leg_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_bwd_leg_scalar_matmul(jst, nj_rlm, num_lj, P_je)
!
      integer(kind = kint), intent(in) :: jst, nj_rlm
      integer(kind = kint), intent(in) :: num_lj
      real(kind = kreal), intent(inout) :: P_je(num_lj)
!
      integer(kind = kint) :: l_rtm, i_lj, jj, j_rlm
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do l_rtm = 1, nidx_rtm(2)
          i_lj = l_rtm + (jj-1) * nidx_rtm(2)
          P_je(i_lj) = P_jl(j_rlm,l_rtm)
        end do
      end do
!
      end subroutine set_bwd_leg_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_bwd_leg_vector_sym_matmul                          &
     &         (jst, n_jk_e, n_jk_o, nl_rtm, num_lj,                    &
     &          Pg3_je, dPdt_je, Pg3_jo, dPdt_jo)
!
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: nl_rtm, num_lj
      real(kind = kreal), intent(inout) :: Pg3_je(num_lj)
      real(kind = kreal), intent(inout) :: dPdt_je(num_lj)
      real(kind = kreal), intent(inout) :: Pg3_jo(num_lj)
      real(kind = kreal), intent(inout) :: dPdt_jo(num_lj)
!
      integer(kind = kint) :: lp_rtm, i_lj, jj, j_rlm
!
!
!   even l-m
      do jj = 1, n_jk_e
        j_rlm = 2*jj + jst - 1
        do lp_rtm = 1, nl_rtm
          i_lj = lp_rtm + (jj-1) * nl_rtm
          Pg3_je(i_lj) =  P_rtm(lp_rtm,j_rlm)
          dPdt_je(i_lj) = dPdt_rtm(lp_rtm,j_rlm)
        end do
      end do
!   odd l-m
      do jj = 1, n_jk_o
        j_rlm = 2*jj + jst
        do lp_rtm = 1, nl_rtm
          i_lj = lp_rtm + (jj-1) * nl_rtm
          Pg3_jo(i_lj) =  P_rtm(lp_rtm,j_rlm)
          dPdt_jo(i_lj) = dPdt_rtm(lp_rtm,j_rlm)
        end do
      end do
!
      end subroutine set_bwd_leg_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_bwd_leg_scalar_sym_matmul                          &
     &         (jst, n_jk_e, n_jk_o, nl_rtm, num_lj, P_je, P_jo)
!
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: nl_rtm, num_lj
      real(kind = kreal), intent(inout) :: P_je(num_lj)
      real(kind = kreal), intent(inout) :: P_jo(num_lj)
!
      integer(kind = kint) :: lp_rtm, i_lj, jj, j_rlm
!
!
!   even l-m
      do jj = 1, n_jk_e
        j_rlm = 2*jj + jst - 1
        do lp_rtm = 1, nl_rtm
          i_lj = lp_rtm + (jj-1) * nl_rtm
          P_je(i_lj) = P_rtm(lp_rtm,j_rlm)
        end do
      end do
!
!   odd l-m
      do jj = 1, n_jk_o
        j_rlm = 2*jj + jst
        do lp_rtm = 1, nl_rtm
          i_lj = lp_rtm + (jj-1) * nl_rtm
          P_jo(i_lj) = P_rtm(lp_rtm,j_rlm)
        end do
      end do
!
      end subroutine set_bwd_leg_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module set_legendre_for_matmul
