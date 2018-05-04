!!@file   sph_SGS_nl_gradient_pout.f90
!!@brief  module sph_SGS_nl_gradient_pout
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in MAy., 2018
!
!>@brief SGS terms by nonlinear gradient model in spherical coordinate
!!
!!@verbatim
!!      subroutine sph_SGS_induct_nl_gradient_pout                      &
!!     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t,             &
!!     &          r_moments, sph_moments, coef,                         &
!!     &          u_rtp, grad_ux, grad_uy, grad_uz,                     &
!!     &          b_rtp, grad_bx, grad_by, grad_bz, d_SGS)
!!      subroutine sph_SGS_s_flux_nl_gradient_pout                      &
!!     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t,             &
!!     &          r_moments, sph_moments, coef,                         &
!!     &          u_rtp, grad_ux, grad_uy, grad_uz, grad_s, d_SGS)
!!      subroutine sph_SGS_m_flux_nl_gradient_pout                      &
!!     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t,             &
!!     &          r_moments, sph_moments, coef,                         &
!!     &          u_rtp, grad_ux, grad_uy, grad_uz, d_SGS)
!!@endverbatim
!
      module sph_SGS_nl_gradient_pout
!
      use m_precision
      use t_sph_filtering_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sph_SGS_induct_nl_gradient_pout                        &
     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t,               &
     &          r_moments, sph_moments, coef,                           &
     &          u_rtp, grad_ux, grad_uy, grad_uz,                       &
     &          b_rtp, grad_bx, grad_by, grad_bz, d_SGS)
!
      type(sph_filter_moment), intent(in) :: r_moments, sph_moments
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: a_r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cot_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(in) :: u_rtp(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_ux(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_uy(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_uz(nnod_rtp,3)
!
      real(kind = kreal), intent(in) :: b_rtp(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_bx(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_by(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_bz(nnod_rtp,3)
!
      real(kind = kreal), intent(inout) :: d_SGS(nnod_rtp,3)
!
      integer(kind = kint) :: kr, lt, mp, inod
      real(kind = kreal) :: du1_dx1, du1_dx2, du1_dx3
      real(kind = kreal) :: du2_dx1, du2_dx2, du2_dx3
      real(kind = kreal) :: du3_dx1, du3_dx2, du3_dx3
      real(kind = kreal) :: db1_dx1, db1_dx2, db1_dx3
      real(kind = kreal) :: db2_dx1, db2_dx2, db2_dx3
      real(kind = kreal) :: db3_dx1, db3_dx2, db3_dx3
      real(kind = kreal) :: gamma_r, gamma_t, gamma_p
!
!
      write(*,*) 'r_moments%filter_mom', r_moments%filter_mom
      write(*,*) 'sph_moments%filter_mom', sph_moments%filter_mom
!
!$omp  parallel do                                                      &
!$omp    private(du1_dx1,du1_dx2,du1_dx3,db1_dx1,db1_dx2,db1_dx3,       &
!&omp&           du2_dx1,du2_dx2,du2_dx3,db2_dx1,db2_dx2,db2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,db3_dx1,db3_dx2,db3_dx3,       &
!$omp&           mp,inod,kr,lt,gamma_r,gamma_t,gamma_p)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1)*nidx_rtp(1)                              &
     &                 + (mp-1)*nidx_rtp(1)*nidx_rtp(2)
            gamma_r = coef * r_moments%filter_mom(2)
            gamma_t = coef * sph_moments%filter_mom(2)                  &
     &                     * (r(kr))**2
            gamma_p = coef * sph_moments%filter_mom(2)                  &
     &                     * (r(kr) * sin_t(lt))**2
!
            du1_dx1 = grad_ux(inod,1)
            du1_dx2 = grad_ux(inod,2) - u_rtp(inod,2) * a_r(kr)
            du1_dx3 = grad_ux(inod,3) - u_rtp(inod,3) * a_r(kr)
            du2_dx1 = grad_uy(inod,1)
            du2_dx2 = grad_uy(inod,2) + u_rtp(inod,1) * a_r(kr)
            du2_dx3 = grad_uy(inod,3)                                   &
     &               - u_rtp(inod,3) * a_r(kr) * cot_t(lt)
            du3_dx1 = grad_uz(inod,1)
            du3_dx2 = grad_uz(inod,2)
            du3_dx3 = grad_uz(inod,3)                                   &
     &           + (u_rtp(inod,2)*cot_t(lt) - u_rtp(inod,1)) * a_r(kr)
!
            db1_dx1 = grad_bx(inod,1)
            db1_dx2 = grad_bx(inod,2) - b_rtp(inod,2) * a_r(kr)
            db1_dx3 = grad_bx(inod,3) - b_rtp(inod,3) * a_r(kr)
            db2_dx1 = grad_by(inod,1)
            db2_dx2 = grad_by(inod,2) + b_rtp(inod,1) * a_r(kr)
            db2_dx3 = grad_by(inod,3)                                   &
     &               - b_rtp(inod,3) * a_r(kr) * cot_t(lt)
            db3_dx1 = grad_bz(inod,1)
            db3_dx2 = grad_bz(inod,2)
            db3_dx3 = grad_bz(inod,3)                                   &
     &           + (b_rtp(inod,2)*cot_t(lt) - b_rtp(inod,1)) * a_r(kr)
!
            d_SGS(inod,1) = gamma_r * (du2_dx1 * db3_dx1                &
     &                                - du3_dx1 * db2_dx1)              &
     &                   +  gamma_t * (du2_dx2 * db3_dx2                &
     &                                - du3_dx2 * db2_dx2)              &
     &                   +  gamma_p * (du2_dx3 * db3_dx3                &
     &                                - du3_dx3 * db2_dx3)
            d_SGS(inod,2) = gamma_r * (du3_dx1 * db1_dx1                &
     &                                - du1_dx1 * db3_dx1)              &
     &                   +  gamma_t * (du3_dx2 * db1_dx2                &
     &                                - du1_dx2 * db3_dx2)              &
     &                   +  gamma_p * (du3_dx3 * db1_dx3                &
     &                                - du1_dx3 * db3_dx3)
            d_SGS(inod,3) = gamma_r * (du1_dx1 * db2_dx1                &
     &                                - du2_dx1 * db1_dx1)              &
     &                   +  gamma_t * (du1_dx2 * db2_dx2                &
     &                                - du2_dx2 * db1_dx2)              &
     &                   +  gamma_p * (du1_dx3 * db2_dx3                &
     &                                - du2_dx3 * db1_dx3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_SGS_induct_nl_gradient_pout
!
!  ---------------------------------------------------------------------
!
      subroutine sph_SGS_s_flux_nl_gradient_pout                        &
     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t,               &
     &          r_moments, sph_moments, coef,                           &
     &          u_rtp, grad_ux, grad_uy, grad_uz, grad_s, d_SGS)
!
      type(sph_filter_moment), intent(in) :: r_moments, sph_moments
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: a_r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cot_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(in) :: u_rtp(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_ux(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_uy(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_uz(nnod_rtp,3)
!
      real(kind = kreal), intent(in) :: grad_s(nnod_rtp,3)
!
      real(kind = kreal), intent(inout) :: d_SGS(nnod_rtp,3)
!
      integer(kind = kint) :: kr, lt, mp, inod
      real(kind = kreal) :: du1_dx1, du1_dx2, du1_dx3
      real(kind = kreal) :: du2_dx1, du2_dx2, du2_dx3
      real(kind = kreal) :: du3_dx1, du3_dx2, du3_dx3
      real(kind = kreal) :: ds_dx1,  ds_dx2,  ds_dx3
      real(kind = kreal) :: gamma_r, gamma_t, gamma_p
!
!
!$omp  parallel do                                                      &
!$omp    private(du1_dx1,du1_dx2,du1_dx3,du2_dx1,du2_dx2,du2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,ds_dx1, ds_dx2, ds_dx3,        &
!$omp&           mp,inod,kr,lt,gamma_r,gamma_t,gamma_p)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1)*nidx_rtp(1)                              &
     &                 + (mp-1)*nidx_rtp(1)*nidx_rtp(2)
            gamma_r = coef * r_moments%filter_mom(2)
            gamma_t = coef * sph_moments%filter_mom(2)                  &
     &                     * (r(kr))**2
            gamma_p = coef * sph_moments%filter_mom(2)                  &
     &                     * (r(kr) * sin_t(lt))**2
!
            du1_dx1 = grad_ux(inod,1)
            du1_dx2 = grad_ux(inod,2) - u_rtp(inod,2) * a_r(kr)
            du1_dx3 = grad_ux(inod,3) - u_rtp(inod,3) * a_r(kr)
            du2_dx1 = grad_uy(inod,1)
            du2_dx2 = grad_uy(inod,2) + u_rtp(inod,1) * a_r(kr)
            du2_dx3 = grad_uy(inod,3)                                   &
     &               - u_rtp(inod,3) * a_r(kr) * cot_t(lt)
            du3_dx1 = grad_uz(inod,1)
            du3_dx2 = grad_uz(inod,2)
            du3_dx3 = grad_uz(inod,3)                                   &
     &            + (u_rtp(inod,2)*cot_t(lt) - u_rtp(inod,1)) * a_r(kr)
!
            ds_dx1 = grad_s(inod,1)
            ds_dx2 = grad_s(inod,2)
            ds_dx3 = grad_s(inod,3)
!
            d_SGS(inod,1) = gamma_r * (du1_dx1 * ds_dx1)                &
     &                   +  gamma_t * (du1_dx2 * ds_dx2)                &
     &                   +  gamma_p * (du1_dx3 * ds_dx3)
            d_SGS(inod,2) = gamma_r * (du2_dx1 * ds_dx1)                &
     &                   +  gamma_t * (du2_dx2 * ds_dx2)                &
     &                   +  gamma_p * (du2_dx3 * ds_dx3)
            d_SGS(inod,3) = gamma_r * (du3_dx1 * ds_dx1)                &
     &                   +  gamma_t * (du3_dx2 * ds_dx2)                &
     &                   +  gamma_p * (du3_dx3 * ds_dx3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_SGS_s_flux_nl_gradient_pout
!
!  ---------------------------------------------------------------------
!
      subroutine sph_SGS_m_flux_nl_gradient_pout                        &
     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t,               &
     &          r_moments, sph_moments, coef,                           &
     &          u_rtp, grad_ux, grad_uy, grad_uz, d_SGS)
!
      type(sph_filter_moment), intent(in) :: r_moments, sph_moments
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: a_r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cot_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(in) :: u_rtp(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_ux(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_uy(nnod_rtp,3)
      real(kind = kreal), intent(in) :: grad_uz(nnod_rtp,3)
!
      real(kind = kreal), intent(inout) :: d_SGS(nnod_rtp,6)
!
      integer(kind = kint) :: kr, lt, mp, inod
      real(kind = kreal) :: du1_dx1, du1_dx2, du1_dx3
      real(kind = kreal) :: du2_dx1, du2_dx2, du2_dx3
      real(kind = kreal) :: du3_dx1, du3_dx2, du3_dx3
      real(kind = kreal) :: gamma_r, gamma_t, gamma_p
!
!
!$omp  parallel do                                                      &
!$omp    private(du1_dx1,du1_dx2,du1_dx3,du2_dx1,du2_dx2,du2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,mp,inod,kr,lt                  &
!$omp&          ,gamma_r,gamma_t,gamma_p)
      do mp = 1, nidx_rtp(3)
        do lt = 1, nidx_rtp(2)
          do kr = 1, nidx_rtp(1)
            inod = kr + (lt-1)*nidx_rtp(1)                              &
     &                 + (mp-1)*nidx_rtp(1)*nidx_rtp(2)
            gamma_r = coef * r_moments%filter_mom(2)
            gamma_t = coef * sph_moments%filter_mom(2)                  &
     &                     * (r(kr))**2
            gamma_p = coef * sph_moments%filter_mom(2)                  &
     &                     * (r(kr) * sin_t(lt))**2
!
            du1_dx1 = grad_ux(inod,1)
            du1_dx2 = grad_ux(inod,2) - u_rtp(inod,2) * a_r(kr)
            du1_dx3 = grad_ux(inod,3) - u_rtp(inod,3) * a_r(kr)
            du2_dx1 = grad_uy(inod,1)
            du2_dx2 = grad_uy(inod,2) + u_rtp(inod,1) * a_r(kr)
            du2_dx3 = grad_uy(inod,3)                                   &
     &               - u_rtp(inod,3) * a_r(kr) * cot_t(lt)
            du3_dx1 = grad_uz(inod,1)
            du3_dx2 = grad_uz(inod,2)
            du3_dx3 = grad_uz(inod,3) &
     &            + (u_rtp(inod,2)*cot_t(lt) - u_rtp(inod,1)) * a_r(kr)
!
            d_SGS(inod,1) = gamma_r * (du1_dx1 * du1_dx1)               &
     &                   +  gamma_t * (du1_dx2 * du1_dx2)               &
     &                   +  gamma_p * (du1_dx3 * du1_dx3)
            d_SGS(inod,2) = gamma_r * (du1_dx1 * du2_dx1)               &
     &                   +  gamma_t * (du1_dx2 * du2_dx2)               &
     &                   +  gamma_p * (du1_dx3 * du2_dx3)
            d_SGS(inod,3) = gamma_r * (du1_dx1 * du3_dx1)               &
     &                   +  gamma_t * (du1_dx2 * du3_dx2)               &
     &                   +  gamma_p * (du1_dx3 * du3_dx3)
            d_SGS(inod,4) = gamma_r * (du2_dx1 * du2_dx1)               &
     &                   +  gamma_t * (du2_dx2 * du2_dx2)               &
     &                   +  gamma_p * (du2_dx3 * du2_dx3)
            d_SGS(inod,5) = gamma_r * (du2_dx1 * du3_dx1)               &
     &                   +  gamma_t * (du2_dx2 * du3_dx2)               &
     &                   +  gamma_p * (du2_dx3 * du3_dx3)
            d_SGS(inod,6) = gamma_r * (du3_dx1 * du3_dx1)               &
     &                   +  gamma_t * (du3_dx2 * du3_dx2)               &
     &                   +  gamma_p * (du3_dx3 * du3_dx3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_SGS_m_flux_nl_gradient_pout
!
!  ---------------------------------------------------------------------
!
      end module sph_SGS_nl_gradient_pout
