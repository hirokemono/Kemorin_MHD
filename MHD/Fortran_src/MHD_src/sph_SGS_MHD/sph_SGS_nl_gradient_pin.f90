!!@file   sph_SGS_nl_gradient_pin.f90
!!@brief  module sph_SGS_nl_gradient_pin
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in MAy., 2018
!
!>@brief SGS terms by nonlinear gradient model in spherical coordinate
!!
!!@verbatim
!!      subroutine sph_SGS_induct_nl_gradient_pin                       &
!!     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t, gamma, coef,&
!!     &          u_rtp, grad_ux, grad_uy, grad_uz,                     &
!!     &          b_rtp, grad_bx, grad_by, grad_bz, d_SGS)
!!      subroutine sph_SGS_s_flux_nl_gradient_pin                       &
!!     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t, gamma, coef,&
!!     &          u_rtp, grad_ux, grad_uy, grad_uz, grad_s, d_SGS)
!!      subroutine sph_SGS_m_flux_nl_gradient_pin                       &
!!     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t, gamma, coef,&
!!     &          u_rtp, grad_ux, grad_uy, grad_uz, d_SGS)
!!@endverbatim
!
      module sph_SGS_nl_gradient_pin
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sph_SGS_induct_nl_gradient_pin                         &
     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t, gamma, coef,  &
     &          u_rtp, grad_ux, grad_uy, grad_uz,                       &
     &          b_rtp, grad_bx, grad_by, grad_bz, d_SGS)
!
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: a_r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cot_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: gamma(3)
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
!$omp  parallel private(kr,lt,gamma_r,gamma_t,gamma_p)
      do lt = 1, nidx_rtp(2)
        do kr = 1, nidx_rtp(1)
          gamma_r = coef * gamma(1)
          gamma_t = coef * gamma(2) * r(kr)
          gamma_p = coef * gamma(3) * r(kr) * sin_t(lt)
!$omp do private(du1_dx1,du1_dx2,du1_dx3,db1_dx1,db1_dx2,db1_dx3,       &
!&omp&           du2_dx1,du2_dx2,du2_dx3,db2_dx1,db2_dx2,db2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,db3_dx1,db3_dx2,db3_dx3,       &
!$omp&           mp,inod)
          do mp = 1, nidx_rtp(3)
            inod = mp + (kr-1)*nidx_rtp(3)                              &
     &                 + (lt-1)*nidx_rtp(1)*nidx_rtp(3)
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
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine sph_SGS_induct_nl_gradient_pin
!
!  ---------------------------------------------------------------------
!
      subroutine sph_SGS_s_flux_nl_gradient_pin                         &
     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t, gamma, coef,  &
     &          u_rtp, grad_ux, grad_uy, grad_uz, grad_s, d_SGS)
!
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: a_r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cot_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: gamma(3)
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
!$omp  parallel private(kr,lt,gamma_r,gamma_t,gamma_p)
      do lt = 1, nidx_rtp(2)
        do kr = 1, nidx_rtp(1)
          gamma_r = coef * gamma(1)
          gamma_t = coef * gamma(2) * r(kr)
          gamma_p = coef * gamma(3) * r(kr) * sin_t(lt)
!$omp do private(du1_dx1,du1_dx2,du1_dx3,du2_dx1,du2_dx2,du2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,ds_dx1, ds_dx2, ds_dx3,        &
!$omp&           mp,inod)
          do mp = 1, nidx_rtp(3)
            inod = mp + (kr-1)*nidx_rtp(3)                              &
     &                 + (lt-1)*nidx_rtp(1)*nidx_rtp(3)
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
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine sph_SGS_s_flux_nl_gradient_pin
!
!  ---------------------------------------------------------------------
!
      subroutine sph_SGS_m_flux_nl_gradient_pin                         &
     &         (nnod_rtp, nidx_rtp, r, a_r, sin_t, cot_t, gamma,coef,   &
     &          u_rtp, grad_ux, grad_uy, grad_uz, d_SGS)
!
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: a_r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cot_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: gamma(3)
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
!$omp  parallel private(kr,lt,gamma_r,gamma_t,gamma_p)
      do lt = 1, nidx_rtp(2)
        do kr = 1, nidx_rtp(1)
          gamma_r = coef * gamma(1)
          gamma_t = coef * gamma(2) * r(kr)
          gamma_p = coef * gamma(3) * r(kr) * sin_t(lt)
!$omp do private(du1_dx1,du1_dx2,du1_dx3,du2_dx1,du2_dx2,du2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,mp,inod)
          do mp = 1, nidx_rtp(3)
            inod = mp + (kr-1)*nidx_rtp(3)                              &
     &                 + (lt-1)*nidx_rtp(1)*nidx_rtp(3)
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
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine sph_SGS_m_flux_nl_gradient_pin
!
!  ---------------------------------------------------------------------
!
      end module sph_SGS_nl_gradient_pin
