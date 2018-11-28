!!@file   sph_SGS_nl_gradient_pin.f90
!!@brief  module sph_SGS_nl_gradient_pin
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in MAy., 2018
!
!>@brief SGS terms by nonlinear gradient model in spherical coordinate
!!
!!@verbatim
!!      subroutine sph_SGS_induct_nl_gradient_pin(kr_in, kr_out,        &
!!     &          nnod_rtp, nidx_rtp, r, sin_t, cos_t, coef,            &
!!     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment,  &
!!     &          u_rtp, grad_ux, grad_uy, grad_uz,                     &
!!     &          b_rtp, grad_bx, grad_by, grad_bz, d_SGS)
!!      subroutine sph_SGS_s_flux_nl_gradient_pin(kr_in, kr_out,        &
!!     &          nnod_rtp, nidx_rtp, r, sin_t, cos_t, coef,            &
!!     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment,  &
!!     &          u_rtp, grad_ux, grad_uy, grad_uz, grad_s, d_SGS)
!!      subroutine sph_SGS_m_flux_nl_gradient_pin(kr_in, kr_out,        &
!!     &          nnod_rtp, nidx_rtp, r, sin_t, cos_t, coef,            &
!!     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment,  &
!!     &          u_rtp, grad_ux, grad_uy, grad_uz, d_SGS)
!!@endverbatim
!
      module sph_SGS_nl_gradient_pin
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sph_SGS_induct_nl_gradient_pin(kr_in, kr_out,          &
     &          nnod_rtp, nidx_rtp, r, sin_t, cos_t, coef,              &
     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment,    &
     &          u_rtp, grad_ux, grad_uy, grad_uz,                       &
     &          b_rtp, grad_bx, grad_by, grad_bz, d_SGS)
!
      integer(kind = kint), intent(in)  :: kr_in, kr_out
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cos_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(in) :: radial_2nd_moment(nidx_rtp(1))
      real(kind = kreal), intent(in) :: theta_2nd_moment(nidx_rtp(2))
      real(kind = kreal), intent(in) :: phi_2nd_moment
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
!$omp parallel workshare
      d_SGS(1:nnod_rtp,1:3) = zero
!$omp end parallel workshare
!
!$omp parallel private(kr,lt,gamma_r,gamma_t,gamma_p)
      do lt = 1, nidx_rtp(2)
        do kr = kr_in, kr_out
          gamma_r = coef * radial_2nd_moment(kr)
          gamma_t = coef * theta_2nd_moment(lt)
          gamma_p = coef * phi_2nd_moment
!
!$omp do private(du1_dx1,du1_dx2,du1_dx3,db1_dx1,db1_dx2,db1_dx3,       &
!&omp&           du2_dx1,du2_dx2,du2_dx3,db2_dx1,db2_dx2,db2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,db3_dx1,db3_dx2,db3_dx3,       &
!$omp&           mp,inod)
          do mp = 1, nidx_rtp(3)
            inod = mp + (kr-1)*nidx_rtp(3)                              &
     &                 + (lt-1)*nidx_rtp(1)*nidx_rtp(3)
!
            du1_dx1 = grad_ux(inod,1)
            du1_dx2 = grad_ux(inod,2) * r(kr) - u_rtp(inod,2)
            du1_dx3 = grad_ux(inod,3) * sin_t(lt) * r(kr)               &
     &                - u_rtp(inod,3) * sin_t(lt)
            du2_dx1 = grad_uy(inod,1)
            du2_dx2 = grad_uy(inod,2) * r(kr) + u_rtp(inod,1)
            du2_dx3 = grad_uy(inod,3) * sin_t(lt) * r(kr)               &
     &                - u_rtp(inod,3) * cos_t(lt)
            du3_dx1 = grad_uz(inod,1)
            du3_dx2 = grad_uz(inod,2) * r(kr)
            du3_dx3 = grad_uz(inod,3) * sin_t(lt) * r(kr)               &
     &                + u_rtp(inod,2) * cos_t(lt)                       &
     &                - u_rtp(inod,1) * sin_t(lt)
!
            db1_dx1 = grad_bx(inod,1)
            db1_dx2 = grad_bx(inod,2) * r(kr) - b_rtp(inod,2)
            db1_dx3 = grad_bx(inod,3) * sin_t(lt) * r(kr)               &
     &                - b_rtp(inod,3) * sin_t(lt)
            db2_dx1 = grad_by(inod,1)
            db2_dx2 = grad_by(inod,2) * r(kr) + b_rtp(inod,1)
            db2_dx3 = grad_by(inod,3) * sin_t(lt) * r(kr)               &
     &                - b_rtp(inod,3) * cos_t(lt)
            db3_dx1 = grad_bz(inod,1)
            db3_dx2 = grad_bz(inod,2) * r(kr)
            db3_dx3 = grad_bz(inod,3) * sin_t(lt) * r(kr)               &
     &                + b_rtp(inod,2) * cos_t(lt)                       &
     &                - b_rtp(inod,1) * sin_t(lt)
!
            d_SGS(inod,1)                                               &
     &          =  gamma_r * (du2_dx1 * db3_dx1 - du3_dx1 * db2_dx1)    &
     &           + gamma_t * (du2_dx2 * db3_dx2 - du3_dx2 * db2_dx2)    &
     &           + gamma_p * (du2_dx3 * db3_dx3 - du3_dx3 * db2_dx3)
            d_SGS(inod,2)                                               &
     &          =  gamma_r * (du3_dx1 * db1_dx1 - du1_dx1 * db3_dx1)    &
     &           + gamma_t * (du3_dx2 * db1_dx2 - du1_dx2 * db3_dx2)    &
     &           + gamma_p * (du3_dx3 * db1_dx3 - du1_dx3 * db3_dx3)
            d_SGS(inod,3)                                               &
     &          =  gamma_r * (du1_dx1 * db2_dx1 - du2_dx1 * db1_dx1)    &
     &           + gamma_t * (du1_dx2 * db2_dx2 - du2_dx2 * db1_dx2)    &
     &           + gamma_p * (du1_dx3 * db2_dx3 - du2_dx3 * db1_dx3)
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
      subroutine sph_SGS_s_flux_nl_gradient_pin(kr_in, kr_out,          &
     &          nnod_rtp, nidx_rtp, r, sin_t, cos_t, coef,              &
     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment,    &
     &          u_rtp, grad_ux, grad_uy, grad_uz, grad_s, d_SGS)
!
      integer(kind = kint), intent(in)  :: kr_in, kr_out
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cos_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(in) :: radial_2nd_moment(nidx_rtp(1))
      real(kind = kreal), intent(in) :: theta_2nd_moment(nidx_rtp(2))
      real(kind = kreal), intent(in) :: phi_2nd_moment
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
      real(kind = kreal) :: gamma_r, gamma_t, gamma_p
!
!
!$omp parallel workshare
      d_SGS(1:nnod_rtp,1:3) = zero
!$omp end parallel workshare
!
!$omp  parallel private(kr,lt,gamma_r,gamma_t,gamma_p)
      do lt = 1, nidx_rtp(2)
        do kr = kr_in, kr_out
          gamma_r = coef * radial_2nd_moment(kr)
          gamma_t = coef * theta_2nd_moment(lt)
          gamma_p = coef * phi_2nd_moment
!
!$omp do private(du1_dx1,du1_dx2,du1_dx3,du2_dx1,du2_dx2,du2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3, mp,inod)
          do mp = 1, nidx_rtp(3)
            inod = mp + (kr-1)*nidx_rtp(3)                              &
     &                 + (lt-1)*nidx_rtp(1)*nidx_rtp(3)
!
            du1_dx1 = grad_ux(inod,1)
            du1_dx2 = grad_ux(inod,2) * r(kr) - u_rtp(inod,2)
            du1_dx3 = grad_ux(inod,3) * sin_t(lt) * r(kr)               &
     &                - u_rtp(inod,3) * sin_t(lt)
            du2_dx1 = grad_uy(inod,1)
            du2_dx2 = grad_uy(inod,2) * r(kr) + u_rtp(inod,1)
            du2_dx3 = grad_uy(inod,3) * sin_t(lt) * r(kr)               &
     &                - u_rtp(inod,3) * cos_t(lt)
            du3_dx1 = grad_uz(inod,1)
            du3_dx2 = grad_uz(inod,2) * r(kr)
            du3_dx3 = grad_uz(inod,3) * sin_t(lt) * r(kr)               &
     &                + u_rtp(inod,2) * cos_t(lt)                       &
     &                - u_rtp(inod,1) * sin_t(lt)
!
            d_SGS(inod,1) = gamma_r * (du1_dx1 * grad_s(inod,1))        &
     &                   +  gamma_t * (du1_dx2 * grad_s(inod,2))        &
     &                   +  gamma_p * (du1_dx3 * grad_s(inod,3))
            d_SGS(inod,2) = gamma_r * (du2_dx1 * grad_s(inod,1))        &
     &                   +  gamma_t * (du2_dx2 * grad_s(inod,2))        &
     &                   +  gamma_p * (du2_dx3 * grad_s(inod,3))
            d_SGS(inod,3) = gamma_r * (du3_dx1 * grad_s(inod,1))        &
     &                   +  gamma_t * (du3_dx2 * grad_s(inod,2))        &
     &                   +  gamma_p * (du3_dx3 * grad_s(inod,3))
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
      subroutine sph_SGS_m_flux_nl_gradient_pin(kr_in, kr_out,          &
     &          nnod_rtp, nidx_rtp, r, sin_t, cos_t, coef,              &
     &          radial_2nd_moment, theta_2nd_moment, phi_2nd_moment,    &
     &          u_rtp, grad_ux, grad_uy, grad_uz, d_SGS)
!
      integer(kind = kint), intent(in)  :: kr_in, kr_out
      integer(kind = kint), intent(in)  :: nnod_rtp
      integer(kind = kint), intent(in)  :: nidx_rtp(3)
      real(kind = kreal), intent(in) :: r(nidx_rtp(1))
      real(kind = kreal), intent(in) :: sin_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: cos_t(nidx_rtp(2))
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(in) :: radial_2nd_moment(nidx_rtp(1))
      real(kind = kreal), intent(in) :: theta_2nd_moment(nidx_rtp(2))
      real(kind = kreal), intent(in) :: phi_2nd_moment
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
!$omp parallel workshare
      d_SGS(1:nnod_rtp,1:6) = zero
!$omp end parallel workshare
!
!$omp  parallel private(kr,lt,gamma_r,gamma_t,gamma_p)
      do lt = 1, nidx_rtp(2)
        do kr = kr_in, kr_out
          gamma_r = coef * radial_2nd_moment(kr)
          gamma_t = coef * theta_2nd_moment(lt)
          gamma_p = coef * phi_2nd_moment
!
!$omp do private(du1_dx1,du1_dx2,du1_dx3,du2_dx1,du2_dx2,du2_dx3,       &
!$omp&           du3_dx1,du3_dx2,du3_dx3,mp,inod)
          do mp = 1, nidx_rtp(3)
            inod = mp + (kr-1)*nidx_rtp(3)                              &
     &                 + (lt-1)*nidx_rtp(1)*nidx_rtp(3)
!
            du1_dx1 = grad_ux(inod,1)
            du1_dx2 = grad_ux(inod,2) * r(kr) - u_rtp(inod,2)
            du1_dx3 = grad_ux(inod,3) * sin_t(lt) * r(kr)               &
     &                - u_rtp(inod,3) * sin_t(lt)
            du2_dx1 = grad_uy(inod,1)
            du2_dx2 = grad_uy(inod,2) * r(kr) + u_rtp(inod,1)
            du2_dx3 = grad_uy(inod,3) * sin_t(lt) * r(kr)               &
     &                - u_rtp(inod,3) * cos_t(lt)
            du3_dx1 = grad_uz(inod,1)
            du3_dx2 = grad_uz(inod,2) * r(kr)
            du3_dx3 = grad_uz(inod,3) * sin_t(lt) * r(kr)               &
     &                + u_rtp(inod,2) * cos_t(lt)                       &
     &                - u_rtp(inod,1) * sin_t(lt)
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
