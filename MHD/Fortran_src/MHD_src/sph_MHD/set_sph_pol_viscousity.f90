!>@file   set_sph_pol_viscousity.f90
!!@brief  module set_sph_pol_viscousity
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Substitute viscousity matrix in each layer
!!
!!@verbatim
!!      subroutine add_exp2_sph_pol_viscous(kr, nnod_rj, jmax,          &
!!     &          mat1_grad_p, mat2_viscous, d_vpol, press_e,           &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p( 0:1)
!!        real(kind = kreal), intent(in) :: mat2_viscous(jmax,-1:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine add_exp4_sph_pol_viscous(kr, nnod_rj, jmax,          &
!!     &          mat3_grad_p, mat4_viscous, d_vpol, press_e,           &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p(-1:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous(jmax,-2:2)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!@endverbatim
!!
      module set_sph_pol_viscousity
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine add_exp2_sph_pol_viscous(kr, nnod_rj, jmax,            &
     &          mat1_grad_p, mat2_viscous, d_vpol, press_e,             &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p( 0:1)
      real(kind = kreal), intent(in) :: mat2_viscous(jmax,-1:1)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, inod
!
!
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                        + mat2_viscous(j,-1) * d_vpol(i_n1)       &
     &                        - mat1_grad_p( 0) *  press_e(inod)        &
     &                        + mat2_viscous(j, 0) * d_vpol(inod)       &
     &                        - mat1_grad_p( 1) *  press_e(i_p1)        &
     &                        + mat2_viscous(j, 1) * d_vpol(i_p1)
      end do
!
      end subroutine add_exp2_sph_pol_viscous
!
!  -------------------------------------------------------------------
!
      subroutine add_exp4_sph_pol_viscous(kr, nnod_rj, jmax,            &
     &          mat3_grad_p, mat4_viscous, d_vpol, press_e,             &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p(-1:2)
      real(kind = kreal), intent(in) :: mat4_viscous(jmax,-2:2)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, i_n2, i_p2, inod
!
!
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                        + mat4_viscous(j,-2) * d_vpol(i_n2)       &
     &                        - mat3_grad_p(-1) *  press_e(i_n1)        &
     &                        + mat4_viscous(j,-1) * d_vpol(i_n1)       &
     &                        - mat3_grad_p( 0) *  press_e(inod)        &
     &                        + mat4_viscous(j, 0) * d_vpol(inod)       &
     &                        - mat3_grad_p( 1) *  press_e(i_p1)        &
     &                        + mat4_viscous(j, 1) * d_vpol(i_p1)       &
     &                        - mat3_grad_p( 2) *  press_e(i_p2)        &
     &                        + mat4_viscous(j, 2) * d_vpol(i_p2)
      end do
!
      end subroutine add_exp4_sph_pol_viscous
!
!  -------------------------------------------------------------------
!
      end module set_sph_pol_viscousity
