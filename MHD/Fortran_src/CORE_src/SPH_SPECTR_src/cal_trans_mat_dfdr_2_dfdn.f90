!>@file   cal_trans_mat_dfdr_2_dfdn.f90
!!@brief  module cal_trans_mat_dfdr_2_dfdn
!!
!!@author H. Matsui
!!@date Programmed in May, 2013
!
!>@brief Set transfer matrix from d^{(n)}f / dr^{n}
!!                           to d^{(n)}f / dN^{n}
!!@verbatim
!!      subroutine set_fdm2_dfdn_matrix(drdn, d2rdn2, fdm2_mat_dr_to_dn)
!!      subroutine set_fdm3_dfdn_matrix(drdn, d2rdn2, d3rdn3,           &
!!     &          fdm3_mat_dr_to_dn)
!!      subroutine set_fdm4_dfdn_matrix(drdn, d2rdn2, d3rdn3, d4rdn4,   &
!!     &          fdm4_mat_dr_to_dn)
!!@endverbatim
!
      module cal_trans_mat_dfdr_2_dfdn
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
      subroutine set_fdm2_dfdn_matrix(drdn, d2rdn2, fdm2_mat_dr_to_dn)
!
      real(kind = kreal), intent(in) :: drdn
      real(kind = kreal), intent(in) :: d2rdn2
      real(kind = kreal), intent(inout) :: fdm2_mat_dr_to_dn(3,3)
!
!
      fdm2_mat_dr_to_dn(1,1) = one
      fdm2_mat_dr_to_dn(1,2) = zero
      fdm2_mat_dr_to_dn(1,3) = zero
!
      fdm2_mat_dr_to_dn(2,1) = zero
      fdm2_mat_dr_to_dn(2,2) = drdn
      fdm2_mat_dr_to_dn(2,3) = zero
!
      fdm2_mat_dr_to_dn(3,1) = zero
      fdm2_mat_dr_to_dn(3,2) = d2rdn2
      fdm2_mat_dr_to_dn(3,3) = drdn**2
!
      end subroutine set_fdm2_dfdn_matrix
!
!  -------------------------------------------------------------------
!
      subroutine set_fdm3_dfdn_matrix(drdn, d2rdn2, d3rdn3,             &
     &          fdm3_mat_dr_to_dn)
!
      real(kind = kreal), intent(in) :: drdn
      real(kind = kreal), intent(in) :: d2rdn2
      real(kind = kreal), intent(in) :: d3rdn3
      real(kind = kreal), intent(inout) :: fdm3_mat_dr_to_dn(4,4)
!
!
      fdm3_mat_dr_to_dn(1,1) = one
      fdm3_mat_dr_to_dn(1,2) = zero
      fdm3_mat_dr_to_dn(1,3) = zero
      fdm3_mat_dr_to_dn(1,4) = zero
!
      fdm3_mat_dr_to_dn(2,1) = zero
      fdm3_mat_dr_to_dn(2,2) = drdn
      fdm3_mat_dr_to_dn(2,3) = zero
      fdm3_mat_dr_to_dn(2,4) = zero
!
      fdm3_mat_dr_to_dn(3,1) = zero
      fdm3_mat_dr_to_dn(3,2) = d2rdn2
      fdm3_mat_dr_to_dn(3,3) = drdn**2
      fdm3_mat_dr_to_dn(3,4) = zero
!
      fdm3_mat_dr_to_dn(4,1) = zero
      fdm3_mat_dr_to_dn(4,2) = d3rdn3
      fdm3_mat_dr_to_dn(4,3) = three * d2rdn2 * drdn
      fdm3_mat_dr_to_dn(4,4) = drdn**3
!
      end subroutine set_fdm3_dfdn_matrix
!
!  -------------------------------------------------------------------
!
      subroutine set_fdm4_dfdn_matrix(drdn, d2rdn2, d3rdn3, d4rdn4,     &
     &          fdm4_mat_dr_to_dn)
!
      real(kind = kreal), intent(in) :: drdn
      real(kind = kreal), intent(in) :: d2rdn2
      real(kind = kreal), intent(in) :: d3rdn3
      real(kind = kreal), intent(in) :: d4rdn4
      real(kind = kreal), intent(inout) :: fdm4_mat_dr_to_dn(5,5)
!
!
      fdm4_mat_dr_to_dn(1,1) = one
      fdm4_mat_dr_to_dn(1,2) = zero
      fdm4_mat_dr_to_dn(1,3) = zero
      fdm4_mat_dr_to_dn(1,4) = zero
      fdm4_mat_dr_to_dn(1,5) = zero
!
      fdm4_mat_dr_to_dn(2,1) = zero
      fdm4_mat_dr_to_dn(2,2) = drdn
      fdm4_mat_dr_to_dn(2,3) = zero
      fdm4_mat_dr_to_dn(2,4) = zero
      fdm4_mat_dr_to_dn(2,5) = zero
!
      fdm4_mat_dr_to_dn(3,1) = zero
      fdm4_mat_dr_to_dn(3,2) = d2rdn2
      fdm4_mat_dr_to_dn(3,3) = drdn**2
      fdm4_mat_dr_to_dn(3,4) = zero
      fdm4_mat_dr_to_dn(3,5) = zero
!
      fdm4_mat_dr_to_dn(4,1) = zero
      fdm4_mat_dr_to_dn(4,2) = d3rdn3
      fdm4_mat_dr_to_dn(4,3) = three * d2rdn2 * drdn
      fdm4_mat_dr_to_dn(4,4) = drdn**3
      fdm4_mat_dr_to_dn(4,5) = zero
!
      fdm4_mat_dr_to_dn(5,1) = zero
      fdm4_mat_dr_to_dn(5,2) = d4rdn4
      fdm4_mat_dr_to_dn(5,3) = four * d3rdn3 * drdn
      fdm4_mat_dr_to_dn(5,4) = six * d2rdn2 * drdn**2
      fdm4_mat_dr_to_dn(5,5) = drdn**4
!
      end subroutine set_fdm4_dfdn_matrix
!
!  -------------------------------------------------------------------
!
      end module cal_trans_mat_dfdr_2_dfdn
