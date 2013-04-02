!set_vol_xi_2_surf_xi.f90
!     module set_vol_xi_2_surf_xi
!
!> @brief  convert position in element to surface
!
!      written by H. Matsui
!
!      subroutine set_volume_xi_to_surface_xi(isf, xi_s, xi_v)
!
      module set_vol_xi_2_surf_xi
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_volume_xi_to_surface_xi(isf, xi_s, xi_v)
!
      integer(kind = kint), intent(in) :: isf
      real(kind = kreal), intent(in) :: xi_s(2)
      real(kind = kreal), intent(inout) :: xi_v(3)
!
!
      if     (isf .eq. 1) then
        xi_v(1) = -one
        xi_v(2) = xi_s(2)
        xi_v(3) = xi_s(1)
      else if(isf .eq. 2) then
        xi_v(1) = one
        xi_v(2) = xi_s(1)
        xi_v(3) = xi_s(2)
      else if(isf .eq. 3) then
        xi_v(1) = xi_s(1)
        xi_v(2) = -one
        xi_v(3) = xi_s(2)
      else if(isf .eq. 4) then
        xi_v(1) = xi_s(2)
        xi_v(2) = one
        xi_v(3) = xi_s(1)
      else if(isf .eq. 5) then
        xi_v(1) = xi_s(2)
        xi_v(2) = xi_s(1)
        xi_v(3) = -one
      else if(isf .eq. 6) then
        xi_v(1) = xi_s(1)
        xi_v(2) = xi_s(2)
        xi_v(3) = one
      end if
!
      end subroutine set_volume_xi_to_surface_xi
!
! ------------------------------------------------------
!
      end module set_vol_xi_2_surf_xi
