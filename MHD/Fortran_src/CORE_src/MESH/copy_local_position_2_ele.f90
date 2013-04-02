!copy_local_position_2_ele.f90
!     module copy_local_position_2_ele
!
!     Written by H. Matsui on May, 2010
!
!      subroutine copy_surf_local_posi_2_element(isf_ele, irot_sf,      &
!     &          xi_surf, xi_ele)
!      subroutine copy_edge_local_posi_2_element(iedge_ele, iflag_dir,  &
!     &          xi_edge, xi_ele)
!
!      subroutine set_interpolate_flag_by_xi(xi_ele, iflag_itp)
!
      module copy_local_position_2_ele
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_surf_local_posi_2_element(isf_ele, irot_sf,       &
     &          xi_surf, xi_ele)
!
      integer(kind = kint), intent(in) :: isf_ele, irot_sf
      real(kind = kreal), intent(in) :: xi_surf(2)
      real(kind = kreal), intent(inout) :: xi_ele(3)
      real(kind = kreal) :: xsi(2)
!
!
      if     (irot_sf .eq. 1) then
        xsi(1) =  xi_surf(2)
        xsi(2) =  xi_surf(1)
      else if(irot_sf .eq. 2) then
        xsi(1) = -xi_surf(1)
        xsi(2) =  xi_surf(2)
      else if(irot_sf .eq. 3) then
        xsi(1) = -xi_surf(2)
        xsi(2) = -xi_surf(1)
      else if(irot_sf .eq. 4) then
        xsi(1) =  xi_surf(1)
        xsi(2) = -xi_surf(2)
      else
        xsi(1) =  xi_surf(1)
        xsi(2) =  xi_surf(2)
      end if
!
      if     (isf_ele .eq. 1) then
        xi_ele(1) = -one
        xi_ele(2) = xsi(2)
        xi_ele(3) = xsi(1)
      else if(isf_ele .eq. 2) then
        xi_ele(1) =  one
        xi_ele(2) = xsi(1)
        xi_ele(3) = xsi(2)
      else if(isf_ele .eq. 3) then
        xi_ele(2) = -one
        xi_ele(3) = xsi(2)
        xi_ele(1) = xsi(1)
      else if(isf_ele .eq. 4) then
        xi_ele(2) =  one
        xi_ele(3) = xsi(1)
        xi_ele(1) = xsi(2)
      else if(isf_ele .eq. 5) then
        xi_ele(3) = -one
        xi_ele(1) = xsi(2)
        xi_ele(2) = xsi(1)
      else if(isf_ele .eq. 6) then
        xi_ele(3) =  one
        xi_ele(1) = xsi(1)
        xi_ele(2) = xsi(2)
      end if
!
      end subroutine copy_surf_local_posi_2_element
!
! ----------------------------------------------------------------------
!
      subroutine copy_edge_local_posi_2_element(iedge_ele, iflag_dir,   &
     &          xi_edge, xi_ele)
!
      integer(kind = kint), intent(in) :: iedge_ele, iflag_dir
      real(kind = kreal), intent(in) :: xi_edge
      real(kind = kreal), intent(inout) :: xi_ele(3)
      real(kind = kreal) :: xse
!
!
      if     (iflag_dir .eq. 1) then
        xse =  xi_edge
      else if(iflag_dir .eq. -1) then
        xse = -xi_edge
      end if
!
      if     (iedge_ele .eq.  1) then
        xi_ele(1) =  xse
        xi_ele(2) = -one
        xi_ele(3) = -one
      else if(iedge_ele .eq.  2) then
        xi_ele(1) =  one
        xi_ele(2) =  xse
        xi_ele(3) = -one
      else if(iedge_ele .eq.  3) then
        xi_ele(1) =  xse
        xi_ele(2) =  one
        xi_ele(3) = -one
      else if(iedge_ele .eq.  4) then
        xi_ele(1) = -one
        xi_ele(2) =  xse
        xi_ele(3) = -one
      else if(iedge_ele .eq.  5) then
        xi_ele(1) =  xse
        xi_ele(2) = -one
        xi_ele(3) =  one
      else if(iedge_ele .eq.  6) then
        xi_ele(1) =  one
        xi_ele(2) =  xse
        xi_ele(3) =  one
      else if(iedge_ele .eq.  7) then
        xi_ele(1) =  xse
        xi_ele(2) =  one
        xi_ele(3) =  one
      else if(iedge_ele .eq.  8) then
        xi_ele(1) = -one
        xi_ele(2) =  xse
        xi_ele(3) =  one
      else if(iedge_ele .eq.  9) then
        xi_ele(1) = -one
        xi_ele(2) = -one
        xi_ele(3) =  xse
      else if(iedge_ele .eq. 10) then
        xi_ele(1) =  one
        xi_ele(2) = -one
        xi_ele(3) =  xse
      else if(iedge_ele .eq. 11) then
        xi_ele(1) =  one
        xi_ele(2) =  one
        xi_ele(3) =  xse
      else if(iedge_ele .eq. 12) then
        xi_ele(1) = -one
        xi_ele(2) =  one
        xi_ele(3) =  xse
      end if
!
      end subroutine copy_edge_local_posi_2_element
!
! ----------------------------------------------------------------------
!
      subroutine copy_node_local_posi_2_element(k1, xi_ele)
!
      integer(kind = kint), intent(in) :: k1
      real(kind = kreal), intent(inout) :: xi_ele(3)
!
!
      if     (k1 .eq.  1) then
        xi_ele(1) = -one
        xi_ele(2) = -one
        xi_ele(3) = -one
      else if(k1 .eq.  2) then
        xi_ele(1) =  one
        xi_ele(2) = -one
        xi_ele(3) = -one
      else if(k1 .eq.  3) then
        xi_ele(1) =  one
        xi_ele(2) =  one
        xi_ele(3) = -one
      else if(k1 .eq.  4) then
        xi_ele(1) = -one
        xi_ele(2) =  one
        xi_ele(3) = -one
      else if(k1 .eq.  5) then
        xi_ele(1) = -one
        xi_ele(2) = -one
        xi_ele(3) =  one
      else if(k1 .eq.  6) then
        xi_ele(1) =  one
        xi_ele(2) = -one
        xi_ele(3) =  one
      else if(k1 .eq.  7) then
        xi_ele(1) =  one
        xi_ele(2) =  one
        xi_ele(3) =  one
      else if(k1 .eq.  8) then
        xi_ele(1) = -one
        xi_ele(2) =  one
        xi_ele(3) =  one
      end if
!
      end subroutine copy_node_local_posi_2_element
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_flag_by_xi(xi_ele, iflag_itp)
!
      real(kind = kreal), intent(in) :: xi_ele(3)
      integer(kind = kint), intent(inout) :: iflag_itp
!
!
      if     (xi_ele(1) .eq. -one  .and.  xi_ele(2) .eq. -one           &
     &  .and. xi_ele(3) .eq. -one) then
        iflag_itp = 1
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(2) .eq. -one           &
     &  .and. xi_ele(3) .eq. -one) then
        iflag_itp = 2
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(2) .eq.  one           &
     &  .and. xi_ele(3) .eq. -one) then
        iflag_itp = 3
      else if(xi_ele(1) .eq. -one  .and.  xi_ele(2) .eq.  one           &
     &  .and. xi_ele(3) .eq. -one) then
        iflag_itp = 4
      else if(xi_ele(1) .eq. -one  .and.  xi_ele(2) .eq. -one           &
     &  .and. xi_ele(3) .eq.  one) then
        iflag_itp = 5
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(2) .eq. -one           &
     &  .and. xi_ele(3) .eq.  one) then
        iflag_itp = 6
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(2) .eq.  one           &
     &  .and. xi_ele(3) .eq.  one) then
        iflag_itp = 7
      else if(xi_ele(1) .eq. -one  .and.  xi_ele(2) .eq.  one           &
     &  .and. xi_ele(3) .eq.  one) then
        iflag_itp = 8
!
      else if(xi_ele(2) .eq. -one  .and.  xi_ele(3) .eq. -one) then
        iflag_itp = 101
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(3) .eq. -one) then
        iflag_itp = 102
      else if(xi_ele(2) .eq.  one  .and.  xi_ele(3) .eq. -one) then
        iflag_itp = 103
      else if(xi_ele(1) .eq. -one  .and.  xi_ele(3) .eq. -one) then
        iflag_itp = 104
      else if(xi_ele(2) .eq. -one  .and.  xi_ele(3) .eq.  one) then
        iflag_itp = 105
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(3) .eq.  one) then
        iflag_itp = 106
      else if(xi_ele(2) .eq.  one  .and.  xi_ele(3) .eq.  one) then
        iflag_itp = 107
      else if(xi_ele(1) .eq. -one  .and.  xi_ele(3) .eq.  one) then
        iflag_itp = 108
      else if(xi_ele(1) .eq. -one  .and.  xi_ele(2) .eq. -one) then
        iflag_itp = 109
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(2) .eq. -one) then
        iflag_itp = 110
      else if(xi_ele(1) .eq.  one  .and.  xi_ele(2) .eq.  one) then
        iflag_itp = 111
      else if(xi_ele(1) .eq. -one  .and.  xi_ele(2) .eq.  one) then
        iflag_itp = 112
!
      else if(xi_ele(1) .eq. -one) then
        iflag_itp = 201
      else if(xi_ele(1) .eq.  one) then
        iflag_itp = 202
      else if(xi_ele(2) .eq. -one) then
        iflag_itp = 203
      else if(xi_ele(2) .eq. -one) then
        iflag_itp = 204
      else if(xi_ele(3) .eq. -one) then
        iflag_itp = 205
      else if(xi_ele(3) .eq.  one) then
        iflag_itp = 206
!
      else
        iflag_itp = 0
      end if
!
      end subroutine set_interpolate_flag_by_xi
!
! ----------------------------------------------------------------------
!
      end module copy_local_position_2_ele
