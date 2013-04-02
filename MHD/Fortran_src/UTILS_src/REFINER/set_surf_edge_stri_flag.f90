!set_surf_edge_stri_flag.f90
!      module set_surf_edge_stri_flag
!
      module set_surf_edge_stri_flag
!
!      Writen by H. Matsui on Oct., 2007
!
      use m_precision
!
      use m_refine_flag_parameters
!
      implicit none
!
!      subroutine surf_edge_refine_flag_tri_1d(iflag_refine,            &
!     &          iflag_refine_sf_l, iflag_refine_ed_l)
!      subroutine surf_edge_refine_flag_stri_surf(iflag_refine,         &
!     &          iflag_refine_sf_l, iflag_refine_ed_l)
!      subroutine surf_edge_refine_flag_stri_edge(iflag_refine,         &
!     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_tri_1d(iflag_refine,             &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      integer(kind = kint), intent(in) :: iflag_refine
!
      integer(kind = kint), intent(inout) :: iflag_refine_sf_l(6)
      integer(kind = kint), intent(inout) :: iflag_refine_ed_l(12)
!
!
      iflag_refine_ed_l(1:12) = iflag_nothing_ed
!
      if      (iflag_refine .eq. iflag_tri_x) then
        iflag_refine_sf_l(1) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(2) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(3) = iflag_tri_y_sf
        iflag_refine_sf_l(4) = iflag_tri_x_sf
        iflag_refine_sf_l(5) = iflag_tri_x_sf
        iflag_refine_sf_l(6) = iflag_tri_y_sf
!
        iflag_refine_ed_l(2) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(4) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(6) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(8) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(9) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(10) = iflag_tri_full_ed_eq
        iflag_refine_ed_l(11) = iflag_tri_full_ed_eq
        iflag_refine_ed_l(12) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_tri_y) then
        iflag_refine_sf_l(1) = iflag_tri_x_sf
        iflag_refine_sf_l(2) = iflag_tri_y_sf
        iflag_refine_sf_l(3) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(4) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(5) = iflag_tri_y_sf
        iflag_refine_sf_l(6) = iflag_tri_x_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(3) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(5) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(7) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(9) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(10) = iflag_tri_full_ed_eq
        iflag_refine_ed_l(11) = iflag_tri_full_ed_eq
        iflag_refine_ed_l(12) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_tri_z) then
        iflag_refine_sf_l(1) = iflag_tri_y_sf
        iflag_refine_sf_l(2) = iflag_tri_x_sf
        iflag_refine_sf_l(3) = iflag_tri_x_sf
        iflag_refine_sf_l(4) = iflag_tri_y_sf
        iflag_refine_sf_l(5) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(6) = iflag_tri_full_sf_eq
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(2) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(3) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(4) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(5) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(6) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(7) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(8) =  iflag_tri_full_ed_eq
      end if
!
      end subroutine surf_edge_refine_flag_tri_1d
!
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_stri_surf(iflag_refine,          &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      integer(kind = kint), intent(in) :: iflag_refine
!
      integer(kind = kint), intent(inout) :: iflag_refine_sf_l(6)
      integer(kind = kint), intent(inout) :: iflag_refine_ed_l(12)
!
      iflag_refine_sf_l(1:6) =  iflag_nothing_sf
      iflag_refine_ed_l(1:12) = iflag_nothing_ed
!
!
      if      (iflag_refine .eq. iflag_tri_xs1) then
        iflag_refine_sf_l(1) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(3) = iflag_tri_ye4_sf
        iflag_refine_sf_l(4) = iflag_tri_xe1_sf
        iflag_refine_sf_l(5) = iflag_tri_xe1_sf
        iflag_refine_sf_l(6) = iflag_tri_ye4_sf
!
        iflag_refine_ed_l(4) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(8) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(9) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(12) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_tri_xs2) then
        iflag_refine_sf_l(2) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(3) = iflag_tri_ye2_sf
        iflag_refine_sf_l(4) = iflag_tri_xe3_sf
        iflag_refine_sf_l(5) = iflag_tri_xe3_sf
        iflag_refine_sf_l(6) = iflag_tri_ye2_sf
!
        iflag_refine_ed_l(2) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(6) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(10) = iflag_tri_full_ed_eq
        iflag_refine_ed_l(11) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_tri_ys3) then
        iflag_refine_sf_l(1) = iflag_tri_xe1_sf
        iflag_refine_sf_l(2) = iflag_tri_ye4_sf
        iflag_refine_sf_l(3) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(5) = iflag_tri_ye4_sf
        iflag_refine_sf_l(6) = iflag_tri_xe1_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(5) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(9) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(10) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_tri_ys4) then
        iflag_refine_sf_l(1) = iflag_tri_xe3_sf
        iflag_refine_sf_l(2) = iflag_tri_ye2_sf
        iflag_refine_sf_l(4) = iflag_tri_full_sf_eq
        iflag_refine_sf_l(5) = iflag_tri_ye2_sf
        iflag_refine_sf_l(6) = iflag_tri_xe3_sf
!
        iflag_refine_ed_l(3) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(7) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(11) = iflag_tri_full_ed_eq
        iflag_refine_ed_l(12) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_tri_zs5) then
        iflag_refine_sf_l(1) = iflag_tri_ye4_sf
        iflag_refine_sf_l(2) = iflag_tri_xe1_sf
        iflag_refine_sf_l(3) = iflag_tri_xe1_sf
        iflag_refine_sf_l(4) = iflag_tri_ye4_sf
        iflag_refine_sf_l(5) = iflag_tri_full_sf_eq
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(2) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(3) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(4) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_tri_zs6) then
        iflag_refine_sf_l(1) = iflag_tri_ye2_sf
        iflag_refine_sf_l(2) = iflag_tri_xe3_sf
        iflag_refine_sf_l(3) = iflag_tri_xe3_sf
        iflag_refine_sf_l(4) = iflag_tri_ye2_sf
        iflag_refine_sf_l(6) = iflag_tri_full_sf_eq
!
        iflag_refine_ed_l(5) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(6) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(7) =  iflag_tri_full_ed_eq
        iflag_refine_ed_l(8) =  iflag_tri_full_ed_eq
      end if
!
      end subroutine surf_edge_refine_flag_stri_surf
!
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_stri_edge(iflag_refine,          &
     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
      integer(kind = kint), intent(in) :: iflag_refine
!
      integer(kind = kint), intent(inout) :: iflag_refine_sf_l(6)
      integer(kind = kint), intent(inout) :: iflag_refine_ed_l(12)
!
!
      iflag_refine_sf_l(1:6) =  iflag_nothing_sf
      iflag_refine_ed_l(1:12) = iflag_nothing_ed
!
      if      (iflag_refine .eq. iflag_stri_e1) then
        iflag_refine_sf_l(3) = iflag_tri_xe1_sf
        iflag_refine_sf_l(5) = iflag_tri_ye4_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e2) then
        iflag_refine_sf_l(2) = iflag_tri_xe1_sf
        iflag_refine_sf_l(5) = iflag_tri_xe3_sf
!
        iflag_refine_ed_l(2) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e3) then
        iflag_refine_sf_l(4) = iflag_tri_ye4_sf
        iflag_refine_sf_l(5) = iflag_tri_ye2_sf
!
        iflag_refine_ed_l(3) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e4) then
        iflag_refine_sf_l(1) = iflag_tri_ye4_sf
        iflag_refine_sf_l(5) = iflag_tri_xe1_sf
!
        iflag_refine_ed_l(4) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e5) then
        iflag_refine_sf_l(3) = iflag_tri_xe3_sf
        iflag_refine_sf_l(6) = iflag_tri_xe1_sf
!
        iflag_refine_ed_l(5) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e6) then
        iflag_refine_sf_l(2) = iflag_tri_xe3_sf
        iflag_refine_sf_l(6) = iflag_tri_ye2_sf
!
        iflag_refine_ed_l(6) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e7) then
        iflag_refine_sf_l(4) = iflag_tri_ye2_sf
        iflag_refine_sf_l(6) = iflag_tri_xe3_sf
!
        iflag_refine_ed_l(7) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e8) then
        iflag_refine_sf_l(1) = iflag_tri_ye2_sf
        iflag_refine_sf_l(6) = iflag_tri_ye4_sf
!
        iflag_refine_ed_l(8) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e9) then
        iflag_refine_sf_l(1) = iflag_tri_xe1_sf
        iflag_refine_sf_l(3) = iflag_tri_ye4_sf
!
        iflag_refine_ed_l(9) =  iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e10) then
        iflag_refine_sf_l(2) = iflag_tri_ye4_sf
        iflag_refine_sf_l(3) = iflag_tri_ye2_sf
!
        iflag_refine_ed_l(10) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e11) then
        iflag_refine_sf_l(2) = iflag_tri_ye2_sf
        iflag_refine_sf_l(4) = iflag_tri_xe3_sf
!
        iflag_refine_ed_l(11) = iflag_tri_full_ed_eq
!
      else if (iflag_refine .eq. iflag_stri_e12) then
        iflag_refine_sf_l(1) = iflag_tri_xe3_sf
        iflag_refine_sf_l(4) = iflag_tri_xe1_sf
!
        iflag_refine_ed_l(12) = iflag_tri_full_ed_eq
      end if
!
      end subroutine surf_edge_refine_flag_stri_edge
!
!  ---------------------------------------------------------------------
!
      end module set_surf_edge_stri_flag
