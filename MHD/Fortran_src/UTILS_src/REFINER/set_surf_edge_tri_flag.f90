!set_surf_edge_tri_flag.f90
!      module set_surf_edge_tri_flag
!
      module set_surf_edge_tri_flag
!
!      Writen by H. Matsui on Oct., 2007
!
      use m_precision
!
      use m_refine_flag_parameters
!
      implicit none
!
!      subroutine surf_edge_refine_flag_tri_surf(iflag_refine,          &
!     &          iflag_refine_sf_l, iflag_refine_ed_l)
!      subroutine surf_edge_refine_flag_tri_edge(iflag_refine,          &
!     &          iflag_refine_sf_l, iflag_refine_ed_l)
!      subroutine surf_edge_refine_flag_tri_node(iflag_refine,          &
!     &          iflag_refine_sf_l, iflag_refine_ed_l)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_tri_surf(iflag_refine,           &
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
!
      if (iflag_refine .eq. iflag_tri_s1) then
        iflag_refine_sf_l(1) = iflag_tri_full_sf
        iflag_refine_sf_l(3) = iflag_tri_e4_sf
        iflag_refine_sf_l(4) = iflag_tri_e1_sf
        iflag_refine_sf_l(5) = iflag_tri_e1_sf
        iflag_refine_sf_l(6) = iflag_tri_e4_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n1_ed
        iflag_refine_ed_l(3) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_full_ed
        iflag_refine_ed_l(5) =  iflag_tri_n1_ed
        iflag_refine_ed_l(7) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_full_ed
        iflag_refine_ed_l(9) =  iflag_tri_full_ed
        iflag_refine_ed_l(12) = iflag_tri_full_ed
!
      else if (iflag_refine .eq. iflag_tri_s2) then
        iflag_refine_sf_l(2) = iflag_tri_full_sf
        iflag_refine_sf_l(3) = iflag_tri_e2_sf
        iflag_refine_sf_l(4) = iflag_tri_e3_sf
        iflag_refine_sf_l(5) = iflag_tri_e3_sf
        iflag_refine_sf_l(6) = iflag_tri_e2_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n2_ed
        iflag_refine_ed_l(2) =  iflag_tri_full_ed
        iflag_refine_ed_l(3) =  iflag_tri_n2_ed
        iflag_refine_ed_l(5) =  iflag_tri_n2_ed
        iflag_refine_ed_l(6) =  iflag_tri_full_ed
        iflag_refine_ed_l(7) =  iflag_tri_n2_ed
        iflag_refine_ed_l(10) = iflag_tri_full_ed
        iflag_refine_ed_l(11) = iflag_tri_full_ed
!
      else if (iflag_refine .eq. iflag_tri_s3) then
        iflag_refine_sf_l(1) = iflag_tri_e1_sf
        iflag_refine_sf_l(2) = iflag_tri_e4_sf
        iflag_refine_sf_l(3) = iflag_tri_full_sf
        iflag_refine_sf_l(5) = iflag_tri_e4_sf
        iflag_refine_sf_l(6) = iflag_tri_e1_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed
        iflag_refine_ed_l(2) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_n1_ed
        iflag_refine_ed_l(5) =  iflag_tri_full_ed
        iflag_refine_ed_l(6) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_n1_ed
        iflag_refine_ed_l(9) =  iflag_tri_full_ed
        iflag_refine_ed_l(10) = iflag_tri_full_ed
!
      else if (iflag_refine .eq. iflag_tri_s4) then
        iflag_refine_sf_l(1) = iflag_tri_e3_sf
        iflag_refine_sf_l(2) = iflag_tri_e2_sf
        iflag_refine_sf_l(4) = iflag_tri_full_sf
        iflag_refine_sf_l(5) = iflag_tri_e2_sf
        iflag_refine_sf_l(6) = iflag_tri_e3_sf
!
        iflag_refine_ed_l(2) =  iflag_tri_n2_ed
        iflag_refine_ed_l(3) =  iflag_tri_full_ed
        iflag_refine_ed_l(4) =  iflag_tri_n2_ed
        iflag_refine_ed_l(6) =  iflag_tri_n2_ed
        iflag_refine_ed_l(7) =  iflag_tri_full_ed
        iflag_refine_ed_l(8) =  iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_full_ed
        iflag_refine_ed_l(12) = iflag_tri_full_ed
!
      else if (iflag_refine .eq. iflag_tri_s5) then
        iflag_refine_sf_l(1) = iflag_tri_e4_sf
        iflag_refine_sf_l(2) = iflag_tri_e1_sf
        iflag_refine_sf_l(3) = iflag_tri_e1_sf
        iflag_refine_sf_l(4) = iflag_tri_e4_sf
        iflag_refine_sf_l(5) = iflag_tri_full_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed
        iflag_refine_ed_l(2) =  iflag_tri_full_ed
        iflag_refine_ed_l(3) =  iflag_tri_full_ed
        iflag_refine_ed_l(4) =  iflag_tri_full_ed
        iflag_refine_ed_l(9) =  iflag_tri_n1_ed
        iflag_refine_ed_l(10) = iflag_tri_n1_ed
        iflag_refine_ed_l(11) = iflag_tri_n1_ed
        iflag_refine_ed_l(12) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_s6) then
        iflag_refine_sf_l(1) = iflag_tri_e2_sf
        iflag_refine_sf_l(2) = iflag_tri_e3_sf
        iflag_refine_sf_l(3) = iflag_tri_e3_sf
        iflag_refine_sf_l(4) = iflag_tri_e2_sf
        iflag_refine_sf_l(6) = iflag_tri_full_sf
!
        iflag_refine_ed_l(5) =  iflag_tri_full_ed
        iflag_refine_ed_l(6) =  iflag_tri_full_ed
        iflag_refine_ed_l(7) =  iflag_tri_full_ed
        iflag_refine_ed_l(8) =  iflag_tri_full_ed
        iflag_refine_ed_l(9) =  iflag_tri_n2_ed
        iflag_refine_ed_l(10) = iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_n2_ed
        iflag_refine_ed_l(12) = iflag_tri_n2_ed
      end if
!
      end subroutine surf_edge_refine_flag_tri_surf
!
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_tri_edge(iflag_refine,           &
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
      if (iflag_refine .eq. iflag_tri_e1) then
        iflag_refine_sf_l(1) = iflag_tri_n1_sf
        iflag_refine_sf_l(2) = iflag_tri_n1_sf
        iflag_refine_sf_l(3) = iflag_tri_e1_sf
        iflag_refine_sf_l(5) = iflag_tri_e4_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_full_ed
        iflag_refine_ed_l(2) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_n1_ed
        iflag_refine_ed_l(9) =  iflag_tri_n1_ed
        iflag_refine_ed_l(10) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_e2) then
        iflag_refine_sf_l(2) = iflag_tri_e1_sf
        iflag_refine_sf_l(3) = iflag_tri_n2_sf
        iflag_refine_sf_l(4) = iflag_tri_n4_sf
        iflag_refine_sf_l(5) = iflag_tri_e3_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n2_ed
        iflag_refine_ed_l(2) =  iflag_tri_full_ed
        iflag_refine_ed_l(3) =  iflag_tri_n2_ed
        iflag_refine_ed_l(10) = iflag_tri_n1_ed
        iflag_refine_ed_l(11) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_e3) then
        iflag_refine_sf_l(1) = iflag_tri_n4_sf
        iflag_refine_sf_l(2) = iflag_tri_n2_sf
        iflag_refine_sf_l(4) = iflag_tri_e4_sf
        iflag_refine_sf_l(5) = iflag_tri_e2_sf
!
        iflag_refine_ed_l(2) =  iflag_tri_n2_ed
        iflag_refine_ed_l(3) =  iflag_tri_full_ed
        iflag_refine_ed_l(4) =  iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_n1_ed
        iflag_refine_ed_l(12) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_e4) then
        iflag_refine_sf_l(1) = iflag_tri_e4_sf
        iflag_refine_sf_l(3) = iflag_tri_n1_sf
        iflag_refine_sf_l(4) = iflag_tri_n1_sf
        iflag_refine_sf_l(5) = iflag_tri_e1_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n1_ed
        iflag_refine_ed_l(3) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_full_ed
        iflag_refine_ed_l(9) =  iflag_tri_n1_ed
        iflag_refine_ed_l(12) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_e5) then
        iflag_refine_sf_l(1) = iflag_tri_n2_sf
        iflag_refine_sf_l(2) = iflag_tri_n4_sf
        iflag_refine_sf_l(3) = iflag_tri_e3_sf
        iflag_refine_sf_l(6) = iflag_tri_e1_sf
!
        iflag_refine_ed_l(5) =  iflag_tri_full_ed
        iflag_refine_ed_l(6) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_n1_ed
        iflag_refine_ed_l(9) =  iflag_tri_n2_ed
        iflag_refine_ed_l(10) = iflag_tri_n2_ed
!
      else if (iflag_refine .eq. iflag_tri_e6) then
        iflag_refine_sf_l(2) = iflag_tri_e3_sf
        iflag_refine_sf_l(3) = iflag_tri_n3_sf
        iflag_refine_sf_l(4) = iflag_tri_n3_sf
        iflag_refine_sf_l(6) = iflag_tri_e2_sf
!
        iflag_refine_ed_l(5) =  iflag_tri_n2_ed
        iflag_refine_ed_l(6) =  iflag_tri_full_ed
        iflag_refine_ed_l(7) =  iflag_tri_n2_ed
        iflag_refine_ed_l(10) = iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_n2_ed
!
      else if (iflag_refine .eq. iflag_tri_e7) then
        iflag_refine_sf_l(1) = iflag_tri_n3_sf
        iflag_refine_sf_l(2) = iflag_tri_n3_sf
        iflag_refine_sf_l(4) = iflag_tri_e2_sf
        iflag_refine_sf_l(6) = iflag_tri_e3_sf
!
        iflag_refine_ed_l(6) =  iflag_tri_n2_ed
        iflag_refine_ed_l(7) =  iflag_tri_full_ed
        iflag_refine_ed_l(8) =  iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_n2_ed
        iflag_refine_ed_l(12) = iflag_tri_n2_ed
!
      else if (iflag_refine .eq. iflag_tri_e8) then
        iflag_refine_sf_l(1) = iflag_tri_e2_sf
        iflag_refine_sf_l(3) = iflag_tri_n4_sf
        iflag_refine_sf_l(4) = iflag_tri_n2_sf
        iflag_refine_sf_l(6) = iflag_tri_e4_sf
!
        iflag_refine_ed_l(5) =  iflag_tri_n1_ed
        iflag_refine_ed_l(7) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_full_ed
        iflag_refine_ed_l(9) =  iflag_tri_n2_ed
        iflag_refine_ed_l(12) = iflag_tri_n2_ed
!
      else if (iflag_refine .eq. iflag_tri_e9) then
        iflag_refine_sf_l(1) = iflag_tri_e1_sf
        iflag_refine_sf_l(3) = iflag_tri_e4_sf
        iflag_refine_sf_l(5) = iflag_tri_n1_sf
        iflag_refine_sf_l(6) = iflag_tri_n1_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_n1_ed
        iflag_refine_ed_l(5) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_n1_ed
        iflag_refine_ed_l(9) =  iflag_tri_full_ed
!
      else if (iflag_refine .eq. iflag_tri_e10) then
        iflag_refine_sf_l(2) = iflag_tri_e4_sf
        iflag_refine_sf_l(3) = iflag_tri_e2_sf
        iflag_refine_sf_l(5) = iflag_tri_n4_sf
        iflag_refine_sf_l(6) = iflag_tri_n2_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n2_ed
        iflag_refine_ed_l(2) =  iflag_tri_n1_ed
        iflag_refine_ed_l(5) =  iflag_tri_n2_ed
        iflag_refine_ed_l(6) =  iflag_tri_n1_ed
        iflag_refine_ed_l(10) = iflag_tri_full_ed
!
      else if (iflag_refine .eq. iflag_tri_e11) then
        iflag_refine_sf_l(2) = iflag_tri_e2_sf
        iflag_refine_sf_l(4) = iflag_tri_e3_sf
        iflag_refine_sf_l(5) = iflag_tri_n3_sf
        iflag_refine_sf_l(6) = iflag_tri_n3_sf
!
        iflag_refine_ed_l(2) =  iflag_tri_n2_ed
        iflag_refine_ed_l(3) =  iflag_tri_n2_ed
        iflag_refine_ed_l(6) =  iflag_tri_n2_ed
        iflag_refine_ed_l(7) =  iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_full_ed
!
      else if (iflag_refine .eq. iflag_tri_e12) then
        iflag_refine_sf_l(1) = iflag_tri_e3_sf
        iflag_refine_sf_l(4) = iflag_tri_e1_sf
        iflag_refine_sf_l(5) = iflag_tri_n2_sf
        iflag_refine_sf_l(6) = iflag_tri_n4_sf
!
        iflag_refine_ed_l(3) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_n2_ed
        iflag_refine_ed_l(7) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_n2_ed
        iflag_refine_ed_l(12) = iflag_tri_full_ed
      end if
!
      end subroutine surf_edge_refine_flag_tri_edge
!
!  ---------------------------------------------------------------------
!
      subroutine surf_edge_refine_flag_tri_node(iflag_refine,           &
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
      if        (iflag_refine .eq. iflag_tri_n1) then
        iflag_refine_sf_l(1) = iflag_tri_n1_sf
        iflag_refine_sf_l(3) = iflag_tri_n1_sf
        iflag_refine_sf_l(5) = iflag_tri_n1_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_n1_ed
        iflag_refine_ed_l(9) =  iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_n2) then
        iflag_refine_sf_l(2) = iflag_tri_n1_sf
        iflag_refine_sf_l(3) = iflag_tri_n2_sf
        iflag_refine_sf_l(5) = iflag_tri_n4_sf
!
        iflag_refine_ed_l(1) =  iflag_tri_n2_ed
        iflag_refine_ed_l(2) =  iflag_tri_n1_ed
        iflag_refine_ed_l(10) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_n3) then
        iflag_refine_sf_l(2) = iflag_tri_n2_sf
        iflag_refine_sf_l(4) = iflag_tri_n4_sf
        iflag_refine_sf_l(5) = iflag_tri_n3_sf
!
        iflag_refine_ed_l(2) =  iflag_tri_n2_ed
        iflag_refine_ed_l(3) =  iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_n4) then
        iflag_refine_sf_l(1) = iflag_tri_n4_sf
        iflag_refine_sf_l(4) = iflag_tri_n1_sf
        iflag_refine_sf_l(5) = iflag_tri_n2_sf
!
        iflag_refine_ed_l(3) =  iflag_tri_n1_ed
        iflag_refine_ed_l(4) =  iflag_tri_n2_ed
        iflag_refine_ed_l(12) = iflag_tri_n1_ed
!
      else if (iflag_refine .eq. iflag_tri_n5) then
        iflag_refine_sf_l(1) = iflag_tri_n2_sf
        iflag_refine_sf_l(3) = iflag_tri_n4_sf
        iflag_refine_sf_l(6) = iflag_tri_n1_sf
!
        iflag_refine_ed_l(5) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_n1_ed
        iflag_refine_ed_l(9) =  iflag_tri_n2_ed
!
      else if (iflag_refine .eq. iflag_tri_n6) then
        iflag_refine_sf_l(2) = iflag_tri_n4_sf
        iflag_refine_sf_l(3) = iflag_tri_n3_sf
        iflag_refine_sf_l(6) = iflag_tri_n2_sf
!
        iflag_refine_ed_l(5) =  iflag_tri_n2_ed
        iflag_refine_ed_l(6) =  iflag_tri_n1_ed
        iflag_refine_ed_l(10) = iflag_tri_n2_ed
!
      else if (iflag_refine .eq. iflag_tri_n7) then
        iflag_refine_sf_l(2) = iflag_tri_n3_sf
        iflag_refine_sf_l(4) = iflag_tri_n3_sf
        iflag_refine_sf_l(6) = iflag_tri_n3_sf
!
        iflag_refine_ed_l(6) =  iflag_tri_n2_ed
        iflag_refine_ed_l(7) =  iflag_tri_n2_ed
        iflag_refine_ed_l(11) = iflag_tri_n2_ed
!
      else if (iflag_refine .eq. iflag_tri_n8) then
        iflag_refine_sf_l(1) = iflag_tri_n3_sf
        iflag_refine_sf_l(4) = iflag_tri_n1_sf
        iflag_refine_sf_l(6) = iflag_tri_n4_sf
!
        iflag_refine_ed_l(7) =  iflag_tri_n1_ed
        iflag_refine_ed_l(8) =  iflag_tri_n2_ed
        iflag_refine_ed_l(12) = iflag_tri_n2_ed
      end if
!
      end subroutine surf_edge_refine_flag_tri_node
!
!  ---------------------------------------------------------------------
!
      end module set_surf_edge_tri_flag
