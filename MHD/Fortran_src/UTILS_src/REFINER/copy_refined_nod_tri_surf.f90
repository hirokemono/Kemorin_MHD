!copy_refined_nod_tri_surf.f90
!      module copy_refined_nod_tri_surf
!
      module copy_refined_nod_tri_surf
!
      use m_precision
!
      implicit none
!
      private :: copy_refined_nod_tri_s6, copy_refined_nod_tri_s5
      private :: copy_refined_nod_tri_s4, copy_refined_nod_tri_s3
      private :: copy_refined_nod_tri_s2, copy_refined_nod_tri_s1
!
!      subroutine copy_refined_nod_tri_s(iflag_refine,                  &
!     &          inod_refine_local, inod_refine_nod_local,              &
!     &          inod_refine_ele_local, inod_refine_surf_local,         &
!     &          inod_refine_edge_local)
!
!      subroutine copy_refined_nod_tri_s6(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_s5(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_s4(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_s3(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_s2(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_s1(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_s(iflag_refine,                   &
     &          inod_refine_local, inod_refine_nod_local,               &
     &          inod_refine_ele_local, inod_refine_surf_local,          &
     &          inod_refine_edge_local)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iflag_refine
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(52)
!
!
        if      (iflag_refine .eq. iflag_tri_s1) then
          call copy_refined_nod_tri_s1(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_s2) then
          call copy_refined_nod_tri_s2(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_s3) then
          call copy_refined_nod_tri_s3(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_s4) then
          call copy_refined_nod_tri_s4(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_s5) then
          call copy_refined_nod_tri_s5(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_s6) then
          call copy_refined_nod_tri_s6(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
        end if
!
      end subroutine copy_refined_nod_tri_s
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_s6(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(52)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 2) = inod_refine_nod_local(2)
      inod_refine_local( 3) = inod_refine_nod_local(3)
      inod_refine_local( 4) = inod_refine_nod_local(4)
      inod_refine_local( 5) = inod_refine_nod_local(5)
      inod_refine_local( 6) = inod_refine_nod_local(6)
      inod_refine_local( 7) = inod_refine_nod_local(7)
      inod_refine_local( 8) = inod_refine_nod_local(8)
!
      inod_refine_local( 9) = inod_refine_surf_local( 3,1)
      inod_refine_local(10) = inod_refine_surf_local( 3,2)
      inod_refine_local(11) = inod_refine_edge_local( 9,1)
      inod_refine_local(12) = inod_refine_surf_local( 3,4)
      inod_refine_local(13) = inod_refine_surf_local( 3,3)
      inod_refine_local(14) = inod_refine_edge_local(10,1)
!
      inod_refine_local(16) = inod_refine_edge_local(5,1)
      inod_refine_local(17) = inod_refine_edge_local(5,2)
!
      inod_refine_local(19) = inod_refine_surf_local(1,1)
      inod_refine_local(20) = inod_refine_ele_local(1)
      inod_refine_local(21) = inod_refine_ele_local(2)
      inod_refine_local(22) = inod_refine_surf_local(2,1)
      inod_refine_local(23) = inod_refine_surf_local(1,2)
      inod_refine_local(24) = inod_refine_ele_local(5)
      inod_refine_local(25) = inod_refine_ele_local(6)
      inod_refine_local(26) = inod_refine_surf_local(2,4)
!
      inod_refine_local(27) = inod_refine_edge_local(8,1)
      inod_refine_local(28) = inod_refine_surf_local(6,1)
      inod_refine_local(29) = inod_refine_surf_local(6,2)
      inod_refine_local(30) = inod_refine_edge_local(6,1)
!
      inod_refine_local(31) = inod_refine_surf_local(1,4)
      inod_refine_local(32) = inod_refine_ele_local(4)
      inod_refine_local(33) = inod_refine_ele_local(3)
      inod_refine_local(34) = inod_refine_surf_local(2,2)
      inod_refine_local(35) = inod_refine_surf_local(1,3)
      inod_refine_local(36) = inod_refine_ele_local(8)
      inod_refine_local(37) = inod_refine_ele_local(7)
      inod_refine_local(38) = inod_refine_surf_local(2,3)
!
      inod_refine_local(39) = inod_refine_edge_local(8,2)
      inod_refine_local(40) = inod_refine_surf_local(6,4)
      inod_refine_local(41) = inod_refine_surf_local(6,3)
      inod_refine_local(42) = inod_refine_edge_local(6,2)
!
      inod_refine_local(43) = inod_refine_surf_local( 4,1)
      inod_refine_local(44) = inod_refine_surf_local( 4,4)
      inod_refine_local(45) = inod_refine_edge_local(12,1)
      inod_refine_local(46) = inod_refine_surf_local( 4,2)
      inod_refine_local(47) = inod_refine_surf_local( 4,3)
      inod_refine_local(48) = inod_refine_edge_local(11,1)
!
      inod_refine_local(50) = inod_refine_edge_local(7,1)
      inod_refine_local(51) = inod_refine_edge_local(7,2)
!
      end subroutine copy_refined_nod_tri_s6
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_s5(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(52)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(5)
      inod_refine_local( 2) = inod_refine_nod_local(8)
      inod_refine_local( 3) = inod_refine_nod_local(7)
      inod_refine_local( 4) = inod_refine_nod_local(6)
      inod_refine_local( 5) = inod_refine_nod_local(1)
      inod_refine_local( 6) = inod_refine_nod_local(4)
      inod_refine_local( 7) = inod_refine_nod_local(3)
      inod_refine_local( 8) = inod_refine_nod_local(2)
!
      inod_refine_local( 9) = inod_refine_surf_local( 1,2)
      inod_refine_local(10) = inod_refine_surf_local( 1,3)
      inod_refine_local(11) = inod_refine_edge_local( 9,1)
      inod_refine_local(12) = inod_refine_surf_local( 1,1)
      inod_refine_local(13) = inod_refine_surf_local( 1,4)
      inod_refine_local(14) = inod_refine_edge_local(12,1)
!
      inod_refine_local(16) = inod_refine_edge_local(4,1)
      inod_refine_local(17) = inod_refine_edge_local(4,2)
!
      inod_refine_local(19) = inod_refine_surf_local(3,4)
      inod_refine_local(20) = inod_refine_ele_local(5)
      inod_refine_local(21) = inod_refine_ele_local(8)
      inod_refine_local(22) = inod_refine_surf_local(4,2)
      inod_refine_local(23) = inod_refine_surf_local(3,1)
      inod_refine_local(24) = inod_refine_ele_local(1)
      inod_refine_local(25) = inod_refine_ele_local(4)
      inod_refine_local(26) = inod_refine_surf_local(4,1)
!
      inod_refine_local(27) = inod_refine_edge_local(1,1)
      inod_refine_local(28) = inod_refine_surf_local(5,1)
      inod_refine_local(29) = inod_refine_surf_local(5,2)
      inod_refine_local(30) = inod_refine_edge_local(3,1)
!
      inod_refine_local(31) = inod_refine_surf_local(3,3)
      inod_refine_local(32) = inod_refine_ele_local(6)
      inod_refine_local(33) = inod_refine_ele_local(7)
      inod_refine_local(34) = inod_refine_surf_local(4,3)
      inod_refine_local(35) = inod_refine_surf_local(3,2)
      inod_refine_local(36) = inod_refine_ele_local(2)
      inod_refine_local(37) = inod_refine_ele_local(3)
      inod_refine_local(38) = inod_refine_surf_local(4,4)
!
      inod_refine_local(39) = inod_refine_edge_local(1,2)
      inod_refine_local(40) = inod_refine_surf_local(5,4)
      inod_refine_local(41) = inod_refine_surf_local(5,3)
      inod_refine_local(42) = inod_refine_edge_local(3,2)
!
      inod_refine_local(43) = inod_refine_surf_local( 2,4)
      inod_refine_local(44) = inod_refine_surf_local( 2,3)
      inod_refine_local(45) = inod_refine_edge_local(10,1)
      inod_refine_local(46) = inod_refine_surf_local( 2,1)
      inod_refine_local(47) = inod_refine_surf_local( 2,2)
      inod_refine_local(48) = inod_refine_edge_local(11,1)
!
      inod_refine_local(50) = inod_refine_edge_local(2,1)
      inod_refine_local(51) = inod_refine_edge_local(2,2)
!
      end subroutine copy_refined_nod_tri_s5
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_s4(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(52)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 2) = inod_refine_nod_local(5)
      inod_refine_local( 3) = inod_refine_nod_local(6)
      inod_refine_local( 4) = inod_refine_nod_local(2)
      inod_refine_local( 5) = inod_refine_nod_local(4)
      inod_refine_local( 6) = inod_refine_nod_local(8)
      inod_refine_local( 7) = inod_refine_nod_local(7)
      inod_refine_local( 8) = inod_refine_nod_local(3)
!
      inod_refine_local( 9) = inod_refine_surf_local(1,1)
      inod_refine_local(10) = inod_refine_surf_local(1,2)
      inod_refine_local(11) = inod_refine_edge_local(4,1)
      inod_refine_local(12) = inod_refine_surf_local(1,4)
      inod_refine_local(13) = inod_refine_surf_local(1,3)
      inod_refine_local(14) = inod_refine_edge_local(8,1)
!
      inod_refine_local(16) = inod_refine_edge_local(12,1)
      inod_refine_local(17) = inod_refine_edge_local(12,2)
!
      inod_refine_local(19) = inod_refine_surf_local(5,1)
      inod_refine_local(20) = inod_refine_ele_local(1)
      inod_refine_local(21) = inod_refine_ele_local(5)
      inod_refine_local(22) = inod_refine_surf_local(6,1)
      inod_refine_local(23) = inod_refine_surf_local(5,2)
      inod_refine_local(24) = inod_refine_ele_local(4)
      inod_refine_local(25) = inod_refine_ele_local(8)
      inod_refine_local(26) = inod_refine_surf_local(6,4)
!
      inod_refine_local(27) = inod_refine_edge_local(3,1)
      inod_refine_local(28) = inod_refine_surf_local(4,1)
      inod_refine_local(29) = inod_refine_surf_local(4,2)
      inod_refine_local(30) = inod_refine_edge_local(7,1)
!
      inod_refine_local(31) = inod_refine_surf_local(5,4)
      inod_refine_local(32) = inod_refine_ele_local(2)
      inod_refine_local(33) = inod_refine_ele_local(6)
      inod_refine_local(34) = inod_refine_surf_local(6,2)
      inod_refine_local(35) = inod_refine_surf_local(5,3)
      inod_refine_local(36) = inod_refine_ele_local(3)
      inod_refine_local(37) = inod_refine_ele_local(7)
      inod_refine_local(38) = inod_refine_surf_local(6,3)
!
      inod_refine_local(39) = inod_refine_edge_local(3,2)
      inod_refine_local(40) = inod_refine_surf_local(4,4)
      inod_refine_local(41) = inod_refine_surf_local(4,3)
      inod_refine_local(42) = inod_refine_edge_local(7,2)
!
      inod_refine_local(43) = inod_refine_surf_local(2,1)
      inod_refine_local(44) = inod_refine_surf_local(2,4)
      inod_refine_local(45) = inod_refine_edge_local(2,1)
      inod_refine_local(46) = inod_refine_surf_local(2,2)
      inod_refine_local(47) = inod_refine_surf_local(2,3)
      inod_refine_local(48) = inod_refine_edge_local(6,1)
!
      inod_refine_local(50) = inod_refine_edge_local(11,1)
      inod_refine_local(51) = inod_refine_edge_local(11,2)
!
      end subroutine copy_refined_nod_tri_s4
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_s3(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(52)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(4)
      inod_refine_local( 2) = inod_refine_nod_local(3)
      inod_refine_local( 3) = inod_refine_nod_local(7)
      inod_refine_local( 4) = inod_refine_nod_local(8)
      inod_refine_local( 5) = inod_refine_nod_local(1)
      inod_refine_local( 6) = inod_refine_nod_local(2)
      inod_refine_local( 7) = inod_refine_nod_local(6)
      inod_refine_local( 8) = inod_refine_nod_local(5)
!
      inod_refine_local( 9) = inod_refine_surf_local(5,2)
      inod_refine_local(10) = inod_refine_surf_local(5,3)
      inod_refine_local(11) = inod_refine_edge_local(4,1)
      inod_refine_local(12) = inod_refine_surf_local(5,1)
      inod_refine_local(13) = inod_refine_surf_local(5,4)
      inod_refine_local(14) = inod_refine_edge_local(2,1)
!
      inod_refine_local(16) = inod_refine_edge_local(1,1)
      inod_refine_local(17) = inod_refine_edge_local(1,2)
!
      inod_refine_local(19) = inod_refine_surf_local(1,4)
      inod_refine_local(20) = inod_refine_ele_local(4)
      inod_refine_local(21) = inod_refine_ele_local(3)
      inod_refine_local(22) = inod_refine_surf_local(2,2)
      inod_refine_local(23) = inod_refine_surf_local(1,1)
      inod_refine_local(24) = inod_refine_ele_local(1)
      inod_refine_local(25) = inod_refine_ele_local(2)
      inod_refine_local(26) = inod_refine_surf_local(2,1)
!
      inod_refine_local(27) = inod_refine_edge_local( 9,1)
      inod_refine_local(28) = inod_refine_surf_local( 3,1)
      inod_refine_local(29) = inod_refine_surf_local( 3,2)
      inod_refine_local(30) = inod_refine_edge_local(10,1)
!
      inod_refine_local(31) = inod_refine_surf_local(1,3)
      inod_refine_local(32) = inod_refine_ele_local(8)
      inod_refine_local(33) = inod_refine_ele_local(7)
      inod_refine_local(34) = inod_refine_surf_local(2,3)
      inod_refine_local(35) = inod_refine_surf_local(1,2)
      inod_refine_local(36) = inod_refine_ele_local(5)
      inod_refine_local(37) = inod_refine_ele_local(6)
      inod_refine_local(38) = inod_refine_surf_local(2,4)
!
      inod_refine_local(39) = inod_refine_edge_local( 9,2)
      inod_refine_local(40) = inod_refine_surf_local( 3,4)
      inod_refine_local(41) = inod_refine_surf_local( 3,3)
      inod_refine_local(42) = inod_refine_edge_local(10,2)
!
      inod_refine_local(43) = inod_refine_surf_local(6,4)
      inod_refine_local(44) = inod_refine_surf_local(6,3)
      inod_refine_local(45) = inod_refine_edge_local(8,1)
      inod_refine_local(46) = inod_refine_surf_local(6,1)
      inod_refine_local(47) = inod_refine_surf_local(6,2)
      inod_refine_local(48) = inod_refine_edge_local(6,1)
!
      inod_refine_local(50) = inod_refine_edge_local(5,1)
      inod_refine_local(51) = inod_refine_edge_local(5,2)
!
      end subroutine copy_refined_nod_tri_s3
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_s2(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(52)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 2) = inod_refine_nod_local(4)
      inod_refine_local( 3) = inod_refine_nod_local(8)
      inod_refine_local( 4) = inod_refine_nod_local(5)
      inod_refine_local( 5) = inod_refine_nod_local(2)
      inod_refine_local( 6) = inod_refine_nod_local(3)
      inod_refine_local( 7) = inod_refine_nod_local(7)
      inod_refine_local( 8) = inod_refine_nod_local(6)
!
      inod_refine_local( 9) = inod_refine_surf_local(5,1)
      inod_refine_local(10) = inod_refine_surf_local(5,2)
      inod_refine_local(11) = inod_refine_edge_local(1,1)
      inod_refine_local(12) = inod_refine_surf_local(5,4)
      inod_refine_local(13) = inod_refine_surf_local(5,3)
      inod_refine_local(14) = inod_refine_edge_local(3,1)
!
      inod_refine_local(16) = inod_refine_edge_local(2,1)
      inod_refine_local(17) = inod_refine_edge_local(2,2)
!
      inod_refine_local(19) = inod_refine_surf_local(3,1)
      inod_refine_local(20) = inod_refine_ele_local(1)
      inod_refine_local(21) = inod_refine_ele_local(4)
      inod_refine_local(22) = inod_refine_surf_local(4,1)
      inod_refine_local(23) = inod_refine_surf_local(3,2)
      inod_refine_local(24) = inod_refine_ele_local(2)
      inod_refine_local(25) = inod_refine_ele_local(3)
      inod_refine_local(26) = inod_refine_surf_local(4,4)
!
      inod_refine_local(27) = inod_refine_edge_local(10,1)
      inod_refine_local(28) = inod_refine_surf_local( 2,1)
      inod_refine_local(29) = inod_refine_surf_local( 2,2)
      inod_refine_local(30) = inod_refine_edge_local(11,1)
!
      inod_refine_local(31) = inod_refine_surf_local(3,4)
      inod_refine_local(32) = inod_refine_ele_local(5)
      inod_refine_local(33) = inod_refine_ele_local(8)
      inod_refine_local(34) = inod_refine_surf_local(4,2)
      inod_refine_local(35) = inod_refine_surf_local(3,3)
      inod_refine_local(36) = inod_refine_ele_local(6)
      inod_refine_local(37) = inod_refine_ele_local(7)
      inod_refine_local(38) = inod_refine_surf_local(4,3)
!
      inod_refine_local(39) = inod_refine_edge_local(10,2)
      inod_refine_local(40) = inod_refine_surf_local( 2,4)
      inod_refine_local(41) = inod_refine_surf_local( 2,3)
      inod_refine_local(42) = inod_refine_edge_local(11,2)
!
      inod_refine_local(43) = inod_refine_surf_local(6,1)
      inod_refine_local(44) = inod_refine_surf_local(6,4)
      inod_refine_local(45) = inod_refine_edge_local(5,1)
      inod_refine_local(46) = inod_refine_surf_local(6,2)
      inod_refine_local(47) = inod_refine_surf_local(6,3)
      inod_refine_local(48) = inod_refine_edge_local(7,1)
!
      inod_refine_local(50) = inod_refine_edge_local(6,1)
      inod_refine_local(51) = inod_refine_edge_local(6,2)
!
      end subroutine copy_refined_nod_tri_s2
!
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_s1(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(52)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(2)
      inod_refine_local( 2) = inod_refine_nod_local(6)
      inod_refine_local( 3) = inod_refine_nod_local(7)
      inod_refine_local( 4) = inod_refine_nod_local(3)
      inod_refine_local( 5) = inod_refine_nod_local(1)
      inod_refine_local( 6) = inod_refine_nod_local(5)
      inod_refine_local( 7) = inod_refine_nod_local(8)
      inod_refine_local( 8) = inod_refine_nod_local(4)
!
      inod_refine_local( 9) = inod_refine_surf_local(3,2)
      inod_refine_local(10) = inod_refine_surf_local(3,3)
      inod_refine_local(11) = inod_refine_edge_local(1,1)
      inod_refine_local(12) = inod_refine_surf_local(3,1)
      inod_refine_local(13) = inod_refine_surf_local(3,4)
      inod_refine_local(14) = inod_refine_edge_local(5,1)
!
      inod_refine_local(16) = inod_refine_edge_local(9,1)
      inod_refine_local(17) = inod_refine_edge_local(9,2)
!
      inod_refine_local(19) = inod_refine_surf_local(5,4)
      inod_refine_local(20) = inod_refine_ele_local(2)
      inod_refine_local(21) = inod_refine_ele_local(6)
      inod_refine_local(22) = inod_refine_surf_local(6,2)
      inod_refine_local(23) = inod_refine_surf_local(5,1)
      inod_refine_local(24) = inod_refine_ele_local(1)
      inod_refine_local(25) = inod_refine_ele_local(5)
      inod_refine_local(26) = inod_refine_surf_local(6,1)
!
      inod_refine_local(27) = inod_refine_edge_local(4,1)
      inod_refine_local(28) = inod_refine_surf_local(1,1)
      inod_refine_local(29) = inod_refine_surf_local(1,2)
      inod_refine_local(30) = inod_refine_edge_local(8,1)
!
      inod_refine_local(31) = inod_refine_surf_local(5,3)
      inod_refine_local(32) = inod_refine_ele_local(3)
      inod_refine_local(33) = inod_refine_ele_local(7)
      inod_refine_local(34) = inod_refine_surf_local(6,3)
      inod_refine_local(35) = inod_refine_surf_local(5,2)
      inod_refine_local(36) = inod_refine_ele_local(4)
      inod_refine_local(37) = inod_refine_ele_local(8)
      inod_refine_local(38) = inod_refine_surf_local(6,4)
!
      inod_refine_local(39) = inod_refine_edge_local(4,2)
      inod_refine_local(40) = inod_refine_surf_local(1,4)
      inod_refine_local(41) = inod_refine_surf_local(1,3)
      inod_refine_local(42) = inod_refine_edge_local(8,2)
!
      inod_refine_local(43) = inod_refine_surf_local(4,4)
      inod_refine_local(44) = inod_refine_surf_local(4,3)
      inod_refine_local(45) = inod_refine_edge_local(3,1)
      inod_refine_local(46) = inod_refine_surf_local(4,1)
      inod_refine_local(47) = inod_refine_surf_local(4,2)
      inod_refine_local(48) = inod_refine_edge_local(7,1)
!
      inod_refine_local(50) = inod_refine_edge_local(12,1)
      inod_refine_local(51) = inod_refine_edge_local(12,2)
!
      end subroutine copy_refined_nod_tri_s1
!
! ----------------------------------------------------------------------
!
      end module copy_refined_nod_tri_surf
