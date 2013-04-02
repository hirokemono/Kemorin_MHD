!copy_refined_nod_tri_edge.f90
!      module copy_refined_nod_tri_edge
!
      module copy_refined_nod_tri_edge
!
      use m_precision
!
      implicit none
!
      private :: copy_refined_nod_tri_e12, copy_refined_nod_tri_e11
      private :: copy_refined_nod_tri_e10, copy_refined_nod_tri_e9
      private :: copy_refined_nod_tri_e8,  copy_refined_nod_tri_e7
      private :: copy_refined_nod_tri_e6,  copy_refined_nod_tri_e5
      private :: copy_refined_nod_tri_e4,  copy_refined_nod_tri_e3
      private :: copy_refined_nod_tri_e2,  copy_refined_nod_tri_e1
!
!      subroutine copy_refined_nod_tri_e(iflag_refine,                  &
!     &          inod_refine_local, inod_refine_nod_local,              &
!     &          inod_refine_ele_local, inod_refine_surf_local,         &
!     &          inod_refine_edge_local)
!
!      subroutine copy_refined_nod_tri_e12(inod_refine_local,           &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e11(inod_refine_local,           &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e10(inod_refine_local,           &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e9(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e8(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e7(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e6(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e5(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e4(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e3(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e2(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_e1(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e(iflag_refine,                   &
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
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
!
!
        if      (iflag_refine .eq. iflag_tri_e1) then
          call copy_refined_nod_tri_e1(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e2) then
          call copy_refined_nod_tri_e2(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e3) then
          call copy_refined_nod_tri_e3(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e4) then
          call copy_refined_nod_tri_e4(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e5) then
          call copy_refined_nod_tri_e5(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e6) then
          call copy_refined_nod_tri_e6(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e7) then
          call copy_refined_nod_tri_e7(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e8) then
          call copy_refined_nod_tri_e8(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e9) then
          call copy_refined_nod_tri_e9(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e10) then
          call copy_refined_nod_tri_e10(inod_refine_local,              &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e11) then
          call copy_refined_nod_tri_e11(inod_refine_local,              &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_e12) then
          call copy_refined_nod_tri_e12(inod_refine_local,              &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
        end if
!
      end subroutine copy_refined_nod_tri_e
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e12(inod_refine_local,            &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
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
      inod_refine_local(20) = inod_refine_ele_local(4)
      inod_refine_local(21) = inod_refine_ele_local(8)
      inod_refine_local(22) = inod_refine_surf_local(6,1)
      inod_refine_local(23) = inod_refine_edge_local(3,1)
      inod_refine_local(24) = inod_refine_surf_local(4,1)
      inod_refine_local(25) = inod_refine_surf_local(4,2)
      inod_refine_local(26) = inod_refine_edge_local(7,1)
!
      inod_refine_local(27) = inod_refine_surf_local(4,4)
      inod_refine_local(28) = inod_refine_surf_local(4,3)
!
      inod_refine_local(29) = inod_refine_ele_local(2)
      inod_refine_local(30) = inod_refine_ele_local(6)
!
      end subroutine copy_refined_nod_tri_e12
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e11(inod_refine_local,            &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(4)
      inod_refine_local( 2) = inod_refine_nod_local(8)
      inod_refine_local( 3) = inod_refine_nod_local(5)
      inod_refine_local( 4) = inod_refine_nod_local(1)
      inod_refine_local( 5) = inod_refine_nod_local(3)
      inod_refine_local( 6) = inod_refine_nod_local(7)
      inod_refine_local( 7) = inod_refine_nod_local(6)
      inod_refine_local( 8) = inod_refine_nod_local(2)
!
      inod_refine_local( 9) = inod_refine_surf_local(4,1)
      inod_refine_local(10) = inod_refine_surf_local(4,2)
      inod_refine_local(11) = inod_refine_edge_local(3,1)
      inod_refine_local(12) = inod_refine_surf_local(4,4)
      inod_refine_local(13) = inod_refine_surf_local(4,3)
      inod_refine_local(14) = inod_refine_edge_local(7,1)
!
      inod_refine_local(16) = inod_refine_edge_local(11,1)
      inod_refine_local(17) = inod_refine_edge_local(11,2)
!
      inod_refine_local(19) = inod_refine_surf_local(5,1)
      inod_refine_local(20) = inod_refine_ele_local(3)
      inod_refine_local(21) = inod_refine_ele_local(7)
      inod_refine_local(22) = inod_refine_surf_local(6,1)
      inod_refine_local(23) = inod_refine_edge_local(2,1)
      inod_refine_local(24) = inod_refine_surf_local(2,2)
      inod_refine_local(25) = inod_refine_surf_local(2,3)
      inod_refine_local(26) = inod_refine_edge_local(6,1)
!
      inod_refine_local(27) = inod_refine_surf_local(2,1)
      inod_refine_local(28) = inod_refine_surf_local(2,4)
!
      inod_refine_local(29) = inod_refine_ele_local(1)
      inod_refine_local(30) = inod_refine_ele_local(5)
!
      end subroutine copy_refined_nod_tri_e11
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e10(inod_refine_local,            &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(3)
      inod_refine_local( 2) = inod_refine_nod_local(7)
      inod_refine_local( 3) = inod_refine_nod_local(8)
      inod_refine_local( 4) = inod_refine_nod_local(4)
      inod_refine_local( 5) = inod_refine_nod_local(2)
      inod_refine_local( 6) = inod_refine_nod_local(6)
      inod_refine_local( 7) = inod_refine_nod_local(5)
      inod_refine_local( 8) = inod_refine_nod_local(1)
!
      inod_refine_local( 9) = inod_refine_surf_local(2,2)
      inod_refine_local(10) = inod_refine_surf_local(2,3)
      inod_refine_local(11) = inod_refine_edge_local(2,1)
      inod_refine_local(12) = inod_refine_surf_local(2,1)
      inod_refine_local(13) = inod_refine_surf_local(2,4)
      inod_refine_local(14) = inod_refine_edge_local(6,1)
!
      inod_refine_local(16) = inod_refine_edge_local(10,1)
      inod_refine_local(17) = inod_refine_edge_local(10,2)
!
      inod_refine_local(19) = inod_refine_surf_local(5,1)
      inod_refine_local(20) = inod_refine_ele_local(2)
      inod_refine_local(21) = inod_refine_ele_local(6)
      inod_refine_local(22) = inod_refine_surf_local(6,1)
      inod_refine_local(23) = inod_refine_edge_local(1,1)
      inod_refine_local(24) = inod_refine_surf_local(3,2)
      inod_refine_local(25) = inod_refine_surf_local(3,3)
      inod_refine_local(26) = inod_refine_edge_local(5,1)
!
      inod_refine_local(27) = inod_refine_surf_local(3,1)
      inod_refine_local(28) = inod_refine_surf_local(3,4)
!
      inod_refine_local(29) = inod_refine_ele_local(4)
      inod_refine_local(30) = inod_refine_ele_local(8)
!
      end subroutine copy_refined_nod_tri_e10
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e9(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
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
      inod_refine_local(19) = inod_refine_surf_local(5,1)
      inod_refine_local(20) = inod_refine_ele_local(1)
      inod_refine_local(21) = inod_refine_ele_local(5)
      inod_refine_local(22) = inod_refine_surf_local(6,1)
      inod_refine_local(23) = inod_refine_edge_local(4,1)
      inod_refine_local(24) = inod_refine_surf_local(1,1)
      inod_refine_local(25) = inod_refine_surf_local(1,2)
      inod_refine_local(26) = inod_refine_edge_local(8,1)
!
      inod_refine_local(27) = inod_refine_surf_local(1,4)
      inod_refine_local(28) = inod_refine_surf_local(1,3)
!
      inod_refine_local(29) = inod_refine_ele_local(3)
      inod_refine_local(30) = inod_refine_ele_local(7)
!
      end subroutine copy_refined_nod_tri_e9
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e8(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(6)
      inod_refine_local( 2) = inod_refine_nod_local(7)
      inod_refine_local( 3) = inod_refine_nod_local(3)
      inod_refine_local( 4) = inod_refine_nod_local(2)
      inod_refine_local( 5) = inod_refine_nod_local(5)
      inod_refine_local( 6) = inod_refine_nod_local(8)
      inod_refine_local( 7) = inod_refine_nod_local(4)
      inod_refine_local( 8) = inod_refine_nod_local(1)
!
      inod_refine_local( 9) = inod_refine_surf_local(6,2)
      inod_refine_local(10) = inod_refine_surf_local(6,3)
      inod_refine_local(11) = inod_refine_edge_local(5,1)
      inod_refine_local(12) = inod_refine_surf_local(6,1)
      inod_refine_local(13) = inod_refine_surf_local(6,4)
      inod_refine_local(14) = inod_refine_edge_local(7,1)
!
      inod_refine_local(16) = inod_refine_edge_local(8,1)
      inod_refine_local(17) = inod_refine_edge_local(8,2)
!
      inod_refine_local(19) = inod_refine_surf_local( 3,1)
      inod_refine_local(20) = inod_refine_ele_local(5)
      inod_refine_local(21) = inod_refine_ele_local(8)
      inod_refine_local(22) = inod_refine_surf_local( 4,1)
      inod_refine_local(23) = inod_refine_edge_local( 9,1)
      inod_refine_local(24) = inod_refine_surf_local( 1,2)
      inod_refine_local(25) = inod_refine_surf_local( 1,3)
      inod_refine_local(26) = inod_refine_edge_local(12,1)
!
      inod_refine_local(27) = inod_refine_surf_local( 1,1)
      inod_refine_local(28) = inod_refine_surf_local( 1,4)
!
      inod_refine_local(29) = inod_refine_ele_local(2)
      inod_refine_local(30) = inod_refine_ele_local(3)
!
      end subroutine copy_refined_nod_tri_e8
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e7(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(5)
      inod_refine_local( 2) = inod_refine_nod_local(6)
      inod_refine_local( 3) = inod_refine_nod_local(2)
      inod_refine_local( 4) = inod_refine_nod_local(1)
      inod_refine_local( 5) = inod_refine_nod_local(8)
      inod_refine_local( 6) = inod_refine_nod_local(7)
      inod_refine_local( 7) = inod_refine_nod_local(3)
      inod_refine_local( 8) = inod_refine_nod_local(4)
!
      inod_refine_local( 9) = inod_refine_surf_local(6,1)
      inod_refine_local(10) = inod_refine_surf_local(6,2)
      inod_refine_local(11) = inod_refine_edge_local(8,1)
      inod_refine_local(12) = inod_refine_surf_local(6,4)
      inod_refine_local(13) = inod_refine_surf_local(6,3)
      inod_refine_local(14) = inod_refine_edge_local(6,1)
!
      inod_refine_local(16) = inod_refine_edge_local(7,1)
      inod_refine_local(17) = inod_refine_edge_local(7,2)
!
      inod_refine_local(19) = inod_refine_surf_local( 1,1)
      inod_refine_local(20) = inod_refine_ele_local(8)
      inod_refine_local(21) = inod_refine_ele_local(7)
      inod_refine_local(22) = inod_refine_surf_local( 2,1)
      inod_refine_local(23) = inod_refine_edge_local(12,1)
      inod_refine_local(24) = inod_refine_surf_local( 4,2)
      inod_refine_local(25) = inod_refine_surf_local( 4,3)
      inod_refine_local(26) = inod_refine_edge_local(11,1)
!
      inod_refine_local(27) = inod_refine_surf_local( 4,1)
      inod_refine_local(28) = inod_refine_surf_local( 4,4)
!
      inod_refine_local(29) = inod_refine_ele_local(1)
      inod_refine_local(30) = inod_refine_ele_local(2)
!
      end subroutine copy_refined_nod_tri_e7
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e6(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(2)
      inod_refine_local( 2) = inod_refine_nod_local(3)
      inod_refine_local( 3) = inod_refine_nod_local(4)
      inod_refine_local( 4) = inod_refine_nod_local(1)
      inod_refine_local( 5) = inod_refine_nod_local(6)
      inod_refine_local( 6) = inod_refine_nod_local(7)
      inod_refine_local( 7) = inod_refine_nod_local(8)
      inod_refine_local( 8) = inod_refine_nod_local(5)
!
      inod_refine_local( 9) = inod_refine_surf_local( 2,1)
      inod_refine_local(10) = inod_refine_surf_local( 2,2)
      inod_refine_local(11) = inod_refine_edge_local(10,1)
      inod_refine_local(12) = inod_refine_surf_local( 2,4)
      inod_refine_local(13) = inod_refine_surf_local( 2,3)
      inod_refine_local(14) = inod_refine_edge_local(11,1)
!
      inod_refine_local(16) = inod_refine_edge_local(6,1)
      inod_refine_local(17) = inod_refine_edge_local(6,2)
!
      inod_refine_local(19) = inod_refine_surf_local(3,1)
      inod_refine_local(20) = inod_refine_ele_local(6)
      inod_refine_local(21) = inod_refine_ele_local(7)
      inod_refine_local(22) = inod_refine_surf_local(4,1)
      inod_refine_local(23) = inod_refine_edge_local(5,1)
      inod_refine_local(24) = inod_refine_surf_local(6,2)
      inod_refine_local(25) = inod_refine_surf_local(6,3)
      inod_refine_local(26) = inod_refine_edge_local(7,1)
!
      inod_refine_local(27) = inod_refine_surf_local(6,1)
      inod_refine_local(28) = inod_refine_surf_local(6,4)
!
      inod_refine_local(29) = inod_refine_ele_local(1)
      inod_refine_local(30) = inod_refine_ele_local(4)
!
      end subroutine copy_refined_nod_tri_e6
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e5(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
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
      inod_refine_local(20) = inod_refine_ele_local(5)
      inod_refine_local(21) = inod_refine_ele_local(6)
      inod_refine_local(22) = inod_refine_surf_local(2,1)
      inod_refine_local(23) = inod_refine_edge_local(8,1)
      inod_refine_local(24) = inod_refine_surf_local(6,1)
      inod_refine_local(25) = inod_refine_surf_local(6,2)
      inod_refine_local(26) = inod_refine_edge_local(6,1)
!
      inod_refine_local(27) = inod_refine_surf_local(6,4)
      inod_refine_local(28) = inod_refine_surf_local(6,3)
!
      inod_refine_local(29) = inod_refine_ele_local(4)
      inod_refine_local(30) = inod_refine_ele_local(3)
!
      end subroutine copy_refined_nod_tri_e5
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e4(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
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
      inod_refine_local(19) = inod_refine_surf_local(3,1)
      inod_refine_local(20) = inod_refine_ele_local(1)
      inod_refine_local(21) = inod_refine_ele_local(4)
      inod_refine_local(22) = inod_refine_surf_local(4,1)
      inod_refine_local(23) = inod_refine_edge_local(1,1)
      inod_refine_local(24) = inod_refine_surf_local(5,1)
      inod_refine_local(25) = inod_refine_surf_local(5,2)
      inod_refine_local(26) = inod_refine_edge_local(3,1)
!
      inod_refine_local(27) = inod_refine_surf_local(5,4)
      inod_refine_local(28) = inod_refine_surf_local(5,3)
!
      inod_refine_local(29) = inod_refine_ele_local(6)
      inod_refine_local(30) = inod_refine_ele_local(7)
!
      end subroutine copy_refined_nod_tri_e4
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e3(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(8)
      inod_refine_local( 2) = inod_refine_nod_local(7)
      inod_refine_local( 3) = inod_refine_nod_local(6)
      inod_refine_local( 4) = inod_refine_nod_local(5)
      inod_refine_local( 5) = inod_refine_nod_local(4)
      inod_refine_local( 6) = inod_refine_nod_local(3)
      inod_refine_local( 7) = inod_refine_nod_local(2)
      inod_refine_local( 8) = inod_refine_nod_local(1)
!
      inod_refine_local( 9) = inod_refine_surf_local( 4,2)
      inod_refine_local(10) = inod_refine_surf_local( 4,3)
      inod_refine_local(11) = inod_refine_edge_local(12,1)
      inod_refine_local(12) = inod_refine_surf_local( 4,1)
      inod_refine_local(13) = inod_refine_surf_local( 4,4)
      inod_refine_local(14) = inod_refine_edge_local(11,1)
!
      inod_refine_local(16) = inod_refine_edge_local(3,1)
      inod_refine_local(17) = inod_refine_edge_local(3,2)
!
      inod_refine_local(19) = inod_refine_surf_local(1,1)
      inod_refine_local(20) = inod_refine_ele_local(4)
      inod_refine_local(21) = inod_refine_ele_local(3)
      inod_refine_local(22) = inod_refine_surf_local(2,1)
      inod_refine_local(23) = inod_refine_edge_local(4,1)
      inod_refine_local(24) = inod_refine_surf_local(5,2)
      inod_refine_local(25) = inod_refine_surf_local(5,3)
      inod_refine_local(26) = inod_refine_edge_local(2,1)
!
      inod_refine_local(27) = inod_refine_surf_local(5,1)
      inod_refine_local(28) = inod_refine_surf_local(5,4)
!
      inod_refine_local(29) = inod_refine_ele_local(5)
      inod_refine_local(30) = inod_refine_ele_local(6)
!
      end subroutine copy_refined_nod_tri_e3
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e2(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
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
      inod_refine_local(19) = inod_refine_surf_local( 3,1)
      inod_refine_local(20) = inod_refine_ele_local(2)
      inod_refine_local(21) = inod_refine_ele_local(3)
      inod_refine_local(22) = inod_refine_surf_local( 4,1)
      inod_refine_local(23) = inod_refine_edge_local(10,1)
      inod_refine_local(24) = inod_refine_surf_local( 2,1)
      inod_refine_local(25) = inod_refine_surf_local( 2,2)
      inod_refine_local(26) = inod_refine_edge_local(11,1)
!
      inod_refine_local(27) = inod_refine_surf_local( 2,4)
      inod_refine_local(28) = inod_refine_surf_local( 2,3)
!
      inod_refine_local(29) = inod_refine_ele_local(5)
      inod_refine_local(30) = inod_refine_ele_local(8)
!
      end subroutine copy_refined_nod_tri_e2
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_e1(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(30)
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
      inod_refine_local(19) = inod_refine_surf_local( 1,1)
      inod_refine_local(20) = inod_refine_ele_local(1)
      inod_refine_local(21) = inod_refine_ele_local(2)
      inod_refine_local(22) = inod_refine_surf_local( 2,1)
      inod_refine_local(23) = inod_refine_edge_local( 9,1)
      inod_refine_local(24) = inod_refine_surf_local( 3,1)
      inod_refine_local(25) = inod_refine_surf_local( 3,2)
      inod_refine_local(26) = inod_refine_edge_local(10,1)
!
      inod_refine_local(27) = inod_refine_surf_local( 3,4)
      inod_refine_local(28) = inod_refine_surf_local( 3,3)
!
      inod_refine_local(29) = inod_refine_ele_local(8)
      inod_refine_local(30) = inod_refine_ele_local(7)
!
      end subroutine copy_refined_nod_tri_e1
!
! ----------------------------------------------------------------------
!
      end module copy_refined_nod_tri_edge
