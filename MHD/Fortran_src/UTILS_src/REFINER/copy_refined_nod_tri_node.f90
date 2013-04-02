!copy_refined_nod_tri_node.f90
!      module copy_refined_nod_tri_node
!
      module copy_refined_nod_tri_node
!
      use m_precision
!
      implicit none
!
      private :: copy_refined_nod_tri_n8, copy_refined_nod_tri_n7
      private :: copy_refined_nod_tri_n6, copy_refined_nod_tri_n5
      private :: copy_refined_nod_tri_n4, copy_refined_nod_tri_n3
      private :: copy_refined_nod_tri_n2, copy_refined_nod_tri_n1
!
!      subroutine copy_refined_nod_tri_n(iflag_refine,                  &
!     &          inod_refine_local, inod_refine_nod_local,              &
!     &          inod_refine_ele_local, inod_refine_surf_local,         &
!     &          inod_refine_edge_local)
!
!      subroutine copy_refined_nod_tri_n8(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_n7(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_n6(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_n5(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_n4(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_n3(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_n2(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine copy_refined_nod_tri_n1(inod_refine_local,            &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n(iflag_refine,                   &
     &          inod_refine_local, inod_refine_nod_local,               &
     &          inod_refine_ele_local, inod_refine_surf_local,          &
     &          inod_refine_edge_local)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: iflag_refine
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
!
!
        if      (iflag_refine .eq. iflag_tri_n1) then
          call copy_refined_nod_tri_n1(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_n2) then
          call copy_refined_nod_tri_n2(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_n3) then
          call copy_refined_nod_tri_n3(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_n4) then
          call copy_refined_nod_tri_n4(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_n5) then
          call copy_refined_nod_tri_n5(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_n6) then
          call copy_refined_nod_tri_n6(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_n7) then
          call copy_refined_nod_tri_n7(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        else if (iflag_refine .eq. iflag_tri_n8) then
          call copy_refined_nod_tri_n8(inod_refine_local,               &
     &        inod_refine_nod_local, inod_refine_ele_local,             &
     &        inod_refine_surf_local, inod_refine_edge_local)
!
        end if
!
      end subroutine copy_refined_nod_tri_n
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n8(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
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
      inod_refine_local( 9) = inod_refine_surf_local( 1,1)
      inod_refine_local(10) = inod_refine_edge_local( 8,1)
      inod_refine_local(11) = inod_refine_surf_local( 6,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local(12,1)
!
      inod_refine_local(15) = inod_refine_edge_local( 7,1)
      inod_refine_local(16) = inod_refine_surf_local( 4,1)
!
      end subroutine copy_refined_nod_tri_n8
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n7(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
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
      inod_refine_local(10) = inod_refine_edge_local(11,1)
      inod_refine_local(11) = inod_refine_surf_local( 4,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local( 6,1)
!
      inod_refine_local(15) = inod_refine_edge_local( 7,1)
      inod_refine_local(16) = inod_refine_surf_local( 6,1)
!
      end subroutine copy_refined_nod_tri_n7
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n6(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
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
      inod_refine_local(10) = inod_refine_edge_local(10,1)
      inod_refine_local(11) = inod_refine_surf_local( 2,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local( 5,1)
!
      inod_refine_local(15) = inod_refine_edge_local( 6,1)
      inod_refine_local(16) = inod_refine_surf_local( 6,1)
!
      end subroutine copy_refined_nod_tri_n6
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n5(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
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
      inod_refine_local( 9) = inod_refine_surf_local(3,1)
      inod_refine_local(10) = inod_refine_edge_local(5,1)
      inod_refine_local(11) = inod_refine_surf_local(6,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local(9,1)
!
      inod_refine_local(15) = inod_refine_edge_local(8,1)
      inod_refine_local(16) = inod_refine_surf_local(1,1)
!
      end subroutine copy_refined_nod_tri_n5
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n4(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
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
      inod_refine_local( 9) = inod_refine_surf_local( 1,1)
      inod_refine_local(10) = inod_refine_edge_local(12,1)
      inod_refine_local(11) = inod_refine_surf_local( 4,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local(4,1)
!
      inod_refine_local(15) = inod_refine_edge_local( 3,1)
      inod_refine_local(16) = inod_refine_surf_local( 5,1)
!
      end subroutine copy_refined_nod_tri_n4
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n3(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
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
      inod_refine_local( 9) = inod_refine_surf_local( 5,1)
      inod_refine_local(10) = inod_refine_edge_local( 3,1)
      inod_refine_local(11) = inod_refine_surf_local( 4,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local( 2,1)
!
      inod_refine_local(15) = inod_refine_edge_local(11,1)
      inod_refine_local(16) = inod_refine_surf_local( 2,1)
!
      end subroutine copy_refined_nod_tri_n3
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n2(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
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
      inod_refine_local( 9) = inod_refine_surf_local( 5,1)
      inod_refine_local(10) = inod_refine_edge_local( 2,1)
      inod_refine_local(11) = inod_refine_surf_local( 2,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local( 1,1)
!
      inod_refine_local(15) = inod_refine_edge_local(10,1)
      inod_refine_local(16) = inod_refine_surf_local( 3,1)
!
      end subroutine copy_refined_nod_tri_n2
!
! ----------------------------------------------------------------------
!
      subroutine copy_refined_nod_tri_n1(inod_refine_local,             &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(16)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(3)
      inod_refine_local( 2) = inod_refine_nod_local(2)
      inod_refine_local( 3) = inod_refine_nod_local(6)
      inod_refine_local( 4) = inod_refine_nod_local(7)
      inod_refine_local( 5) = inod_refine_nod_local(4)
      inod_refine_local( 6) = inod_refine_nod_local(1)
      inod_refine_local( 7) = inod_refine_nod_local(5)
      inod_refine_local( 8) = inod_refine_nod_local(8)
!
      inod_refine_local( 9) = inod_refine_surf_local(5,1)
      inod_refine_local(10) = inod_refine_edge_local(1,1)
      inod_refine_local(11) = inod_refine_surf_local(3,1)
      inod_refine_local(12) = inod_refine_ele_local(1)
      inod_refine_local(13) = inod_refine_edge_local(4,1)
!
      inod_refine_local(15) = inod_refine_edge_local(9,1)
      inod_refine_local(16) = inod_refine_surf_local(1,1)
!
      end subroutine copy_refined_nod_tri_n1
!
! ----------------------------------------------------------------------
!
      end module copy_refined_nod_tri_node
