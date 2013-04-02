!copy_refined_nod_dbl.f90
!      module copy_refined_nod_dbl
!
      module copy_refined_nod_dbl
!
!      Written by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit none
!
!      subroutine s_copy_refined_nod_dbl_full(inod_refine_local,        &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!      subroutine s_copy_refined_nod_dbl_z(inod_refine_local,           &
!     &          inod_refine_nod_local, inod_refine_surf_local,         &
!     &          inod_refine_edge_local)
!      subroutine s_copy_refined_nod_dbl_y(inod_refine_local,           &
!     &          inod_refine_nod_local, inod_refine_surf_local,         &
!     &          inod_refine_edge_local)
!      subroutine s_copy_refined_nod_dbl_x(inod_refine_local,           &
!     &          inod_refine_nod_local, inod_refine_surf_local,         &
!     &          inod_refine_edge_local)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_dbl_full(inod_refine_local,         &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(1)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(27)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 2) = inod_refine_edge_local(1,1)
      inod_refine_local( 3) = inod_refine_nod_local(2)
!
      inod_refine_local( 4) = inod_refine_edge_local(4,1)
      inod_refine_local( 5) = inod_refine_surf_local(5,1)
      inod_refine_local( 6) = inod_refine_edge_local(2,1)
!
      inod_refine_local( 7) = inod_refine_nod_local(4)
      inod_refine_local( 8) = inod_refine_edge_local(3,1)
      inod_refine_local( 9) = inod_refine_nod_local(3)
!
!
      inod_refine_local(10) = inod_refine_edge_local(9,1)
      inod_refine_local(11) = inod_refine_surf_local(3,1)
      inod_refine_local(12) = inod_refine_edge_local(10,1)
!
      inod_refine_local(13) = inod_refine_surf_local(1,1)
      inod_refine_local(14) = inod_refine_ele_local(1)
      inod_refine_local(15) = inod_refine_surf_local(2,1)
!
      inod_refine_local(16) = inod_refine_edge_local(12,1)
      inod_refine_local(17) = inod_refine_surf_local(4,1)
      inod_refine_local(18) = inod_refine_edge_local(11,1)
!
!
      inod_refine_local(19) = inod_refine_nod_local(5)
      inod_refine_local(20) = inod_refine_edge_local(5,1)
      inod_refine_local(21) = inod_refine_nod_local(6)
!
      inod_refine_local(22) = inod_refine_edge_local(8,1)
      inod_refine_local(23) = inod_refine_surf_local(6,1)
      inod_refine_local(24) = inod_refine_edge_local(6,1)
!
      inod_refine_local(25) = inod_refine_nod_local(8)
      inod_refine_local(26) = inod_refine_edge_local(7,1)
      inod_refine_local(27) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_dbl_full
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_dbl_z(inod_refine_local,            &
     &          inod_refine_nod_local, inod_refine_surf_local,          &
     &          inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(27)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 2) = inod_refine_edge_local(1,1)
      inod_refine_local( 3) = inod_refine_nod_local(2)
!
      inod_refine_local( 4) = inod_refine_edge_local(4,1)
      inod_refine_local( 5) = inod_refine_surf_local(5,1)
      inod_refine_local( 6) = inod_refine_edge_local(2,1)
!
      inod_refine_local( 7) = inod_refine_nod_local(4)
      inod_refine_local( 8) = inod_refine_edge_local(3,1)
      inod_refine_local( 9) = inod_refine_nod_local(3)
!
!
      inod_refine_local(19) = inod_refine_nod_local(5)
      inod_refine_local(20) = inod_refine_edge_local(5,1)
      inod_refine_local(21) = inod_refine_nod_local(6)
!
      inod_refine_local(22) = inod_refine_edge_local(8,1)
      inod_refine_local(23) = inod_refine_surf_local(6,1)
      inod_refine_local(24) = inod_refine_edge_local(6,1)
!
      inod_refine_local(25) = inod_refine_nod_local(8)
      inod_refine_local(26) = inod_refine_edge_local(7,1)
      inod_refine_local(27) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_dbl_z
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_dbl_y(inod_refine_local,            &
     &          inod_refine_nod_local, inod_refine_surf_local,          &
     &          inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(27)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 2) = inod_refine_edge_local(1,1)
      inod_refine_local( 3) = inod_refine_nod_local(2)
!
      inod_refine_local( 7) = inod_refine_nod_local(4)
      inod_refine_local( 8) = inod_refine_edge_local(3,1)
      inod_refine_local( 9) = inod_refine_nod_local(3)
!
!
      inod_refine_local(10) = inod_refine_edge_local(9,1)
      inod_refine_local(11) = inod_refine_surf_local(3,1)
      inod_refine_local(12) = inod_refine_edge_local(10,1)
!
      inod_refine_local(16) = inod_refine_edge_local(12,1)
      inod_refine_local(17) = inod_refine_surf_local(4,1)
      inod_refine_local(18) = inod_refine_edge_local(11,1)
!
!
      inod_refine_local(19) = inod_refine_nod_local(5)
      inod_refine_local(20) = inod_refine_edge_local(5,1)
      inod_refine_local(21) = inod_refine_nod_local(6)
!
      inod_refine_local(25) = inod_refine_nod_local(8)
      inod_refine_local(26) = inod_refine_edge_local(7,1)
      inod_refine_local(27) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_dbl_y
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_dbl_x(inod_refine_local,            &
     &          inod_refine_nod_local, inod_refine_surf_local,          &
     &          inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,1)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,1)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(27)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 3) = inod_refine_nod_local(2)
!
      inod_refine_local( 4) = inod_refine_edge_local(4,1)
      inod_refine_local( 6) = inod_refine_edge_local(2,1)
!
      inod_refine_local( 7) = inod_refine_nod_local(4)
      inod_refine_local( 9) = inod_refine_nod_local(3)
!
!
      inod_refine_local(10) = inod_refine_edge_local(9,1)
      inod_refine_local(12) = inod_refine_edge_local(10,1)
!
      inod_refine_local(13) = inod_refine_surf_local(1,1)
      inod_refine_local(15) = inod_refine_surf_local(2,1)
!
      inod_refine_local(16) = inod_refine_edge_local(12,1)
      inod_refine_local(18) = inod_refine_edge_local(11,1)
!
!
      inod_refine_local(19) = inod_refine_nod_local(5)
      inod_refine_local(21) = inod_refine_nod_local(6)
!
      inod_refine_local(22) = inod_refine_edge_local(8,1)
      inod_refine_local(24) = inod_refine_edge_local(6,1)
!
      inod_refine_local(25) = inod_refine_nod_local(8)
      inod_refine_local(27) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_dbl_x
!
! ----------------------------------------------------------------------
!
      end module copy_refined_nod_dbl
