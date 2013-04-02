!copy_refined_nod_for_five.f90
!      module copy_refined_nod_for_five
!
      module copy_refined_nod_for_five
!
      use m_precision
!
      implicit none
!
      private :: s_copy_refined_nod_five_wz
      private :: s_copy_refined_nod_five_wy
      private :: s_copy_refined_nod_five_wx
!
!      subroutine s_copy_refined_nod_five(iflag_refine,                 &
!     &          inod_refine_local, inod_refine_nod_local,              &
!     &          inod_refine_ele_local, inod_refine_surf_local,         &
!     &          inod_refine_edge_local)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_five(iflag_refine,                  &
     &          inod_refine_local, inod_refine_nod_local,               &
     &          inod_refine_ele_local, inod_refine_surf_local,          &
     &          inod_refine_edge_local)
!
      use m_refine_flag_parameters
!
      integer(kind = kint) :: iflag_refine
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(64)
!
!
      if      (iflag_refine .eq. iflag_five_z) then
        call s_copy_refined_nod_five_wz(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_five_y) then
        call s_copy_refined_nod_five_wy(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_five_x) then
        call s_copy_refined_nod_five_wx(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      end if
!
      end subroutine s_copy_refined_nod_five
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_five_wz(inod_refine_local,          &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(64)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 4) = inod_refine_nod_local(2)
!
      inod_refine_local( 6) = inod_refine_surf_local(5,1)
      inod_refine_local( 7) = inod_refine_surf_local(5,4)
!
      inod_refine_local(10) = inod_refine_surf_local(5,2)
      inod_refine_local(11) = inod_refine_surf_local(5,3)
!
      inod_refine_local(13) = inod_refine_nod_local(4)
      inod_refine_local(16) = inod_refine_nod_local(3)
!
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(54) = inod_refine_surf_local(6,1)
      inod_refine_local(55) = inod_refine_surf_local(6,2)
!
      inod_refine_local(58) = inod_refine_surf_local(6,4)
      inod_refine_local(59) = inod_refine_surf_local(6,3)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_five_wz
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_five_wy(inod_refine_local,          &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(64)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 4) = inod_refine_nod_local(2)
!
      inod_refine_local(13) = inod_refine_nod_local(4)
      inod_refine_local(16) = inod_refine_nod_local(3)
!
!
      inod_refine_local(18) = inod_refine_surf_local(3,1)
      inod_refine_local(19) = inod_refine_surf_local(3,2)
!
      inod_refine_local(30) = inod_refine_surf_local(4,1)
      inod_refine_local(31) = inod_refine_surf_local(4,4)
!
!
      inod_refine_local(34) = inod_refine_surf_local(3,4)
      inod_refine_local(35) = inod_refine_surf_local(3,3)
!
      inod_refine_local(46) = inod_refine_surf_local(4,2)
      inod_refine_local(47) = inod_refine_surf_local(4,3)
!
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_five_wy
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_five_wx(inod_refine_local,         &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      integer(kind = kint), intent(in) :: inod_refine_nod_local(8)
      integer(kind = kint), intent(in) :: inod_refine_ele_local(8)
      integer(kind = kint), intent(in) :: inod_refine_surf_local(6,4)
      integer(kind = kint), intent(in) :: inod_refine_edge_local(12,2)
!
      integer(kind = kint), intent(inout) :: inod_refine_local(64)
!
!
      inod_refine_local( 1) = inod_refine_nod_local(1)
      inod_refine_local( 4) = inod_refine_nod_local(2)
!
      inod_refine_local(13) = inod_refine_nod_local(4)
      inod_refine_local(16) = inod_refine_nod_local(3)
!
!
      inod_refine_local(21) = inod_refine_surf_local(1,1)
      inod_refine_local(24) = inod_refine_surf_local(2,1)
!
      inod_refine_local(25) = inod_refine_surf_local(1,4)
      inod_refine_local(28) = inod_refine_surf_local(2,2)
!
!
      inod_refine_local(37) = inod_refine_surf_local(1,2)
      inod_refine_local(40) = inod_refine_surf_local(2,4)
!
      inod_refine_local(41) = inod_refine_surf_local(1,3)
      inod_refine_local(44) = inod_refine_surf_local(2,3)
!
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_five_wx
!
! ----------------------------------------------------------------------
!
      end module copy_refined_nod_for_five
