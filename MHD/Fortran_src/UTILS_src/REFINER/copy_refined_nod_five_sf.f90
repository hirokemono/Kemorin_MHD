!copy_refined_nod_five_sf.f90
!      module copy_refined_nod_five_sf
!
      module copy_refined_nod_five_sf
!
      use m_precision
!
      implicit none
!
      private :: s_copy_refined_nod_five_s6
      private :: s_copy_refined_nod_five_s5
      private :: s_copy_refined_nod_five_s4
      private :: s_copy_refined_nod_five_s3
      private :: s_copy_refined_nod_five_s2
      private :: s_copy_refined_nod_five_s1
!
!      subroutine copy_refined_nod_five_sf(inod_refine_local,           &
!     &          inod_refine_nod_local, inod_refine_ele_local,          &
!     &          inod_refine_surf_local, inod_refine_edge_local)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_refined_nod_five_sf(iflag_refine,               &
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
      if      (iflag_refine .eq. iflag_five_s6) then
        call s_copy_refined_nod_five_s6(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_five_s5) then
        call s_copy_refined_nod_five_s5(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_five_s4) then
        call s_copy_refined_nod_five_s4(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_five_s3) then
        call s_copy_refined_nod_five_s3(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_five_s2) then
        call s_copy_refined_nod_five_s2(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_five_s1) then
        call s_copy_refined_nod_five_s1(inod_refine_local,              &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      end if
!
      end subroutine s_copy_refined_nod_five_sf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_five_s6(48)            &
!     &        = (/  1,    4,   23,   22,   49,   52,   55,   54,       &
!     &              4,   16,   27,   23,   52,   64,   59,   55,       &
!     &             16,   13,   26,   27,   64,   61,   58,   59,       &
!     &             13,    1,   22,   26,   61,   49,   54,   58,       &
!     &             22,   23,   27,   26,   54,   55,   59,   58,       &
!     &              1,    4,   16,   13,   22,   23,   27,   26/)
!
      subroutine s_copy_refined_nod_five_s6(inod_refine_local,          &
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
      inod_refine_local(22) = inod_refine_ele_local(1)
      inod_refine_local(23) = inod_refine_ele_local(2)
!
      inod_refine_local(26) = inod_refine_ele_local(4)
      inod_refine_local(27) = inod_refine_ele_local(3)
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
      end subroutine s_copy_refined_nod_five_s6
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_five_s5(48)            &
!     &        = (/  1,    4,    7,    6,   49,   52,   39,   38,       &
!     &              4,   16,   11,    7,   52,   64,   43,   39,       &
!     &             16,   13,   10,   11,   64,   61,   42,   43,       &
!     &             13,    1,    6,   10,   61,   49,   38,   42,       &
!     &              6,    7,   11,   10,   38,   39,   43,   42,       &
!     &             38,   39,   43,   42,   49,   52,   64,   61/)
!
      subroutine s_copy_refined_nod_five_s5(inod_refine_local,          &
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
      inod_refine_local(38) = inod_refine_ele_local(5)
      inod_refine_local(39) = inod_refine_ele_local(6)
!
      inod_refine_local(42) = inod_refine_ele_local(8)
      inod_refine_local(43) = inod_refine_ele_local(7)
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_five_s5
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_five_s4(48)            &
!     &        = (/ 1,    4,   16,   13,   22,   23,   31,   30,        &
!     &             1,   22,   30,   13,   49,   38,   46,   61,        &
!     &            23,    4,   16,   31,   39,   52,   64,   47,        &
!     &            38,   39,   47,   46,   49,   52,   64,   61,        &
!     &            22,   23,   31,   30,   38,   39,   47,   46,        &
!     &             1,    4,   23,   22,   49,   52,   39,   38/)
!
      subroutine s_copy_refined_nod_five_s4(inod_refine_local,          &
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
      inod_refine_local(22) = inod_refine_ele_local(1)
      inod_refine_local(23) = inod_refine_ele_local(2)
!
      inod_refine_local(30) = inod_refine_surf_local(4,1)
      inod_refine_local(31) = inod_refine_surf_local(4,4)
!
      inod_refine_local(38) = inod_refine_ele_local(5)
      inod_refine_local(39) = inod_refine_ele_local(6)
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
      end subroutine s_copy_refined_nod_five_s4
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_five_s3(48)            &
!     &        = (/ 1,    4,   16,   13,   18,   19,   27,   26,        &
!     &             1,   18,   26,   13,   49,   34,   42,   61,        &
!     &            19,    4,   16,   27,   35,   52,   64,   43,        &
!     &            34,   35,   43,   42,   49,   52,   64,   61,        &
!     &            18,   19,   27,   26,   34,   35,   43,   42,        &
!     &            26,   27,   16,   13,   42,   43,   64,   61/)
!
      subroutine s_copy_refined_nod_five_s3(inod_refine_local,          &
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
      inod_refine_local(18) = inod_refine_surf_local(3,1)
      inod_refine_local(19) = inod_refine_surf_local(3,2)
!
      inod_refine_local(26) = inod_refine_ele_local(4)
      inod_refine_local(27) = inod_refine_ele_local(3)
!
      inod_refine_local(34) = inod_refine_surf_local(3,4)
      inod_refine_local(35) = inod_refine_surf_local(3,3)
!
      inod_refine_local(42) = inod_refine_ele_local(8)
      inod_refine_local(43) = inod_refine_ele_local(7)
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_five_s3
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_five_s2(48)            &
!     &        = (/ 1,    4,   16,   13,   22,   24,   28,   26,        &
!     &             1,    4,   24,   22,   49,   52,   40,   38,        &
!     &            26,   28,   16,   13,   42,   44,   64,   61,        &
!     &            38,   40,   44,   42,   49,   52,   64,   61,        &
!     &            22,   24,   28,   26,   38,   40,   44,   42,        &
!     &             1,   22,   26,   13,   49,   38,   42,   61/)
!
      subroutine s_copy_refined_nod_five_s2(inod_refine_local,          &
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
      inod_refine_local(22) = inod_refine_ele_local(1)
      inod_refine_local(24) = inod_refine_surf_local(2,1)
!
      inod_refine_local(26) = inod_refine_ele_local(4)
      inod_refine_local(28) = inod_refine_surf_local(2,2)
!
      inod_refine_local(38) = inod_refine_ele_local(5)
      inod_refine_local(40) = inod_refine_surf_local(2,4)
!
      inod_refine_local(42) = inod_refine_ele_local(8)
      inod_refine_local(44) = inod_refine_surf_local(2,3)
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_five_s2
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_five_s1(48)            &
!     &        = (/ 1,    4,   16,   13,   21,   23,   27,   25,        &
!     &             1,    4,   23,   21,   49,   52,   39,   37,        &
!     &            25,   27,   16,   13,   41,   43,   64,   61,        &
!     &            37,   39,   43,   41,   49,   52,   64,   61,        &
!     &            21,   23,   27,   25,   37,   39,   43,   41,        &
!     &            23,    4,   16,   27,   39,   52,   64,   43/)
!
      subroutine s_copy_refined_nod_five_s1(inod_refine_local,          &
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
      inod_refine_local(21) = inod_refine_surf_local(1,1)
      inod_refine_local(23) = inod_refine_ele_local(2)
!
      inod_refine_local(25) = inod_refine_surf_local(1,4)
      inod_refine_local(27) = inod_refine_ele_local(3)
!
      inod_refine_local(37) = inod_refine_surf_local(1,2)
      inod_refine_local(39) = inod_refine_ele_local(6)
!
      inod_refine_local(41) = inod_refine_surf_local(1,3)
      inod_refine_local(43) = inod_refine_ele_local(7)
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine s_copy_refined_nod_five_s1
!
! ----------------------------------------------------------------------
!
      end module copy_refined_nod_five_sf
