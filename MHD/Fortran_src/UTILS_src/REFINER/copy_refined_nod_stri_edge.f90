!copy_refined_nod_stri_edge.f90
!      module copy_refined_nod_stri_edge
!
      module copy_refined_nod_stri_edge
!
      use m_precision
!
      implicit none
!
!
      private :: copy_refined_nod_stri_edge_e1
      private :: copy_refined_nod_stri_edge_e2
      private :: copy_refined_nod_stri_edge_e3
      private :: copy_refined_nod_stri_edge_e4
      private :: copy_refined_nod_stri_edge_e5
      private :: copy_refined_nod_stri_edge_e6
      private :: copy_refined_nod_stri_edge_e7
      private :: copy_refined_nod_stri_edge_e8
      private :: copy_refined_nod_stri_edge_e9
      private :: copy_refined_nod_stri_edge_e10
      private :: copy_refined_nod_stri_edge_e11
      private :: copy_refined_nod_stri_edge_e12
!
!      subroutine s_copy_refined_nod_stri_edge(iflag_refine,            &
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
      subroutine s_copy_refined_nod_stri_edge(iflag_refine,             &
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
      integer(kind = kint), intent(inout) :: inod_refine_local(64)
!
!
      if      (iflag_refine .eq. iflag_stri_e1) then
        call copy_refined_nod_stri_edge_e1(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e2) then
        call copy_refined_nod_stri_edge_e2(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e3) then
        call copy_refined_nod_stri_edge_e3(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e4) then
        call copy_refined_nod_stri_edge_e4(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e5) then
        call copy_refined_nod_stri_edge_e5(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e6) then
        call copy_refined_nod_stri_edge_e6(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e7) then
        call copy_refined_nod_stri_edge_e7(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e8) then
        call copy_refined_nod_stri_edge_e8(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e9) then
        call copy_refined_nod_stri_edge_e9(inod_refine_local,           &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e10) then
        call copy_refined_nod_stri_edge_e10(inod_refine_local,          &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e11) then
        call copy_refined_nod_stri_edge_e11(inod_refine_local,          &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      else if (iflag_refine .eq. iflag_stri_e12) then
        call copy_refined_nod_stri_edge_e12(inod_refine_local,          &
     &          inod_refine_nod_local, inod_refine_ele_local,           &
     &          inod_refine_surf_local, inod_refine_edge_local)
!
      end if
!
      end subroutine s_copy_refined_nod_stri_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e1(40)          &
!     &        = (/  1,    2,   10,   13,   49,   34,   42,   61,       &
!     &              2,    3,   11,   10,   34,   35,   43,   42,       &
!     &              3,    4,   16,   11,   35,   52,   64,   43,       &
!     &             10,   11,   16,   13,   42,   43,   64,   61,       &
!     &             34,   35,   43,   42,   49,   52,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e1(inod_refine_local,       &
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
      inod_refine_local( 2) = inod_refine_edge_local(1,1)
      inod_refine_local( 3) = inod_refine_edge_local(1,2)
      inod_refine_local( 4) = inod_refine_nod_local(2)
!
      inod_refine_local(10) = inod_refine_surf_local(5,2)
      inod_refine_local(11) = inod_refine_surf_local(5,3)
!
      inod_refine_local(13) = inod_refine_nod_local(4)
      inod_refine_local(16) = inod_refine_nod_local(3)
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
      end subroutine copy_refined_nod_stri_edge_e1
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e2(40)          &
!     &        = (/  1,    4,    8,    6,   49,   52,   40,   38,       &
!     &              1,    6,   10,   13,   49,   38,   42,   61,       &
!     &              6,    8,   12,   10,   38,   40,   44,   42,       &
!     &             10,   12,   16,   13,   42,   44,   64,   61,       &
!     &             38,   40,   44,   42,   49,   52,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e2(inod_refine_local,       &
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
      inod_refine_local( 8) = inod_refine_edge_local(2,1)
!
      inod_refine_local(10) = inod_refine_surf_local(5,2)
      inod_refine_local(12) = inod_refine_edge_local(2,2)
!
      inod_refine_local(13) = inod_refine_nod_local(4)
      inod_refine_local(16) = inod_refine_nod_local(3)
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
      end subroutine copy_refined_nod_stri_edge_e2
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e3(40)          &
!     &        = (/  1,    4,    7,    6,   49,   52,   39,   38,       &
!     &              1,    6,   14,   13,   49,   38,   46,   61,       &
!     &              6,    7,   15,   14,   38,   39,   47,   46,       &
!     &              7,    4,   16,   15,   39,   52,   64,   47,       &
!     &             38,   39,   47,   46,   49,   52,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e3(inod_refine_local,       &
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
      inod_refine_local(13) = inod_refine_nod_local(4)
      inod_refine_local(14) = inod_refine_edge_local(3,1)
      inod_refine_local(15) = inod_refine_edge_local(3,2)
      inod_refine_local(16) = inod_refine_nod_local(3)
!
      inod_refine_local(38) = inod_refine_ele_local(5)
      inod_refine_local(39) = inod_refine_ele_local(6)
!
      inod_refine_local(46) = inod_refine_surf_local(4,2)
      inod_refine_local(47) = inod_refine_surf_local(4,3)
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine copy_refined_nod_stri_edge_e3
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e4(40)          &
!     &        = (/  1,    4,    7,    5,   49,   52,   39,   37,       &
!     &              5,    7,   11,    9,   37,   39,   43,   41,       &
!     &              7,    4,   16,   11,   39,   52,   64,   43,       &
!     &              9,   11,   16,   13,   41,   43,   64,   61,       &
!     &             37,   39,   43,   41,   49,   52,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e4(inod_refine_local,       &
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
      inod_refine_local( 5) = inod_refine_edge_local(4,1)
      inod_refine_local( 7) = inod_refine_surf_local(5,4)
!
      inod_refine_local( 9) = inod_refine_edge_local(4,2)
      inod_refine_local(11) = inod_refine_surf_local(5,3)
!
      inod_refine_local(13) = inod_refine_nod_local(4)
      inod_refine_local(16) = inod_refine_nod_local(3)
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
      end subroutine copy_refined_nod_stri_edge_e4
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e5(40)          &
!     &        = (/  1,    4,   16,   13,   18,   19,   27,   26,       &
!     &              1,   18,   26,   13,   49,   50,   58,   61,       &
!     &             18,   19,   27,   26,   50,   51,   59,   58,       &
!     &             19,    4,   16,   27,   51,   52,   64,   59,       &
!     &             26,   27,   16,   13,   58,   59,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e5(inod_refine_local,       &
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
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(50) = inod_refine_edge_local(5,1)
      inod_refine_local(51) = inod_refine_edge_local(5,2)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(58) = inod_refine_surf_local(6,4)
      inod_refine_local(59) = inod_refine_surf_local(6,3)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine copy_refined_nod_stri_edge_e5
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e6(40)          &
!     &        = (/  1,    4,   16,   13,   22,   24,   28,   26,       &
!     &              1,    4,   24,   22,   49,   52,   56,   54,       &
!     &              1,   22,   26,   13,   49,   54,   58,   61,       &
!     &             22,   24,   28,   26,   54,   56,   60,   58,       &
!     &             26,   28,   16,   13,   58,   60,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e6(inod_refine_local,       &
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
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(54) = inod_refine_surf_local(6,1)
      inod_refine_local(56) = inod_refine_edge_local(6,1)
!
      inod_refine_local(58) = inod_refine_surf_local(6,4)
      inod_refine_local(60) = inod_refine_edge_local(6,2)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine copy_refined_nod_stri_edge_e6
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e7(40)          &
!     &        = (/  1,    4,   16,   13,   22,   23,   31,   30,       &
!     &              1,    4,   23,   22,   49,   52,   55,   54,       &
!     &              1,   22,   30,   13,   49,   54,   62,   61,       &
!     &             22,   23,   31,   30,   54,   55,   63,   62,       &
!     &             23,    4,   16,   31,   55,   52,   64,   63/)
!
      subroutine copy_refined_nod_stri_edge_e7(inod_refine_local,       &
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
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(54) = inod_refine_surf_local(6,1)
      inod_refine_local(55) = inod_refine_surf_local(6,2)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(62) = inod_refine_edge_local(7,1)
      inod_refine_local(63) = inod_refine_edge_local(7,2)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine copy_refined_nod_stri_edge_e7
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e8(40)          &
!     &        = (/  1,    4,   16,   13,   21,   23,   27,   25,       &
!     &              1,    4,   23,   21,   49,   52,   55,   53,       &
!     &             21,   23,   27,   25,   53,   55,   59,   57,       &
!     &             23,    4,   16,   27,   55,   52,   64,   59,       &
!     &             25,   27,   16,   13,   57,   59,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e8(inod_refine_local,       &
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
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(53) = inod_refine_edge_local(8,1)
      inod_refine_local(55) = inod_refine_surf_local(6,2)
!
      inod_refine_local(57) = inod_refine_edge_local(8,2)
      inod_refine_local(59) = inod_refine_surf_local(6,3)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine copy_refined_nod_stri_edge_e8
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e9(40)          &
!     &        = (/  1,    4,   16,   13,   17,   19,   27,   25,       &
!     &             17,   19,   27,   25,   33,   35,   43,   41,       &
!     &             19,    4,   16,   27,   35,   52,   64,   43,       &
!     &             25,   27,   16,   13,   41,   43,   64,   61,       &
!     &             33,   35,   43,   41,   49,   52,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e9(inod_refine_local,       &
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
      inod_refine_local(17) = inod_refine_edge_local(9,1)
      inod_refine_local(19) = inod_refine_surf_local(3,2)
!
      inod_refine_local(25) = inod_refine_surf_local(1,4)
      inod_refine_local(27) = inod_refine_ele_local(3)
!
      inod_refine_local(33) = inod_refine_edge_local(9,2)
      inod_refine_local(35) = inod_refine_surf_local(3,3)
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
      end subroutine copy_refined_nod_stri_edge_e9
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e10(40)         &
!     &        = (/  1,    4,   16,   13,   18,   20,   28,   26,       &
!     &              1,   18,   26,   13,   49,   34,   42,   61,       &
!     &             18,   20,   28,   26,   34,   36,   44,   42,       &
!     &             26,   28,   16,   13,   42,   44,   64,   61,       &
!     &             34,   36,   44,   42,   49,   52,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e10(inod_refine_local,      &
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
      inod_refine_local(20) = inod_refine_edge_local(10,1)
!
      inod_refine_local(26) = inod_refine_ele_local(4)
      inod_refine_local(28) = inod_refine_surf_local(2,2)
!
      inod_refine_local(34) = inod_refine_surf_local(3,4)
      inod_refine_local(36) = inod_refine_edge_local(10,2)
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
      end subroutine copy_refined_nod_stri_edge_e10
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e11(40)         &
!     &        = (/  1,    4,   16,   13,   22,   24,   32,   30,       &
!     &              1,    4,   24,   22,   49,   52,   40,   38,       &
!     &              1,   22,   30,   13,   49,   38,   46,   61,       &
!     &             22,   24,   32,   30,   38,   40,   48,   46,       &
!     &             38,   40,   48,   46,   49,   52,   64,   61/)
!
      subroutine copy_refined_nod_stri_edge_e11(inod_refine_local,      &
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
      inod_refine_local(30) = inod_refine_surf_local(4,1)
      inod_refine_local(32) = inod_refine_edge_local(11,1)
!
      inod_refine_local(38) = inod_refine_ele_local(5)
      inod_refine_local(40) = inod_refine_surf_local(2,4)
!
      inod_refine_local(46) = inod_refine_surf_local(4,2)
      inod_refine_local(48) = inod_refine_edge_local(11,2)
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine copy_refined_nod_stri_edge_e11
!
! ----------------------------------------------------------------------
!
!      integer(kind = kint), parameter :: ie_new_sm_tri_e12(40)         &
!     &        = (/  1,    4,   16,   13,   21,   23,   31,   29,       &
!     &              1,    4,   23,   21,   49,   52,   39,   37,       &
!     &             21,   23,   31,   29,   37,   39,   47,   45,       &
!     &             23,    4,   16,   31,   39,   52,   64,   47,       &
!     &             37,   39,   47,   45,   49,   52,   64,   61/)
!
!
      subroutine copy_refined_nod_stri_edge_e12(inod_refine_local,      &
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
      inod_refine_local(29) = inod_refine_edge_local(12,1)
      inod_refine_local(31) = inod_refine_surf_local(4,4)
!
      inod_refine_local(37) = inod_refine_surf_local(1,2)
      inod_refine_local(39) = inod_refine_ele_local(6)
!
      inod_refine_local(45) = inod_refine_edge_local(12,2)
      inod_refine_local(47) = inod_refine_surf_local(4,3)
!
      inod_refine_local(49) = inod_refine_nod_local(5)
      inod_refine_local(52) = inod_refine_nod_local(6)
!
      inod_refine_local(61) = inod_refine_nod_local(8)
      inod_refine_local(64) = inod_refine_nod_local(7)
!
      end subroutine copy_refined_nod_stri_edge_e12
!
! ----------------------------------------------------------------------
!
      end module copy_refined_nod_stri_edge
