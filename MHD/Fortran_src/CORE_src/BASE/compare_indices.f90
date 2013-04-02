!
!      module compare_indices
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine check_4_on_3(i1, i2, i3, j1, j2, j3, j4, iflag)
!      subroutine check_3_on_2(i1, i2, j1, j2, j3, iflag)
!      subroutine check_2_on_1(i1, j1, j2, iflag)
!
!
      module compare_indices
!
      use m_precision
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine check_4_on_3(i1, i2, i3, j1, j2, j3, j4, iflag)
!
      integer (kind = kint), intent(in) :: i1, i2, i3, j1, j2, j3, j4
      integer (kind = kint), intent(inout) :: iflag
!
!
      iflag = 0
      if ( i1 .eq. j1 ) then
       call check_3_on_2(i2, i3, j2, j3, j4, iflag)
      else if ( i1 .eq. j2 ) then
       call check_3_on_2(i2, i3, j3, j4, j1, iflag)
      else if ( i1 .eq. j3 ) then
       call check_3_on_2(i2, i3, j4, j1, j2, iflag)
      else if ( i1 .eq. j4 ) then
       call check_3_on_2(i2, i3, j1, j2, j3, iflag)
      end if
!
      end subroutine check_4_on_3
!
!------------------------------------------------------------------
!
      subroutine check_3_on_2(i1, i2, j1, j2, j3, iflag)
!
      integer (kind = kint), intent(in) :: i1, i2, j1, j2, j3
      integer (kind = kint), intent(inout) :: iflag
!
      if ( i1 .eq. j1 ) then
       call check_2_on_1(i2, j2, j3, iflag)
      else if ( i1 .eq. j2 ) then
       call check_2_on_1(i2, j3, j1, iflag)
      else if ( i1 .eq. j3 ) then
       call check_2_on_1(i2, j1, j2, iflag)
      end if
!
      end subroutine check_3_on_2
!
!------------------------------------------------------------------
!
      subroutine check_2_on_1(i1, j1, j2, iflag)
!
      integer (kind = kint), intent(in) :: i1, j1, j2
      integer (kind = kint), intent(inout) :: iflag
!
!
      if ( i1 .eq. j1 ) then
       iflag = 1
      else if ( i1 .eq. j2 ) then
       iflag = 1
      end if
!
      return
      end subroutine check_2_on_1
!
!------------------------------------------------------------------
!
      end module compare_indices
