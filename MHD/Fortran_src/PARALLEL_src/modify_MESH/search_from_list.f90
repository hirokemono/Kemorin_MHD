!>@file   search_from_list.f90
!!@brief  module search_from_list
!!
!!@author H. Matsui
!!@date Programmed on Nov.., 2020
!
!>@brief  search index from integer list
!!
!!@verbatim
!!      integer(kind = kint) function search_from_list_data             &
!!     &                   (i_target, ist, ied, num, input_list)
!!      integer(kind = kint) function search_from_sorted_data           &
!!     &                   (i_target, ist, ied, num, input_list)
!!@endverbatim
!
      module search_from_list
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_from_list_data               &
     &                   (i_target, ist, ied, num, input_list)
!
      integer(kind = kint), intent(in) :: i_target
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: input_list(num)
!
      integer(kind = kint) :: jnum, iflag
!
!
      iflag = ist - 1
      do jnum = ist, ied
        if(i_target .eq. input_list(jnum)) then
          iflag = jnum
          exit
        end if
      end do
      search_from_list_data = iflag
!
      end function search_from_list_data
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_from_sorted_data             &
     &                   (i_target, ist, ied, num, input_list)
!
      integer(kind = kint), intent(in) :: i_target
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: input_list(num)
!
      integer(kind = kint) :: jst, jed, jnum, iflag
!
!
      jst = ist
      jed = ied
      jnum = (jst+jed) / 2
      do 
        if(i_target .eq. input_list(jnum)) then
          iflag = jnum
          exit
        else if(jst .eq. jed) then
          iflag = ist - 1
          exit
        else if((jed-jst) .eq. 1) then
          iflag = ist - 1
          if(i_target .eq. input_list(jst)) iflag = jst
          if(i_target .eq. input_list(jed)) iflag = jed
          exit
        else if(i_target .lt. input_list(jnum)) then
          jed = jnum
          jnum = (jst+jed) / 2
        else
          jst = jnum
          jnum = (jst+jed) / 2
        end if
      end do
      search_from_sorted_data = iflag
!
      end function search_from_sorted_data
!
! ----------------------------------------------------------------------
!
      end module search_from_list
