!>@file   append_control_items.f90
!!@brief  module append_control_items
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine append_control_item_real(read_r1, array_r1)
!!        type(read_real_item), intent(in) ::    read_r1
!!        type(ctl_array_real), intent(inout) :: array_r1
!!      subroutine append_control_item_r2(read_r2, array_r2)
!!        type(read_real2_item), intent(in) ::    read_r2
!!        type(ctl_array_r2), intent(inout) :: array_r2
!!      subroutine append_control_item_int(read_i1, array_i1)
!!        type(read_integer_item), intent(in) ::    read_i1
!!        type(ctl_array_int), intent(inout) :: array_i1
!!      subroutine append_control_item_c1(read_c1, array_c1)
!!        type(read_character_item), intent(in) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!@endverbatim
!!
      module append_control_items
!
      use m_precision
!
      use t_control_elements
      use t_read_control_arrays
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_real(read_r1, array_r1)
!
      type(read_real_item), intent(in) ::    read_r1
      type(ctl_array_real), intent(inout) :: array_r1
!
!
      array_r1%icou = array_r1%icou + read_r1%iflag
      array_r1%vect(array_r1%num) = read_r1%realvalue
!
      end subroutine append_control_item_real
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_r2(read_r2, array_r2)
!
      type(read_real2_item), intent(in) ::    read_r2
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      array_r2%icou = array_r2%icou + read_r2%iflag
      array_r2%vec1(array_r2%num) = read_r2%realvalue(1)
      array_r2%vec2(array_r2%num) = read_r2%realvalue(2)
!
      end subroutine append_control_item_r2
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_int(read_i1, array_i1)
!
      type(read_integer_item), intent(in) ::    read_i1
      type(ctl_array_int), intent(inout) :: array_i1
!
!
      array_i1%icou = array_i1%icou + read_i1%iflag
      array_i1%ivec(array_i1%num) = read_i1%intvalue
!
      end subroutine append_control_item_int
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c1(read_c1, array_c1)
!
      type(read_character_item), intent(in) ::    read_c1
      type(ctl_array_chara), intent(inout) :: array_c1
!
!
      array_c1%icou = array_c1%icou + read_c1%iflag
      array_c1%c_tbl(array_c1%num) = read_c1%charavalue
!
      end subroutine append_control_item_c1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module append_control_items
