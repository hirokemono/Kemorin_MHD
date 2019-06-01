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
!!      subroutine append_control_item_r3(read_r3, array_r3)
!!        type(read_real3_item), intent(in) ::    read_r3
!!        type(ctl_array_r3), intent(inout) :: array_r3
!!      subroutine append_control_item_int(read_i1, array_i1)
!!        type(read_integer_item), intent(in) ::    read_i1
!!        type(ctl_array_int), intent(inout) :: array_i1
!!      subroutine append_control_item_i2(read_i2, array_i2)
!!        type(read_int2_item), intent(in) ::    read_i2
!!        type(ctl_array_i2), intent(inout) :: array_i2
!!      subroutine append_control_item_c1(read_c1, array_c1)
!!        type(read_character_item), intent(in) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!      subroutine append_control_item_c_r(read_cr, array_cr)
!!        type(read_chara_real_item), intent(in) ::    read_cr
!!        type(ctl_array_cr), intent(inout) :: array_cr
!!      subroutine append_control_item_c_i(read_ci, array_ci)
!!        type(read_chara_int_item), intent(in) ::    read_ci
!!        type(ctl_array_ci), intent(inout) :: array_ci
!!      subroutine append_control_item_c_r2(read_cr2, array_cr2)
!!        type(read_chara_real2_item), intent(in) ::    read_cr2
!!        type(ctl_array_cr2), intent(inout) :: array_cr2
!!      subroutine append_control_item_c2_r(read_c2r, array_c2r)
!!        type(read_chara2_real_item), intent(in) ::    read_c2r
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!      subroutine append_control_item_i_c_r(read_icr, array_icr)
!!        type(read_int_chara_real_item), intent(in) ::    read_icr
!!        type(ctl_array_icr), intent(inout) :: array_icr
!!      subroutine append_control_item_i_r(read_ir, array_ir)
!!        type(read_int_real_item), intent(in) ::    read_ir
!!        type(ctl_array_ir), intent(inout) :: array_ir
!!      subroutine append_control_item_i2_r(read_i2r, array_i2r)
!!        type(read_int2_real_item), intent(in) ::    read_i2r
!!        type(ctl_array_i2r), intent(inout) :: array_i2r
!!      subroutine append_control_item_i2_r2(read_i2r2, array_i2r2)
!!        type(read_int2_real2_item), intent(in) ::    read_i2r2
!!        type(ctl_array_i2r2), intent(inout) :: array_i2r2
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
      subroutine append_control_item_r3(read_r3, array_r3)
!
      type(read_real3_item), intent(in) ::    read_r3
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      array_r3%icou = array_r3%icou + read_r3%iflag
      array_r3%vec1(array_r3%num) = read_r3%realvalue(1)
      array_r3%vec2(array_r3%num) = read_r3%realvalue(2)
      array_r3%vec3(array_r3%num) = read_r3%realvalue(3)
!
      end subroutine append_control_item_r3
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
      subroutine append_control_item_i2(read_i2, array_i2)
!
      type(read_int2_item), intent(in) ::    read_i2
      type(ctl_array_i2), intent(inout) :: array_i2
!
!
      array_i2%icou = array_i2%icou + read_i2%iflag
      array_i2%int1(array_i2%num) = read_i2%intvalue(1)
      array_i2%int2(array_i2%num) = read_i2%intvalue(2)
!
      end subroutine append_control_item_i2
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
      subroutine append_control_item_c_r(read_cr, array_cr)
!
      type(read_chara_real_item), intent(in) ::    read_cr
      type(ctl_array_cr), intent(inout) :: array_cr
!
!
      array_cr%icou = array_cr%icou + read_cr%iflag
      array_cr%c_tbl(array_cr%num) = read_cr%charavalue
      array_cr%vect(array_cr%num) =  read_cr%realvalue
!
      end subroutine append_control_item_c_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c_i(read_ci, array_ci)
!
      type(read_chara_int_item), intent(in) ::    read_ci
      type(ctl_array_ci), intent(inout) :: array_ci
!
!
      array_ci%icou = array_ci%icou + read_ci%iflag
      array_ci%c_tbl(array_ci%num) = read_ci%charavalue
      array_ci%ivec(array_ci%num) =  read_ci%intvalue
!
      end subroutine append_control_item_c_i
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c_r2(read_cr2, array_cr2)
!
      type(read_chara_real2_item), intent(in) ::    read_cr2
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      array_cr2%icou = array_cr2%icou + read_cr2%iflag
      array_cr2%c_tbl(array_cr2%num) = read_cr2%charavalue
      array_cr2%vec1(array_cr2%num) =  read_cr2%realvalue(1)
      array_cr2%vec2(array_cr2%num) =  read_cr2%realvalue(2)
!
      end subroutine append_control_item_c_r2
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c2_r(read_c2r, array_c2r)
!
      type(read_chara2_real_item), intent(in) ::    read_c2r
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      array_c2r%icou = array_c2r%icou + read_c2r%iflag
      array_c2r%c1_tbl(array_c2r%num) = read_c2r%charavalue(1)
      array_c2r%c2_tbl(array_c2r%num) = read_c2r%charavalue(2)
      array_c2r%vect(array_c2r%num) =   read_c2r%realvalue
!
      end subroutine append_control_item_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i_c_r(read_icr, array_icr)
!
      type(read_int_chara_real_item), intent(in) ::    read_icr
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      array_icr%icou = array_icr%icou + read_icr%iflag
      array_icr%ivec(array_icr%num) =  read_icr%intvalue
      array_icr%c_tbl(array_icr%num) = read_icr%charavalue
      array_icr%vect(array_icr%num) =  read_icr%realvalue
!
      end subroutine append_control_item_i_c_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i_r(read_ir, array_ir)
!
      type(read_int_real_item), intent(in) ::    read_ir
      type(ctl_array_ir), intent(inout) :: array_ir
!
!
      array_ir%icou = array_ir%icou + read_ir%iflag
      array_ir%ivec(array_ir%num) = read_ir%intvalue
      array_ir%vect(array_ir%num) = read_ir%realvalue
!
      end subroutine append_control_item_i_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i2_r(read_i2r, array_i2r)
!
      type(read_int2_real_item), intent(in) ::    read_i2r
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      array_i2r%icou = array_i2r%icou + read_i2r%iflag
      array_i2r%int1(array_i2r%num) = read_i2r%intvalue(1)
      array_i2r%int2(array_i2r%num) = read_i2r%intvalue(2)
      array_i2r%vect(array_i2r%num) = read_i2r%realvalue
!
      end subroutine append_control_item_i2_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i2_r2(read_i2r2, array_i2r2)
!
      type(read_int2_real2_item), intent(in) ::    read_i2r2
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      array_i2r2%icou = array_i2r2%icou + read_i2r2%iflag
      array_i2r2%int1(array_i2r2%num) = read_i2r2%intvalue(1)
      array_i2r2%int2(array_i2r2%num) = read_i2r2%intvalue(2)
      array_i2r2%vec1(array_i2r2%num) = read_i2r2%realvalue(1)
      array_i2r2%vec2(array_i2r2%num) = read_i2r2%realvalue(2)
!
      end subroutine append_control_item_i2_r2
!
! -----------------------------------------------------------------------
!
      end module append_control_items
