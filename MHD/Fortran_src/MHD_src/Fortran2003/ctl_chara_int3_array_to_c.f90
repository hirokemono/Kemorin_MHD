!>@file   ctl_chara_int3_array_to_c.f90
!!        module ctl_chara_int3_array_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      subroutine c_chara_int3_item_block_name(c_ctl)                  &
!!     &          bind(C, NAME = 'c_chara_int3_item_block_name')
!!      subroutine c_chara_int3_item_iflag(c_ctl)                       &
!!     &          bind(C, NAME = 'c_chara_int3_item_iflag')
!!      subroutine c_chara_int3_item_charavalue(c_ctl)                  &
!!     &          bind(C, NAME = 'c_chara_int3_item_charavalue')
!!      integer(c_int) function c_chara_int3_item_intvalue1(c_ctl)      &
!!     &          bind(C, NAME = 'c_chara_int3_item_intvalue1')
!!      integer(c_int) function c_chara_int3_item_intvalue2(c_ctl)      &
!!     &          bind(C, NAME = 'c_chara_int3_item_intvalue2')
!!      integer(c_int) function c_chara_int3_item_intvalue3(c_ctl)      &
!!     &          bind(C, NAME = 'c_chara_int3_item_intvalue3')
!!      subroutine c_store_chara_int3_items(c_ctl, c_in,                &
!!     &                                    i1_in, i2_in, i3_in)        &
!!     &          bind(C, NAME = 'c_store_chara_int3_items')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        character(C_char), intent(in) :: c_in(*)
!!        integer(C_int), intent(in), value :: i_in
!!
!!      subroutine c_chara_int3_array_block_name(c_ctl)                 &
!!     &          bind(C, NAME = 'c_chara_int3_array_block_name')
!!      integer(c_int) function c_chara_int3_array_num(c_ctl)           &
!!     &          bind(C, NAME = 'c_chara_int3_array_num')
!!      type(c_ptr) function c_chara_int3_array_c_tbl(idx_in, c_ctl)    &
!!     &          bind(C, NAME = 'c_chara_int3_array_c_tbl')
!!      integer(c_int) function c_chara_int3_array_i1_tbl(idx_in, c_ctl)&
!!     &          bind(C, NAME = 'c_chara_int3_array_i1_tbl')
!!      integer(c_int) function c_chara_int3_array_i2_tbl(idx_in, c_ctl)&
!!     &          bind(C, NAME = 'c_chara_int3_array_i2_tbl')
!!      integer(c_int) function c_chara_int3_array_i3_tbl(idx_in, c_ctl)&
!!     &          bind(C, NAME = 'c_chara_int3_array_i3_tbl')
!!      subroutine c_store_chara_int3_array(c_ctl, idx_in, c_in,        &
!!     &                                    i1_in, i2_in, i3_in)        &
!!     &          bind(C, NAME = 'c_store_chara_int3_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        character(C_char), intent(in) :: c_in(kchara)
!!       integer(C_int), value, intent(in) :: idx_in, i_in
!!
!!      subroutine c_dealloc_chara_int3_array(c_ctl)                    &
!!     &          bind(C, NAME = 'c_dealloc_chara_int3_array')
!!      subroutine c_alloc_chara_int3_array(num, c_ctl)                 &
!!     &          bind(C, NAME = 'c_alloc_chara_int3_array')
!!      subroutine c_check_chara_int3_array(c_ctl)                      &
!!     &          bind(C, NAME = 'c_check_chara_int3_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_chara_int3_array_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_charaint3
      use ctl_array_chara_to_c
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_int3_item_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_chara_int3_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_int3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_item_block_name = C_loc(f_ctl%item_name)
      end function c_chara_int3_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_int3_item_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_chara_int3_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_int3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_item_iflag = C_loc(f_ctl%iflag)
      end function c_chara_int3_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_int3_item_charavalue(c_ctl)          &
     &          bind(C, NAME = 'c_chara_int3_item_charavalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_int3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_item_charavalue = C_loc(f_ctl%charavalue)
      end function c_chara_int3_item_charavalue
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara_int3_item_intvalue1(c_ctl)        &
     &          bind(C, NAME = 'c_chara_int3_item_intvalue1')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_int3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_item_intvalue1 = f_ctl%intvalue(1)
      end function c_chara_int3_item_intvalue1
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara_int3_item_intvalue2(c_ctl)        &
     &          bind(C, NAME = 'c_chara_int3_item_intvalue2')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_int3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_item_intvalue2 = f_ctl%intvalue(2)
      end function c_chara_int3_item_intvalue2
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara_int3_item_intvalue3(c_ctl)        &
     &          bind(C, NAME = 'c_chara_int3_item_intvalue3')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_int3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_item_intvalue3 = f_ctl%intvalue(3)
      end function c_chara_int3_item_intvalue3
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_chara_int3_items(c_ctl, c_in,                  &
     &                                    i1_in, i2_in, i3_in)          &
     &          bind(C, NAME = 'c_store_chara_int3_items')
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), intent(in) :: c_in(*)
      integer(C_int), intent(in), value :: i1_in, i2_in, i3_in
      type(read_chara_int3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%charavalue = copy_char_from_c(c_in)
      f_ctl%intvalue(1) =  i1_in
      f_ctl%intvalue(2) =  i2_in
      f_ctl%intvalue(3) =  i3_in
      end subroutine c_store_chara_int3_items
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_int3_array_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_chara_int3_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_array_block_name = C_loc(f_ctl%array_name)
      end function c_chara_int3_array_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara_int3_array_num(c_ctl)             &
     &          bind(C, NAME = 'c_chara_int3_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_array_num = f_ctl%num
      end function c_chara_int3_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_int3_array_c_tbl(idx_in, c_ctl)      &
     &          bind(C, NAME = 'c_chara_int3_array_c_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_array_c_tbl = C_loc(f_ctl%c_tbl(idx_in+1))
      end function c_chara_int3_array_c_tbl
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara_int3_array_i1_tbl(idx_in, c_ctl)  &
     &          bind(C, NAME = 'c_chara_int3_array_i1_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_array_i1_tbl = f_ctl%ivec1(idx_in+1)
      end function c_chara_int3_array_i1_tbl
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara_int3_array_i2_tbl(idx_in, c_ctl)  &
     &          bind(C, NAME = 'c_chara_int3_array_i2_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_array_i2_tbl = f_ctl%ivec2(idx_in+1)
      end function c_chara_int3_array_i2_tbl
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara_int3_array_i3_tbl(idx_in, c_ctl)  &
     &          bind(C, NAME = 'c_chara_int3_array_i3_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_int3_array_i3_tbl = f_ctl%ivec3(idx_in+1)
      end function c_chara_int3_array_i3_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_chara_int3_array(c_ctl, idx_in, c_in,          &
     &                                    i1_in, i2_in, i3_in)          &
     &          bind(C, NAME = 'c_store_chara_int3_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), intent(in) :: c_in(*)
      integer(C_int), value, intent(in) :: idx_in, i1_in, i2_in, i3_in
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%c_tbl(idx_in+1) = copy_char_from_c(c_in)
      f_ctl%ivec1(idx_in+1) = i1_in
      f_ctl%ivec2(idx_in+1) = i2_in
      f_ctl%ivec3(idx_in+1) = i3_in
      end subroutine c_store_chara_int3_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_chara_int3_array(c_ctl)                      &
     &          bind(C, NAME = 'c_dealloc_chara_int3_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_c_i3(f_ctl)
      end subroutine c_dealloc_chara_int3_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_chara_int3_array(num, c_ctl)                   &
     &          bind(C, NAME = 'c_alloc_chara_int3_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      call alloc_control_array_c_i3(f_ctl)
      end subroutine c_alloc_chara_int3_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_chara_int3_array(c_ctl)                        &
     &          bind(C, NAME = 'c_check_chara_int3_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ci3), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%c_tbl(i), f_ctl%ivec1(i),              &
     &              f_ctl%ivec2(i),  f_ctl%ivec3(i)
      end do

      end subroutine c_check_chara_int3_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_chara_int3_array_to_c
