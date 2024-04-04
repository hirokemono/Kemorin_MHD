!>@file   ctl_chara2_int_array_to_c.f90
!!        module ctl_chara2_int_array_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      subroutine c_chara2_int_item_block_name(c_ctl)                  &
!!     &          bind(C, NAME = 'c_chara2_int_item_block_name')
!!      subroutine c_chara2_int_item_iflag(c_ctl)                       &
!!     &          bind(C, NAME = 'c_chara2_int_item_iflag')
!!      type(c_ptr) function c_chara2_int_item_charavalue1(c_ctl)       &
!!     &          bind(C, NAME = 'c_chara2_int_item_charavalue1')
!!      type(c_ptr) function c_chara2_int_item_charavalue2(c_ctl)       &
!!     &          bind(C, NAME = 'c_chara2_int_item_charavalue2')
!!      integer(c_int) function c_chara2_int_item_intvalue(c_ctl)       &
!!     &          bind(C, NAME = 'c_chara2_int_item_intvalue')
!!      subroutine c_store_chara2_int_items(c_ctl, c1_in, c2_in, i_in)  &
!!     &          bind(C, NAME = 'c_store_chara2_int_items')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        character(C_char), intent(in) :: c1_in(*), c2_in(*)
!!        integer(C_int), value, intent(in) :: i_in
!!
!!      subroutine c_chara2_int_array_block_name(c_ctl)                 &
!!     &          bind(C, NAME = 'c_chara2_int_array_block_name')
!!      integer(c_int) function c_chara2_int_array_num(c_ctl)           &
!!     &          bind(C, NAME = 'c_chara2_int_array_num')
!!      subroutine c_chara2_int_array_icou(c_ctl)                       &
!!     &          bind(C, NAME = 'c_chara2_int_array_icou')
!!      type(c_ptr) function c_chara2_int_array_c1_tbl(idx_in, c_ctl)   &
!!     &          bind(C, NAME = 'c_chara2_int_array_c1_tbl')
!!      type(c_ptr) function c_chara2_int_array_c2_tbl(idx_in, c_ctl)   &
!!     &          bind(C, NAME = 'c_chara2_int_array_c2_tbl')
!!      integer(c_int) function c_chara2_int_array_i_tbl(idx_in, c_ctl) &
!!     &          bind(C, NAME = 'c_chara2_int_array_i_tbl')
!!      subroutine c_store_chara2_int_array                             &
!!     &         (c_ctl, idx_in, c1_in, c2_in, i_in)                    &
!!     &          bind(C, NAME = 'c_store_chara2_int_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        character(C_char), intent(in) :: c_in(kchara)
!!        integer(C_int), value, intent(in) :: idx_in, i_in
!!
!!      subroutine c_dealloc_chara2_int_array(c_ctl)                    &
!!     &          bind(C, NAME = 'c_dealloc_chara2_int_array')
!!      subroutine c_alloc_chara2_int_array(num, c_ctl)                 &
!!     &          bind(C, NAME = 'c_alloc_chara2_int_array')
!!      subroutine c_check_chara2_int_array(c_ctl)                      &
!!     &          bind(C, NAME = 'c_check_chara2_int_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_chara2_int_array_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_chara2int
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
      type(c_ptr) function c_chara2_int_item_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_chara2_int_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara2_int_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_item_block_name = C_loc(f_ctl%item_name)
      end function c_chara2_int_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara2_int_item_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_chara2_int_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara2_int_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_item_iflag = C_loc(f_ctl%iflag)
      end function c_chara2_int_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara2_int_item_charavalue1(c_ctl)         &
     &          bind(C, NAME = 'c_chara2_int_item_charavalue1')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara2_int_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_item_charavalue1 = C_loc(f_ctl%charavalue(1))
      end function c_chara2_int_item_charavalue1
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara2_int_item_charavalue2(c_ctl)         &
     &          bind(C, NAME = 'c_chara2_int_item_charavalue2')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara2_int_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_item_charavalue2 = C_loc(f_ctl%charavalue(2))
      end function c_chara2_int_item_charavalue2
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara2_int_item_intvalue(c_ctl)         &
     &          bind(C, NAME = 'c_chara2_int_item_intvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara2_int_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_item_intvalue = f_ctl%intvalue
      end function c_chara2_int_item_intvalue
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_chara2_int_items(c_ctl, c1_in, c2_in, i_in)    &
     &          bind(C, NAME = 'c_store_chara2_int_items')
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), intent(in) :: c1_in(*), c2_in(*)
      integer(C_int), value, intent(in) :: i_in
      type(read_chara2_int_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%charavalue(1) = copy_char_from_c(c1_in)
      f_ctl%charavalue(2) = copy_char_from_c(c2_in)
      f_ctl%intvalue =  i_in
      end subroutine c_store_chara2_int_items
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara2_int_array_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_chara2_int_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_array_block_name = C_loc(f_ctl%array_name)
      end function c_chara2_int_array_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara2_int_array_num(c_ctl)             &
     &          bind(C, NAME = 'c_chara2_int_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_array_num = f_ctl%num
      end function c_chara2_int_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara2_int_array_icou(c_ctl)               &
     &          bind(C, NAME = 'c_chara2_int_array_icou')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_array_icou = C_loc(f_ctl%icou)
      end function c_chara2_int_array_icou
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara2_int_array_c1_tbl(idx_in, c_ctl)     &
     &          bind(C, NAME = 'c_chara2_int_array_c1_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_array_c1_tbl = C_loc(f_ctl%c1_tbl(idx_in+1))
      end function c_chara2_int_array_c1_tbl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara2_int_array_c2_tbl(idx_in, c_ctl)     &
     &          bind(C, NAME = 'c_chara2_int_array_c2_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_array_c2_tbl = C_loc(f_ctl%c2_tbl(idx_in+1))
      end function c_chara2_int_array_c2_tbl
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_chara2_int_array_i_tbl(idx_in, c_ctl)   &
     &          bind(C, NAME = 'c_chara2_int_array_i_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara2_int_array_i_tbl = f_ctl%ivec(idx_in+1)
      end function c_chara2_int_array_i_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_chara2_int_array                               &
     &         (c_ctl, idx_in, c1_in, c2_in, i_in)                      &
     &          bind(C, NAME = 'c_store_chara2_int_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), intent(in) :: c1_in(*), c2_in(*)
      integer(C_int), value, intent(in) :: idx_in, i_in
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%c1_tbl(idx_in+1) = copy_char_from_c(c1_in)
      f_ctl%c2_tbl(idx_in+1) = copy_char_from_c(c2_in)
      f_ctl%ivec(idx_in+1) = i_in
      end subroutine c_store_chara2_int_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_chara2_int_array(c_ctl)                      &
     &          bind(C, NAME = 'c_dealloc_chara2_int_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_c2_i(f_ctl)
      end subroutine c_dealloc_chara2_int_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_chara2_int_array(num, c_ctl)                   &
     &          bind(C, NAME = 'c_alloc_chara2_int_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      if(.not. allocated(f_ctl%c1_tbl))                                 &
     &               call alloc_control_array_c2_i(f_ctl)
      end subroutine c_alloc_chara2_int_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_chara2_int_array(c_ctl)                        &
     &          bind(C, NAME = 'c_check_chara2_int_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_c2i), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', trim(f_ctl%c1_tbl(i)),                       &
     &             '    ', trim(f_ctl%c2_tbl(i)),                       &
     &             '    ', f_ctl%ivec(i)
      end do

      end subroutine c_check_chara2_int_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_chara2_int_array_to_c
