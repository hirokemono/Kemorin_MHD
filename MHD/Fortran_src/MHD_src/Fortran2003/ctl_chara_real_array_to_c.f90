!>@file   ctl_chara_real_array_to_c.f90
!!        module ctl_chara_real_array_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      type(c_ptr) function c_chara_real_item_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_chara_real_item_block_name')
!!      type(c_ptr) function c_chara_real_item_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_chara_real_item_iflag')
!!      type(c_ptr) function c_chara_real_item_charavalue(c_ctl)        &
!!     &          bind(C, NAME = 'c_chara_real_item_charavalue')
!!      real(c_double) function c_chara_real_item_realvalue(c_ctl)      &
!!     &          bind(C, NAME = 'c_chara_real_item_realvalue')
!!      subroutine c_store_chara_real_items(c_ctl, c_in, r_in)          &
!!     &          bind(C, NAME = 'c_store_chara_real_items')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        character(C_char), intent(in) :: c_in(*)
!!         real(C_double), intent(in), value :: r_in
!!
!!      type(c_ptr) function c_chara_real_array_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_chara_real_array_block_name')
!!      integer(C_int) function c_chara_real_array_num(c_ctl)           &
!!     &          bind(C, NAME = 'c_chara_real_array_num')
!!      type(c_ptr) function c_chara_real_array_c_tbl(idx_in, c_ctl)    &
!!     &          bind(C, NAME = 'c_chara_real_array_c_tbl')
!!      real(C_double) function c_chara_real_array_r_tbl(idx_in, c_ctl) &
!!     &          bind(C, NAME = 'c_chara_real_array_r_tbl')
!!      subroutine c_store_chara_real_array(c_ctl, idx_in, c_in, r_in)  &
!!     &          bind(C, NAME = 'c_store_chara_real_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), value, intent(in) :: idx_in
!!        character(C_char), intent(in) :: c_in(*)
!!        real(c_double), value, intent(in) :: r_in
!!
!!      subroutine c_dealloc_chara_real_array(c_ctl)                    &
!!     &          bind(C, NAME = 'c_dealloc_chara_real_array')
!!      subroutine c_alloc_chara_real_array(num, c_ctl)                 &
!!     &          bind(C, NAME = 'c_alloc_chara_real_array')
!!      subroutine c_check_chara_real_array(c_ctl)                      &
!!     &          bind(C, NAME = 'c_check_chara_real_array')
!!        integer(C_int), value, intent(in) :: num
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_chara_real_array_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_charareal
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
      type(c_ptr) function c_chara_real_item_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_chara_real_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_item_block_name = C_loc(f_ctl%item_name)
      end function c_chara_real_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_real_item_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_chara_real_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_item_iflag = C_loc(f_ctl%iflag)
      end function c_chara_real_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_real_item_charavalue(c_ctl)          &
     &          bind(C, NAME = 'c_chara_real_item_charavalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_item_charavalue = C_loc(f_ctl%charavalue)
      end function c_chara_real_item_charavalue
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_chara_real_item_realvalue(c_ctl)        &
     &          bind(C, NAME = 'c_chara_real_item_realvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_item_realvalue = f_ctl%realvalue
      end function c_chara_real_item_realvalue
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_chara_real_items(c_ctl, c_in, r_in)            &
     &          bind(C, NAME = 'c_store_chara_real_items')
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), intent(in) :: c_in(*)
      real(C_double), intent(in), value :: r_in
      type(read_chara_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%charavalue = copy_char_from_c(c_in)
      f_ctl%realvalue =  r_in
      end subroutine c_store_chara_real_items
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_real_array_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_chara_real_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_block_name = C_loc(f_ctl%array_name)
      end function c_chara_real_array_block_name
!
!  ---------------------------------------------------------------------
!
      integer(C_int) function c_chara_real_array_num(c_ctl)             &
     &          bind(C, NAME = 'c_chara_real_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_num = f_ctl%num
      end function c_chara_real_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_real_array_c_tbl(idx_in, c_ctl)      &
     &          bind(C, NAME = 'c_chara_real_array_c_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_c_tbl = C_loc(f_ctl%c_tbl(idx_in+1))
      end function c_chara_real_array_c_tbl
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_chara_real_array_r_tbl(idx_in, c_ctl)   &
     &          bind(C, NAME = 'c_chara_real_array_r_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_r_tbl = f_ctl%vect(idx_in+1)
      end function c_chara_real_array_r_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_chara_real_array(c_ctl, idx_in, c_in, r_in)    &
     &          bind(C, NAME = 'c_store_chara_real_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in
      character(C_char), intent(in) :: c_in(*)
      real(C_double), value, intent(in) :: r_in
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%c_tbl(idx_in+1) = copy_char_from_c(c_in)
      f_ctl%vect(idx_in+1) = r_in
      end subroutine c_store_chara_real_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_chara_real_array(c_ctl)                      &
     &          bind(C, NAME = 'c_dealloc_chara_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_c_r(f_ctl)
      end subroutine c_dealloc_chara_real_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_chara_real_array(num, c_ctl)                   &
     &          bind(C, NAME = 'c_alloc_chara_real_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      call alloc_control_array_c_r(f_ctl)
      end subroutine c_alloc_chara_real_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_chara_real_array(c_ctl)                        &
     &          bind(C, NAME = 'c_check_chara_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%c_tbl(i), f_ctl%vect(i)
      end do

      end subroutine c_check_chara_real_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_chara_real_array_to_c
