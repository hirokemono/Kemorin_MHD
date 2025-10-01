!>@file   ctl_array_int_to_c.f90
!!        module ctl_array_int_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      type(c_ptr) function c_int_item_block_name(c_ctl)               &
!!     &          bind(C, NAME = 'c_int_item_block_name')
!!      type(c_ptr) function c_int_item_iflag(c_ctl)                    &
!!     &          bind(C, NAME = 'c_int_item_iflag')
!!      integer(c_int) function c_int_item_intvalue(c_ctl)              &
!!     &          bind(C, NAME = 'c_int_item_intvalue')
!!      subroutine c_store_int_item_intvalue(c_ctl, i_in)               &
!!     &          bind(C, NAME = 'c_store_int_item_intvalue')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), value, intent(in) :: i_in
!!
!!      type(c_ptr) function c_int_array_block_name(c_ctl)              &
!!     &          bind(C, NAME = 'c_int_array_block_name')
!!      integer(c_int) function c_int_array_num(c_ctl)                  &
!!     &          bind(C, NAME = 'c_int_array_num')
!!      integer(c_int) function c_int_array_i_tbl(idx_in, c_ctl)        &
!!     &          bind(C, NAME = 'c_int_array_i_tbl')
!!      subroutine c_store_int_array(c_ctl, idx_in, i_in)               &
!!     &          bind(C, NAME = 'c_store_int_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), value, intent(in) :: idx_in, i_in
!!        type(ctl_array_real), pointer :: f_ctl
!!
!!      subroutine c_dealloc_int_array(c_ctl)                           &
!!     &          bind(C, NAME = 'c_dealloc_int_array')
!!      subroutine c_alloc_int_array(num, c_ctl)                        &
!!     &          bind(C, NAME = 'c_alloc_int_array')
!!      subroutine c_check_int_array(c_ctl)                             &
!!     &          bind(C, NAME = 'c_check_int_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_array_int_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_integer
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_item_block_name(c_ctl)                 &
     &          bind(C, NAME = 'c_int_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_integer_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_item_block_name = C_loc(f_ctl%item_name)
      end function c_int_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_item_iflag(c_ctl)                      &
     &          bind(C, NAME = 'c_int_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_integer_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_item_iflag = C_loc(f_ctl%iflag)
      end function c_int_item_iflag
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_int_item_intvalue(c_ctl)                &
     &          bind(C, NAME = 'c_int_item_intvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_integer_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_item_intvalue = f_ctl%intvalue
      end function c_int_item_intvalue
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_int_item_intvalue(c_ctl, i_in)                 &
     &          bind(C, NAME = 'c_store_int_item_intvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: i_in
      type(read_integer_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%intvalue = i_in
      end subroutine c_store_int_item_intvalue
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_array_block_name(c_ctl)                &
     &          bind(C, NAME = 'c_int_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_array_block_name = C_loc(f_ctl%array_name)
      end function c_int_array_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_int_array_num(c_ctl)                    &
     &          bind(C, NAME = 'c_int_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_array_num = f_ctl%num
      end function c_int_array_num
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_int_array_i_tbl(idx_in, c_ctl)          &
     &          bind(C, NAME = 'c_int_array_i_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_array_i_tbl = f_ctl%ivec(idx_in+1)
      end function c_int_array_i_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_int_array(c_ctl, idx_in, i_in)                 &
     &          bind(C, NAME = 'c_store_int_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in, i_in
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%ivec(idx_in+1) = i_in
      end subroutine c_store_int_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_int_array(c_ctl)                             &
     &          bind(C, NAME = 'c_dealloc_int_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_int(f_ctl)
      end subroutine c_dealloc_int_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_int_array(num, c_ctl)                          &
     &          bind(C, NAME = 'c_alloc_int_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      call alloc_control_array_int(f_ctl)
      end subroutine c_alloc_int_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_int_array(c_ctl)                               &
     &          bind(C, NAME = 'c_check_int_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%ivec(i)
      end do

      end subroutine c_check_int_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_array_int_to_c
