!>@file   ctl_int_real_array_to_c.f90
!!        module ctl_int_real_array_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      subroutine c_int_real_item_block_name(c_ctl)                    &
!!     &          bind(C, NAME = 'c_int_real_item_block_name')
!!      subroutine c_int_real_item_iflag(c_ctl)                         &
!!     &          bind(C, NAME = 'c_int_real_item_iflag')
!!      integer(c_int) function c_int_real_item_intvalue(c_ctl)         &
!!     &          bind(C, NAME = 'c_int_real_item_intvalue')
!!      real(c_double) function c_int_real_item_realvalue(c_ctl)        &
!!     &          bind(C, NAME = 'c_int_real_item_realvalue')
!!      subroutine c_store_int_real_items(c_ctl, i_in, r_in)            &
!!     &          bind(C, NAME = 'c_store_int_real_items')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), intent(in), value :: i_in
!!        real(C_double), intent(in), value :: r_in
!!
!!      type(c_ptr) function c_int_real_array_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_int_real_array_block_name')
!!      integer(c_int) function c_int_real_array_num(c_ctl)             &
!!     &          bind(C, NAME = 'c_int_real_array_num')
!!      integer(c_int) function c_int_real_array_i_tbl(idx_in, c_ctl)   &
!!     &          bind(C, NAME = 'c_int_real_array_i_tbl')
!!      real(c_double) function c_int_real_array_r_tbl(idx_in, c_ctl)   &
!!     &          bind(C, NAME = 'c_int_real_array_r_tbl')
!!      subroutine c_store_int_real_array(c_ctl, idx_in, i_in, r_in)    &
!!     &          bind(C, NAME = 'c_store_int_real_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), value, intent(in) :: idx_in, i_in
!!        real(c_double), value, intent(in) :: r_in
!!
!!      subroutine c_dealloc_int_real_array(c_ctl)                      &
!!     &          bind(C, NAME = 'c_dealloc_int_real_array')
!!      subroutine c_alloc_int_real_array(num, c_ctl)                   &
!!     &          bind(C, NAME = 'c_alloc_int_real_array')
!!      subroutine c_check_int_real_array(c_ctl)                        &
!!     &          bind(C, NAME = 'c_check_int_real_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_int_real_array_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_intreal
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_real_item_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_int_real_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_int_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_item_block_name = C_loc(f_ctl%item_name)
      end function c_int_real_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_real_item_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_int_real_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_int_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_item_iflag = C_loc(f_ctl%iflag)
      end function c_int_real_item_iflag
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_int_real_item_intvalue(c_ctl)           &
     &          bind(C, NAME = 'c_int_real_item_intvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_int_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_item_intvalue = f_ctl%intvalue
      end function c_int_real_item_intvalue
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_int_real_item_realvalue(c_ctl)          &
     &          bind(C, NAME = 'c_int_real_item_realvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_int_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_item_realvalue = f_ctl%realvalue
      end function c_int_real_item_realvalue
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_int_real_items(c_ctl, i_in, r_in)              &
     &          bind(C, NAME = 'c_store_int_real_items')
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), intent(in), value :: i_in
      real(C_double), intent(in), value :: r_in
      type(read_int_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%intvalue =  i_in
      f_ctl%realvalue = r_in
      end subroutine c_store_int_real_items
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_real_array_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_int_real_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ir), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_array_block_name = C_loc(f_ctl%array_name)
      end function c_int_real_array_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_int_real_array_num(c_ctl)               &
     &          bind(C, NAME = 'c_int_real_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ir), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_array_num = f_ctl%num
      end function c_int_real_array_num
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_int_real_array_i_tbl(idx_in, c_ctl)     &
     &          bind(C, NAME = 'c_int_real_array_i_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ir), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_array_i_tbl = f_ctl%ivec(idx_in+1)
      end function c_int_real_array_i_tbl
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_int_real_array_r_tbl(idx_in, c_ctl)     &
     &          bind(C, NAME = 'c_int_real_array_r_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ir), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_real_array_r_tbl = f_ctl%vect(idx_in+1)
      end function c_int_real_array_r_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_int_real_array(c_ctl, idx_in, i_in, r_in)      &
     &          bind(C, NAME = 'c_store_int_real_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in, i_in
      real(c_double), value, intent(in) :: r_in
      type(ctl_array_ir), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%ivec(idx_in+1) = i_in
      f_ctl%vect(idx_in+1) = r_in
      end subroutine c_store_int_real_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_int_real_array(c_ctl)                        &
     &          bind(C, NAME = 'c_dealloc_int_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ir), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_i_r(f_ctl)
      end subroutine c_dealloc_int_real_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_int_real_array(num, c_ctl)                     &
     &          bind(C, NAME = 'c_alloc_int_real_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ir), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      call alloc_control_array_i_r(f_ctl)
      end subroutine c_alloc_int_real_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_int_real_array(c_ctl)                          &
     &          bind(C, NAME = 'c_check_int_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_ir), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%ivec(i), f_ctl%vect(i)
      end do

      end subroutine c_check_int_real_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_int_real_array_to_c
