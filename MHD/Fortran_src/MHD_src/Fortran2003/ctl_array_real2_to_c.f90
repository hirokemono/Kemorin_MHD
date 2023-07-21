!>@file   ctl_array_real2_to_c.f90
!!        module ctl_array_real2_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      type(c_ptr) function c_real2_item_block_name(c_ctl)             &
!!     &          bind(C, NAME = 'c_real2_item_block_name')
!!      type(c_ptr) function c_real2_item_iflag(c_ctl)                  &
!!     &          bind(C, NAME = 'c_real2_item_iflag')
!!      real(c_double) function c_real2_item_realvalue1(c_ctl)          &
!!     &          bind(C, NAME = 'c_real2_item_realvalue1')
!!      real(c_double) function c_real2_item_realvalue2(c_ctl)          &
!!     &          bind(C, NAME = 'c_real2_item_realvalue3')
!!      subroutine c_store_real2_items(c_ctl, r1_in, r2_in)             &
!!     &          bind(C, NAME = 'c_store_real2_items')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        real(C_double), value, intent(in) :: r1_in, r2_in
!!
!!      type(c_ptr) function c_real2_array_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_real2_array_block_name')
!!      integer(c_int) function c_real2_array_num(c_ctl)                &
!!     &          bind(C, NAME = 'c_real2_array_num')
!!      real(C_double) function c_real2_array_r1_tbl(idx_in, c_ctl)     &
!!     &          bind(C, NAME = 'c_real2_array_r1_tbl')
!!      real(C_double) function c_real2_array_r2_tbl(idx_in, c_ctl)     &
!!     &          bind(C, NAME = 'c_real2_array_r2_tbl')
!!      subroutine c_store_real2_array(c_ctl, idx_in, r1_in, r2_in)     &
!!     &          bind(C, NAME = 'c_store_real2_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), value, intent(in) :: idx_in
!!        real(C_double), value, intent(in) :: r1_in, r2_in
!!
!!      subroutine c_dealloc_real2_array(c_ctl)                         &
!!     &          bind(C, NAME = 'c_dealloc_real2_array')
!!      subroutine c_alloc_real2_array(num, c_ctl)                      &
!!     &          bind(C, NAME = 'c_alloc_real2_array')
!!      subroutine c_check_real2_array(c_ctl)                           &
!!     &          bind(C, NAME = 'c_check_real2_array')
!!        integer(C_int), value, intent(in) :: num
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_array_real2_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_real2
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real2_item_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_real2_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_item_block_name = C_loc(f_ctl%item_name)
      end function c_real2_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real2_item_iflag(c_ctl)                    &
     &          bind(C, NAME = 'c_real2_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_item_iflag = C_loc(f_ctl%iflag)
      end function c_real2_item_iflag
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_real2_item_realvalue1(c_ctl)            &
     &          bind(C, NAME = 'c_real2_item_realvalue1')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_item_realvalue1 = f_ctl%realvalue(1)
      end function c_real2_item_realvalue1
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_real2_item_realvalue2(c_ctl)            &
     &          bind(C, NAME = 'c_real2_item_realvalue2')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_item_realvalue2 = f_ctl%realvalue(2)
      end function c_real2_item_realvalue2
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_real2_items(c_ctl, r1_in, r2_in)               &
     &          bind(C, NAME = 'c_store_real2_items')
      type(c_ptr), value, intent(in) :: c_ctl
      real(C_double), value, intent(in) :: r1_in, r2_in
      type(read_real2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%realvalue(1) = r1_in
      f_ctl%realvalue(2) = r2_in
      end subroutine c_store_real2_items
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real2_array_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_real2_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_array_block_name = C_loc(f_ctl%array_name)
      end function c_real2_array_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_real2_array_num(c_ctl)                  &
     &          bind(C, NAME = 'c_real2_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_array_num = f_ctl%num
      end function c_real2_array_num
!
!  ---------------------------------------------------------------------
!
      real(C_double) function c_real2_array_r1_tbl(idx_in, c_ctl)       &
     &          bind(C, NAME = 'c_real2_array_r1_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_array_r1_tbl = f_ctl%vec1(idx_in+1)
      end function c_real2_array_r1_tbl
!
!  ---------------------------------------------------------------------
!
      real(C_double) function c_real2_array_r2_tbl(idx_in, c_ctl)       &
     &          bind(C, NAME = 'c_real2_array_r2_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real2_array_r2_tbl = f_ctl%vec2(idx_in+1)
      end function c_real2_array_r2_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_real2_array(c_ctl, idx_in, r1_in, r2_in)       &
     &          bind(C, NAME = 'c_store_real2_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in
      real(C_double), value, intent(in) :: r1_in, r2_in
      type(ctl_array_r2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%vec1(idx_in+1) = r1_in
      f_ctl%vec2(idx_in+1) = r2_in
      end subroutine c_store_real2_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_real2_array(c_ctl)                           &
     &          bind(C, NAME = 'c_dealloc_real2_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call alloc_control_array_r2(f_ctl)
      end subroutine c_dealloc_real2_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_real2_array(num, c_ctl)                        &
     &          bind(C, NAME = 'c_alloc_real2_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      call dealloc_control_array_r2(f_ctl)
      end subroutine c_alloc_real2_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_real2_array(c_ctl)                             &
     &          bind(C, NAME = 'c_check_real2_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r2), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%vec1(i), f_ctl%vec2(i)
      end do

      end subroutine c_check_real2_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_array_real2_to_c
