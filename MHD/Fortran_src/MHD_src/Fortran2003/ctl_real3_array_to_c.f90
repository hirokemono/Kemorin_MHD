!>@file   ctl_real3_array_to_c.f90
!!        module ctl_real3_array_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      subroutine c_real3_item_block_name(c_ctl)                       &
!!     &          bind(C, NAME = 'c_real3_item_block_name')
!!      subroutine c_real3_item_iflag(c_ctl)                            &
!!     &          bind(C, NAME = 'c_real3_item_iflag')
!!      subroutine c_real3_item_realvalue(c_ctl)                        &
!!     &          bind(C, NAME = 'c_real3_item_realvalue')
!!      subroutine c_store_real3_items(c_ctl, r1_in, r2_in, r3_in)      &
!!     &          bind(C, NAME = 'c_store_real3_items')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        real(C_double), intent(in) :: r1_in, r2_in, r3_in
!!
!!      type(c_ptr) function c_real3_array_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_real3_array_block_name')
!!      integer(c_int) function c_real3_array_num(c_ctl)                &
!!     &          bind(C, NAME = 'c_real3_array_num')
!!      real(c_double) function c_real3_array_r1_tbl(c_ctl, idx_in)     &
!!     &          bind(C, NAME = 'c_real3_array_r1_tbl')
!!      real(c_double) function c_real3_array_r2_tbl(c_ctl, idx_in)     &
!!     &          bind(C, NAME = 'c_real3_array_r2_tbl')
!!      real(c_double) function c_real3_array_r3_tbl(c_ctl, idx_in)     &
!!     &          bind(C, NAME = 'c_real3_array_r4_tbl')
!!      subroutine c_store_real3_array                                  &
!!     &         (c_ctl, idx_in, r1_in, r2_in, r3_in)                   &
!!     &          bind(C, NAME = 'c_store_real3_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        real(C_double), intent(in) :: r1_in, r2_in, r3_in
!!        character(C_char), value, intent(in) :: c_in(kchara)
!!        integer(C_double), value, intent(in) :: v_in
!!
!!      subroutine c_dealloc_real3_array(c_ctl)                         &
!!     &          bind(C, NAME = 'c_dealloc_real3_array')
!!      subroutine c_alloc_real3_array(num, c_ctl)                      &
!!     &          bind(C, NAME = 'c_alloc_real3_array')
!!      subroutine c_check_real3_array(c_ctl)                           &
!!     &          bind(C, NAME = 'c_check_real3_array')
!!        integer(C_int), value, intent(in) :: num
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_real3_array_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_real3
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real3_item_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_real3_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_item_block_name = C_loc(f_ctl%item_name)
      end function c_real3_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real3_item_iflag(c_ctl)                    &
     &          bind(C, NAME = 'c_real3_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_item_iflag = C_loc(f_ctl%iflag)
      end function c_real3_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real3_item_realvalue(c_ctl)                &
     &          bind(C, NAME = 'c_real3_item_realvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_item_realvalue = C_loc(f_ctl%realvalue)
      end function c_real3_item_realvalue
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_real3_items(c_ctl, r1_in, r2_in, r3_in)        &
     &          bind(C, NAME = 'c_store_real3_items')
!
      type(c_ptr), value, intent(in) :: c_ctl
      real(C_double), intent(in) :: r1_in, r2_in, r3_in
      type(read_real3_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%realvalue(1) = r1_in
      f_ctl%realvalue(2) = r2_in
      f_ctl%realvalue(3) = r3_in
      end subroutine c_store_real3_items
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real3_array_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_real3_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_array_block_name = C_loc(f_ctl%array_name)
      end function c_real3_array_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_real3_array_num(c_ctl)                  &
     &          bind(C, NAME = 'c_real3_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_array_num = f_ctl%num
      end function c_real3_array_num
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_real3_array_r1_tbl(c_ctl, idx_in)       &
     &          bind(C, NAME = 'c_real3_array_r1_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_array_r1_tbl = f_ctl%vec1(idx_in+1)
      end function c_real3_array_r1_tbl
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_real3_array_r2_tbl(c_ctl, idx_in)       &
     &          bind(C, NAME = 'c_real3_array_r2_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_array_r2_tbl = f_ctl%vec2(idx_in+1)
      end function c_real3_array_r2_tbl
!
!  ---------------------------------------------------------------------
!
      real(c_double) function c_real3_array_r3_tbl(c_ctl, idx_in)       &
     &          bind(C, NAME = 'c_real3_array_r3_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real3_array_r3_tbl = f_ctl%vec3(idx_in+1)
      end function c_real3_array_r3_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_real3_array                                    &
     &         (c_ctl, idx_in, r1_in, r2_in, r3_in)                     &
     &          bind(C, NAME = 'c_store_real3_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in
      real(C_double), value, intent(in) :: r1_in, r2_in, r3_in
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%vec1(idx_in+1) = r1_in
      f_ctl%vec2(idx_in+1) = r2_in
      f_ctl%vec3(idx_in+1) = r3_in
      end subroutine c_store_real3_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_real3_array(c_ctl)                           &
     &          bind(C, NAME = 'c_dealloc_real3_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_r3(f_ctl)
      end subroutine c_dealloc_real3_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_real3_array(num, c_ctl)                        &
     &          bind(C, NAME = 'c_alloc_real3_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r3), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      call alloc_control_array_r3(f_ctl)
      end subroutine c_alloc_real3_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_real3_array(c_ctl)                             &
     &          bind(C, NAME = 'c_check_real3_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_r3), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%vec1(i), f_ctl%vec2(i), f_ctl%vec3(i)
      end do

      end subroutine c_check_real3_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_real3_array_to_c
