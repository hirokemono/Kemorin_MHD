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
!!      type(c_ptr) function c_int_item_intvalue(c_ctl)                 &
!!     &          bind(C, NAME = 'c_int_item_intvalue')
!!      type(c_ptr) function c_int_array_block_name(c_ctl)              &
!!     &          bind(C, NAME = 'c_int_array_block_name')
!!      type(c_ptr) function c_int_array_num(c_ctl)                     &
!!     &          bind(C, NAME = 'c_int_array_num')
!!      type(c_ptr) function c_int_array_icou(c_ctl)                    &
!!     &          bind(C, NAME = 'c_int_array_icou')
!!      type(c_ptr) function c_int_array_i_tbl(c_ctl)                   &
!!     &          bind(C, NAME = 'c_int_array_i_tbl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      subroutine c_dealloc_int_array(c_ctl)                           &
!!     &          bind(C, NAME = 'c_dealloc_int_array')
!!      subroutine c_alloc_int_array(c_ctl)                             &
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
      type(c_ptr) function c_int_item_intvalue(c_ctl)                   &
     &          bind(C, NAME = 'c_int_item_intvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_integer_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_item_intvalue = C_loc(f_ctl%intvalue)
      end function c_int_item_intvalue
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
      type(c_ptr) function c_int_array_num(c_ctl)                       &
     &          bind(C, NAME = 'c_int_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_array_num = C_loc(f_ctl%num)
      end function c_int_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_array_icou(c_ctl)                      &
     &          bind(C, NAME = 'c_int_array_icou')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_array_icou = C_loc(f_ctl%icou)
      end function c_int_array_icou
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int_array_i_tbl(c_ctl)                     &
     &          bind(C, NAME = 'c_int_array_i_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int_array_i_tbl = C_loc(f_ctl%ivec)
      end function c_int_array_i_tbl
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
      subroutine c_alloc_int_array(c_ctl)                               &
     &          bind(C, NAME = 'c_alloc_int_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_int), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
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
