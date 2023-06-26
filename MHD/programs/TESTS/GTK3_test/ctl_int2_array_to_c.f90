!>@file   ctl_int2_array_to_c.f90
!!        module ctl_int2_array_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      subroutine c_int2_item_block_name(c_ctl)                        &
!!     &          bind(C, NAME = 'c_int2_item_block_name')
!!      subroutine c_int2_item_iflag(c_ctl)                             &
!!     &          bind(C, NAME = 'c_int2_item_iflag')
!!      subroutine c_int2_item_intvalue(c_ctl)                          &
!!     &          bind(C, NAME = 'c_int2_item_intvalue')
!!
!!      subroutine c_int2_array_block_name(c_ctl)                       &
!!     &          bind(C, NAME = 'c_int2_array_block_name')
!!      subroutine c_int2_array_num(c_ctl)                              &
!!     &          bind(C, NAME = 'c_int2_array_num')
!!      subroutine c_int2_array_icou(c_ctl)                             &
!!     &          bind(C, NAME = 'c_int2_array_icou')
!!      subroutine c_int2_array_i1_tbl(c_ctl)                           &
!!     &          bind(C, NAME = 'c_int2_array_i1_tbl')
!!      subroutine c_int2_array_i2_tbl(c_ctl)                           &
!!     &          bind(C, NAME = 'c_int2_array_i2_tbl')
!!
!!      subroutine c_dealloc_int2_array(c_ctl)                          &
!!     &          bind(C, NAME = 'c_dealloc_int2_array')
!!      subroutine c_alloc_int2_array(c_ctl)                            &
!!     &          bind(C, NAME = 'c_alloc_int2_array')
!!      subroutine c_check_int2_array(c_ctl)                            &
!!     &          bind(C, NAME = 'c_check_int2_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_int2_array_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_integer2
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_item_block_name(c_ctl)                &
     &          bind(C, NAME = 'c_int2_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_int2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_item_block_name = C_loc(f_ctl%item_name)
      end function c_int2_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_item_iflag(c_ctl)                     &
     &          bind(C, NAME = 'c_int2_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_int2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_item_iflag = C_loc(f_ctl%iflag)
      end function c_int2_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_item_intvalue(c_ctl)                  &
     &          bind(C, NAME = 'c_int2_item_intvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_int2_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_item_intvalue = C_loc(f_ctl%intvalue)
      end function c_int2_item_intvalue
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_array_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_int2_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_array_block_name = C_loc(f_ctl%array_name)
      end function c_int2_array_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_array_num(c_ctl)                      &
     &          bind(C, NAME = 'c_int2_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_array_num = C_loc(f_ctl%num)
      end function c_int2_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_array_icou(c_ctl)                     &
     &          bind(C, NAME = 'c_int2_array_icou')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_array_icou = C_loc(f_ctl%icou)
      end function c_int2_array_icou
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_array_i1_tbl(c_ctl)                   &
     &          bind(C, NAME = 'c_int2_array_i1_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_array_i1_tbl = C_loc(f_ctl%int1)
      end function c_int2_array_i1_tbl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_int2_array_i2_tbl(c_ctl)                   &
     &          bind(C, NAME = 'c_int2_array_i2_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_int2_array_i2_tbl = C_loc(f_ctl%int2)
      end function c_int2_array_i2_tbl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_int2_array(c_ctl)                            &
     &          bind(C, NAME = 'c_dealloc_int2_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_i2(f_ctl)
      end subroutine c_dealloc_int2_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_int2_array(c_ctl)                              &
     &          bind(C, NAME = 'c_alloc_int2_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call alloc_control_array_i2(f_ctl)
      end subroutine c_alloc_int2_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_int2_array(c_ctl)                              &
     &          bind(C, NAME = 'c_check_int2_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_i2), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%int1(i), f_ctl%int2(i)
      end do

      end subroutine c_check_int2_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_int2_array_to_c
