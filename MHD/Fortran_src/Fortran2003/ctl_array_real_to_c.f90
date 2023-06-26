!>@file   ctl_array_real_to_c.f90
!!        module ctl_array_real_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      type(c_ptr) function c_real_item_block_name(c_ctl)              &
!!     &          bind(C, NAME = 'c_real_item_block_name')
!!      type(c_ptr) function c_real_item_iflag(c_ctl)                   &
!!     &          bind(C, NAME = 'c_real_item_iflag')
!!      type(c_ptr) function c_real_item_realvalue(c_ctl)               &
!!     &          bind(C, NAME = 'c_real_item_realvalue')
!!      type(c_ptr) function c_real_array_block_name(c_ctl)             &
!!     &          bind(C, NAME = 'c_real_array_block_name')
!!      type(c_ptr) function c_real_array_num(c_ctl)                    &
!!     &          bind(C, NAME = 'c_real_array_num')
!!      type(c_ptr) function c_real_array_icou(c_ctl)                   &
!!     &          bind(C, NAME = 'c_real_array_icou')
!!      type(c_ptr) function c_real_array_r_tbl(c_ctl)                  &
!!     &          bind(C, NAME = 'c_real_array_r_tbl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      subroutine c_dealloc_real_array(c_ctl)                   \        &
!!     &          bind(C, NAME = 'c_dealloc_real_array')
!!      subroutine c_alloc_real_array(c_ctl)                            &
!!     &          bind(C, NAME = 'c_alloc_real_array')
!!      subroutine c_check_real_array(c_ctl)                            &
!!     &          bind(C, NAME = 'c_check_real_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_array_real_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_real
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real_item_block_name(c_ctl)                &
     &          bind(C, NAME = 'c_real_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real_item_block_name = C_loc(f_ctl%item_name)
      end function c_real_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real_item_iflag(c_ctl)                     &
     &          bind(C, NAME = 'c_real_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real_item_iflag = C_loc(f_ctl%iflag)
      end function c_real_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real_item_realvalue(c_ctl)                 &
     &          bind(C, NAME = 'c_real_item_realvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real_item_realvalue = C_loc(f_ctl%realvalue)
      end function c_real_item_realvalue
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real_array_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_real_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_real), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real_array_block_name = C_loc(f_ctl%array_name)
      end function c_real_array_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real_array_num(c_ctl)                      &
     &          bind(C, NAME = 'c_real_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_real), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real_array_num = C_loc(f_ctl%num)
      end function c_real_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real_array_icou(c_ctl)                     &
     &          bind(C, NAME = 'c_real_array_icou')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_real), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real_array_icou = C_loc(f_ctl%icou)
      end function c_real_array_icou
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_real_array_r_tbl(c_ctl)                    &
     &          bind(C, NAME = 'c_real_array_r_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_real), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_real_array_r_tbl = C_loc(f_ctl%vect)
      end function c_real_array_r_tbl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_real_array(c_ctl)                            &
     &          bind(C, NAME = 'c_dealloc_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_real), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_real(f_ctl)
      end subroutine c_dealloc_real_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_real_array(c_ctl)                              &
     &          bind(C, NAME = 'c_alloc_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_real), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call alloc_control_array_real(f_ctl)
      end subroutine c_alloc_real_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_real_array(c_ctl)                              &
     &          bind(C, NAME = 'c_check_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_real), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%vect(i)
      end do

      end subroutine c_check_real_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_array_real_to_c
