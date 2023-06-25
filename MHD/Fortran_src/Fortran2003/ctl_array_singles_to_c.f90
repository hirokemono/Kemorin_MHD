!>@file   ctl_array_singles_to_c.f90
!!        module ctl_array_singles_to_c
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!!
!>@brief  Send pointers for control items to C
!!
!!@verbatim
!!      subroutine load_chara_from_c(c_ctl)                             &
!!     &          bind(C, NAME = 'load_chara_from_c')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!      subroutine c_chara_item_clength(c_ctl, length_c)                &
!!     &          bind(C, NAME = 'c_chara_item_clength')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), intent(inout) :: length_c
!!      integer(C_int) function c_yes_flag(c_ctl)                       &
!!     &          bind(C, NAME = 'c_yes_flag')
!!      integer(C_int) function c_no_file_flag(c_ctl)                   &
!!     &          bind(C, NAME = 'c_no_file_flag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_chara_item_block_name(c_ctl)             &
!!     &          bind(C, NAME = 'c_chara_item_block_name')
!!      type(c_ptr) function c_chara_item_iflag(c_ctl)                  &
!!     &          bind(C, NAME = 'c_chara_item_iflag')
!!      type(c_ptr) function c_chara_item_charavalue(c_ctl)             &
!!     &          bind(C, NAME = 'c_chara_item_charavalue')
!!      type(c_ptr) function c_chara_array_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_chara_array_block_name')
!!      type(c_ptr) function c_chara_array_num(c_ctl)                   &
!!     &          bind(C, NAME = 'c_chara_array_num')
!!      type(c_ptr) function c_chara_array_icou(c_ctl)                  &
!!     &          bind(C, NAME = 'c_chara_array_icou')
!!      type(c_ptr) function c_chara_array_c_tbl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_chara_array_c_tbl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
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
!!@endverbatim
      module ctl_array_singles_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_real
      use t_control_array_charareal
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_chara_from_c(c_ctl)                               &
     &          bind(C, NAME = 'load_chara_from_c')
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), pointer :: c_in(:)
      integer :: i
!
      call c_f_pointer(c_ctl, c_in, [kchara])
!
      do i = 1, kchara
        if(c_in(i) .eq. char(0)) then
          c_in(i:kchara) = char(32)
          exit
        end if
      end do
      end subroutine load_chara_from_c
!
!  ---------------------------------------------------------------------
!
      subroutine c_chara_item_clength(c_ctl, length_c)                  &
     &          bind(C, NAME = 'c_chara_item_clength')
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), intent(inout) :: length_c
      character(len=kchara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      length_c = len_trim(f_ctl)
      end subroutine c_chara_item_clength
!
!  ---------------------------------------------------------------------
!
      integer(C_int) function c_yes_flag(c_ctl)                         &
     &          bind(C, NAME = 'c_yes_flag')
      use skip_comment_f
      type(c_ptr), value, intent(in) :: c_ctl
      character(len=kchara), pointer :: f_ctl
!
      c_yes_flag = 0
      call c_f_pointer(c_ctl, f_ctl)
      if(yes_flag(f_ctl)) c_yes_flag = 1
      end function c_yes_flag
!
!  ---------------------------------------------------------------------
!
      integer(C_int) function c_no_file_flag(c_ctl)                     &
     &          bind(C, NAME = 'c_no_file_flag')
      use skip_comment_f
      type(c_ptr), value, intent(in) :: c_ctl
      character(len=kchara), pointer :: f_ctl
!
      c_no_file_flag = 0
      call c_f_pointer(c_ctl, f_ctl)
      if(no_file_flag(f_ctl)) c_no_file_flag = 1
      end function c_no_file_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_item_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_chara_item_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_character_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_item_block_name = C_loc(f_ctl%item_name)
      end function c_chara_item_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_item_iflag(c_ctl)                    &
     &          bind(C, NAME = 'c_chara_item_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_character_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_item_iflag = C_loc(f_ctl%iflag)
      end function c_chara_item_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_item_charavalue(c_ctl)               &
     &          bind(C, NAME = 'c_chara_item_charavalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_character_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_item_charavalue = C_loc(f_ctl%charavalue)
      end function c_chara_item_charavalue
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_array_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_chara_array_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_array_block_name = C_loc(f_ctl%array_name)
      end function c_chara_array_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_array_num(c_ctl)                     &
     &          bind(C, NAME = 'c_chara_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_array_num = C_loc(f_ctl%num)
      end function c_chara_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_array_icou(c_ctl)                    &
     &          bind(C, NAME = 'c_chara_array_icou')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_array_icou = C_loc(f_ctl%icou)
      end function c_chara_array_icou
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_array_c_tbl(c_ctl)                   &
     &          bind(C, NAME = 'c_chara_array_c_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_array_c_tbl = C_loc(f_ctl%c_tbl)
      end function c_chara_array_c_tbl
!
!  ---------------------------------------------------------------------
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
      type(c_ptr) function c_chara_real_item_realvalue(c_ctl)           &
     &          bind(C, NAME = 'c_chara_real_item_realvalue')
      type(c_ptr), value, intent(in) :: c_ctl
      type(read_chara_real_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_item_realvalue = C_loc(f_ctl%realvalue)
      end function c_chara_real_item_realvalue
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
      type(c_ptr) function c_chara_real_array_num(c_ctl)                &
     &          bind(C, NAME = 'c_chara_real_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_num = C_loc(f_ctl%num)
      end function c_chara_real_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_real_array_icou(c_ctl)               &
     &          bind(C, NAME = 'c_chara_real_array_icou')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_icou = C_loc(f_ctl%icou)
      end function c_chara_real_array_icou
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_real_array_c_tbl(c_ctl)              &
     &          bind(C, NAME = 'c_chara_real_array_c_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_c_tbl = C_loc(f_ctl%c_tbl)
      end function c_chara_real_array_c_tbl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_real_array_r_tbl(c_ctl)              &
     &          bind(C, NAME = 'c_chara_real_array_r_tbl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_real_array_r_tbl = C_loc(f_ctl%vect)
      end function c_chara_real_array_r_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_chara_real_array(c_ctl)               &
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
      subroutine c_alloc_chara_real_array(c_ctl)                &
     &          bind(C, NAME = 'c_alloc_chara_real_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_cr), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call alloc_control_array_c_r(f_ctl)
      end subroutine c_alloc_chara_real_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_chara_real_array(c_ctl)               &
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
      end module ctl_array_singles_to_c
