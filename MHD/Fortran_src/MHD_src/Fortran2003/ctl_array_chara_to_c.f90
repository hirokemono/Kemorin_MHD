!>@file   ctl_array_chara_to_c.f90
!!        module ctl_array_chara_to_c
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
!!      subroutine c_store_chara_item_charavalue(c_ctl, c_in)           &
!!     &          bind(C, NAME = 'c_store_chara_item_charavalue')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        character(C_char), intent(in) :: c_in(*)
!!
!!      type(c_ptr) function c_chara_array_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_chara_array_block_name')
!!      integer(c_int) function c_chara_array_num(c_ctl)                &
!!     &          bind(C, NAME = 'c_chara_array_num')
!!      type(c_ptr) function c_chara_array_c_tbl(idx_in, c_ctl)         &
!!     &          bind(C, NAME = 'c_chara_array_c_tbl')
!!      subroutine c_store_chara_array(c_ctl, idx_in, c_in)             &
!!     &          bind(C, NAME = 'c_store_chara_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!        integer(C_int), value, intent(in) :: idx_in
!!        character(C_char), intent(in) :: c_in(*)
!!
!!      subroutine c_dealloc_chara_array(c_ctl)                         &
!!     &          bind(C, NAME = 'c_dealloc_chara_array')
!!      subroutine c_alloc_chara_array(num, c_ctl)                      &
!!     &          bind(C, NAME = 'c_alloc_chara_array')
!!      subroutine c_check_chara_array(c_ctl)                           &
!!     &          bind(C, NAME = 'c_check_chara_array')
!!        integer(C_int), value, intent(in) :: num
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module ctl_array_chara_to_c
!
      use m_precision
      use iso_c_binding
      use t_control_array_character
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      character(len = kchara) function copy_char_from_c(c_in)
!
      character(C_char), intent(in) :: c_in(*)
      integer :: i
!
      do i = 1, kchara
        copy_char_from_c(i:i) = c_in(i)
        if(c_in(i) .eq. char(0)) then
          copy_char_from_c(i:kchara) = char(32)
          exit
        end if
      end do
      end function copy_char_from_c
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
!
      subroutine c_store_chara_item_charavalue(c_ctl, c_in)             &
     &          bind(C, NAME = 'c_store_chara_item_charavalue')
      type(c_ptr), value, intent(in) :: c_ctl
      character(C_char), intent(in) :: c_in(*)
      type(read_character_item), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%charavalue = copy_char_from_c(c_in)
      end subroutine c_store_chara_item_charavalue
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
      integer(c_int) function c_chara_array_num(c_ctl)                  &
     &          bind(C, NAME = 'c_chara_array_num')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_array_num = f_ctl%num
      end function c_chara_array_num
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_chara_array_c_tbl(idx_in, c_ctl)           &
     &          bind(C, NAME = 'c_chara_array_c_tbl')
      integer(C_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_chara_array_c_tbl = C_loc(f_ctl%c_tbl(idx_in+1))
      end function c_chara_array_c_tbl
!
!  ---------------------------------------------------------------------
!
      subroutine c_store_chara_array(c_ctl, idx_in, c_in)               &
     &          bind(C, NAME = 'c_store_chara_array')
!
      type(c_ptr), value, intent(in) :: c_ctl
      integer(C_int), value, intent(in) :: idx_in
      character(C_char), intent(in) :: c_in(*)
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%c_tbl(idx_in+1) = copy_char_from_c(c_in)
      end subroutine c_store_chara_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine c_dealloc_chara_array(c_ctl)                           &
     &          bind(C, NAME = 'c_dealloc_chara_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call dealloc_control_array_chara(f_ctl)
      end subroutine c_dealloc_chara_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_alloc_chara_array(num, c_ctl)                        &
     &          bind(C, NAME = 'c_alloc_chara_array')
      integer(C_int), value, intent(in) :: num
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      f_ctl%num =  num
      f_ctl%icou = num
      call alloc_control_array_chara(f_ctl)
      end subroutine c_alloc_chara_array
!
!  ---------------------------------------------------------------------
!
      subroutine c_check_chara_array(c_ctl)                             &
     &          bind(C, NAME = 'c_check_chara_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(ctl_array_chara), pointer :: f_ctl
!
      integer :: i
      call c_f_pointer(c_ctl, f_ctl)
       write(*,*) 'f_ctl%num', f_ctl%num, f_ctl%icou
      do i = 1, f_ctl%num
        write(*,*) 'data', f_ctl%c_tbl(i)
      end do

      end subroutine c_check_chara_array
!
!  ---------------------------------------------------------------------
!
      end module ctl_array_chara_to_c
