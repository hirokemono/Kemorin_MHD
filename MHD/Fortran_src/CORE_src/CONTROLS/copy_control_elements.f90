!>@file   copy_control_elements.f90
!!        module copy_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!!
!>@brief  Subroutines to copy control items
!!
!!@verbatim
!!      subroutine copy_real_ctl(org_r1, new_r1)
!!        type(read_real_item), intent(in) :: org_r1
!!        type(read_real_item), intent(inout) :: new_r1
!!      subroutine copy_integer_ctl(org_i1, new_i1)
!!        type(read_integer_item), intent(in) :: org_i1
!!        type(read_integer_item), intent(inout) :: new_i1
!!      subroutine copy_chara_ctl(org_c1, new_c1)
!!        type(read_character_item), intent(in) :: org_c1
!!        type(read_character_item), intent(inout) :: new_c1
!!      subroutine copy_real2_ctl(org_r2, new_r2)
!!        type(read_real2_item), intent(inout) :: org_r2
!!        type(read_real2_item), intent(inout) :: new_r2
!!      subroutine copy_real3_ctl(org_r3, new_r3)
!!        type(read_real3_item), intent(in) :: org_r3
!!        type(read_real3_item), intent(inout) :: new_r3
!!      subroutine copy_integer2_ctl(org_i2, new_i2)
!!        type(read_int2_item), intent(in) :: org_i2
!!        type(read_int2_item), intent(inout) :: new_i2
!!      subroutine copy_character2_ctl(org_c2, new_c2)
!!        type(read_chara2_item), intent(in) :: org_c2
!!        type(read_chara2_item), intent(inout) :: new_c2
!!@endverbatim
!
      module copy_control_elements
!
      use m_precision
      use m_machine_parameter
      use t_control_elements
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine copy_real_ctl(org_r1, new_r1)
!
      type(read_real_item), intent(in) :: org_r1
      type(read_real_item), intent(inout) :: new_r1
!
!
      new_r1%iflag =     org_r1%iflag
      new_r1%realvalue = org_r1%realvalue
!
      end subroutine copy_real_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_integer_ctl(org_i1, new_i1)
!
      type(read_integer_item), intent(in) :: org_i1
      type(read_integer_item), intent(inout) :: new_i1
!
!
      new_i1%iflag =    org_i1%iflag
      new_i1%intvalue = org_i1%intvalue
!
       end subroutine copy_integer_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_chara_ctl(org_c1, new_c1)
!
      type(read_character_item), intent(in) :: org_c1
      type(read_character_item), intent(inout) :: new_c1
!
!
      new_c1%iflag =      org_c1%iflag
      new_c1%charavalue = org_c1%charavalue
!
       end subroutine copy_chara_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_real2_ctl(org_r2, new_r2)
!
      type(read_real2_item), intent(in) :: org_r2
      type(read_real2_item), intent(inout) :: new_r2
!
!
      new_r2%iflag =          org_r2%iflag
      new_r2%realvalue(1:2) = org_r2%realvalue(1:2)
!
       end subroutine copy_real2_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_real3_ctl(org_r3, new_r3)
!
      type(read_real3_item), intent(in) :: org_r3
      type(read_real3_item), intent(inout) :: new_r3
!
!
      new_r3%iflag =          org_r3%iflag
      new_r3%realvalue(1:3) = org_r3%realvalue(1:3)
!
       end subroutine copy_real3_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_integer2_ctl(org_i2, new_i2)
!
      type(read_int2_item), intent(in) :: org_i2
      type(read_int2_item), intent(inout) :: new_i2
!
      new_i2%iflag =         org_i2%iflag
      new_i2%intvalue(1:2) = org_i2%intvalue(1:2)
!
      end subroutine copy_integer2_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_character2_ctl(org_c2, new_c2)
!
      type(read_chara2_item), intent(in) :: org_c2
      type(read_chara2_item), intent(inout) :: new_c2
!
!
      new_c2%iflag =           org_c2%iflag
      new_c2%charavalue(1:2) = org_c2%charavalue(1:2)
!
       end subroutine copy_character2_ctl
!
!   --------------------------------------------------------------------
!
      end module copy_control_elements
