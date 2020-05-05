!>@file   copy_control_elements.f90
!!        module copy_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!!
!>@brief  Subroutines to copy control items
!!
!!@verbatim
!!      subroutine copy_chara_ctl(org_c1, new_c1)
!!        type(read_character_item), intent(in) :: org_c1
!!        type(read_character_item), intent(inout) :: new_c1
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
!
      end module copy_control_elements
