!>@file   append_control_array.f90
!!@brief  module append_control_array
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine append_control_array_real(read_r1, array_r1)
!!        type(read_real_item), intent(inout) ::    read_r1
!!        type(ctl_array_real), intent(inout) :: array_r1
!!      subroutine append_control_array_r2(read_r2, array_r2)
!!        type(read_real2_item), intent(inout) ::    read_r2
!!        type(ctl_array_r2), intent(inout) :: array_r2
!!      subroutine append_control_array_int(read_i1, array_i1)
!!        type(read_integer_item), intent(inout) ::    read_i1
!!        type(ctl_array_int), intent(inout) :: array_i1
!!      subroutine append_control_array_c1(read_c1, array_c1)
!!        type(read_character_item), intent(inout) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!@endverbatim
!!
      module append_control_array
!
      use m_precision
!
      use t_control_elements
      use t_read_control_arrays
!
      use copy_control_arrays
      use append_control_items
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_real(read_r1, array_r1)
!
      type(read_real_item), intent(inout) ::    read_r1
      type(ctl_array_real), intent(inout) :: array_r1
!
      type(ctl_array_real) ::    org_r1
!
!
      org_r1%num = array_r1%num
      call alloc_control_array_real(org_r1)
      call copy_control_array_real(org_r1%num, array_r1, org_r1)
      call dealloc_control_array_real(array_r1)
!
      array_r1%num = org_r1%num + 1
      call alloc_control_array_real(array_r1)
      call copy_control_array_real(org_r1%num, org_r1, array_r1)
      call append_control_item_real(read_r1, array_r1)
      read_r1%iflag = 0
!
      call dealloc_control_array_real(org_r1)
!
      end subroutine append_control_array_real
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_r2(read_r2, array_r2)
!
      type(read_real2_item), intent(inout) ::    read_r2
      type(ctl_array_r2), intent(inout) :: array_r2
!
      type(ctl_array_r2) ::    org_r2
!
!
      org_r2%num = array_r2%num
      call alloc_control_array_r2(org_r2)
      call copy_control_array_r2(org_r2%num, array_r2, org_r2)
      call dealloc_control_array_r2(array_r2)
!
      array_r2%num = org_r2%num + 1
      call alloc_control_array_r2(array_r2)
      call copy_control_array_r2(org_r2%num, org_r2, array_r2)
      call append_control_item_r2(read_r2, array_r2)
      read_r2%iflag = 0
!
      call dealloc_control_array_r2(org_r2)
!
      end subroutine append_control_array_r2
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_int(read_i1, array_i1)
!
      type(read_integer_item), intent(inout) ::    read_i1
      type(ctl_array_int), intent(inout) :: array_i1
!
      type(ctl_array_int) ::    org_i1
!
!
      org_i1%num = array_i1%num
      call alloc_control_array_int(org_i1)
      call copy_control_array_int(org_i1%num, array_i1, org_i1)
      call dealloc_control_array_int(array_i1)
!
      array_i1%num = org_i1%num + 1
      call alloc_control_array_int(array_i1)
      call copy_control_array_int(org_i1%num, org_i1, array_i1)
      call append_control_item_int(read_i1, array_i1)
      read_i1%iflag = 0
!
      call dealloc_control_array_int(org_i1)
!
      end subroutine append_control_array_int
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_c1(read_c1, array_c1)
!
      type(read_character_item), intent(inout) ::    read_c1
      type(ctl_array_chara), intent(inout) :: array_c1
!
      type(ctl_array_chara) ::    org_c1
!
!
      org_c1%num = array_c1%num
      call alloc_control_array_chara(org_c1)
      call copy_control_array_c1(org_c1%num, array_c1, org_c1)
      call dealloc_control_array_chara(array_c1)
!
      array_c1%num = org_c1%num + 1
      call alloc_control_array_chara(array_c1)
      call copy_control_array_c1(org_c1%num, org_c1, array_c1)
      call append_control_item_c1(read_c1, array_c1)
      read_c1%iflag = 0
!
      call dealloc_control_array_chara(org_c1)
!
      end subroutine append_control_array_c1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module append_control_array
