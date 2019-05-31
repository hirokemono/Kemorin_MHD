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
!!      subroutine append_control_array_r3(read_r3, array_r3)
!!        type(read_real3_item), intent(inout) ::    read_r3
!!        type(ctl_array_r3), intent(inout) :: array_r3
!!      subroutine append_control_array_int(read_i1, array_i1)
!!        type(read_integer_item), intent(inout) ::    read_i1
!!        type(ctl_array_int), intent(inout) :: array_i1
!!      subroutine append_control_array_i2(read_i2, array_i2)
!!        type(read_int2_item), intent(inout) ::    read_i2
!!        type(ctl_array_i2), intent(inout) :: array_i2
!!      subroutine append_control_array_c1(read_c1, array_c1)
!!        type(read_character_item), intent(inout) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!      subroutine append_control_array_c2(read_c2, array_c2)
!!        type(read_chara2_item), intent(inout) ::    read_c2
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!      subroutine append_control_array_c_r(read_cr, array_cr)
!!        type(read_chara_real_item), intent(inout) ::    read_cr
!!        type(ctl_array_cr), intent(inout) :: array_cr
!!      subroutine append_control_array_c_i(read_ci, array_ci)
!!        type(read_chara_int_item), intent(inout) ::    read_ci
!!        type(ctl_array_ci), intent(inout) :: array_ci
!!      subroutine append_control_array_c_r2(read_cr2, array_cr2)
!!        type(read_chara_real2_item), intent(inout) ::    read_cr2
!!        type(ctl_array_cr2), intent(inout) :: array_cr2
!!      subroutine append_control_array_c2_r(read_c2r, array_c2r)
!!        type(read_chara2_real_item), intent(inout) ::    read_c2r
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!      subroutine append_control_array_i_c_r(read_icr, array_icr)
!!        type(read_int_chara_real_item), intent(inout) ::    read_icr
!!        type(ctl_array_icr), intent(inout) :: array_icr
!!      subroutine append_control_array_i_r(read_ir, array_ir)
!!        type(read_int_real_item), intent(inout) ::    read_ir
!!        type(ctl_array_ir), intent(inout) :: array_ir
!!      subroutine append_control_array_i2_r(read_i2r, array_i2r)
!!        type(read_int2_real_item), intent(inout) ::    read_i2r
!!        type(ctl_array_i2r), intent(inout) :: array_i2r
!!      subroutine append_control_array_i2_r2(read_i2r2, array_i2r2)
!!        type(read_int2_real2_item), intent(inout) ::    read_i2r2
!!        type(ctl_array_i2r2), intent(inout) :: array_i2r2
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
      subroutine append_control_array_r3(read_r3, array_r3)
!
      type(read_real3_item), intent(inout) ::    read_r3
      type(ctl_array_r3), intent(inout) :: array_r3
!
      type(ctl_array_r3) ::    org_r3
!
!
      org_r3%num = array_r3%num
      call alloc_control_array_r3(org_r3)
      call copy_control_array_r3(org_r3%num, array_r3, org_r3)
      call dealloc_control_array_r3(array_r3)
!
      array_r3%num = org_r3%num + 1
      call alloc_control_array_r3(array_r3)
      call copy_control_array_r3(org_r3%num, org_r3, array_r3)
      call append_control_item_r3(read_r3, array_r3)
      read_r3%iflag = 0
!
      call dealloc_control_array_r3(org_r3)
!
      end subroutine append_control_array_r3
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
      subroutine append_control_array_i2(read_i2, array_i2)
!
      type(read_int2_item), intent(inout) ::    read_i2
      type(ctl_array_i2), intent(inout) :: array_i2
!
      type(ctl_array_i2) ::    org_i2
!
!
      org_i2%num = array_i2%num
      call alloc_control_array_i2(org_i2)
      call copy_control_array_i2(org_i2%num, array_i2, org_i2)
      call dealloc_control_array_i2(array_i2)
!
      array_i2%num = org_i2%num + 1
      call alloc_control_array_i2(array_i2)
      call copy_control_array_i2(org_i2%num, org_i2, array_i2)
      call append_control_item_i2(read_i2, array_i2)
      read_i2%iflag = 0
!
      call dealloc_control_array_i2(org_i2)
!
      end subroutine append_control_array_i2
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
!
      subroutine append_control_array_c2(read_c2, array_c2)
!
      type(read_chara2_item), intent(inout) ::    read_c2
      type(ctl_array_c2), intent(inout) :: array_c2
!
      type(ctl_array_c2) ::    org_c2
!
!
      org_c2%num = array_c2%num
      call alloc_control_array_c2(org_c2)
      call copy_control_array_c2(org_c2%num, array_c2, org_c2)
      call dealloc_control_array_c2(array_c2)
!
      array_c2%num = org_c2%num + 1
      call alloc_control_array_c2(array_c2)
      call copy_control_array_c2(org_c2%num, org_c2, array_c2)
      call append_control_item_c2(read_c2, array_c2)
      read_c2%iflag = 0
!
      call dealloc_control_array_c2(org_c2)
!
      end subroutine append_control_array_c2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_control_array_c_r(read_cr, array_cr)
!
      type(read_chara_real_item), intent(inout) ::    read_cr
      type(ctl_array_cr), intent(inout) :: array_cr
!
      type(ctl_array_cr) ::    org_cr
!
!
      org_cr%num = array_cr%num
      call alloc_control_array_c_r(org_cr)
      call copy_control_array_c_r(org_cr%num, array_cr, org_cr)
      call dealloc_control_array_c_r(array_cr)
!
      array_cr%num = org_cr%num + 1
      call alloc_control_array_c_r(array_cr)
      call copy_control_array_c_r(org_cr%num, org_cr, array_cr)
      call append_control_item_c_r(read_cr, array_cr)
      read_cr%iflag = 0
!
      call dealloc_control_array_c_r(org_cr)
!
      end subroutine append_control_array_c_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_c_i(read_ci, array_ci)
!
      type(read_chara_int_item), intent(inout) ::    read_ci
      type(ctl_array_ci), intent(inout) :: array_ci
!
      type(ctl_array_ci) ::    org_ci
!
!
      org_ci%num = array_ci%num
      call alloc_control_array_c_i(org_ci)
      call copy_control_array_c_i(org_ci%num, array_ci, org_ci)
      call dealloc_control_array_c_i(array_ci)
!
      array_ci%num = org_ci%num + 1
      call alloc_control_array_c_i(array_ci)
      call copy_control_array_c_i(org_ci%num, org_ci, array_ci)
      call append_control_item_c_i(read_ci, array_ci)
      read_ci%iflag = 0
!
      call dealloc_control_array_c_i(org_ci)
!
      end subroutine append_control_array_c_i
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_c_r2(read_cr2, array_cr2)
!
      type(read_chara_real2_item), intent(inout) ::    read_cr2
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
      type(ctl_array_cr2) ::    org_cr2
!
!
      org_cr2%num = array_cr2%num
      call alloc_control_array_c_r2(org_cr2)
      call copy_control_array_c_r2(org_cr2%num, array_cr2, org_cr2)
      call dealloc_control_array_c_r2(array_cr2)
!
      array_cr2%num = org_cr2%num + 1
      call alloc_control_array_c_r2(array_cr2)
      call copy_control_array_c_r2(org_cr2%num, org_cr2, array_cr2)
      call append_control_item_c_r2(read_cr2, array_cr2)
      read_cr2%iflag = 0
!
      call dealloc_control_array_c_r2(org_cr2)
!
      end subroutine append_control_array_c_r2
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_c2_r(read_c2r, array_c2r)
!
      type(read_chara2_real_item), intent(inout) ::    read_c2r
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
      type(ctl_array_c2r) ::    org_c2r
!
!
      org_c2r%num = array_c2r%num
      call alloc_control_array_c2_r(org_c2r)
      call copy_control_array_c2_r(org_c2r%num, array_c2r, org_c2r)
      call dealloc_control_array_c2_r(array_c2r)
!
      array_c2r%num = org_c2r%num + 1
      call alloc_control_array_c2_r(array_c2r)
      call copy_control_array_c2_r(org_c2r%num, org_c2r, array_c2r)
      call append_control_item_c2_r(read_c2r, array_c2r)
      read_c2r%iflag = 0
!
      call dealloc_control_array_c2_r(org_c2r)
!
      end subroutine append_control_array_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_i_c_r(read_icr, array_icr)
!
      type(read_int_chara_real_item), intent(inout) ::    read_icr
      type(ctl_array_icr), intent(inout) :: array_icr
!
      type(ctl_array_icr) ::    org_icr
!
!
      org_icr%num = array_icr%num
      call alloc_control_array_i_c_r(org_icr)
      call copy_control_array_i_c_r(org_icr%num, array_icr, org_icr)
      call dealloc_control_array_i_c_r(array_icr)
!
      array_icr%num = org_icr%num + 1
      call alloc_control_array_i_c_r(array_icr)
      call copy_control_array_i_c_r(org_icr%num, org_icr, array_icr)
      call append_control_item_i_c_r(read_icr, array_icr)
      read_icr%iflag = 0
!
      call dealloc_control_array_i_c_r(org_icr)
!
      end subroutine append_control_array_i_c_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_i_r(read_ir, array_ir)
!
      type(read_int_real_item), intent(inout) ::    read_ir
      type(ctl_array_ir), intent(inout) :: array_ir
!
      type(ctl_array_ir) ::    org_ir
!
!
      org_ir%num = array_ir%num
      call alloc_control_array_i_r(org_ir)
      call copy_control_array_i_r(org_ir%num, array_ir, org_ir)
      call dealloc_control_array_i_r(array_ir)
!
      array_ir%num = org_ir%num + 1
      call alloc_control_array_i_r(array_ir)
      call copy_control_array_i_r(org_ir%num, org_ir, array_ir)
      call append_control_item_i_r(read_ir, array_ir)
      read_ir%iflag = 0
!
      call dealloc_control_array_i_r(org_ir)
!
      end subroutine append_control_array_i_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_i2_r(read_i2r, array_i2r)
!
      type(read_int2_real_item), intent(inout) ::    read_i2r
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
      type(ctl_array_i2r) ::    org_i2r
!
!
      org_i2r%num = array_i2r%num
      call alloc_control_array_i2_r(org_i2r)
      call copy_control_array_i2_r(org_i2r%num, array_i2r, org_i2r)
      call dealloc_control_array_i2_r(array_i2r)
!
      array_i2r%num = org_i2r%num + 1
      call alloc_control_array_i2_r(array_i2r)
      call copy_control_array_i2_r(org_i2r%num, org_i2r, array_i2r)
      call append_control_item_i2_r(read_i2r, array_i2r)
      read_i2r%iflag = 0
!
      call dealloc_control_array_i2_r(org_i2r)
!
      end subroutine append_control_array_i2_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_array_i2_r2(read_i2r2, array_i2r2)
!
      type(read_int2_real2_item), intent(inout) ::    read_i2r2
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
      type(ctl_array_i2r2) ::    org_i2r2
!
!
      org_i2r2%num = array_i2r2%num
      call alloc_control_array_i2_r2(org_i2r2)
      call copy_control_array_i2_r2(org_i2r2%num, array_i2r2, org_i2r2)
      call dealloc_control_array_i2_r2(array_i2r2)
!
      array_i2r2%num = org_i2r2%num + 1
      call alloc_control_array_i2_r2(array_i2r2)
      call copy_control_array_i2_r2(org_i2r2%num, org_i2r2, array_i2r2)
      call append_control_item_i2_r2(read_i2r2, array_i2r2)
      read_i2r2%iflag = 0
!
      call dealloc_control_array_i2_r2(org_i2r2)
!
      end subroutine append_control_array_i2_r2
!
! -----------------------------------------------------------------------
!
      end module append_control_array
