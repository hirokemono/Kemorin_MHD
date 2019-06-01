!>@file   copy_control_arrays.f90
!!@brief  module copy_control_arrays
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine copy_control_array_real(num_copy, org_r1, tgt_r1)
!!        type(ctl_array_real), intent(in) ::    org_r1
!!        type(ctl_array_real), intent(inout) :: tgt_r1
!!      subroutine copy_control_array_r2(num_copy, org_r2, tgt_r2)
!!        type(ctl_array_r2), intent(in) ::    org_r2
!!        type(ctl_array_r2), intent(inout) :: tgt_r2
!!      subroutine copy_control_array_r3(num_copy, org_r3, tgt_r3)
!!        type(ctl_array_r3), intent(in) ::    org_r3
!!        type(ctl_array_r3), intent(inout) :: tgt_r3
!!      subroutine copy_control_array_int(num_copy, org_i1, tgt_i1)
!!        type(ctl_array_int), intent(in) ::    org_i1
!!        type(ctl_array_int), intent(inout) :: tgt_i1
!!      subroutine copy_control_array_i2(num_copy, org_i2, tgt_i2)
!!        type(ctl_array_i2), intent(in) ::    org_i2
!!        type(ctl_array_i2), intent(inout) :: tgt_i2
!!      subroutine copy_control_array_c1(num_copy, org_c1, tgt_c1)
!!        type(ctl_array_chara), intent(in) ::    org_c1
!!        type(ctl_array_chara), intent(inout) :: tgt_c1
!!      subroutine copy_control_array_c_r(num_copy, org_cr, tgt_cr)
!!        type(ctl_array_cr), intent(in) ::    org_cr
!!        type(ctl_array_cr), intent(inout) :: tgt_cr
!!      subroutine copy_control_array_c_i(num_copy, org_ci, tgt_ci)
!!        type(ctl_array_ci), intent(in) ::    org_ci
!!        type(ctl_array_ci), intent(inout) :: tgt_ci
!!      subroutine copy_control_array_c_r2(num_copy, org_cr2, tgt_cr2)
!!        type(ctl_array_cr2), intent(in) ::    org_cr2
!!        type(ctl_array_cr2), intent(inout) :: tgt_cr2
!!@endverbatim
!!
      module copy_control_arrays
!
      use m_precision
!
      use t_read_control_arrays
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_real(num_copy, org_r1, tgt_r1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_real), intent(in) ::    org_r1
      type(ctl_array_real), intent(inout) :: tgt_r1
!
!
      if(num_copy .le. 0) return
      tgt_r1%icou = org_r1%icou
      tgt_r1%vect(1:num_copy) = org_r1%vect(1:num_copy)
!
      end subroutine copy_control_array_real
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_r2(num_copy, org_r2, tgt_r2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_r2), intent(in) ::    org_r2
      type(ctl_array_r2), intent(inout) :: tgt_r2
!
!
      if(num_copy .le. 0) return
      tgt_r2%icou = org_r2%icou
      tgt_r2%vec1(1:num_copy) = org_r2%vec1(1:num_copy)
      tgt_r2%vec2(1:num_copy) = org_r2%vec2(1:num_copy)
!
      end subroutine copy_control_array_r2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_r3(num_copy, org_r3, tgt_r3)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_r3), intent(in) ::    org_r3
      type(ctl_array_r3), intent(inout) :: tgt_r3
!
!
      if(num_copy .le. 0) return
      tgt_r3%icou = org_r3%icou
      tgt_r3%vec1(1:num_copy) = org_r3%vec1(1:num_copy)
      tgt_r3%vec2(1:num_copy) = org_r3%vec2(1:num_copy)
      tgt_r3%vec3(1:num_copy) = org_r3%vec3(1:num_copy)
!
      end subroutine copy_control_array_r3
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_int(num_copy, org_i1, tgt_i1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_int), intent(in) ::    org_i1
      type(ctl_array_int), intent(inout) :: tgt_i1
!
!
      if(num_copy .le. 0) return
      tgt_i1%icou = org_i1%icou
      tgt_i1%ivec(1:num_copy) = org_i1%ivec(1:num_copy)
!
      end subroutine copy_control_array_int
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_i2(num_copy, org_i2, tgt_i2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_i2), intent(in) ::    org_i2
      type(ctl_array_i2), intent(inout) :: tgt_i2
!
!
      if(num_copy .le. 0) return
      tgt_i2%icou = org_i2%icou
      tgt_i2%int1(1:num_copy) = org_i2%int1(1:num_copy)
      tgt_i2%int2(1:num_copy) = org_i2%int2(1:num_copy)
!
      end subroutine copy_control_array_i2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c1(num_copy, org_c1, tgt_c1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_chara), intent(in) ::    org_c1
      type(ctl_array_chara), intent(inout) :: tgt_c1
!
!
      if(num_copy .le. 0) return
      tgt_c1%icou = org_c1%icou
      tgt_c1%c_tbl(1:num_copy) = org_c1%c_tbl(1:num_copy)
!
      end subroutine copy_control_array_c1
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_r(num_copy, org_cr, tgt_cr)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_cr), intent(in) ::    org_cr
      type(ctl_array_cr), intent(inout) :: tgt_cr
!
!
      if(num_copy .le. 0) return
      tgt_cr%icou = org_cr%icou
      tgt_cr%c_tbl(1:num_copy) = org_cr%c_tbl(1:num_copy)
      tgt_cr%vect(1:num_copy) =  org_cr%vect(1:num_copy)
!
      end subroutine copy_control_array_c_r
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_i(num_copy, org_ci, tgt_ci)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_ci), intent(in) ::    org_ci
      type(ctl_array_ci), intent(inout) :: tgt_ci
!
!
      if(num_copy .le. 0) return
      tgt_ci%icou = org_ci%icou
      tgt_ci%c_tbl(1:num_copy) = org_ci%c_tbl(1:num_copy)
      tgt_ci%ivec(1:num_copy) =  org_ci%ivec(1:num_copy)
!
      end subroutine copy_control_array_c_i
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_r2(num_copy, org_cr2, tgt_cr2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_cr2), intent(in) ::    org_cr2
      type(ctl_array_cr2), intent(inout) :: tgt_cr2
!
!
      if(num_copy .le. 0) return
      tgt_cr2%icou = org_cr2%icou
      tgt_cr2%c_tbl(1:num_copy) = org_cr2%c_tbl(1:num_copy)
      tgt_cr2%vec1(1:num_copy) =  org_cr2%vec1(1:num_copy)
      tgt_cr2%vec2(1:num_copy) =  org_cr2%vec2(1:num_copy)
!
      end subroutine copy_control_array_c_r2
!
! -----------------------------------------------------------------------
!
      end module copy_control_arrays
