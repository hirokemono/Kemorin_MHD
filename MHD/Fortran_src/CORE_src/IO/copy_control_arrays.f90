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
!!      subroutine copy_control_array_int(num_copy, org_i1, tgt_i1)
!!        type(ctl_array_int), intent(in) ::    org_i1
!!        type(ctl_array_int), intent(inout) :: tgt_i1
!!      subroutine copy_control_array_c1(num_copy, org_c1, tgt_c1)
!!        type(ctl_array_chara), intent(in) ::    org_c1
!!        type(ctl_array_chara), intent(inout) :: tgt_c1
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
      end module copy_control_arrays
