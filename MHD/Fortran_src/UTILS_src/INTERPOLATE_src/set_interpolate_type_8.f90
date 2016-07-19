!>@file   set_interpolate_type_8.f90
!!@brief  module set_interpolate_type_8
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Sep., 2006
!
!>@brief Sort interpolation table by interpolation type
!!       for linear elements
!!
!!@verbatim
!!      subroutine s_order_interpolate_type_8(my_rank, ist, ied,        &
!!     &          itp_dest, inod_stack_type, nnod_interpolate_type,     &
!!     &          itp_coef_dest)
!!        type(interpolate_table_dest), intent(in) :: itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!!@endverbatim
!
      module set_interpolate_type_8
!
      use m_precision
      use m_constants
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_order_interpolate_type_8(my_rank, ist, ied,          &
     &          itp_dest, inod_stack_type, nnod_interpolate_type,       &
     &          itp_coef_dest)
!
      use m_work_const_itp_table
!
      type(interpolate_table_dest), intent(in) :: itp_dest
      integer(kind = kint), intent(in) :: my_rank, ist, ied
      integer(kind = kint), intent(in) :: inod_stack_type(0:4)
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout)                               &
     &      :: nnod_interpolate_type(4)
!
      integer(kind = kint) :: inod, icou
!
!
      nnod_interpolate_type(1:4) = 0
      do inod = ist, ied
!
!   for nodes
!
        if (      itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 1
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 2
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 3
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 4
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 5
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 6
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 7
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itp_coef_dest%itype_inter_dest(icou) = 8
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
!
!   for edges
!
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &     .and.  itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 101
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 102
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 103
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 104
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 105
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 106
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 107
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 108
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 109
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 110
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 111
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itp_coef_dest%itype_inter_dest(icou) = 112
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
!
!   for surfaces
!
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itp_coef_dest%itype_inter_dest(icou) = 201
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itp_coef_dest%itype_inter_dest(icou) = 202
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itp_coef_dest%itype_inter_dest(icou) = 203
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .eq.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itp_coef_dest%itype_inter_dest(icou) = 204
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itp_coef_dest%itype_inter_dest(icou) = 205
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itp_coef_dest%itype_inter_dest(icou) = 206
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
!
!   for volume
!
!
        else if(  itp_coef_dest%coef_inter_dest(inod,1) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,1) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,2) .lt.  one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .gt. -one       &
     &    .and.   itp_coef_dest%coef_inter_dest(inod,3) .lt.  one       &
     &  ) then
!
          nnod_interpolate_type(4) = nnod_interpolate_type(4) + 1
          icou = inod_stack_type(3) + nnod_interpolate_type(4)
          itp_coef_dest%itype_inter_dest(icou) = 0
          call swap_interpolation_table                                 &
     &       (icou, inod, itp_dest, itp_coef_dest)
!
        else
          write(*,*) 'Where I am???', my_rank,                          &
     &              itp_dest%inod_dest_4_dest(inod),                    &
     &              itp_coef_dest%iele_org_4_dest(inod),                &
     &              itp_coef_dest%coef_inter_dest(inod,1:3)
        end if
      end do
!
      end subroutine s_order_interpolate_type_8
!
!-----------------------------------------------------------------------
!
      end module set_interpolate_type_8
