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
!!     &          itp_dest, inod_stack_type, nnod_interpolate_type)
!!        type(interpolate_table_dest), intent(in) :: itp_dest
!!@endverbatim
!
      module set_interpolate_type_8
!
      use m_precision
      use m_constants
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
     &          itp_dest, inod_stack_type, nnod_interpolate_type)
!
      use t_interpolate_tbl_dest
      use m_interpolate_coefs_dest
      use m_work_const_itp_table
!
      type(interpolate_table_dest), intent(in) :: itp_dest
      integer(kind = kint), intent(in) :: my_rank, ist, ied
      integer(kind = kint), intent(in) :: inod_stack_type(0:4)
!
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
        if (     coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 1
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 2
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 3
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 4
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 5
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 6
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 7
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(1) = nnod_interpolate_type(1) + 1
          icou = inod_stack_type(0) + nnod_interpolate_type(1)
          itype_inter_dest(icou) = 8
          call swap_interpolation_table(icou, inod, itp_dest)
!
!
!   for edges
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &     .and. coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 101
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 102
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 103
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 104
          call swap_interpolation_table(icou, inod, itp_dest)
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 105
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 106
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 107
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 108
          call swap_interpolation_table(icou, inod, itp_dest)
!
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 109
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 110
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 111
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(2) = nnod_interpolate_type(2) + 1
          icou = inod_stack_type(1) + nnod_interpolate_type(2)
          itype_inter_dest(icou) = 112
          call swap_interpolation_table(icou, inod, itp_dest)
!
!
!   for surfaces
!
!
        else if( coef_inter_dest(inod,1) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itype_inter_dest(icou) = 201
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itype_inter_dest(icou) = 202
          call swap_interpolation_table(icou, inod, itp_dest)
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq. -one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itype_inter_dest(icou) = 203
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .eq.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itype_inter_dest(icou) = 204
          call swap_interpolation_table(icou, inod, itp_dest)
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq. -one) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itype_inter_dest(icou) = 205
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .eq.  one) then
!
          nnod_interpolate_type(3) = nnod_interpolate_type(3) + 1
          icou = inod_stack_type(2) + nnod_interpolate_type(3)
          itype_inter_dest(icou) = 206
          call swap_interpolation_table(icou, inod, itp_dest)
!
!
!   for volume
!
!
        else if( coef_inter_dest(inod,1) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,1) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,2) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,2) .lt.  one                      &
     &    .and.  coef_inter_dest(inod,3) .gt. -one                      &
     &    .and.  coef_inter_dest(inod,3) .lt.  one                      &
     &  ) then
!
          nnod_interpolate_type(4) = nnod_interpolate_type(4) + 1
          icou = inod_stack_type(3) + nnod_interpolate_type(4)
          itype_inter_dest(icou) = 0
          call swap_interpolation_table(icou, inod, itp_dest)
!
        else
          write(*,*) 'Where I am???', my_rank,                          &
     &              itp_dest%inod_dest_4_dest(inod),                    &
     &              iele_org_4_dest(inod), coef_inter_dest(inod,1:3)
        end if
      end do
!
      end subroutine s_order_interpolate_type_8
!
!-----------------------------------------------------------------------
!
      end module set_interpolate_type_8
