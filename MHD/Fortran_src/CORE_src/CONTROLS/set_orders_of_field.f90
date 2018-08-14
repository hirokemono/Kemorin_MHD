!>@file   set_orders_of_field.f90
!!@brief  module set_orders_of_field
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in Jan., 2010
!
!> @brief Chenge order of field by element type
!!
!!@verbatim
!!      subroutine s_set_orders_of_field(fld_ctl, nnod_4_ele,           &
!!     &          num_nod_phys, phys_nod_name, iorder_nod_phys)
!!        type(field_control), intent(in) :: fld_ctl
!!@endverbatim
!
      module set_orders_of_field
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_orders_of_field(fld_ctl, nnod_4_ele,             &
     &          num_nod_phys, phys_nod_name, iorder_nod_phys)
!
      use m_geometry_constants
      use t_ctl_data_4_fields
!
      type(field_control), intent(in) :: fld_ctl
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len = kchara), intent(in)                               &
     &                     :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: iorder_nod_phys(num_nod_phys)
!
      integer(kind = kint) :: i, j
!
!
      iorder_nod_phys(1:num_nod_phys) = nnod_4_ele
!
      if(nnod_4_ele .eq. num_t_quad) then
        iorder_nod_phys(1:num_nod_phys) = num_t_linear
!
        do i = 1, num_nod_phys
          do j = 1, fld_ctl%quad_phys%num
            if(phys_nod_name(i) .eq. fld_ctl%quad_phys%c_tbl(j)) then
              iorder_nod_phys(i) = num_t_quad
              exit
            end if
          end do
      end if
!
      end subroutine s_set_orders_of_field
!
! -----------------------------------------------------------------------
!
      end module set_orders_of_field
