!
!      module set_orders_of_field
!
!        programmed by H.Matsui on Jan., 2010
!
!      subroutine s_set_orders_of_field(nnod_4_ele,                     &
!     &          num_nod_phys, phys_nod_name, iorder_nod_phys)
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
      subroutine s_set_orders_of_field(nnod_4_ele,                      &
     &          num_nod_phys, phys_nod_name, iorder_nod_phys)
!
      use m_geometry_constants
      use m_ctl_data_4_fields
!
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
!
        do i = 1, num_nod_phys
          do j = 1, num_quad_field_ctl
            if (phys_nod_name(i) .eq. quad_phys_name_ctl(j)) then
              iorder_nod_phys(i) = num_t_quad
              exit
            end if
          end do
!
          do j = 1, num_linear_field_ctl
            if (phys_nod_name(i) .eq. linear_phys_name_ctl(j)) then
              iorder_nod_phys(i) = num_t_linear
              exit
            end if
          end do
        end do
!
      end if
!
      end subroutine s_set_orders_of_field
!
! -----------------------------------------------------------------------
!
      end module set_orders_of_field
