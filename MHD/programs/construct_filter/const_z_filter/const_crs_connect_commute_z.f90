!
!      module const_crs_connect_commute_z
!
!     Written by Hiroaki Matsui
!
!      subroutine set_crs_connect_commute_z
!
      module const_crs_connect_commute_z
!
      use m_precision
!
      use m_crs_matrix
      use m_commute_filter_z
!
      implicit none
!
      private :: set_num_off_diag_z_commute
      private :: set_stack_crs_z_commute, set_item_crs_z_commute
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_crs_connect_commute_z
!
      use m_geometry_data
!
!
      call set_num_off_diag_z_commute(node1%internal_node)
!
      call alloc_crs_stack(node1%numnod, tbl1_crs)
      call alloc_crs_connect(tbl1_crs)
!
      call set_stack_crs_z_commute(node1%internal_node)
!
      call set_item_crs_z_commute(node1%internal_node)
!
      end subroutine set_crs_connect_commute_z
!
! ----------------------------------------------------------------------
!
      subroutine set_num_off_diag_z_commute(internal_node)
!
      integer(kind = kint), intent(in) :: internal_node
!
!
      tbl1_crs%ntot_l = internal_node - 1
      tbl1_crs%ntot_u = internal_node - 1
!
      end subroutine set_num_off_diag_z_commute
!
! ----------------------------------------------------------------------
!
      subroutine set_stack_crs_z_commute(internal_node)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint) :: i
!
!    set lower component
!
      do i = 1, internal_node
        tbl1_crs%istack_l(i) = i-1
      end do
!
!   set upper component
!
      do i = 1, internal_node-1
        tbl1_crs%istack_u(i) = i
      end do
      tbl1_crs%istack_u(internal_node) = internal_node-1
!
      end subroutine set_stack_crs_z_commute
!
! ----------------------------------------------------------------------
!
      subroutine set_item_crs_z_commute(internal_node)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint) :: i
!
!    set lower component
!
      do i = 1, internal_node-1
        tbl1_crs%item_l(i) = i
      end do
!
!   set upper component
!
      do i = 1, internal_node-1
        tbl1_crs%item_u(i) = i+1
      end do
!
      end subroutine set_item_crs_z_commute
!
! ----------------------------------------------------------------------
!
      end module const_crs_connect_commute_z
