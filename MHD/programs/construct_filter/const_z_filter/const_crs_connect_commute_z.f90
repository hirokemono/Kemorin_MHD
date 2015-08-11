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
      use m_crs_connect
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
      call set_num_off_diag_z_commute(internal_node)
!
      call allocate_crs_stack(node1%numnod)
      call allocate_crs_connect
!
      call set_stack_crs_z_commute(internal_node)
!
      call set_item_crs_z_commute(internal_node)
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
      ntot_crs_l = internal_node - 1
      ntot_crs_u = internal_node - 1
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
        istack_crs_l(i)   = i-1
      end do
!
!   set upper component
!
      do i = 1, internal_node-1
        istack_crs_u(i) = i
      end do
      istack_crs_u(internal_node) = internal_node-1
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
        item_crs_l(i) = i
      end do
!
!   set upper component
!
      do i = 1, internal_node-1
        item_crs_u(i) = i+1
      end do
!
      end subroutine set_item_crs_z_commute
!
! ----------------------------------------------------------------------
!
      end module const_crs_connect_commute_z
