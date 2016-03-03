!
!     module m_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!!      subroutine set_bc_velo_id(node, ele, fluid, nod_grp)
!!      subroutine set_bc_press_id                                      &
!!     &         (node, ele, fluid, nod_grp, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(group_data), intent(in) :: nod_grp
!
      module m_bc_data_velo
!
      use m_precision
      use t_bc_data_velo
!
      implicit  none
!
      type (nodal_bcs_4_momentum_type), save :: Vnod1_bcs
!
      end module m_bc_data_velo
