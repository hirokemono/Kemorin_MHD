!!set_cube_node.f90
!     module set_cube_node
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_node(c_size, c_each, c_vert, nb_rng, ipe, jpe,   &
!!     &          loc_id, node)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(inout) :: loc_id
!!        type(node_data), intent(inout) :: node
!
      module set_cube_node
!
      use m_precision
!
      use t_neib_range_cube
      use t_sleeve_cube
      use t_size_of_cube
      use t_cube_position
      use t_local_node_id_cube
      use t_geometry_data
      use set_internal_nod_cube
      use set_sleeve_node_cube
      use set_sleeve_nod_peri_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_node(c_size, c_each, c_vert, nb_rng, ipe, jpe,     &
     &          loc_id, node)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
!
      type(local_node_id_cube), intent(inout) :: loc_id
      type(node_data), intent(inout) :: node
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inod
!
!
      node%numnod =        c_each%nodtot
      node%internal_node = c_each%intnodtot
      call alloc_node_geometry_base(node)
!
! ***** set and write coordinate for internal nodes

      inod = 0
!
      call set_internal_size(nb_rng, sl_rng1)
      call set_internal_node                                            &
     &   (c_size, c_vert, nb_rng, sl_rng1, loc_id, node, inod)
!
! ***** set and write coordinate for sleeve area nodes
!
      call set_sleeve_node(c_size, c_vert, nb_rng, loc_id, node, inod)
      call set_sleeve_node_peri(c_size, c_vert, nb_rng, ipe, jpe,       &
     &    loc_id, node, inod)
!
! ***** set table from node id to x,y,z, positions
!
      call set_inod_table(c_each, loc_id)
      call check_inod_table(c_each, loc_id)
!
      end subroutine set_node
!
! ----------------------------------------------------------------------
!
      end module set_cube_node
