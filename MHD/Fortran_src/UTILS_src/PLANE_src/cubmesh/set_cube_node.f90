!!set_cube_node.f90
!     module set_cube_node
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_node                                             &
!!     &         (c_size, c_each, c_vert, nb_rng, ipe, jpe, loc_id)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(inout) :: loc_id
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
      use m_cube_files_data
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
      subroutine set_node                                               &
     &         (c_size, c_each, c_vert, nb_rng, ipe, jpe, loc_id)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
!
      type(local_node_id_cube), intent(inout) :: loc_id
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inod
!
! ..... write 2.mesh information (nodes and elements in partition)
!
      write(l_out,'( a )') '!'
      write(l_out,'( a )')                                              &
     &        '! 2.mesh information (nodes and elements in partition)'
      write(l_out,'( a )')                                              &
     &        '! 2.1 node'

      write(l_out,'(10i16)')  c_each%nodtot, c_each%intnodtot
!
! ***** set and write coordinate for internal nodes

      inod = 0
!
      call set_internal_size(nb_rng, sl_rng1)
      call set_internal_node                                            &
     &   (c_size, c_vert, nb_rng, sl_rng1, loc_id, inod)
!
! ***** set and write coordinate for sleeve area nodes
!
      call set_sleeve_node(c_size, c_vert, nb_rng, loc_id, inod)
      call set_sleeve_node_peri(c_size, c_vert, nb_rng, ipe, jpe,       &
     &    loc_id, inod)
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
