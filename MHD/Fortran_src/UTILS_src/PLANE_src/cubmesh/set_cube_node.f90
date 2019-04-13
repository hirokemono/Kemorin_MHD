!set_cube_node.f90
!     module set_cube_node
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_node(nb_rng, ipe, jpe)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_cube_node
!
      use m_precision
!
      use t_neib_range_cube
      use t_sleeve_cube
      use m_size_of_cube
      use m_cube_position
      use m_cube_files_data
      use m_local_node_id_cube
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
      subroutine set_node(nb_rng, ipe, jpe)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inod
!
! ..... write 2.mesh information (nodes and elements in partition)
!
            write(l_out,'( a )') '!'
            write(l_out,'( a )')                                        &
     &        '! 2.mesh information (nodes and elements in partition)'
            write(l_out,'( a )')                                        &
     &        '! 2.1 node'

            write(l_out,'(10i16)')  nodtot,intnodtot
!
! ***** set and write coordinate for internal nodes

            inod = 0
!
            call set_internal_size(nb_rng, sl_rng1)
            call set_internal_node(nb_rng, sl_rng1, inod)
!
! ***** set and write coordinate for sleeve area nodes
!
            call set_sleeve_node(nb_rng, inod)
            call set_sleeve_node_peri(nb_rng, ipe, jpe, inod)
!
! ***** set table from node id to x,y,z, positions
!
            call set_inod_table
            call check_inod_table
!
      end subroutine set_node
!
! ----------------------------------------------------------------------
!
      end module set_cube_node
