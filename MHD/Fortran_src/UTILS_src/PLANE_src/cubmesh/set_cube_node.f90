!set_cube_node.f90
!     module set_cube_node
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine set_node(ipe, jpe, kpe)
!
      module set_cube_node
!
      use m_precision
!
      use m_size_of_cube
      use m_offset_size_cube
      use m_cube_position
      use m_neighb_range_cube
      use m_sleeve_cube
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
      subroutine set_node(ipe, jpe, kpe)
!
      integer (kind = kint) :: ipe, jpe, kpe
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
! ***** set coordinate off set (starting corner for pe node)
! ***** set nodal position off set (i,j,k starting position -1)
!
            call init_node_para_4_each_pe(ipe, jpe, kpe)
            call set_offset_of_domain(ipe, jpe, kpe)
!
! ***** set and write coordinate for internal nodes
!
            call s_set_range_4_nodeloop(kpe)

            inod = 0
!
            call set_internal_size
            call set_internal_node(inod)
!
! ***** set and write coordinate for sleeve area nodes
!
            call set_sleeve_node(inod)
!
!
            call set_sleeve_node_peri(ipe, jpe, inod)
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
