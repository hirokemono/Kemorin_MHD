!set_cube_node_quad.f90
!     module set_cube_node_quad
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_node_quad(nb_rng, ipe, jpe, kpe)
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_cube_node_quad
!
      use m_precision
!
      use t_neib_range_cube
      use m_size_4_plane
      use m_size_of_cube
      use m_offset_size_cube
      use m_cube_position
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
      subroutine set_node_quad(nb_rng, ipe, jpe, kpe)
!
      use set_sleeve_edge_peri_cube
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind = kint) :: inod, nd
      integer (kind = kint) :: inp,  jnp,  knp
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
! *****  initialization to construct node information
!
            write(*,*) 'init_node_parameter_4_each_pe', ipe, jpe, kpe
            call init_node_para_4_each_pe(ipe, jpe, kpe)
            call set_offset_of_domain(ipe, jpe, kpe)
!
! *****   set position of internal node
!
            inod = 0
!
            call set_internal_size(nb_rng)
            call set_internal_node(inod)
!
!     set position of internal edge
!
            nd = 1
            inp = 0
            call set_internal_edge_size(nb_rng, nd)
            write(*,*) 'set_internal_edge', ipe, jpe, kpe
            call set_internal_edge(kpe, inp, jnp, knp, inod, nd)
!
            nd = 2
            jnp = 0
            call set_internal_edge_size(nb_rng, nd)
            call set_internal_edge(kpe, inp, jnp, knp, inod, nd)
!
            nd = 3
            knp = -1
            call set_internal_edge_size(nb_rng, nd)
            call set_internal_edge(kpe, inp, jnp, knp, inod, nd)
!
! ***** set and write coordinate for sleeve area nodes
!
            write(*,*) 'set_sleeve_node_quad', ipe, jpe, kpe
            call set_sleeve_node_quad(nb_rng, kpe, inod)
!
! ***** set and write for sleeve area nodes for periodical boundary
!
            write(*,*) 'set_sleeve_node_peri_quad', ipe, jpe, kpe
            call set_sleeve_node_peri_quad(nb_rng, ipe, jpe, inod)
!
! ***** set and write for sleeve area edge for periodical boundary
!
            write(*,*) 'set_sleeve_edge_peri', ipe, jpe, kpe
            call set_sleeve_edge_peri(nb_rng, ipe, jpe, kpe, inod)
!
! ***** set table from node id to x,y,z, positions
!
            call set_inod_table
            call set_iedge_table
            call check_inod_table
!
!
      end subroutine set_node_quad
!
! ----------------------------------------------------------------------
!
      end module set_cube_node_quad
