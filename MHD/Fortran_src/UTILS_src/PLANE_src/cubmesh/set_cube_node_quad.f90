!set_cube_node_quad.f90
!     module set_cube_node_quad
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_node_quad(c_size, c_each, c_vert, nb_rng,        &
!!     &          ipe, jpe, kpe)
!!        type(size_of_cube) :: c_size
!!        type(size_of_each_cube) :: c_each
!!        type(vertical_position_cube), intent(in) :: c_vert
!!        type(neib_range_cube), intent(in) :: nb_rng
!
      module set_cube_node_quad
!
      use m_precision
      use m_constants
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use t_cube_position
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
      subroutine set_node_quad(c_size, c_each, c_vert, nb_rng,          &
     &          ipe, jpe, kpe)
!
      use set_sleeve_edge_peri_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(vertical_position_cube), intent(in) :: c_vert
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      type(slleve_range) :: sl_rng1
      integer (kind = kint) :: inod
      integer (kind = kint) :: inp,  jnp,  knp
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
! *****   set position of internal node
!
      inod = 0
!
      call set_internal_size(nb_rng, sl_rng1)
      call set_internal_node(c_size, c_vert, nb_rng, sl_rng1, inod)
!
!     set position of internal edge
!
      inp = 0
      call set_internal_edge_size(nb_rng, ione, sl_rng1)
      write(*,*) 'set_internal_edge', ipe, jpe, kpe
      call set_internal_edge(c_size, c_vert, nb_rng, sl_rng1,           &
     &    kpe, inp, jnp, knp, inod, ione)
!
      jnp = 0
      call set_internal_edge_size(nb_rng, itwo, sl_rng1)
      call set_internal_edge(c_size, c_vert, nb_rng, sl_rng1,           &
     &    kpe, inp, jnp, knp, inod, itwo)
!
      knp = -1
      call set_internal_edge_size(nb_rng, ithree, sl_rng1)
      call set_internal_edge(c_size, c_vert, nb_rng, sl_rng1,           &
     &    kpe, inp, jnp, knp, inod, ithree)
!
! ***** set and write coordinate for sleeve area nodes
!
      write(*,*) 'set_sleeve_node_quad', ipe, jpe, kpe
      call set_sleeve_node_quad(c_size, c_vert, nb_rng, kpe, inod)
!
! ***** set and write for sleeve area nodes for periodical boundary
!
      write(*,*) 'set_sleeve_node_peri_quad', ipe, jpe, kpe
      call set_sleeve_node_peri_quad                                    &
     &   (c_size, c_vert, nb_rng, ipe, jpe, inod)
!
! ***** set and write for sleeve area edge for periodical boundary
!
      write(*,*) 'set_sleeve_edge_peri', ipe, jpe, kpe
      call set_sleeve_edge_peri                                         &
     &   (c_size, c_vert, nb_rng, ipe, jpe, kpe, inod)
!
! ***** set table from node id to x,y,z, positions
!
      call set_inod_table(c_each)
      call set_iedge_table(c_each)
      call check_inod_table(c_each)
!
!
      end subroutine set_node_quad
!
! ----------------------------------------------------------------------
!
      end module set_cube_node_quad
