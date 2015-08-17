!
!      module set_node_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_node_geometry_from_IO
!
      module set_node_geometry_4_IO
!
      use m_precision
!
      use m_geometry_data
!
      use m_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_node_geometry_from_IO
!
      use set_node_types_4_IO
!
!
      call copy_node_type_from_IO(node1)
      call allocate_sph_node_geometry(node1)
      call allocate_node_geometry
!
      end subroutine copy_node_geometry_from_IO
!
!------------------------------------------------------------------
!
      end module set_node_geometry_4_IO
