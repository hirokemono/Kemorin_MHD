!
!      module set_2nd_node_geometry_4_IO
!
!     Written by H. Matsui on June, 2007
!
!      subroutine copy_2nd_node_geometry_to_IO
!      subroutine copy_2nd_node_geometry_from_IO
!
      module set_2nd_node_geometry_4_IO
!
      use m_precision
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
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
      subroutine copy_2nd_node_geometry_to_IO
!
!
      numnod_dummy = nnod_2nd
      internal_node_dummy = internal_nod_2nd
!
      call allocate_node_data_dummy
!
      globalnodid_dummy(1:nnod_2nd) = globalnodid_2nd(1:nnod_2nd)
      xx_dummy(1:nnod_2nd,1) = xx_2nd(1:nnod_2nd,1)
      xx_dummy(1:nnod_2nd,2) = xx_2nd(1:nnod_2nd,2)
      xx_dummy(1:nnod_2nd,3) = xx_2nd(1:nnod_2nd,3)
!
      end subroutine copy_2nd_node_geometry_to_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_2nd_node_geometry_from_IO
!
!
      nnod_2nd = numnod_dummy
      internal_nod_2nd = internal_node_dummy
!
      call allocate_2nd_node_position
!
      globalnodid_2nd(1:nnod_2nd) = globalnodid_dummy(1:nnod_2nd)
      xx_2nd(1:nnod_2nd,1) = xx_dummy(1:nnod_2nd,1)
      xx_2nd(1:nnod_2nd,2) = xx_dummy(1:nnod_2nd,2)
      xx_2nd(1:nnod_2nd,3) = xx_dummy(1:nnod_2nd,3)
!
      call deallocate_node_data_dummy
!
      end subroutine copy_2nd_node_geometry_from_IO
!
!------------------------------------------------------------------
!
      end module set_2nd_node_geometry_4_IO
