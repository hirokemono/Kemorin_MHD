!
!      module set_filter_geometry_4_IO
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine copy_filtering_geometry_to_IO
!      subroutine copy_filtering_geometry_from_IO
!
      module set_filter_geometry_4_IO
!
      use m_precision
!
      use m_nod_filter_comm_table
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
      subroutine copy_filtering_geometry_to_IO
!
!
      numnod_dummy = nnod_filtering
      internal_node_dummy = inter_nod_3dfilter
      call allocate_node_data_dummy
!
      globalnodid_dummy(1:nnod_filtering)                               &
     &      = id_globalnod_filtering(1:nnod_filtering)
      xx_dummy(1:nnod_filtering,1) = xx_filtering(1:nnod_filtering,1)
      xx_dummy(1:nnod_filtering,2) = xx_filtering(1:nnod_filtering,2)
      xx_dummy(1:nnod_filtering,3) = xx_filtering(1:nnod_filtering,3)
!
      end subroutine copy_filtering_geometry_to_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_filtering_geometry_from_IO
!
!
      nnod_filtering = numnod_dummy
      inter_nod_3dfilter = internal_node_dummy
      call allocate_globalnod_filter
!
      id_globalnod_filtering(1:nnod_filtering)                          &
     &      = globalnodid_dummy(1:nnod_filtering)
      xx_filtering(1:nnod_filtering,1) = xx_dummy(1:nnod_filtering,1)
      xx_filtering(1:nnod_filtering,2) = xx_dummy(1:nnod_filtering,2)
      xx_filtering(1:nnod_filtering,3) = xx_dummy(1:nnod_filtering,3)
!
      call deallocate_node_data_dummy
!
      end subroutine copy_filtering_geometry_from_IO
!
!------------------------------------------------------------------
!
      end module set_filter_geometry_4_IO
