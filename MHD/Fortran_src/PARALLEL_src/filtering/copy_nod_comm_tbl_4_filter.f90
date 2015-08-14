!copy_nod_comm_tbl_4_filter.f90
!      module copy_nod_comm_tbl_4_filter
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine copy_node_data_to_filter
!      subroutine copy_comm_table_to_filter
!
      module copy_nod_comm_tbl_4_filter
!
      use m_precision
!
      use m_nod_filter_comm_table
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_node_data_to_filter
!
      use m_geometry_data
!
!
      inter_nod_3dfilter = node1%internal_node
      nnod_filtering =     node1%numnod
      call allocate_globalnod_filter
!
      id_globalnod_filtering(1:node1%numnod)                            &
     &                               = inod_global(1:node1%numnod)
      xx_filtering(1:node1%numnod,1) = node1%xx(1:node1%numnod,1)
      xx_filtering(1:node1%numnod,2) = node1%xx(1:node1%numnod,2)
      xx_filtering(1:node1%numnod,3) = node1%xx(1:node1%numnod,3)
!
      end subroutine copy_node_data_to_filter
!
!------------------------------------------------------------------
!
      subroutine copy_comm_table_to_filter
!
      use m_nod_comm_table
      use t_comm_table
!
!
      call copy_comm_tbl_types(nod_comm, flt_comm)
!
      end subroutine copy_comm_table_to_filter
!
!------------------------------------------------------------------
!
      end module copy_nod_comm_tbl_4_filter
