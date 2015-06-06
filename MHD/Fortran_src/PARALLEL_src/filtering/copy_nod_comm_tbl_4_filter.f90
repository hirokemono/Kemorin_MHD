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
      use m_geometry_parameter
      use m_geometry_data
!
!
      inter_nod_3dfilter = internal_node
      nnod_filtering = numnod
      call allocate_globalnod_filter
!
      id_globalnod_filtering(1:numnod) = inod_global(1:numnod)
      xx_filtering(1:numnod,1) = xx(1:numnod,1)
      xx_filtering(1:numnod,2) = xx(1:numnod,2)
      xx_filtering(1:numnod,3) = xx(1:numnod,3)
!
      end subroutine copy_node_data_to_filter
!
!------------------------------------------------------------------
!
      subroutine copy_comm_table_to_filter
!
      use m_nod_comm_table
!
!
      num_neib_filter = nod_comm%num_neib
      ntot_import_filter = ntot_import
      ntot_export_filter = ntot_export
!
      call allocate_neib_filter_id
      call allocate_filter_import_num
      call allocate_filter_export_num
      call allocate_filter_import_item
      call allocate_filter_export_item
!
      if (nod_comm%num_neib .gt. 0) then
        id_neib_filter(1:nod_comm%num_neib) =        id_neib(1:nod_comm%num_neib)
        num_import_filter(1:nod_comm%num_neib) =     num_import(1:nod_comm%num_neib)
        istack_import_filter(0:nod_comm%num_neib) =  istack_import(0:nod_comm%num_neib)
        num_export_filter(1:nod_comm%num_neib) =     num_export(1:nod_comm%num_neib)
        istack_export_filter(0:nod_comm%num_neib) =  istack_export(0:nod_comm%num_neib)
      end if
      if (ntot_import .gt. 0) then
        item_import_filter(1:ntot_import) = item_import(1:ntot_import)
      end if
      if (ntot_export .gt. 0) then
        item_export_filter(1:ntot_export) = item_export(1:ntot_export)
      end if
!
      end subroutine copy_comm_table_to_filter
!
!------------------------------------------------------------------
!
      end module copy_nod_comm_tbl_4_filter
