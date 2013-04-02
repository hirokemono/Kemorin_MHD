!
!      module const_comm_tables_4_peri
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine s_const_comm_tables_4_peri
!
      module const_comm_tables_4_peri
!
      use m_precision
!
      use m_domain_group_4_partition
      use set_ele_import_items_peri
      use set_ele_export_items_peri
!
      implicit  none
!
      private :: const_ele_comm_table_4_peri
      private :: const_edge_comm_table_4_peri
      private :: const_surf_comm_table_4_peri
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_comm_tables_4_peri
!
      use m_nod_comm_table
!
!
      if (num_neib .eq. 1) then
        call const_ele_comm_table_4_peri
        call const_edge_comm_table_4_peri
        call const_surf_comm_table_4_peri
      end if
!
      end subroutine s_const_comm_tables_4_peri
!
!------------------------------------------------------------------
!
      subroutine const_ele_comm_table_4_peri
!
      use m_nod_comm_table
      use m_ele_comm_table
      use m_geometry_parameter
      use m_geometry_data
!
!
      num_neib_ele = num_neib
!
      call allocate_ele_neib_id
      call allocate_ele_import_num
      call allocate_ele_export_num
!
      id_neib_ele(1:num_neib) = id_neib(1:num_neib)
!
      call count_import_ele_peri(internal_node, numele, nnod_4_ele,     &
     &    ie, num_import_ele, istack_import_ele, ntot_import_ele,       &
     &    num_export_ele, istack_export_ele, ntot_export_ele)
!
      call allocate_ele_import_item
      call allocate_ele_export_item
!
      call set_import_ele_peri(internal_node, numele, nnod_4_ele,       &
     &    ie, istack_import_ele, ntot_import_ele, item_import_ele)
!
      call set_ele_export_item_peri(id_glnode_org)
!
      end subroutine const_ele_comm_table_4_peri
!
!------------------------------------------------------------------
!
      subroutine const_surf_comm_table_4_peri
!
      use m_nod_comm_table
      use m_surf_comm_table
      use m_geometry_parameter
      use m_geometry_data
!
!
      num_neib_surf = num_neib
!
      call allocate_surf_neib_id
      call allocate_surf_import_num
      call allocate_surf_export_num
!
      id_neib_surf(1:num_neib) = id_neib(1:num_neib)
!
      call count_import_ele_peri(internal_node, numsurf, nnod_4_surf,   &
     &    ie_surf, num_import_surf, istack_import_surf,                 &
     &    ntot_import_surf, num_export_surf, istack_export_surf,        &
     &    ntot_export_surf)
!
      call allocate_surf_import_item
      call allocate_surf_export_item
!
      call set_import_ele_peri(internal_node, numsurf, nnod_4_surf,     &
     &    ie_surf, istack_import_surf, ntot_import_surf,                &
     &    item_import_surf)
!
      call set_surf_export_item_peri(id_glnode_org)
!
      end subroutine const_surf_comm_table_4_peri
!
!------------------------------------------------------------------
!
      subroutine const_edge_comm_table_4_peri
!
      use m_nod_comm_table
      use m_edge_comm_table
      use m_geometry_parameter
      use m_geometry_data
!
!
      num_neib_edge = num_neib
!
      call allocate_edge_neib_id
      call allocate_edge_import_num
      call allocate_edge_export_num
!
      id_neib_edge(1:num_neib) = id_neib(1:num_neib)
!
      call count_import_ele_peri(internal_node, numedge, nnod_4_edge,   &
     &    ie_edge, num_import_edge, istack_import_edge,                 &
     &    ntot_import_edge, num_export_edge, istack_export_edge,        &
     &    ntot_export_edge)
!
      call allocate_edge_import_item
      call allocate_edge_export_item
!
      call set_import_ele_peri(internal_node, numedge, nnod_4_edge,     &
     &    ie_edge, istack_import_edge, ntot_import_edge,                &
     &    item_import_edge)
!
      call set_edge_export_item_peri(id_glnode_org)
!
      end subroutine const_edge_comm_table_4_peri
!
!------------------------------------------------------------------
!
      end module const_comm_tables_4_peri
