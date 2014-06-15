!copy_partitioner_comm_table.f90
!     module copy_partitioner_comm_table
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine copy_all_import_to_mem(ip)
!      subroutine copy_all_import_from_mem(ip)
!
!      subroutine copy_all_export_to_mem(ip)
!      subroutine copy_all_export_from_mem(ip)
!
!      subroutine copy_all_import_num_tmp(ip)
!      subroutine copy_all_import_item_tmp(ip)
!
      module copy_partitioner_comm_table
!
      use m_precision
!
      use t_comm_table
      use m_partitioner_comm_table
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_all_import_to_mem(ip)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
!
!
      ele_comm_tbl_part(ip)%num_neib = num_neib_2
      ele_comm_tbl_part(ip)%ntot_import = ntot_import_ele_2
!
      surf_comm_tbl_part(ip)%num_neib = num_neib_2
      surf_comm_tbl_part(ip)%ntot_import = ntot_import_surf_2
!
      edge_comm_tbl_part(ip)%num_neib = num_neib_2
      edge_comm_tbl_part(ip)%ntot_import = edge_comm_2nd%ntot_import
!
      call allocate_type_neib_id( ele_comm_tbl_part(ip) )
      call allocate_type_import_num( ele_comm_tbl_part(ip) )
!
      call allocate_type_neib_id( surf_comm_tbl_part(ip) )
      call allocate_type_import_num( surf_comm_tbl_part(ip) )
!
      call allocate_type_neib_id( edge_comm_tbl_part(ip) )
      call allocate_type_import_num( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%id_neib(1:num_neib_2)                       &
     &       = id_neib_2(1:num_neib_2)
      ele_comm_tbl_part(ip)%istack_import(0:num_neib_2)                 &
     &       = istack_import_ele_2(0:num_neib_2)
!
      surf_comm_tbl_part(ip)%id_neib(1:num_neib_2)                      &
     &       = id_neib_2(1:num_neib_2)
      surf_comm_tbl_part(ip)%istack_import(0:num_neib_2)                &
     &       = istack_import_surf_2(0:num_neib_2)
!
      edge_comm_tbl_part(ip)%id_neib(1:num_neib_2)                      &
     &       = id_neib_2(1:num_neib_2)
      edge_comm_tbl_part(ip)%istack_import(0:num_neib_2)                &
     &       = edge_comm_2nd%istack_import(0:num_neib_2)
!
      call allocate_type_import_item( ele_comm_tbl_part(ip) )
      call allocate_type_import_item( surf_comm_tbl_part(ip) )
      call allocate_type_import_item( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%item_import(1:ntot_import_ele_2)            &
     &       = item_import_ele_2(1:ntot_import_ele_2)
      surf_comm_tbl_part(ip)%item_import(1:ntot_import_surf_2)          &
     &       = item_import_surf_2(1:ntot_import_surf_2)
      edge_comm_tbl_part(ip)%item_import(1:edge_comm_2nd%ntot_import) &
     &       = edge_comm_2nd%item_import(1:edge_comm_2nd%ntot_import)
!
      call copy_node_import_to_mem(ip)
!
      end subroutine copy_all_import_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_all_import_from_mem(ip)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
      integer(kind = kint) :: i
!
!
      call copy_node_import_from_mem(ip)
!
      num_neib_ele_2 =  num_neib_2
      num_neib_surf_2 = num_neib_2
      edge_comm_2nd%num_neib = num_neib_2
!
      ntot_import_ele_2 =  ele_comm_tbl_part(ip)%ntot_import
      ntot_import_surf_2 = surf_comm_tbl_part(ip)%ntot_import
      edge_comm_2nd%ntot_import = edge_comm_tbl_part(ip)%ntot_import
!
      call allocate_2nd_ele_neib_id
      call allocate_2nd_surf_neib_id
      call allocate_type_neib_id(edge_comm_2nd)
!
      call allocate_2nd_ele_import_num
      call allocate_2nd_surf_import_num
      call allocate_type_import_num(edge_comm_2nd)
!
      id_neib_ele_2(1:num_neib_2)                                       &
     &       = ele_comm_tbl_part(ip)%id_neib(1:num_neib_2)
      istack_import_ele_2(0:num_neib_2)                                 &
     &       = ele_comm_tbl_part(ip)%istack_import(0:num_neib_2)
!
      id_neib_surf_2(1:num_neib_2)                                      &
     &       = surf_comm_tbl_part(ip)%id_neib(1:num_neib_2)
      istack_import_surf_2(0:num_neib_2)                                &
     &       = surf_comm_tbl_part(ip)%istack_import(0:num_neib_2)
!
      edge_comm_2nd%id_neib(1:num_neib_2)                               &
     &       = edge_comm_tbl_part(ip)%id_neib(1:num_neib_2)
      edge_comm_2nd%istack_import(0:num_neib_2)    &
     &       = edge_comm_tbl_part(ip)%istack_import(0:num_neib_2)
!
      do i = 1, num_neib_2
          num_import_ele_2(i) = istack_import_ele_2(i)                  &
     &                          - istack_import_ele_2(i-1)
          num_import_surf_2(i) = istack_import_surf_2(i)                &
     &                          - istack_import_surf_2(i-1)
          edge_comm_2nd%num_import(i) = edge_comm_2nd%istack_import(i)   &
     &                          - edge_comm_2nd%istack_import(i-1)
      end do
!
      call allocate_2nd_ele_import_item
      call allocate_2nd_surf_import_item
      call allocate_type_import_item(edge_comm_2nd)
!
      item_import_ele_2(1:ntot_import_ele_2)                            &
     &       = ele_comm_tbl_part(ip)%item_import(1:ntot_import_ele_2)
      item_import_surf_2(1:ntot_import_surf_2)                          &
     &       = surf_comm_tbl_part(ip)%item_import(1:ntot_import_surf_2)
      edge_comm_2nd%item_import(1:edge_comm_2nd%ntot_import)   &
     &       = edge_comm_tbl_part(ip)%item_import(1:edge_comm_2nd%ntot_import)
!
      end subroutine copy_all_import_from_mem
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_all_export_to_mem(ip)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
!
!
      ele_comm_tbl_part(ip)%ntot_export =  ntot_export_ele_2
      surf_comm_tbl_part(ip)%ntot_export = ntot_export_surf_2
      edge_comm_tbl_part(ip)%ntot_export = edge_comm_2nd%ntot_export
!
      call allocate_type_export_num( ele_comm_tbl_part(ip) )
      call allocate_type_export_num( surf_comm_tbl_part(ip) )
      call allocate_type_export_num( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%istack_export(0:num_neib_2)                 &
     &       = istack_export_ele_2(0:num_neib_2)
      surf_comm_tbl_part(ip)%istack_export(0:num_neib_2)                &
     &       = istack_export_surf_2(0:num_neib_2)
      edge_comm_tbl_part(ip)%istack_export(0:num_neib_2)                &
     &       = edge_comm_2nd%istack_export(0:num_neib_2)
!
      call allocate_type_export_item( ele_comm_tbl_part(ip) )
      call allocate_type_export_item( surf_comm_tbl_part(ip) )
      call allocate_type_export_item( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%item_export(1:ntot_export_ele_2)            &
     &       = item_export_ele_2(1:ntot_export_ele_2)
      surf_comm_tbl_part(ip)%item_export(1:ntot_export_surf_2)          &
     &       = item_export_surf_2(1:ntot_export_surf_2)
      edge_comm_tbl_part(ip)%item_export(1:edge_comm_2nd%ntot_export)   &
     &       = edge_comm_2nd%item_export(1:edge_comm_2nd%ntot_export)
!
      call copy_node_export_to_mem(ip)
!
      end subroutine copy_all_export_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_all_export_from_mem(ip)
!
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
      use m_2nd_geometry_data
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
      integer(kind = kint) :: i
!
!
      call copy_node_export_from_mem(ip)
!
      ntot_export_ele_2 =  ele_comm_tbl_part(ip)%ntot_export
      ntot_export_surf_2 = surf_comm_tbl_part(ip)%ntot_export
      edge_comm_2nd%ntot_export = edge_comm_tbl_part(ip)%ntot_export
!
      call allocate_2nd_ele_export_num
      call allocate_2nd_surf_export_num
      call allocate_type_export_num(edge_comm_2nd)
!
      istack_export_ele_2(0:num_neib_2)                                 &
     &       = ele_comm_tbl_part(ip)%istack_export(0:num_neib_2)
      istack_export_surf_2(0:num_neib_2)                                &
     &       = surf_comm_tbl_part(ip)%istack_export(0:num_neib_2)
      edge_comm_2nd%istack_export(0:num_neib_2)                         &
     &       = edge_comm_tbl_part(ip)%istack_export(0:num_neib_2)
!
      do i = 1, num_neib_2
          num_export_ele_2(i) = istack_export_ele_2(i)                  &
     &                          - istack_export_ele_2(i-1)
          num_export_surf_2(i) = istack_export_surf_2(i)                &
     &                          - istack_export_surf_2(i-1)
          edge_comm_2nd%num_export(i) = edge_comm_2nd%istack_export(i)  &
     &                          - edge_comm_2nd%istack_export(i-1)
      end do
!
      call allocate_2nd_ele_export_item
      call allocate_2nd_surf_export_item
      call allocate_type_export_item(edge_comm_2nd)
!
      item_export_ele_2(1:ntot_export_ele_2)                            &
     &       = ele_comm_tbl_part(ip)%item_export(1:ntot_export_ele_2)
      item_export_surf_2(1:ntot_export_surf_2)                          &
     &       = surf_comm_tbl_part(ip)%item_export(1:ntot_export_surf_2)
      edge_comm_2nd%item_export(1:edge_comm_2nd%ntot_export)            &
     &       = edge_comm_tbl_part(ip)%item_export(1:edge_comm_2nd%ntot_export)
!
      end subroutine copy_all_export_from_mem
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_all_import_num_tmp(ip)
!
      use work_comm_table_IO
!
      integer(kind = kint), intent(in) :: ip
!
!
      NP_TMP = nod_comm_tbl_part(ip)%num_neib
!
      call allocate_all_import_num_tmp
!
      NEIB_TMP(1:NP_TMP) = nod_comm_tbl_part(ip)%id_neib(1:NP_TMP)
      ISTACK_NOD_TMP(0:NP_TMP)                                          &
     &        =  nod_comm_tbl_part(ip)%istack_import(0:NP_TMP)
      ISTACK_ELE_TMP(0:NP_TMP)                                          &
     &        =  ele_comm_tbl_part(ip)%istack_import(0:NP_TMP)
      ISTACK_SURF_TMP(0:NP_TMP)                                         &
     &        = surf_comm_tbl_part(ip)%istack_import(0:NP_TMP)
      ISTACK_EDGE_TMP(0:NP_TMP)                                         &
     &        = edge_comm_tbl_part(ip)%istack_import(0:NP_TMP)
!
      end subroutine copy_all_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine copy_all_import_item_tmp(ip)
!
      use work_comm_table_IO
!
      integer(kind = kint), intent(in) :: ip
!
!
      call allocate_all_import_item_tmp
!
      IMPORT_NOD_TMP(1:NTOT_NOD_TMP)                                    &
     &        =  nod_comm_tbl_part(ip)%item_import(1:NTOT_NOD_TMP)
      IMPORT_ELE_TMP(1:NTOT_ELE_TMP)                                    &
     &        =  ele_comm_tbl_part(ip)%item_import(1:NTOT_ELE_TMP)
      IMPORT_SURF_TMP(1:NTOT_SURF_TMP)                                  &
     &        =  surf_comm_tbl_part(ip)%item_import(1:NTOT_SURF_TMP)
      IMPORT_EDGE_TMP(1:NTOT_EDGE_TMP)                                  &
     &        =  edge_comm_tbl_part(ip)%item_import(1:NTOT_EDGE_TMP)
!
      end subroutine copy_all_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module copy_partitioner_comm_table
