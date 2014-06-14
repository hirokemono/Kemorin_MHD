!
!     module set_mesh_from_2nd
!
!      written by H. Matsui on June, 2007
!
!      subroutine s_set_mesh_from_2nd
!      subroutine set_geometry_data_from_2nd
!
      module set_mesh_from_2nd
!
      use m_precision
!
      implicit  none
!
!
      private :: copy_node_comm_tbl_from_2nd
      private :: copy_node_geometry_from_2nd
      private :: copy_element_connect_from_2nd
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_mesh_from_2nd
!
      use set_group_data_from_2nd
!
!
      call set_geometry_data_from_2nd
      call s_set_group_data_from_2nd
!
      end subroutine s_set_mesh_from_2nd
!
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_from_2nd
!
      use m_geometry_data
!
!
      call copy_node_comm_tbl_from_2nd
!
      call copy_node_geometry_from_2nd
!
      call copy_element_connect_from_2nd
      call allocate_element_geometry
!
      end subroutine set_geometry_data_from_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_from_2nd
!
      use m_nod_comm_table
      use m_2nd_nod_comm_table
      use copy_communication_table
!
!
      num_neib = num_neib_2
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_communication(num_neib, id_neib,                    &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    id_neib_2, istack_import_2, istack_export_2)
      call copy_num_import_export(num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, item_import_2, item_export_2)
!
      call deallocate_2nd_nod_export
      call deallocate_2nd_nod_import
      call deallocate_2nd_neib_id
!
      end subroutine copy_node_comm_tbl_from_2nd
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_geometry_from_2nd
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      integer(kind = kint) :: inod
!
!
      numnod =        nnod_2nd
      internal_node = internal_nod_2nd
!
      call allocate_node_geometry
!
!$omp parallel do
      do inod = 1, numnod
        globalnodid(inod) = globalnodid_2nd(inod)
        xx(inod,1) = xx_2nd(inod,1)
        xx(inod,2) = xx_2nd(inod,2)
        xx(inod,3) = xx_2nd(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_2nd_node_position
!
      end subroutine copy_node_geometry_from_2nd
!
!------------------------------------------------------------------
!
      subroutine copy_element_connect_from_2nd
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
!
      integer(kind = kint) :: iele, k1
!
!
      numele =            ele_2nd%numele
      first_ele_type = ele_2nd%first_ele_type
!
      nnod_4_ele =  ele_2nd%nnod_4_ele
      nnod_4_surf = surf_2nd%nnod_4_surf
      nnod_4_edge = edge_2nd%nnod_4_edge
!
      call allocate_element_connection
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_ele
!$omp do private(iele)
        do iele = 1, numele
          ie(iele,k1) = ele_2nd%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, numele
        globalelmid(iele) = ele_2nd%iele_global(iele)
        elmtyp(iele) =      ele_2nd%elmtyp(iele)
        nodelm(iele) =      ele_2nd%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      call deallocate_ele_connect_type(ele_2nd)
!
      end subroutine copy_element_connect_from_2nd
!
!------------------------------------------------------------------
!
      end module set_mesh_from_2nd
