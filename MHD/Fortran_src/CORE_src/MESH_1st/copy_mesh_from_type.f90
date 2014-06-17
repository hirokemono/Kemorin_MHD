!
!     module copy_mesh_from_type
!
!      written by H. Matsui on June, 2007
!
!      subroutine set_mesh_from_type(mesh, group)
!      subroutine set_geometry_data_from_type(mesh)
!
      module copy_mesh_from_type
!
      use m_precision
!
      implicit  none
!
!
      private :: set_geometry_data_from_type
      private :: copy_node_comm_tbl_from_type
      private :: copy_node_geometry_from_type
      private :: copy_element_connect_from_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_mesh_from_type(mesh, group)
!
      use t_mesh_data
      use set_group_data_from_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call set_geometry_data_from_type(mesh)
      call group_data_from_type(group)
!
      end subroutine set_mesh_from_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_from_type(mesh)
!
      use m_geometry_data
      use t_mesh_data
!
      type(mesh_geometry),    intent(inout) :: mesh
!
!
      call copy_node_comm_tbl_from_type(mesh%nod_comm)
      call copy_node_geometry_from_type(mesh%node)
      call copy_element_connect_from_type(mesh%ele)
!
      call allocate_element_geometry
!
      end subroutine set_geometry_data_from_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_from_type(nod_comm)
!
      use m_nod_comm_table
      use copy_communication_table
      use t_comm_table
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      num_neib = nod_comm%num_neib
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_communication(num_neib, id_neib,                    &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    nod_comm%id_neib, nod_comm%istack_import,                     &
     &    nod_comm%istack_export)
      call copy_num_import_export(num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item                                      &
     &   (ntot_import, ntot_export, item_import, item_export,           &
     &    nod_comm%item_import, nod_comm%item_export)
!
      call deallocate_type_comm_tbl(nod_comm)
!
      end subroutine copy_node_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_geometry_from_type(node)
!
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
!
      type(node_data), intent(inout) :: node
!
      integer(kind = kint) :: inod
!
!
      numnod =        node%numnod
      internal_node = node%internal_node
!
      call allocate_node_geometry
!
!$omp parallel do
      do inod = 1, numnod
        globalnodid(inod) = node%inod_global(inod)
        xx(inod,1) = node%xx(inod,1)
        xx(inod,2) = node%xx(inod,2)
        xx(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_geometry_type(node)
!
      end subroutine copy_node_geometry_from_type
!
!------------------------------------------------------------------
!
      subroutine copy_element_connect_from_type(ele)
!
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
!
      type(element_data), intent(inout) :: ele
!
!
      integer(kind = kint) :: iele, k1
!
!
      numele =            ele%numele
      first_ele_type = ele%first_ele_type
!
      nnod_4_ele =  ele%nnod_4_ele
!
      call allocate_element_connection
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_ele
!$omp do private(iele)
        do iele = 1, numele
          ie(iele,k1) = ele%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, numele
        globalelmid(iele) = ele%iele_global(iele)
        elmtyp(iele) =      ele%elmtyp(iele)
        nodelm(iele) =      ele%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      call deallocate_ele_connect_type(ele)
!
      end subroutine copy_element_connect_from_type
!
!------------------------------------------------------------------
!
      end module copy_mesh_from_type
