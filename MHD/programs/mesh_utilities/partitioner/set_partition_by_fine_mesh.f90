!set_partition_by_fine_mesh.f90
!      module set_partition_by_fine_mesh
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_set_partition_by_fine_mesh(new_node, new_ele)
!
      module set_partition_by_fine_mesh
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      implicit none
!
      type(mesh_geometry), save :: finermesh
!
      private :: interpolate_domain_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_partition_by_fine_mesh
!
      use m_ctl_param_partitioner
      use m_subdomain_table_IO
      use m_domain_group_4_partition
      use m_ctl_param_partitioner
      use m_interpolate_table_IO
!
      use load_mesh_data
      use copy_domain_list_4_IO
      use itp_table_IO_select_4_zlib
!
      type(interpolate_table)  :: itp_table
      integer(kind = kint) :: ierr
!
!     read finer mesh
!
      call input_mesh_geometry(finer_mesh_file, izero, finermesh, ierr)
      if(ierr .gt. 0) stop 'finer mesh is wrong!!'
!
!     read interpolate table
!
      table_file_header = finer_inter_file_head
      call load_interpolate_table(izero, itp_table)
!
!     read interpolate table
!
      call read_group_4_partition
      call copy_finer_domain_list_from_IO(finermesh%node)
!
!     construct group table
!
      call interpolate_domain_group                                     &
     &   (finermesh%node, finermesh%ele, itp_table%tbl_org)
!
!     deallocate arrays
!
      call deallocate_finer_domain_group
!
      call dealloc_itp_num_org(itp_table%tbl_org)
      call dealloc_itp_table_org(itp_table%tbl_org)
!
      call dealloc_itp_table_dest(itp_table%tbl_dest)
      call dealloc_itp_num_dest(itp_table%tbl_dest)
!
      call deallocate_ele_connect_type(finermesh%ele)
      call dealloc_node_geometry_w_sph(finermesh%node)
      call dealloc_comm_table(finermesh%nod_comm)
!
      end subroutine s_set_partition_by_fine_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine interpolate_domain_group(new_node, new_ele, itp_org)
!
      use t_geometry_data
!
      use m_machine_parameter
      use m_domain_group_4_partition
      use interpolate_imark_1pe
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
      type(interpolate_table_org), intent(in) :: itp_org
!
!      transfer interpolate table
!
      call s_interporate_imark_para(np_smp, new_node%numnod,            &
     &    new_ele%numele, new_ele%nnod_4_ele, new_ele%ie,               &
     &    IGROUP_FINER(1), itp_org%istack_tbl_type_org_smp,             &
     &    itp_org%ntot_table_org, itp_org%iele_org_4_org,               &
     &    itp_org%itype_inter_org, IGROUP_nod(1) )
!
      end subroutine interpolate_domain_group
!
!  ---------------------------------------------------------------------
!
      end module set_partition_by_fine_mesh
