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
!
      implicit none
!
      type(mesh_geometry), save :: finermesh
!
      private :: input_interpolate_table_4_part
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
      use m_read_mesh_data
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use m_domain_group_4_partition
!
      use load_mesh_data
      use copy_domain_list_4_IO
!
!
!     read finer mesh
!
      iflag_mesh_file_fmt = iflag_para_mesh_file_fmt
      mesh_file_head = finer_mesh_file_head
      call input_mesh_geometry(izero, finermesh)
!
!     read interpolate table
!
      call input_interpolate_table_4_part
!
!     read interpolate table
!
      call read_group_4_partition
      call copy_finer_domain_list_from_IO(finermesh%node)
!
!     construct group table
!
      call interpolate_domain_group(finermesh%node, finermesh%ele)
!
!     deallocate arrays
!
      call deallocate_finer_domain_group
!
      call dealloc_itp_num_org(itp1_org)
      call dealloc_itp_table_org(itp1_org)
!
      call dealloc_itp_table_dest(itp1_dest)
      call dealloc_itp_num_dest(itp1_dest)
!
      call deallocate_ele_connect_type(finermesh%ele)
      call deallocate_node_geometry_type(finermesh%node)
      call deallocate_type_comm_tbl(finermesh%nod_comm)
!
      end subroutine s_set_partition_by_fine_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine input_interpolate_table_4_part
!
      use m_ctl_param_partitioner
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
      use itp_table_IO_select_4_zlib
      use copy_interpolate_type_IO
!
      integer(kind = kint), parameter :: my_rank = 0
      integer(kind = kint) :: ierr
!
!
      table_file_header = finer_inter_file_head
      write(*,*) 'sel_read_interpolate_table: ',                        &
     &           trim(table_file_header)
      call sel_read_interpolate_table(my_rank, ierr)
!
      call copy_itp_table_dest_from_IO(my_rank, itp1_dest)
      call copy_itp_table_org_from_IO(my_rank, itp1_org)
!
      call set_stack_tbl_wtype_org_smp(itp1_org)
!
      end subroutine input_interpolate_table_4_part
!
!  ---------------------------------------------------------------------
!
      subroutine interpolate_domain_group(new_node, new_ele)
!
      use t_geometry_data
!
      use m_machine_parameter
      use m_domain_group_4_partition
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use interpolate_imark_1pe
!
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
!
!      transfer interpolate table
!
      call s_interporate_imark_para(np_smp, new_node%numnod,            &
     &    new_ele%numele, new_ele%nnod_4_ele, new_ele%ie,               &
     &    IGROUP_FINER(1), itp1_org%istack_tbl_type_org_smp,            &
     &    itp1_org%ntot_table_org, itp1_org%iele_org_4_org,             &
     &    itp1_org%itype_inter_org, IGROUP_nod(1) )
!
      end subroutine interpolate_domain_group
!
!  ---------------------------------------------------------------------
!
      end module set_partition_by_fine_mesh
