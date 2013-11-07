!set_partition_by_fine_mesh.f90
!      module set_partition_by_fine_mesh
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_set_partition_by_fine_mesh
!
      module set_partition_by_fine_mesh
!
      use m_precision
      use m_constants
!
      implicit none
!
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
      use m_2nd_geometry_data
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use m_domain_group_4_partition
!
      use load_2nd_mesh_data
      use copy_domain_list_4_IO
!
!     read finer mesh
!
      iflag_mesh_file_fmt = iflag_para_mesh_file_fmt
      mesh_file_head = finer_mesh_file_head
      call input_2nd_mesh_geometry(izero)
!
!     read interpolate table
!
      call input_interpolate_table_4_part
!
!     read interpolate table
!
      call read_group_4_partition
      call copy_finer_domain_list_from_IO
!
!     construct group table
!
      call interpolate_domain_group
!
!     deallocate arrays
!
      call deallocate_finer_domain_group
!
      call deallocate_itp_num_org
      call deallocate_itp_table_org
!
      call deallocate_itp_table_dest
      call deallocate_itp_num_dest
!
      call deallocate_2nd_element_connect
      call deallocate_2nd_node_position
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
      use itp_table_IO_select_4_zlib
      use copy_interpolate_dest_IO
      use copy_interpolate_org_IO
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
      call copy_itp_table_dest_from_IO(my_rank)
      call copy_itp_table_org_from_IO(my_rank)
!
      call set_stack_tbl_wtype_org_smp
!
      end subroutine input_interpolate_table_4_part
!
!  ---------------------------------------------------------------------
!
      subroutine interpolate_domain_group
!
      use m_machine_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_domain_group_4_partition
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use interpolate_imark_1pe
!
!      transfer interpolate table
!
      call s_interporate_imark_para(np_smp, nnod_2nd, nele_2nd,         &
     &    nnod_4_ele_2nd, ie_2nd, IGROUP_FINER(1),                      &
     &    istack_tbl_type_org_smp, ntot_table_org, iele_org_4_org,      &
     &    itype_inter_org, IGROUP_nod(1) )
!
      end subroutine interpolate_domain_group
!
!  ---------------------------------------------------------------------
!
      end module set_partition_by_fine_mesh
