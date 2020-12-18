!set_partition_by_fine_mesh.f90
!      module set_partition_by_fine_mesh
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine s_set_partition_by_fine_mesh(part_p, domain_grp)
!!        type(ctl_param_partitioner), intent(in) :: part_p
!!        type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
      module set_partition_by_fine_mesh
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
      subroutine s_set_partition_by_fine_mesh(part_p, domain_grp)
!
      use t_domain_group_4_partition
      use t_subdomain_table_IO
      use t_ctl_param_partitioner
      use m_interpolate_table_IO
!
      use load_mesh_data
      use itp_table_IO_select_4_zlib
!
      type(ctl_param_partitioner), intent(inout) :: part_p
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
      type(interpolate_table)  :: itp_table
      integer(kind = kint) :: ierr
!
!     read finer mesh
!
      call input_mesh_geometry                                          &
     &   (part_p%finer_mesh_file, 0, finermesh, ierr)
      if(ierr .gt. 0) stop 'finer mesh is wrong!!'
!
!     read interpolate table
!
      call load_interpolate_table(0, part_p%itp_file_IO, itp_table)
!
!     read interpolate table
!
      call finer_domain_list_from_file(part_p%fname_subdomain,          &
     &    finermesh%node, domain_grp%nod_f_grp, part_p%num_domain)
!
!     construct group table
!
      call interpolate_domain_group                                     &
     &   (finermesh%node, finermesh%ele, itp_table%tbl_org,             &
     &    domain_grp%nod_f_grp, domain_grp%nod_d_grp)
!
!     deallocate arrays
!
      call dealloc_finer_domain_group(domain_grp%nod_f_grp)
!
      call dealloc_itp_num_org(itp_table%tbl_org)
      call dealloc_itp_table_org(itp_table%tbl_org)
!
      call dealloc_itp_table_dest(itp_table%tbl_dest)
      call dealloc_itp_num_dest(itp_table%tbl_dest)
!
      call dealloc_ele_connect(finermesh%ele)
      call dealloc_node_geometry_w_sph(finermesh%node)
      call dealloc_comm_table(finermesh%nod_comm)
!
      end subroutine s_set_partition_by_fine_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine interpolate_domain_group(new_node, new_ele, itp_org,   &
     &          nod_f_grp, nod_d_grp)
!
      use t_geometry_data
!
      use t_domain_group_4_partition
      use interpolate_imark_1pe
!
      type(node_data), intent(in) :: new_node
      type(element_data), intent(in) :: new_ele
      type(interpolate_table_org), intent(in) :: itp_org
!
      type(finer_domain_group), intent(in) :: nod_f_grp
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!      transfer interpolate table
!
      call s_interporate_imark_para(np_smp, new_node%numnod,            &
     &    new_ele%numele, new_ele%nnod_4_ele, new_ele%ie,               &
     &    nod_f_grp%IGROUP_FINER(1), itp_org%istack_tbl_type_org_smp,   &
     &    itp_org%ntot_table_org, itp_org%iele_org_4_org,               &
     &    itp_org%itype_inter_org, nod_d_grp%IGROUP(1))
!
      end subroutine interpolate_domain_group
!
!  ---------------------------------------------------------------------
!
      end module set_partition_by_fine_mesh
