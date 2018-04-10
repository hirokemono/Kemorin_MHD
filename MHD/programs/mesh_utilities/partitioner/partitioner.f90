!
      program patitioner
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_comm_table
      use t_surface_data
      use t_edge_data
      use t_near_mesh_id_4_node
      use t_mesh_data_4_merge
!
      use m_control_data_4_part
      use m_ctl_param_partitioner
!
      use init_partitioner
      use grouping_for_partition
      use generate_local_mesh
      use set_control_data_4_part
!
      use load_mesh_data
      use const_mesh_information
!
      use single_const_surface_mesh
!
      implicit none
!
      type(mesh_geometry), save :: org_mesh
      type(mesh_groups), save :: org_group
      type(element_geometry), save :: org_ele_mesh
!
      type(near_mesh), save :: included_ele
!
      integer(kind = kint), parameter :: my_rank = izero
      integer(kind = kint) :: ierr
!
!  read control file
!
      call read_control_data_4_part
      call s_set_control_data_4_part
!
!  read global mesh
!
      call input_mesh(global_mesh_file, my_rank, org_mesh, org_group,   &
     &    org_ele_mesh%surf%nnod_4_surf, org_ele_mesh%edge%nnod_4_edge, &
     &    ierr)
      if(ierr .gt. 0) stop 'Global mesh is wrong!'
!
!      write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, org_mesh, org_group, org_ele_mesh)
!
!  ========= Routines for partitioner ==============
!
!      write(*,*) 'initialize_partitioner'
      call initialize_partitioner(org_mesh, org_group)
!      write(*,*) 'grouping_for_partitioner'
      call grouping_for_partitioner                                     &
     &   (org_mesh%node, org_mesh%ele, org_ele_mesh%edge,               &
     &    org_group%nod_grp, org_group%ele_grp, org_group%tbls_ele_grp)
!
!C===
!C-- create subdomain mesh
!      write(*,*) 'PROC_LOCAL_MESH'
      call PROC_LOCAL_MESH                                              &
     &   (org_mesh%node, org_mesh%ele, org_ele_mesh%edge, org_group,    &
     &    included_ele)
!C
!C-- Finalize
!      write(*,*) 'dealloc_nod_ele_infos'
      call dealloc_nod_ele_infos(org_mesh, org_group, org_ele_mesh)
!
!  ========= Construct subdomain information for viewer ==============
!
      write(*,*) 'choose_surface_mesh_sgl'
      call choose_surface_mesh_sgl(distribute_mesh_file)
!
      stop ' * Partitioning finished'
!
      end program patitioner
