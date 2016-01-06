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
!
      use m_control_data_4_part
      use m_ctl_param_partitioner
      use m_read_mesh_data
!
      use init_partitioner
      use grouping_for_partition
      use generate_local_mesh
      use set_control_data_4_part
!
      use load_mesh_data
      use const_mesh_information
!
      use const_surface_mesh
!
      implicit none
!
      type(mesh_geometry), save :: org_mesh
      type(mesh_groups), save :: org_group
      type(surface_data), save :: org_surf
      type(edge_data), save :: org_edge
!
      type(mesh_data), save :: partitioned_fem
      type(near_mesh), save :: included_ele
!
      integer(kind = kint), parameter :: my_rank = izero
!
!  read control file
!
      call read_control_data_4_part
      call s_set_control_data_4_part
!
!  read global mesh
!
      iflag_mesh_file_fmt = ifmt_single_mesh_fmt
      mesh_file_head = global_mesh_head
      call input_mesh                                                   &
     &   (my_rank, org_mesh%nod_comm, org_mesh%node, org_mesh%ele,      &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp,     &
     &    org_surf%nnod_4_surf, org_edge%nnod_4_edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank,                                    &
     &    org_mesh%node, org_mesh%ele, org_surf, org_edge,              &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp,     &
     &    org_group%tbls_ele_grp, org_group%tbls_surf_grp,              &
     &    org_group%surf_nod_grp)
!
!  ========= Routines for partitioner ==============
!
      call initialize_partitioner                                       &
     &   (org_mesh%nod_comm, org_mesh%node, org_mesh%ele,               &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp)
      call grouping_for_partitioner                                     &
     &   (org_mesh%node, org_mesh%ele, org_edge,                        &
     &    org_group%nod_grp, org_group%ele_grp, org_group%tbls_ele_grp)
!
!C===
!C-- create subdomain mesh
      call PROC_LOCAL_MESH                                              &
     &   (org_mesh%node, org_mesh%ele, org_edge, org_group,             &
     &    partitioned_fem, included_ele)
!C
!C-- Finalize
      write(*,*) 'dealloc_nod_ele_infos'
      call dealloc_nod_ele_infos(org_mesh%nod_comm,                     &
     &    org_mesh%node, org_mesh%ele, org_surf, org_edge,              &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp)
!
!  ========= Construct subdomain information for viewer ==============
!
      write(*,*) 'choose_surface_mesh'
      call choose_surface_mesh(local_file_header,                       &
     &    org_mesh%ele, org_surf, org_edge)
!
      stop ' * Partitioning finished'
!
      end program patitioner
