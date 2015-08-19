!
      program patitioner
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_near_mesh_id_4_node
!
      use m_control_data_4_part
      use m_ctl_param_partitioner
      use m_read_mesh_data
      use m_geometry_data
      use m_group_data
      use m_element_group_connect
!
      use init_partitioner
      use grouping_for_partition
      use generate_local_mesh
      use set_control_data_4_part
!
      use load_mesh_data
!
      use const_surface_mesh
!
      implicit none
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
      call input_mesh(my_rank)
!
!  ========= Routines for partitioner ==============
!
      call initialize_partitioner(node1, ele1,                          &
     &    nod_grp1, ele_grp1, sf_grp1)
      call grouping_for_partitioner(node1, ele1, edge1,                 &
     &   nod_grp1, ele_grp1, ele_grp_data1)
!
!C===
!C-- create subdomain mesh
      call PROC_LOCAL_MESH(node1, ele1, edge1, ele_grp1,                &
     &    partitioned_fem, included_ele)
!
!  ========= Construct subdomain information for viewer ==============
!
      call choose_surface_mesh(local_file_header)
!
      stop ' * Partitioning finished'
!
      end program patitioner
