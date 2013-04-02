!
      program patitioner_node
!
      use m_precision
      use m_constants
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
!
      use const_surface_mesh
!
      implicit none
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
      call initialize_nod_partitioner
      call grouping_for_node_partition
!
!C===
!C-- create subdomain mesh
      call PROC_LOCAL_MESH
!
!  ========= Construct subdomain information for viewer ==============
!
      call choose_surface_mesh(local_file_header)
!
      stop ' * Partitioning finished'
!
      end program patitioner_node
