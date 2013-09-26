!
      program patitioner
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
      use generate_local_all_mesh
      use set_control_data_4_part
!
      use load_mesh_data
      use load_element_surface_edge
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
!   initialize global mesh
      call initialize_partitioner
!
      call grouping_for_partitioner
!
      write(mesh_ele_file_head,'(a,a)')                                 &
     &                   trim(global_mesh_head), '_ele'
      write(mesh_surf_file_head,'(a,a)')                                &
     &                   trim(global_mesh_head), '_surf'
      write(mesh_edge_file_head,'(a,a)')                                &
     &                   trim(global_mesh_head), '_edge'
      call output_ele_surf_edge_mesh(my_rank)
!
!C===
!C-- create subdomain mesh
      call const_communication_table
!
!  ========= Construct subdomain information for viewer ==============
!
      call choose_surface_mesh(local_file_header)
!
      stop ' * Partitioning finished'
!
      end program patitioner
