!!analyzer_sleeve_extend.f90
!!
!!      module analyzer_sleeve_extend
!!
!!      modified by H. Matsui on Aug., 2006 
!!
!!      subroutine initialize_sleeve_extend
!!      subroutine analyze_sleeve_extend
!!
!!..................................................
!
      module analyzer_sleeve_extend
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_next_node_ele_4_node
!
      implicit none
!
      type(mesh_geometry), save, private :: mesh
      type(mesh_groups), save, private :: group
      type(element_geometry), save, private :: ele_mesh
      type(next_nod_ele_table), save, private :: next_tbl
!
      type(mesh_geometry), save, private :: newmesh
      type(mesh_groups), save, private :: newgroup
      type(element_geometry), save, private :: new_ele_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sleeve_extend
!
      use m_array_for_send_recv
      use m_default_file_prefix
      use m_control_data_4_part
      use m_ctl_param_partitioner
!
      use copy_mesh_structures
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use mesh_file_IO
      use nod_phys_send_recv
      use sum_normal_4_surf_group
      use set_parallel_file_name
      use extend_comm_table
      use extend_group_table
!
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use load_element_mesh_data
      use set_control_data_4_part
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
!
      type(FEM_file_IO_flags) :: FEM_mesh_flag_P
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Test mesh commnucations'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      call read_control_data_4_part
      call s_set_control_data_4_part
      if(my_rank .eq. 0) iflag_debug = 1
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(global_mesh_file, nprocs, mesh, group,        &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_init_with_IO'
      call FEM_mesh_init_with_IO(FEM_mesh_flag_P%iflag_output_SURF,     &
     &    global_mesh_file, mesh, group, ele_mesh)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
!
      call extend_node_comm_table                                       &
     &   (mesh%nod_comm, mesh%node, next_tbl%neib_nod,                  &
     &    newmesh%nod_comm, newmesh%node)
!
      call extend_ele_comm_table                                        &
     &   (mesh%nod_comm, ele_mesh%ele_comm, mesh%node, mesh%ele,        &
     &    next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,            &
     &    new_ele_mesh%ele_comm, newmesh%ele)
      call s_extend_group_table                                         &
     &   (newmesh%nod_comm, new_ele_mesh%ele_comm,                      &
     &    newmesh%node, newmesh%ele, group, newgroup)
!
      call mpi_output_mesh(distribute_mesh_file, newmesh, newgroup)
!
      end subroutine initialize_sleeve_extend
!
! ----------------------------------------------------------------------
!
        subroutine analyze_sleeve_extend
!
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
        end subroutine analyze_sleeve_extend
!
! ----------------------------------------------------------------------
!
      end module analyzer_sleeve_extend
