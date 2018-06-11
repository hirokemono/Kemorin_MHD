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
      integer(kind = kint), parameter, private :: iflag_output_SURF = 0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sleeve_extend
!
      use m_phys_constants
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
      use const_mesh_information
      use set_table_4_RHS_assemble
      use extend_comm_table
      use extend_group_table
      use const_element_comm_tables
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
!  ------  Initialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh)
!
!  -------------------------------
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbl_only(mesh, ele_mesh)
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
     &   (nprocs, newmesh%nod_comm, new_ele_mesh%ele_comm,              &
     &    newmesh%node, newmesh%ele, group, newgroup)
!
      call dealloc_next_nod_ele_table(next_tbl)
      call dealloc_ele_comm_tbl_only(ele_mesh)
      call dealloc_mesh_infomations(mesh, group, ele_mesh)
!
      call mpi_output_mesh(distribute_mesh_file, newmesh, newgroup)
      call dealloc_mesh_infos(newmesh, newgroup)
!
      end subroutine initialize_sleeve_extend
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sleeve_extend
!
      use m_ctl_param_partitioner
      use para_const_kemoview_mesh
!
!
      if (iflag_debug.gt.0) write(*,*) 'pickup_surface_mesh_para'
      call pickup_surface_mesh_para(distribute_mesh_file)

      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_sleeve_extend
!
! ----------------------------------------------------------------------
!
      end module analyzer_sleeve_extend
