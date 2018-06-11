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
      use mpi_load_mesh_data
!
      implicit none
!
      type(mesh_geometry), save, private :: mesh
      type(mesh_groups), save, private :: group
      type(element_geometry), save, private :: ele_mesh
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
      use nod_phys_send_recv
      use set_parallel_file_name
!
      use parallel_FEM_mesh_init
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
!      if(my_rank .eq. 0) iflag_debug = 1
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
      end subroutine initialize_sleeve_extend
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sleeve_extend
!
      use m_ctl_param_partitioner
!
      use const_mesh_information
      use const_element_comm_tables
      use set_table_4_RHS_assemble
      use extend_comm_table
      use extend_group_table
      use copy_mesh_structures
      use para_const_kemoview_mesh
!
      integer(kind = kint) :: num_extend = 3
      integer(kind = kint) :: ilevel
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(element_geometry), save :: new_ele_mesh
      type(next_nod_ele_table), save :: next_tbl
!
!
      do ilevel = 1, num_extend
        if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
        call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
        if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
        call const_element_comm_tbl_only(mesh, ele_mesh)
!
        if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
        call set_belonged_ele_and_next_nod                              &
     &     (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
!
        if (my_rank.eq.0) write(*,*) 'extend_node_comm_table', ilevel
        call extend_node_comm_table                                     &
     &     (mesh%nod_comm, mesh%node, next_tbl%neib_nod,                &
     &      newmesh%nod_comm, newmesh%node)
!
        call extend_ele_comm_table                                      &
     &     (mesh%nod_comm, ele_mesh%ele_comm, mesh%node, mesh%ele,      &
     &      next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,          &
     &      newmesh%ele)
        write(*,*) 'extend_ele_comm_table'
!
        write(*,*) 'alloc_sph_node_geometry'
        call alloc_sph_node_geometry(newmesh%node)
        write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
        write(*,*)' const_element_comm_tbl_only'
        call const_element_comm_tbl_only(newmesh, new_ele_mesh)
!
!
        call s_extend_group_table                                       &
     &     (nprocs, newmesh%nod_comm, new_ele_mesh%ele_comm,            &
     &      newmesh%node, newmesh%ele, group, newgroup)
!
        call dealloc_next_nod_ele_table(next_tbl)
        call dealloc_ele_comm_tbl_only(mesh, ele_mesh)
        call dealloc_mesh_infomations(mesh, group, ele_mesh)
!
        write(*,*) 'set_mesh_data_from_type in', allocated(mesh%node%rr)
        call set_mesh_data_from_type(newmesh, newgroup, mesh%nod_comm,  &
     &      mesh%node, mesh%ele, ele_mesh%surf, ele_mesh%edge,          &
     &      group%nod_grp, group%ele_grp, group%surf_grp)
        call deallocate_sph_node_geometry(newmesh%node)
        call dealloc_ele_comm_tbl_only(newmesh, new_ele_mesh)
        call deallocate_ele_geometry_type(newmesh%ele)
        call deallocate_ele_param_smp_type(newmesh%ele)
        call deallocate_node_param_smp_type(newmesh%node)
      end do
!
!
      call mpi_output_mesh(distribute_mesh_file, mesh, group)
      call dealloc_mesh_infos(mesh, group)
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
