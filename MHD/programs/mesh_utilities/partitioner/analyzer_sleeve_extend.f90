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
!     ----- read control data
!
      call read_control_data_4_part
      call s_set_control_data_4_part
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
      use parallel_sleeve_extension
      use para_const_kemoview_mesh
!
      integer(kind = kint) :: num_extend = 3
      integer(kind = kint) :: ilevel
!
!
      do ilevel = 1, num_extend
        if(my_rank .eq. 0) write(*,*) 'para_sleeve_extension', ilevel
        call para_sleeve_extension(mesh, group, ele_mesh)
      end do
!
!
      call mpi_output_mesh(distribute_mesh_file, mesh, group)
      call dealloc_mesh_infos(mesh, group)
!
      if (iflag_debug.gt.0) write(*,*) 'pickup_surface_mesh_para'
      call pickup_surface_mesh_para(distribute_mesh_file)
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_sleeve_extend
!
! ----------------------------------------------------------------------
!
      end module analyzer_sleeve_extend
