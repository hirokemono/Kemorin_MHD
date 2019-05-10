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
      use t_control_data_4_part
      use t_partitioner_comm_table
      use t_ctl_param_partitioner
      use para_const_kemoview_mesh
!
      use mpi_load_mesh_data
!
      implicit none
!
      type(ctl_param_partitioner), save, private :: part_p1
      type(control_data_4_partitioner), save, private :: part_ctl1
      type(partitioner_comm_tables), save, private :: comm_part1
!
      type(mesh_data), save, private :: fem_EXT
      type(element_geometry), save, private :: e_mesh_EXT
      type(parallel_make_vierwer_mesh), save, private :: par_viexw_ex
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
      call read_control_data_4_part(part_ctl1)
      call set_control_4_extend_sleeve                                  &
     &   (my_rank, part_ctl1, comm_part1, part_p1)
      call dealloc_ctl_data_4_part(part_ctl1)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh                                               &
     &   (part_p1%global_mesh_file, nprocs, fem_EXT, e_mesh_EXT)
!
!  ------  Initialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver                                   &
     &   (n_sym_tensor, fem_EXT%mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(fem_EXT%mesh)
!
      end subroutine initialize_sleeve_extend
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sleeve_extend
!
      use parallel_sleeve_extension
!
      integer(kind = kint) :: ilevel
!
!
      do ilevel = 1, part_p1%n_overlap
        if(my_rank .eq. 0) write(*,*) 'para_sleeve_extension',          &
     &                               iflag_debug
        call para_sleeve_extension(fem_EXT%mesh, fem_EXT%group)
      end do
!
!
      call mpi_output_mesh                                              &
     &   (part_p1%distribute_mesh_file, fem_EXT%mesh, fem_EXT%group)
      call dealloc_mesh_infos(fem_EXT%mesh, fem_EXT%group)
!
      if (iflag_debug.gt.0) write(*,*) 'pickup_surface_mesh_para'
      call pickup_surface_mesh_para                                     &
     &   (part_p1%distribute_mesh_file, par_viexw_ex)
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_sleeve_extend
!
! ----------------------------------------------------------------------
!
      end module analyzer_sleeve_extend
