!!analyzer_sleeve_extend2.f90
!!
!!      module analyzer_sleeve_extend2
!!
!!      modified by H. Matsui on Aug., 2006 
!!
!!      subroutine initialize_sleeve_extend2
!!      subroutine analyze_sleeve_extend2
!!
!!..................................................
!
      module analyzer_sleeve_extend2
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
      use parallel_sleeve_extension
!
      use mpi_load_mesh_data
      use m_work_time

!
      implicit none
!
      type(ctl_param_partitioner), save, private :: part_p1
      type(control_data_4_partitioner), save, private :: part_ctl1
      type(partitioner_comm_tables), save, private :: comm_part1
!
      type(mesh_data), save, private :: fem_EXT
      type(parallel_make_vierwer_mesh), save, private :: par_viexw_ex
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sleeve_extend2
!
      use m_phys_constants
      use m_default_file_prefix
!
      use nod_phys_send_recv
      use set_parallel_file_name
!
      use parallel_FEM_mesh_init
      use set_control_data_4_part
      use bcast_control_data_4_part
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_sleeve_ext
      call start_elapsed_time(ied_total_elapsed)
!
!     ----- read control data
!
      if(my_rank .eq. 0) call read_control_data_4_part(part_ctl1)
      call bcast_part_control_data(part_ctl1)
!
      call set_control_4_extend_sleeve                                  &
     &   (my_rank, part_ctl1, comm_part1, part_p1)
      call dealloc_ctl_data_4_part(part_ctl1)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(part_p1%global_mesh_file, nprocs, fem_EXT)
!
!  ------  Initialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'init_nod_send_recv'
      call init_nod_send_recv(fem_EXT%mesh)
!
      end subroutine initialize_sleeve_extend2
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sleeve_extend2
!
      integer(kind = kint) :: ilevel
!
!
      call sleeve_extension_loop2(part_p1%n_overlap,                    &
     &                           fem_EXT%mesh, fem_EXT%group)
!
!      call mpi_output_mesh                                             &
!     &   (part_p1%distribute_mesh_file, fem_EXT%mesh, fem_EXT%group)
!      call dealloc_mesh_data(fem_EXT%mesh, fem_EXT%group)
!
!      if (iflag_debug.gt.0) write(*,*) 'pickup_surface_mesh'
!      call pickup_surface_mesh                                         &
!     &   (part_p1%distribute_mesh_file, par_viexw_ex)
!
      call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_sleeve_extend2
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sleeve_extension_loop2(num_level, mesh, group)
!
      use nod_and_ele_derived_info
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_level
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(communication_table) :: ele_comm
!
!
      if(num_level .le. 1) return
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
      call set_nod_and_ele_infos(mesh%node, mesh%ele)
      call const_ele_comm_table(mesh%node, mesh%nod_comm,               &
     &                          ele_comm, mesh%ele)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
      if(my_rank .eq. 0) write(*,*) 'extend sleeve:'
      call para_sleeve_extension2(mesh, group, ele_comm)
!
      call dealloc_comm_table(ele_comm)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_nod_and_ele_infos(mesh)
!
      end subroutine sleeve_extension_loop2
!
! ----------------------------------------------------------------------
!
      subroutine para_sleeve_extension2(mesh, group, ele_comm)
!
      use t_para_double_numbering
      use t_next_node_ele_4_node
!
      use nod_and_ele_derived_info
      use const_element_comm_tables
      use set_table_4_RHS_assemble
      use extend_comm_table
      use extend_element_connect
      use extend_group_table
      use copy_mesh_structures
      use set_nnod_4_ele_by_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(communication_table), intent(inout) :: ele_comm
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(next_nod_ele_table), save :: next_tbl
      type(node_ele_double_number), save :: dbl_id1
!
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
!
      call alloc_double_numbering(mesh%node%numnod, dbl_id1)
      if (iflag_debug.gt.0) write(*,*) 'set_node_double_numbering'
      call set_node_double_numbering                                    &
     &   (mesh%node, mesh%nod_comm, dbl_id1)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+2)
      if (iflag_debug.gt.0) write(*,*) 'extend_node_comm_table2'
      call extend_node_comm_table                                      &
     &   (mesh%nod_comm, mesh%node, dbl_id1, next_tbl%neib_nod,         &
     &    newmesh%nod_comm, newmesh%node)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+2)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+3)
!      if (iflag_debug.gt.0) write(*,*) 'extend_ele_connectivity'
!      call extend_ele_connectivity                                     &
!     &   (mesh%nod_comm, ele_comm, mesh%node, mesh%ele,                &
!     &    dbl_id1, next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,  &
!     &    newmesh%ele, iflag_SLEX_time, ist_elapsed_SLEX)
!      newmesh%ele%first_ele_type                                       &
!     &   = set_cube_eletype_from_num(newmesh%ele%nnod_4_ele)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+3)
!
      call dealloc_next_nod_ele_table(next_tbl)
      call dealloc_double_numbering(dbl_id1)
!      call dealloc_comm_table(ele_comm)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_nod_and_ele_infos(mesh)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
!      call alloc_sph_node_geometry(newmesh%node)
!      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
!      call const_ele_comm_table(newmesh%node, newmesh%nod_comm,        &
!     &                          ele_comm, newmesh%ele)
!
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+4)
!      if (iflag_debug.gt.0) write(*,*) 's_extend_group_table'
!      call s_extend_group_table(nprocs, newmesh%nod_comm, ele_comm,    &
!     &    newmesh%node, newmesh%ele, group, newgroup)
!      call dealloc_mesh_data(mesh, group)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+4)
!
!      if (iflag_debug.gt.0) write(*,*) 'copy_mesh_and_group'
!      call copy_mesh_and_group(newmesh, newgroup, mesh, group)
!      call dup_nod_and_ele_infos(newmesh, mesh)
!
!      call dealloc_numele_stack(newmesh%ele)
!      call dealloc_nod_and_ele_infos(newmesh)
!      call dealloc_mesh_data(newmesh, newgroup)
!
      end subroutine para_sleeve_extension2
!
! ----------------------------------------------------------------------
!
      end module analyzer_sleeve_extend2
