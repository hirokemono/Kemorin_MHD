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
      use t_ctl_param_sleeve_extend
      use t_mesh_SR
!
      use m_work_time
      use m_work_time_4_sleeve_extend
      use mpi_load_mesh_data
      use para_const_kemoview_mesh

!
      implicit none
!
      character (len = kchara), parameter, private                      &
     &         :: control_file_name = 'ctl_part'
!
      type(ctl_param_partitioner), save, private :: part_p1
      type(control_data_4_partitioner), save, private :: part_ctl1
      type(partitioner_comm_tables), save, private :: comm_part1
      type(sleeve_extension_param), save, private :: sleeve_exp_p1
!
      type(mesh_data), save, private :: fem_EXT
      type(mesh_SR), save :: m_SR_E
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
      use set_control_4_extend_sleeve
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_sleeve_ext
      call start_elapsed_time(ied_total_elapsed)
!
!     ----- read control data
!
      if(my_rank .eq. 0) call read_control_data_4_part                  &
     &                      (control_file_name, part_ctl1)
      call bcast_part_control_data(part_ctl1)
!
      if(part_ctl1%i_part_ctl .ne. 1) then
        call calypso_MPI_abort(part_ctl1%i_part_ctl,                    &
     &                         trim(control_file_name))
      end if
!
      call s_set_control_4_extend_sleeve                                &
     &   (my_rank, part_ctl1, comm_part1, part_p1, sleeve_exp_p1)
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
      call init_nod_send_recv(fem_EXT%mesh,                             &
     &    m_SR_E%SR_sig, m_SR_E%SR_r, m_SR_E%SR_i, m_SR_E%SR_il)
!
      end subroutine initialize_sleeve_extend
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sleeve_extend
!
      use sleeve_extend
      use nod_and_ele_derived_info
      use const_element_comm_tables
!
      type(communication_table), save :: ele_comm
      type(sleeve_extension_work), save :: sleeve_exp_WK1
!
!
      call set_nod_and_ele_infos(fem_EXT%mesh%node, fem_EXT%mesh%ele)
      call const_ele_comm_table(fem_EXT%mesh%node,                      &
     &    fem_EXT%mesh%nod_comm, fem_EXT%mesh%ele, ele_comm, m_SR_E)
!
      if(sleeve_exp_p1%iflag_expand_mode .eq. iflag_vector_trace) then
        sleeve_exp_p1%iflag_expand_mode = iflag_distance
      end if
!
      call sleeve_extension_current_mesh(sleeve_exp_p1, fem_EXT%mesh,   &
     &    fem_EXT%group, ele_comm, sleeve_exp_WK1, m_SR_E)
!
      call mpi_output_mesh                                              &
     &   (part_p1%distribute_mesh_file, fem_EXT%mesh, fem_EXT%group)
      call dealloc_mesh_data(fem_EXT%mesh, fem_EXT%group)
!
      if (iflag_debug.gt.0) write(*,*) 'pickup_surface_mesh'
      call pickup_surface_mesh                                          &
     &   (part_p1%distribute_mesh_file, par_viexw_ex)
!
      call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_sleeve_extend
!
! ----------------------------------------------------------------------
!
      end module analyzer_sleeve_extend
