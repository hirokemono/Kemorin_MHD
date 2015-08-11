!analyzer_interpolate_rst.f90
!      module analyzer_interpolate_rst
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_itp_rst
!      subroutine analyze_itp_rst
!
      module analyzer_interpolate_rst
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_t_step_parameter
      use t_field_data_IO
!
      use t_mesh_data
      use t_phys_data
!
      implicit none
!
      type(mesh_data), save :: new_femmesh
      type(surface_geometry), save :: new_surf_mesh
      type(edge_geometry), save ::  new_edge_mesh
!
      type(phys_data), save :: new_phys
!
      type(field_IO), save :: itp_fld_IO
!
      private :: new_femmesh, new_surf_mesh, new_edge_mesh
      private :: new_phys, itp_fld_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_itp_rst
!
      use m_ctl_params_4_gen_table
      use m_geometry_parameter
      use m_node_phys_address
!
      use input_control_interpolate
      use const_mesh_info
      use set_size_4_smp_types
      use nodal_vector_send_recv
      use set_field_to_restart
      use field_IO_select
      use link_data_type_to_1st_mesh
!
      use m_node_phys_data
!
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0)  write(*,*) 'Interpolate data to new mesh'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_interpolate'
      call s_input_control_interpolate(new_femmesh,                     &
     &    new_surf_mesh, new_edge_mesh, ierr)
!
!     --------------------- 
!
      call init_send_recv
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_org) then
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos
      end if
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_dest) then
        call count_size_4_smp_mesh_type                                 &
     &     (new_femmesh%mesh%node, new_femmesh%mesh%ele)
        if (i_debug.eq.iflag_full_msg) then
          call check_smp_size_type(my_rank, new_femmesh%mesh)
        end if
      end if
!
!     --------------------- 
!
      istep_rst_start = int(i_step_init /   i_step_output_rst)
      call set_field_file_fmt_prefix                                    &
     &   (ifmt_org_rst_file, org_rst_file_head, itp_fld_IO)
      call sel_read_alloc_step_FEM_file                                 &
     &   (ndomain_org, izero, istep_rst_start, itp_fld_IO)
      if (iflag_debug.eq.1) write(*,*) 'init_field_name_by_restart'
      call init_field_name_by_restart(itp_fld_IO)
      call dealloc_phys_data_IO(itp_fld_IO)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
      if (iflag_debug.eq.1) write(*,*) 'link_nodal_fld_type_names'
      call link_nodal_fld_type_names(new_phys)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_phys_data_type'
      call alloc_phys_data_type(new_femmesh%mesh%node%numnod, new_phys)
!
      end subroutine initialize_itp_rst
!
! ----------------------------------------------------------------------
!
      subroutine analyze_itp_rst
!
      use calypso_mpi
      use m_node_phys_data
      use m_ctl_params_4_gen_table
      use m_time_data_IO
      use m_geometry_data
      use field_IO_select
      use set_parallel_file_name
      use copy_time_steps_4_restart
      use nod_phys_send_recv
      use set_field_to_restart
      use set_field_type_to_restart
      use interpolate_nod_field_2_type
      use const_global_element_ids
!
      integer(kind = kint) :: i_step
!
!
      istep_rst_start = int(i_step_init /   i_step_output_rst)
      istep_rst_end =   int(i_step_number / i_step_output_rst)
      do i_step = istep_rst_start, istep_rst_end
!
        if (my_rank .lt. ndomain_org) then
          itp_fld_IO%nnod_IO = node1%numnod
          call alloc_phys_data_IO(itp_fld_IO)
!
          call set_field_file_fmt_prefix                                &
     &       (ifmt_org_rst_file, org_rst_file_head, itp_fld_IO)
          call sel_read_step_FEM_field_file                             &
     &       (nprocs, my_rank, i_step, itp_fld_IO)
!
          call copy_field_data_from_restart(itp_fld_IO)
          call dealloc_phys_data_IO(itp_fld_IO)
          time =       time_IO
          i_step_MHD = i_time_step_IO
!
          call phys_send_recv_all
        end if
!
        call MPI_Bcast(time, ione, CALYPSO_REAL, izero,                 &
     &      CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(i_step_MHD, ione, CALYPSO_INTEGER, izero,        &
     &      CALYPSO_COMM, ierr_MPI)
!
        if (iflag_debug.gt.0)  write(*,*) 's_interpolate_nodal_data'
        call interpolate_nodal_data(new_femmesh%mesh%nod_comm,          &
     &      new_femmesh%mesh%node, new_phys)
!
        if (my_rank .lt. ndomain_dest) then
          call copy_time_steps_to_restart
!
          itp_fld_IO%nnod_IO = new_femmesh%mesh%node%numnod
          call alloc_phys_data_IO(itp_fld_IO)
          call copy_field_type_to_rst                                   &
     &       (new_femmesh%mesh%node, new_phys, itp_fld_IO)
!
          call alloc_merged_field_stack(nprocs, itp_fld_IO)
          call count_number_of_node_stack                               &
     &       (itp_fld_IO%nnod_IO, itp_fld_IO%istack_numnod_IO)
!
          call set_field_file_fmt_prefix                                &
     &       (ifmt_itp_rst_file, itp_rst_file_head, itp_fld_IO)
          call sel_write_step_FEM_field_file                            &
     &       (nprocs, my_rank, i_step, itp_fld_IO)
!
          call dealloc_phys_data_IO(itp_fld_IO)
          call dealloc_merged_field_stack(itp_fld_IO)
        end if
      end do
!
      call dealloc_phys_name_IO(itp_fld_IO)
!
      end subroutine analyze_itp_rst
!
! ----------------------------------------------------------------------
!
      end module analyzer_interpolate_rst
