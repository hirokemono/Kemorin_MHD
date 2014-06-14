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
!
      implicit none
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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      use input_control_interpolate
      use const_mesh_info
      use set_smp_size_4_2nd
      use nodal_vector_send_recv
      use set_field_to_restart
      use field_IO_select
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
      call s_input_control_interpolate(ierr)
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
        call s_count_smp_size_4_2nd
        if (i_debug.eq.iflag_full_msg) call check_smp_size_2nd(my_rank)
      end if
!
!     --------------------- 
!
      istep_rst_start = int(i_step_init /   i_step_output_rst)
      iflag_field_data_fmt = ifmt_org_rst_file
      phys_file_head =       org_rst_file_head
      call sel_read_alloc_step_FEM_file(izero, istep_rst_start)
      if (iflag_debug.eq.1) write(*,*) 'init_field_name_by_restart'
      call init_field_name_by_restart
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
      if (iflag_debug.eq.1) write(*,*) 'link_nodal_field_names'
      call link_nodal_field_names
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_phys_data_type'
      call alloc_phys_data_type(nnod_2nd, phys_2nd)
!
!     --------------------- 
!
      call init_send_recv
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
      use m_field_data_IO
      use m_geometry_parameter
      use field_IO_select
      use set_parallel_file_name
      use copy_time_steps_4_restart
      use interpolate_nodal_field
      use nod_phys_send_recv
      use set_field_to_restart
      use set_2nd_field_to_restart
!
      integer(kind = kint) :: i_step
!
!
      istep_rst_start = int(i_step_init /   i_step_output_rst)
      istep_rst_end =   int(i_step_number / i_step_output_rst)
      do i_step = istep_rst_start, istep_rst_end
!
        if (my_rank .lt. ndomain_org) then
          numgrid_phys_IO = numnod
          call allocate_phys_data_IO
!
          iflag_field_data_fmt = ifmt_org_rst_file
          phys_file_head =       org_rst_file_head
          call sel_read_step_FEM_field_file(my_rank, i_step)
!
          call copy_field_data_from_restart
          call deallocate_phys_data_IO
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
        call interpolate_nodal_data
!
        if (my_rank .lt. ndomain_dest) then
          numgrid_phys_IO = nnod_2nd
          call allocate_phys_data_IO
          call copy_2nd_field_data_to_rst
          call copy_time_steps_to_restart
!
          iflag_field_data_fmt = ifmt_itp_rst_file
          phys_file_head =       itp_rst_file_head
          call sel_write_step_FEM_field_file(my_rank, i_step)
          call deallocate_phys_data_IO
        end if
      end do
!
      call deallocate_phys_data_name_IO
!
      end subroutine analyze_itp_rst
!
! ----------------------------------------------------------------------
!
      end module analyzer_interpolate_rst
