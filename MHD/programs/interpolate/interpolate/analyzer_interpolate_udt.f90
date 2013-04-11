!analyzer_interpolate_udt.f90
!      module analyzer_interpolate_udt
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_itp_udt
!      subroutine analyze_itp_udt
!
      module analyzer_interpolate_udt
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_itp_udt
!
      use m_ctl_params_4_gen_table
      use m_geometry_parameter
      use m_node_phys_address
      use m_2nd_geometry_param
      use m_2nd_phys_data
!
      use input_control_interpolate
      use const_mesh_info
      use set_smp_size_4_2nd
      use link_data_to_1st_mesh
      use nodal_vector_send_recv
!
!
      if (my_rank.eq.0)  write(*,*) 'Interpolate data to new mesh'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_interpolate'
      call s_input_control_interpolate
      call set_ctl_interpolate_udt
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
      if (iflag_debug.eq.1) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
      if (iflag_debug.eq.1) write(*,*) 'link_nodal_field_names'
      call link_nodal_field_names
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_2nd_data_arrays'
      call allocate_2nd_data_arrays
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_iccgN_matrix'
      call allocate_iccgN_matrix(isix, numnod)
!
      call init_send_recv
!
      end subroutine initialize_itp_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_itp_udt
!
      use m_t_step_parameter
      use m_ucd_data
      use m_ctl_params_4_gen_table
      use set_ucd_data
      use set_udt_to_2nd_data
      use ucd_IO_select
      use interpolate_nodal_data
      use nod_phys_send_recv
!
!
      do ucd_step = i_step_init, i_step_number, i_step_output_ucd
        if (my_rank .lt. ndomain_org) then
          call link_num_field_2_output
!
          itype_ucd_data_file = itype_org_udt_file
          ucd_header_name =     org_udt_file_head
          call sel_read_udt_param(my_rank, ucd_step)
          call sel_read_udt_file(my_rank, ucd_step)
          call set_ucd_data_from_IO
!
          call deallocate_ucd_data
!
          call phys_send_recv_all
        end if
!
        call time_prog_barrier
!
!    interpolation
!
        if (iflag_debug.gt.0) write(*,*) 's_interpolate_nodal_data'
        call s_interpolate_nodal_data
!
!    output udt data
!
        if (my_rank .lt. ndomain_dest) then
!
          call link_2nd_node_data_2_output
          call link_2nd_field_data_2_output
!
          itype_ucd_data_file = itype_itp_udt_file
          ucd_header_name =     itp_udt_file_head
          call sel_write_udt_file(my_rank, ucd_step)
          call disconnect_ucd_data
          call disconnect_ucd_node
        end if
      end do
!
      end subroutine analyze_itp_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_interpolate_udt
