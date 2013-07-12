!analyzer_ave_udt.f90
!
!      module analyzer_ave_udt
!
!      modified by H. Matsui on Nov., 2007
!
!      subroutine initialize_ave_udt
!      subroutine analyze_ave_udt
!
!..................................................
!
      module analyzer_ave_udt
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_ave_udt
!
      use m_parallel_var_dof
      use m_geometry_parameter
      use m_node_phys_address
      use input_control_udt_diff
      use const_mesh_info
      use nodal_vector_send_recv
!
!
      if (my_rank.eq.0) then
        write(*,*) 'averaging udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_ave_udt'
      call s_input_control_ave_udt
      if (iflag_debug.eq.1) write(*,*) 's_input_mesh_udt_diff'
      call s_input_mesh_udt_diff
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_iccgN_matrix'
      call allocate_iccgN_matrix(isix, numnod)
!
      call init_send_recv
!
      end subroutine initialize_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ave_udt
!
      use m_t_step_parameter
      use m_ctl_params_4_diff_udt
      use m_ucd_data
      use ucd_IO_select
      use set_ucd_data
      use divide_phys_by_delta_t
      use nod_phys_send_recv
      use output_parallel_ucd_file
!
      integer(kind = kint) :: i_step, icou
!
!
      call link_num_field_2_output
!
      ucd_step = i_step_init / i_step_output_ucd
      fem_ucd%file_prefix = org_ucd_header
      call sel_read_udt_param(my_rank, ucd_step, fem_ucd)
!
      call sel_read_udt_file(my_rank, ucd_step, fem_ucd)
      call set_ucd_data_from_IO
!
      icou = 1
      do i_step = i_step_init+1, i_step_number
        if ( mod(i_step,i_step_output_ucd) .eq. 0) then
!
          ucd_step = i_step / i_step_output_ucd
          icou = icou + 1
!
          call sel_read_udt_file(my_rank, ucd_step, fem_ucd)
          call add_by_ucd_data
!
        end if
      end do
!
      call s_divide_phys_by_num_udt(icou)
      call phys_send_recv_all
!
!    output udt data
!
      fem_ucd%file_prefix = ave_udt_file_head
      ucd_step = i_step_number
!
      call output_udt_one_snapshot(ucd_step)
!
      end subroutine analyze_ave_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_ave_udt

