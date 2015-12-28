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
      use calypso_mpi
!
      use m_FEM_utils
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
      use m_array_for_send_recv
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use input_control_udt_diff
      use const_mesh_information
      use nod_phys_send_recv
      use load_mesh_data
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
      call s_input_control_ave_udt(ucd_FUTIL)
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh                                                   &
     &   (my_rank, nod_comm, node1, ele1, nod_grp1, ele_grp1, sf_grp1,  &
     &    surf1%nnod_4_surf, edge1%nnod_4_edge)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, node1%numnod)
!
      call init_send_recv(nod_comm)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos(node1, ele1)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_nod_field_data'
      call initialize_nod_field_data
!
      end subroutine initialize_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ave_udt
!
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_t_step_parameter
      use m_ctl_params_4_diff_udt
      use m_control_params_2nd_files
      use ucd_IO_select
      use set_ucd_data
      use set_ucd_data_to_type
      use divide_phys_by_delta_t
      use nod_phys_send_recv
      use output_parallel_ucd_file
!
      integer(kind = kint) :: istep, istep_ucd, icou
!
!
      call link_num_field_2_ucd(nod_fld1, ucd_FUTIL)
!
      istep_ucd = i_step_init / i_step_output_ucd
      call set_data_by_read_ucd_once(my_rank, istep_ucd,                &
     &    ifmt_org_ucd, org_ucd_header, nod_fld1)
!
      icou = 1
      do istep = i_step_init+1, i_step_number
        if ( mod(istep,i_step_output_ucd) .eq. izero) then
!
          istep_ucd = istep / i_step_output_ucd
          icou = icou + 1
!
          call add_ucd_to_data(my_rank, istep_ucd,                      &
     &        ifmt_org_ucd, org_ucd_header, nod_fld1)
        end if
      end do
!
      call s_divide_phys_by_num_udt(icou, nod_fld1)
      call nod_fields_send_recv(node1, nod_comm, nod_fld1)
!
!    output udt data
!
      call set_ucd_file_prefix(ave_udt_file_head, ucd_FUTIL)
      call output_udt_one_snapshot                                      &
     &   (i_step_number, node1, ele1, nod_comm, nod_fld1,               &
     &    ucd_FUTIL, m_ucd_FUTIL)
!
      end subroutine analyze_ave_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_ave_udt

