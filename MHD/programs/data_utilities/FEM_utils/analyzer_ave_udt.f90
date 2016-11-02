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
      use input_control_udt_diff
      use t_FEM_phys_data
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
      call s_input_control_ave_udt(field_FUTIL, ucd_FUTIL)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_field_address_type'
      call set_field_address_type                                       &
     &   (femmesh_FUTIL%mesh%node%numnod, field_FUTIL, iphys_FUTIL)
!
      end subroutine initialize_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ave_udt
!
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
      call link_num_field_2_ucd(field_FUTIL, ucd_FUTIL)
!
      istep_ucd = i_step_init / i_step_output_ucd
      call set_data_by_read_ucd_once(my_rank, istep_ucd,                &
     &    udt_org_param%iflag_format, udt_org_param%file_prefix,        &
     &    field_FUTIL)
!
      icou = 1
      do istep = i_step_init+1, i_step_number
        if ( mod(istep,i_step_output_ucd) .eq. izero) then
!
          istep_ucd = istep / i_step_output_ucd
          icou = icou + 1
!
          call add_ucd_to_data(my_rank, istep_ucd,                      &
     &        udt_org_param%iflag_format, udt_org_param%file_prefix,    &
     &        field_FUTIL)
        end if
      end do
!
      call s_divide_phys_by_num_udt(icou, field_FUTIL)
      call nod_fields_send_recv                                         &
     &   (femmesh_FUTIL%mesh%nod_comm, field_FUTIL)
!
!    output udt data
!
      call set_ucd_file_prefix(ave_udt_file_head, ucd_FUTIL)
      call output_udt_one_snapshot(i_step_number,                       &
     &    femmesh_FUTIL%mesh%node, femmesh_FUTIL%mesh%ele,              &
     &    femmesh_FUTIL%mesh%nod_comm, field_FUTIL,                     &
     &    ucd_FUTIL, m_ucd_FUTIL)
!
      end subroutine analyze_ave_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_ave_udt

