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
      call s_input_control_ave_udt                                      &
     &   (mesh_file_FUTIL, udt_param_FUTIL, field_FUTIL, ucd_FUTIL)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL(mesh_file_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_field_address'
      call init_field_address                                           &
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
      use ucd_IO_select
      use set_ucd_data
      use set_ucd_data_to_type
      use divide_phys_by_delta_t
      use nod_phys_send_recv
      use output_parallel_ucd_file
!
      integer(kind = kint) :: istep, icou
!
!
      call link_num_field_2_ucd(field_FUTIL, ucd_FUTIL)
!
      ucd_step1%istep_file = i_step_init / ucd_step1%increment
      call set_data_by_read_ucd_once(my_rank, ucd_step1%istep_file,     &
     &    udt_param_FUTIL%iflag_format, udt_param_FUTIL%file_prefix,    &
     &    field_FUTIL, time_IO_FUTIL)
!
      icou = 1
      do istep = i_step_init+1, i_step_number
        if (output_IO_flag(istep,ucd_step1) .eq. izero) then
!
          ucd_step1%istep_file = istep / ucd_step1%increment
          icou = icou + 1
!
          call add_ucd_to_data(my_rank, ucd_step1%istep_file,           &
     &       udt_param_FUTIL%iflag_format, udt_param_FUTIL%file_prefix, &
     &       field_FUTIL)
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
     &    femmesh_FUTIL%mesh%nod_comm, field_FUTIL)
!
      end subroutine analyze_ave_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_ave_udt

