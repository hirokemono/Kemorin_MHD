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
      use init_nodal_field_address
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
      call s_input_control_ave_udt(mesh_file_FUTIL, udt_param_FUTIL,    &
     &    field_FUTIL, time_U)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL(mesh_file_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_nod_fld_address'
      call init_nod_fld_address                                         &
     &   (femmesh_FUTIL%mesh%node, field_FUTIL, iphys_FUTIL)
!
      end subroutine initialize_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_ave_udt
!
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
      time_U%ucd_step%istep_file                                        &
     &    = time_U%init_d%i_time_step / time_U%ucd_step%increment
      call set_data_by_read_ucd_once                                    &
     &   (my_rank, time_U%ucd_step%istep_file,                          &
     &    udt_param_FUTIL, field_FUTIL, time_IO_FUTIL)
!
      icou = 1
      do istep = time_U%init_d%i_time_step, time_U%finish_d%i_end_step
        if (output_IO_flag(istep,time_U%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(istep,time_U%ucd_step)
        icou = icou + 1
!
        call add_ucd_to_data(my_rank, time_U%ucd_step%istep_file,       &
     &      udt_param_FUTIL, field_FUTIL)
      end do
!
      call s_divide_phys_by_num_udt(icou, field_FUTIL)
      call nod_fields_send_recv(femmesh_FUTIL%mesh, field_FUTIL)
!
!    output udt data
!
      call output_udt_one_snapshot                                      &
     &   (time_U%finish_d%i_end_step, ave_ucd_param, time_U%time_d,     &
     &    femmesh_FUTIL%mesh%node, femmesh_FUTIL%mesh%ele,              &
     &    femmesh_FUTIL%mesh%nod_comm, field_FUTIL)
!
      end subroutine analyze_ave_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_ave_udt

