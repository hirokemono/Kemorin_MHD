!analyzer_udt_diff.f90
!
!      module analyzer_udt_diff
!
!
!      modified by H. Matsui on Nov., 2006 
!
!      subroutine initialize_udt_diff
!      subroutine analyze_udt_diff
!
!..................................................
!
      module analyzer_udt_diff
!
      use m_precision
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
      subroutine initialize_udt_diff
!
      use m_phys_constants
      use set_field_data_w_SGS
      use input_control_udt_diff
!
!
      if (my_rank.eq.0) then
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_udt_diff'
      call s_input_control_udt_diff                                     &
     &   (mesh_file_FUTIL, udt_param_FUTIL, field_FUTIL, time_U)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL(mesh_file_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_field_data_w_SGS'
      call init_field_data_w_SGS(femmesh_FUTIL%mesh%node%numnod,        &
     &    field_FUTIL, iphys_FUTIL, iphys_LES_FUTIL)
!
      end subroutine initialize_udt_diff
!
! ----------------------------------------------------------------------
!
      subroutine analyze_udt_diff
!
      use m_ctl_params_4_diff_udt
      use set_ucd_data_to_type
      use output_parallel_ucd_file
      use divide_phys_by_delta_t
      use nod_phys_send_recv
      use ucd_IO_select
      use output_parallel_ucd_file
!
      integer(kind = kint) :: istep, istep_ucd
!
!
!
      do istep = time_U%init_d%i_time_step, time_U%finish_d%i_end_step
        if (output_IO_flag(istep,time_U%ucd_step) .eqv. .FALSE.) cycle
        istep_ucd = IO_step_exc_zero_inc(istep, time_U%ucd_step)
!
        call set_data_by_read_ucd_once(my_rank, istep_ucd,              &
     &      first_ucd_param, field_FUTIL, time_IO_FUTIL)
!
        call subtract_by_ucd_data                                       &
     &     (my_rank, istep_ucd, second_ucd_param, field_FUTIL)
!
        call s_divide_phys_by_delta_t(time_U%time_d%dt, field_FUTIL)
!
        call nod_fields_send_recv(femmesh_FUTIL%mesh,                   &
     &                            field_FUTIL, v_sol_FUTIL)
!
!    output udt data
        call link_output_ucd_file_once                                  &
     &     (istep_ucd, field_FUTIL, diff_ucd_param, time_IO_FUTIL)
      end do
!
      end subroutine analyze_udt_diff
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_diff

