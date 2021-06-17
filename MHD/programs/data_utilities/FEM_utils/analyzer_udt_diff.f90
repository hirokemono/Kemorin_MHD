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
      use t_FEM_utils
      use t_vector_for_solver
      use m_solver_SR
!
      implicit none
!
!       Structure for time stepping parameters
      type(FEM_utils), save :: FUTIL1
!>        Structure for vectors for solver
      type(vectors_4_solver) :: v_sol41
!       Structure for time stepping parameters
      type(time_step_param), save :: time_U
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
     &   (FUTIL1%mesh_file, FUTIL1%udt_file, FUTIL1%nod_fld, time_U)
!
!     --------------------- 
!
      call mesh_setup_4_FEM_UTIL(FUTIL1%mesh_file, FUTIL1%geofem,       &
     &    v_sol41, SR_sig1, SR_r1, SR_i1, SR_il1)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'init_field_data_w_SGS'
      call init_field_data_w_SGS(FUTIL1%geofem%mesh%node%numnod,        &
     &    FUTIL1%nod_fld, FUTIL1%iphys, FUTIL1%iphys_LES)
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
      type(time_data) :: time_IO
!
!
      do istep = time_U%init_d%i_time_step, time_U%finish_d%i_end_step
        if (output_IO_flag(istep,time_U%ucd_step) .eqv. .FALSE.) cycle
        istep_ucd = IO_step_exc_zero_inc(istep, time_U%ucd_step)
!
        call set_data_by_read_ucd_once(my_rank, istep_ucd,              &
     &      first_ucd_param, FUTIL1%nod_fld, time_IO)
!
        call subtract_by_ucd_data                                       &
     &     (my_rank, istep_ucd, second_ucd_param, FUTIL1%nod_fld)
!
        call s_divide_phys_by_delta_t                                   &
     &     (time_U%time_d%dt, FUTIL1%nod_fld)
!
        call nod_fields_send_recv(FUTIL1%geofem%mesh, FUTIL1%nod_fld,   &
     &                            v_sol41, SR_sig1, SR_r1)
!
!    output udt data
        call link_output_ucd_file_once                                  &
     &     (istep_ucd, FUTIL1%nod_fld, diff_ucd_param, time_IO)
      end do
!
      end subroutine analyze_udt_diff
!
! ----------------------------------------------------------------------
!
      end module analyzer_udt_diff

