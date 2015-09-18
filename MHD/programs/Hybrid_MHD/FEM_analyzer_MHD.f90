!
!      module FEM_analyzer_MHD
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_initialize_MHD
!      subroutine FEM_analyze_MHD(istep_psf, istep_iso,                 &
!     &          istep_pvr, istep_fline, visval, retval)
!      subroutine FEM_finalize_MHD
!
      module FEM_analyzer_MHD
!
      use m_precision
      use m_work_time
      use m_machine_parameter
      use m_t_step_parameter
      use m_t_int_parameter
!
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_MHD
!
      use m_control_parameter
      use m_cal_max_indices
      use m_node_phys_data
!
      use input_control
      use initialization_4_MHD
      use lead_physical_values
      use update_after_evolution
!
      use nod_phys_send_recv
      use output_ucd_mesh_w_original
      use cal_model_coefficients
      use check_deltat_by_prev_rms
      use construct_matrices
!
      use chenge_step_4_dynamic
!
!   matrix assembling
!
      call init_analyzer_fl
!
      call phys_send_recv_all
!
!   obtain elemental averages
!
      call reset_update_flag
      if (iflag_debug.eq.1) write(*,*) 'update_fields'
      call update_fields
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'copy_model_coef_2_previous'
        call copy_model_coef_2_previous
      end if
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices
!
!   time evolution loop start!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'lead_fields_by_FEM'
      call lead_fields_by_FEM
!
!     ---------------------
!
      iflag_SGS_initial = 0
!
      if (iflag_flexible_step .eq. iflag_flex_step) then
        call set_ele_rms_4_previous_step
        call s_check_deltat_by_prev_rms
      end if
!
!
!    Open monitor files
      call end_eleps_time(2)
      call start_eleps_time(4)
!
      call output_grd_file_w_org_connect
!
      call allocate_phys_range(nod_fld1%ntot_phys_viz)
!       call s_open_boundary_monitor(my_rank, sf_grp1)
      call end_eleps_time(4)
!
      end subroutine FEM_initialize_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_MHD(istep_psf, istep_iso,                  &
     &          istep_pvr, istep_fline, visval, retval)
!
      use m_control_parameter
!
      use cal_temperature
      use cal_part_temperature
      use cal_velocity
      use cal_vector_potential
      use cal_magnetic_field
      use cal_light_element
!
      use construct_matrices
      use lead_physical_values
      use update_after_evolution
      use cal_model_coefficients
      use check_flexible_time_step
      use chenge_step_4_dynamic
      use convert_temperatures
!
      use time_step_data_IO_control
      use node_monitor_IO
      use sgs_model_coefs_IO
      use fem_mhd_rst_IO_control
      use output_ucd_file_control
      use output_viz_file_control
      use set_exit_flag_4_visualizer
!
      use init_iccg_matrices
      use check_deltat_by_prev_rms
!
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
!
      integer(kind=kint ), intent(inout) :: retval
!
      real(kind = kreal) :: total_max
!
!
!     ---- step to next time!! --- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_time_and_step'
      call set_new_time_and_step
!
      if (iflag_debug.eq.1) write(*,*) 'reset_update_flag'
      call reset_update_flag
!
!     ---- magnetic field update
!
!      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
!        if (iflag_debug.eq.1) write(*,*) 'cal_magne_vector_potential'
!        call cal_magne_vector_potential
!        call update_with_vector_potential
!
!      else if ( iflag_t_evo_4_magne .gt. id_no_evolution) then
!
!        if (iflag_debug.eq.1) write(*,*) 's_cal_magnetic_field'
!        call s_cal_magnetic_field
!        call update_with_magnetic_field
!      end if
!
!     ---- temperature update
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if( iflag_4_ref_temp .ne. id_no_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_parturbation_temp'
          call cal_parturbation_temp
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_temperature_field'
          call cal_temperature_field
        end if
!
        call update_with_temperature
      end if
!
!     ----- composition update
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'cal_light_element_variation'
        call cal_light_element_variation
!
        call update_with_dummy_scalar
      end if
!
!     ---- velocity update
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if (iflag_debug.eq.1) write(*,*) 'velocity_evolution'
        call velocity_evolution
        call update_with_velocity
      end if
!
!     ----- Evaluate model coefficients
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients
      end if
!
!     ---------------------
!
      if (iflag_flexible_step .eq. iflag_flex_step) then
        if (iflag_debug.eq.1) write(*,*) 's_check_flexible_time_step'
        call s_check_flexible_time_step
      end if
!
!     ========  Data output
!
      if(istep_flex_to_max .eq. 0) then
        call lead_fields_by_FEM
!
!     -----Output monitor date
!
        call end_eleps_time(3)
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control
!
        if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
        call output_monitor_control
!
        if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
        call s_output_sgs_model_coefs
!
!     ---- Output restart field data
!
        if (iflag_debug.eq.1) write(*,*) 'output_MHD_restart_file_ctl'
        call output_MHD_restart_file_ctl
!
!     ---- Output voulme field data
!
        if (iflag_debug.eq.1) write(*,*) 's_output_ucd_file_control'
        call s_output_ucd_file_control
!
        call end_eleps_time(4)
        call start_eleps_time(3)
      end if
!
!
!     ----
!
      total_time = MPI_WTIME() - total_start
      call MPI_allREDUCE (total_time, total_max, ione, CALYPSO_REAL,    &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if(iflag_debug.gt.0) write(*,*) 'total_time',                     &
     &                       total_time, elapsed_time
!
      if     (iflag_flexible_step .eq. iflag_flex_step) then
        if      (istep_rst_end.eq.-1                                    &
     &       .and. total_max.gt.elapsed_time) then
          call start_eleps_time(4)
          call elspased_MHD_restart_ctl
          call end_eleps_time(4)
          retval = 0
        else if (istep_rst_end.ne.-1                                    &
     &       .and. time.gt.(istep_rst_end*delta_t_output_rst)) then
          retval = 0
        end if
!
        call output_viz_file_4_flex(istep_psf, istep_iso,               &
     &      istep_pvr, istep_fline, visval)
      else
        if      (i_step_number.eq.-1                                    &
     &       .and. total_max.gt.elapsed_time) then
          call start_eleps_time(4)
          call elspased_MHD_restart_ctl
          call end_eleps_time(4)
          retval = 0
        else if (i_step_number.ne.-1 .and.                              &
     &       istep_max_dt .ge. i_step_number) then
          retval = 0
        end if
!
        call set_flag_to_visualization(istep_max_dt,                    &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
      end if
!
!     --------------------- 
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_chenge_step_4_dynamic'
        call s_chenge_step_4_dynamic(my_rank)
      end if
!
      if ( retval .ne. 0 ) then
        if (iflag_debug.eq.1) write(*,*) 'update_matrices'
        call update_matrices
      end if
!
      end subroutine FEM_analyze_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_MHD
!
      use m_cal_max_indices
      use output_parallel_ucd_file
!
!
      call finalize_ucd_file_output
!
      call deallocate_phys_range
!        call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_MHD
