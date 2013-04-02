!FEM_analyzer_snapshot.f90
!      module FEM_analyzer_snapshot
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_initialize_snapshot
!      subroutine FEM_analyze_snapshot(istep_psf, istep_iso,            &
!     &          istep_pvr, istep_fline, visval)
!      subroutine FEM_finalize_snapshot
!
      module FEM_analyzer_snapshot
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_t_int_parameter
!
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
      subroutine FEM_initialize_snapshot
!
      use m_control_parameter
!
      use load_mesh_data
      use input_control
      use initialize_4_snapshot
!
      use set_ucd_data
      use output_ucd_file_control
      use open_monitor_file
      use node_monitor_IO
      use open_sgs_model_coefs
      use range_data_IO
!
!   --------------------------------
!       setup mesh information
!   --------------------------------
!
!  --  load FEM mesh data
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      if (iflag_debug.eq.1) write(*,*) 'input_meshes_4_MHD'
      call input_meshes_4_MHD
!
!     --------------------- 
!
      call time_prog_barrier
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap
!
      call output_grd_file_w_org_connect
!
      call s_open_monitor_file(my_rank)
      call s_open_sgs_model_coefs(my_rank)
      call open_maximum_file(my_rank)
      call s_open_node_monitor_file(my_rank)
!
      end subroutine FEM_initialize_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snapshot(i_step, istep_psf, istep_iso,     &
     &          istep_pvr, istep_fline, visval)
!
      use m_control_parameter
!
      use read_udt_4_snapshot
!
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use cal_model_coefficients
      use check_flexible_time_step
      use chenge_step_4_dynamic
      use convert_temperatures
!
      use time_step_data_IO
      use node_monitor_IO
      use sgs_model_coefs_IO
      use mhd_restart_file_IO_control
      use output_ucd_file_control
      use output_viz_file_control
      use set_exit_flag_4_visualizer
!
      use check_deltat_by_prev_rms
!
      integer(kind=kint ), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
!
!     ---- Load field data --- 
!
      call reset_update_flag
      istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*) 'step: ', istep_max_dt
!
      if (i_step_output_rst .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot
!
      else if (i_step_output_ucd .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
        call read_udt_4_snap(istep_max_dt)
        time = time_init + dt*dble(istep_max_dt)
        i_step_MHD = istep_max_dt
      end if
!
!     ---- magnetic field update
!
      if (iflag_4_ref_temp .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
        call set_2_perturbation_temp
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call phys_send_recv_all
!
      if (iflag_debug.eq.1)  write(*,*) 'update_fields'
      call update_fields
!
!     ----- Evaluate model coefficients
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients
      end if
!
!     ========  Data output
!
      call lead_fields_by_FEM
!
!     -----Output monitor date
!
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
!     ---- Output voulme field data
!
      if (iflag_debug.eq.1) write(*,*) 's_output_ucd_file_control'
      call s_output_ucd_file_control
!
!     ----
!
      if     (iflag_flexible_step .eq. iflag_flex_step) then
        call output_viz_file_4_flex(istep_psf, istep_iso,               &
     &      istep_pvr, istep_fline, visval)
      else
        call set_flag_to_visualization(istep_max_dt,                    &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
      end if
!
      call end_eleps_time(4)
!
      end subroutine FEM_analyze_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snapshot
!
      use m_t_step_parameter
      use open_monitor_file
      use open_sgs_model_coefs
      use node_monitor_IO
      use range_data_IO
!
!
      call close_monitor_file(my_rank)
      call close_sgs_model_coefs(my_rank)
      call close_maximum_file(my_rank)
      call close_node_monitor_file
!        call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snapshot
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snapshot
