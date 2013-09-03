!FEM_analyzer_snap_tmp.f90
!      module FEM_analyzer_snap_tmp
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_initialize_snapshot
!      subroutine FEM_analyze_snapshot(istep_psf, istep_iso,            &
!     &          istep_pvr, istep_fline, visval)
!      subroutine FEM_finalize_snapshot
!
      module FEM_analyzer_snap_tmp
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
      use m_cal_max_indices
!
      use input_control
      use initialize_4_snapshot
!
      use load_mesh_data
      use output_ucd_file_control
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
      call allocate_phys_range
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
      use time_step_data_IO_control
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
      if (iflag_4_ref_temp .ne. id_no_ref_temp) then
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
      if (iflag_debug.eq.1)  write(*,*) 'lead_specital_SGS'
      call lead_specital_SGS
!
!     -----Output monitor date
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
      end subroutine FEM_analyze_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snapshot
!
      use m_t_step_parameter
      use m_cal_max_indices
      use output_parallel_ucd_file
!
!
      call finalize_ucd_file_output
      call deallocate_phys_range
!        call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snapshot
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine lead_specital_SGS
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_phys_labels
      use m_node_phys_address
      use m_node_phys_data
!
      use cvt_sph_vector_2_xyz_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_sph_tensor_2_xyz_smp
      use cvt_xyz_tensor_2_sph_smp
!
      use products_nodal_fields_smp
      use int_sgs_induction
      use cal_momentum_terms
      use cal_sgs_4_monitor
      use monitor_sgs_terms
!
!
      call overwrite_sph_tensor_smp(np_smp, numnod,                     &
     &    inod_smp_stack, d_nod(1,iphys%i_SGS_m_flux), xx,              &
     &    radius, s_cylinder,  a_radius, a_s_cylinder)
      d_nod(1:numnod,iphys%i_SGS_m_flux  ) = zero
      d_nod(1:numnod,iphys%i_SGS_m_flux+1) = zero
      d_nod(1:numnod,iphys%i_SGS_m_flux+2) = zero
!      d_nod(1:numnod,iphys%i_SGS_m_flux+3) = zero
!      d_nod(1:numnod,iphys%i_SGS_m_flux+4) = zero
!      d_nod(1:numnod,iphys%i_SGS_m_flux+5) = zero
      call overwrite_xyz_tensor_by_sph_smp(np_smp, numnod,              &
     &    inod_smp_stack, d_nod(1,iphys%i_SGS_m_flux), xx,              &
     &    radius, s_cylinder,  a_radius, a_s_cylinder)
!
      if (iphys%i_SGS_div_m_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead radial', trim(fhd_div_SGS_m_flux)
        call cal_terms_4_momentum(iphys%i_SGS_div_m_flux)
      end if
!
!$omp parallel
      if (iphys%i_reynolds_wk .gt. 0) then
        call cal_phys_dot_product(iphys%i_velo, iphys%i_SGS_div_m_flux, &
     &      iphys%i_reynolds_wk)
      end if
!$omp end parallel
!
!
      call overwrite_vector_2_sph_smp(np_smp, numnod,                   &
     &    inod_smp_stack, d_nod(1,iphys%i_velo), xx,                    &
     &    radius, s_cylinder,  a_radius, a_s_cylinder)
!      d_nod(1:numnod,iphys%i_velo  ) = zero
      d_nod(1:numnod,iphys%i_velo+1) = zero
      d_nod(1:numnod,iphys%i_velo+2) = zero
      call overwrite_sph_vect_2_xyz_smp(np_smp, numnod,                 &
     &    inod_smp_stack, d_nod(1,iphys%i_velo), colatitude, longitude)
!
      if (iphys%i_SGS_vp_induct .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_vp_induct)
        call cal_sgs_uxb_2_monitor
      end if
!
      if (iphys%i_SGS_induction .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_induction)
        call int_vol_sgs_induction
      end if
!
!$omp parallel
      if (iphys%i_SGS_me_gen .gt. 0) then
        call cal_phys_dot_product(iphys%i_magne, iphys%i_SGS_induction, &
     &      iphys%i_SGS_me_gen)
      end if
!$omp end parallel
!
      end subroutine lead_specital_SGS
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snap_tmp
