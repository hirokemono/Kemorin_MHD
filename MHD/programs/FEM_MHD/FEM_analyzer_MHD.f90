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
      use m_mesh_data
      use m_ucd_data
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
      use m_geometry_data_MHD
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_int_vol_data
      use m_cal_max_indices
      use m_layering_ele_list
!
      use initialization_4_MHD
      use lead_physical_values
      use update_after_evolution
!
      use nod_phys_send_recv
      use cal_model_coefficients
      use check_deltat_by_prev_rms
      use construct_matrices
!
      use range_data_IO
      use chenge_step_4_dynamic
!
!
!   matrix assembling
!
      call init_analyzer_fl(MHD_mesh1, layer_tbl1)
!
      call nod_fields_send_recv(node1, mesh1%nod_comm, nod_fld1)
!
!   obtain elemental averages
!
      call reset_update_flag(nod_fld1)
      if (iflag_debug.eq.1) write(*,*) 'update_fields'
      call update_fields(mesh1%nod_comm, node1, ele1, surf1,            &
     &    MHD_mesh1, sf_grp1, iphys, iphys_ele, fld_ele1,               &
     &    jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,             &
     &    FEM1_elen, layer_tbl1, m1_lump, mhd_fem1_wk, fem1_wk,         &
     &    f1_l, f1_nl, nod_fld1)
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'copy_model_coef_2_previous'
        call copy_model_coef_2_previous
      end if
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (node1, ele1, MHD_mesh1, rhs_tbl1, mat_tbl_q1)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(node1, ele1, surf1, MHD_mesh1,            &
     &    sf_grp1, jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q,              &
     &    rhs_tbl1, mat_tbl_q1, mhd_fem1_wk)
!
!   time evolution loop start!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients                                   &
     &     (mesh1%nod_comm, node1, ele1, surf1, sf_grp1, iphys,         &
     &      iphys_ele, fld_ele1, MHD_mesh1, layer_tbl1,                 &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,           &
     &      FEM1_elen, m1_lump, mhd_fem1_wk, fem1_wk,                   &
     &      f1_l, f1_nl, nod_fld1)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'lead_fields_by_FEM'
      call lead_fields_by_FEM                                           &
     &   (mesh1%nod_comm, node1, ele1, surf1, edge1, MHD_mesh1,         &
     &    sf_grp1, iphys, iphys_ele, fld_ele1,                          &
     &    jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,             &
     &    FEM1_elen, layer_tbl1, m1_lump, mhd_fem1_wk, fem1_wk,         &
     &    f1_l, f1_nl, nod_fld1)
!
!     ---------------------
!
      iflag_SGS_initial = 0
!
      if (iflag_flexible_step .eq. iflag_flex_step) then
        call set_ele_rms_4_previous_step(node1, ele1, MHD_mesh1%fluid,  &
     &      iphys, nod_fld1, jac1_3d_q, jac1_3d_l, fem1_wk)
        call s_check_deltat_by_prev_rms(node1, ele1, MHD_mesh1%fluid,   &
     &      iphys, nod_fld1, jac1_3d_q, jac1_3d_l, fem1_wk)
      end if
!
!
!    Open monitor files
      call end_eleps_time(2)
      call start_eleps_time(4)
!
      call output_grd_file_w_org_connect                                &
     &   (node1, ele1, mesh1%nod_comm, MHD_mesh1, nod_fld1)
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
      use m_geometry_data_MHD
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_int_vol_data
      use m_layering_ele_list
!
      use construct_matrices
      use lead_physical_values
      use update_after_evolution
      use cal_model_coefficients
      use check_flexible_time_step
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
      use time_step_data_IO_control
      use node_monitor_IO
      use sgs_model_coefs_IO
      use fem_mhd_rst_IO_control
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
!     ---- step to next time!! --- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_time_and_step'
      call set_new_time_and_step(node1, iphys, nod_fld1)
!
!     ----- Time integration
!
      if (iflag_debug.eq.1) write(*,*) 'fields_evolution'
      call fields_evolution(mesh1%nod_comm, node1, ele1, surf1, MHD_mesh1, &
     &    sf_grp1, sf_grp_nod1, iphys, iphys_ele, fld_ele1,             &
     &    jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, jac1_sf_grp_2d_l,     &
     &    rhs_tbl1, FEM1_elen, layer_tbl1, m1_lump,                     &
     &    mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
!
!     ----- Evaluate model coefficients
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients                                   &
     &     (mesh1%nod_comm, node1, ele1, surf1, sf_grp1, iphys,         &
     &      iphys_ele, fld_ele1, MHD_mesh1, layer_tbl1,                 &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,           &
     &      FEM1_elen, m1_lump, mhd_fem1_wk, fem1_wk,                   &
     &      f1_l, f1_nl, nod_fld1)
      end if
!
!     ---------------------
!
      if (iflag_flexible_step .eq. iflag_flex_step) then
        if (iflag_debug.eq.1) write(*,*) 's_check_flexible_time_step'
        call s_check_flexible_time_step(node1, ele1, MHD_mesh1%fluid,   &
     &      iphys, nod_fld1, jac1_3d_q, jac1_3d_l, fem1_wk)
      end if
!
!     ========  Data output
!
      if(istep_flex_to_max .eq. 0) then
        call lead_fields_by_FEM                                         &
     &     (mesh1%nod_comm, node1, ele1, surf1, edge1, MHD_mesh1,       &
     &      sf_grp1, iphys, iphys_ele, fld_ele1,                        &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,           &
     &      FEM1_elen, layer_tbl1, m1_lump, mhd_fem1_wk, fem1_wk,       &
     &      f1_l, f1_nl, nod_fld1)
!
!     -----Output monitor date
!
        call end_eleps_time(3)
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control(node1, ele1, MHD_mesh1,           &
     &      iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, jac1_3d_l, &
     &      fem1_wk, mhd_fem1_wk)
!
        if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
        call output_monitor_control(node1, nod_fld1)
!
        if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
        call s_output_sgs_model_coefs
!
!     ---- Output restart field data
!
        if (iflag_debug.eq.1) write(*,*) 'output_MHD_restart_file_ctl'
        call output_MHD_restart_file_ctl                                &
     &     (node1, mesh1%nod_comm, iphys, nod_fld1)
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
      if(iflag_debug.gt.0) write(*,*) 'total_time',                     &
     &                       total_time, elapsed_time
!
      call MPI_allREDUCE (total_time, total_max, ione, CALYPSO_REAL,    &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if     (iflag_flexible_step .eq. iflag_flex_step) then
        if      (istep_rst_end.eq.-1                                    &
     &       .and. total_max.gt.elapsed_time) then
          call start_eleps_time(4)
          call elspased_MHD_restart_ctl                                 &
     &       (node1, mesh1%nod_comm, iphys, nod_fld1)
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
          call elspased_MHD_restart_ctl                                 &
     &        (node1, mesh1%nod_comm, iphys, nod_fld1)
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
        call update_matrices(node1, ele1, surf1, MHD_mesh1,             &
     &      sf_grp1, jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q,            &
     &      rhs_tbl1, mat_tbl_q1, mhd_fem1_wk)
      end if
!
      end subroutine FEM_analyze_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_MHD
!
      use m_cal_max_indices
!
!
      call finalize_output_ucd
      call deallocate_phys_range
!        call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_MHD
