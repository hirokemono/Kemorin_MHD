!FEM_analyzer_filtered.f90
!      module FEM_analyzer_filtered
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_analyze_filtered(i_step, istep_psf, istep_iso,    &
!     &          istep_pvr, istep_fline, visval)
!
      module FEM_analyzer_filtered
!
      use m_precision
      use m_work_time
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
      subroutine FEM_analyze_filtered(i_step, istep_psf, istep_iso,     &
     &          istep_pvr, istep_fline, visval)
!
      use m_control_parameter
      use m_geometry_data_MHD
      use m_mesh_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_3d_filter_coef_MHD
      use m_layering_ele_list
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_work_4_dynamic_model
      use m_ucd_data
      use m_bc_data_velo
      use m_flexible_time_step
!
      use read_udt_4_snapshot
!
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use cal_model_coefficients
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
      use time_step_data_IO_control
      use node_monitor_IO
      use sgs_model_coefs_IO
      use fem_mhd_rst_IO_control
      use output_viz_file_control
      use set_exit_flag_4_visualizer
      use filter_all_fields
!
      use check_deltat_by_prev_rms
!
      integer(kind=kint ), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
!
!
!     ---- Load field data --- 
!
      call reset_update_flag(nod_fld1, sgs_coefs, diff_coefs)
      istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*) 'step: ', istep_max_dt
!
      if (i_step_output_rst .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot(mesh1%node, nod_fld1)
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
        call subtract_2_nod_scalars(mesh1%node, nod_fld1,               &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(mesh1%nod_comm, nod_fld1)
!
      if (iflag_debug.eq.1)  write(*,*) 'update_fields'
      call update_fields(mesh1, group1, ele_mesh1, MHD_mesh1,           &
     &    nod1_bcs, sf1_bcs, iphys, iphys_ele,                          &
     &    jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,  &
     &      ifld_diff, icomp_diff, iphys_elediff,                       &
     &    filtering1, wide_filtering, layer_tbl1, m1_lump,              &
     &    wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk, fem1_wk, &
     &    surf1_wk, f1_l, f1_nl, nod_fld1, fld_ele1, diff_coefs)
!
!     ----- Evaluate model coefficients
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients                                   &
     &     (mesh1, group1, ele_mesh1, MHD_mesh1, layer_tbl1,            &
     &      nod1_bcs, sf1_bcs, iphys, iphys_ele, fld_ele1,              &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,           &
     &      FEM1_elen, ifld_sgs, icomp_sgs, ifld_diff, icomp_diff,      &
     &      iphys_elediff, filtering1, wide_filtering, m1_lump,         &
     &      wk_cor1, wk_lsq1, wk_sgs1, wk_diff1, wk_filter1,            &
     &      mhd_fem1_wk, fem1_wk, surf1_wk, f1_l, f1_nl,                &
     &      nod_fld1, sgs_coefs, sgs_coefs_nod, diff_coefs)
      end if
!
!     ========  Data output
!
      call lead_fields_by_FEM(mesh1, group1, ele_mesh1,                 &
     &    MHD_mesh1, nod1_bcs, sf1_bcs, iphys, iphys_ele, ak_MHD,       &
     &    jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,             &
     &    FEM1_elen, icomp_sgs, icomp_diff, ifld_diff, iphys_elediff,   &
     &    sgs_coefs, sgs_coefs_nod, filtering1, wide_filtering,         &
     &    layer_tbl1, m1_lump, wk_cor1, wk_lsq1, wk_diff1, wk_filter1,  &
     &    mhd_fem1_wk, fem1_wk, surf1_wk, f1_l, f1_nl,                  &
     &    nod_fld1, fld_ele1, diff_coefs)
!
!     ----Filtering
      if (iflag_debug.eq.1) write(*,*) 'filtering_all_fields'
      call filtering_all_fields                                         &
     &   (mesh1%nod_comm, mesh1%node, filtering1, wk_filter1, nod_fld1)
!
!     -----Output monitor date
!
      if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
      call output_time_step_control(mesh1, MHD_mesh1,                   &
     &    iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, jac1_3d_l,   &
     &    fem1_wk, mhd_fem1_wk)
!
      if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
      call output_monitor_control(mesh1%node, nod_fld1)
!
      if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
      call s_output_sgs_model_coefs(wk_sgs1, wk_diff1)
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
      end subroutine FEM_analyze_filtered
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_filtered
