!FEM_analyzer_snap_tmp.f90
!      module FEM_analyzer_snap_tmp
!
!      modified by H. Matsui on June, 2005 
!
!      subroutine FEM_initialize_snap_tmp
!      subroutine FEM_analyze_snap_tmp(istep_psf, istep_iso,            &
!     &          istep_pvr, istep_fline, visval)
!      subroutine FEM_finalize_snap_tmp
!
      module FEM_analyzer_snap_tmp
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_t_step_parameter
      use m_t_int_parameter
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
      subroutine FEM_initialize_snap_tmp
!
      use m_control_parameter
      use m_cal_max_indices
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
      use m_layering_ele_list
!
      use initialize_4_snapshot
!
      use node_monitor_IO
      use open_sgs_model_coefs
      use range_data_IO
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap(layer_tbl1)
!
      call output_grd_file_w_org_connect                                &
     &   (node1, ele1, nod_comm, nod_fld1)
!
      call allocate_phys_range(nod_fld1%ntot_phys_viz)
!
      end subroutine FEM_initialize_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snap_tmp(i_step, istep_psf, istep_iso,     &
     &          istep_pvr, istep_fline, visval)
!
      use m_control_parameter
      use m_geometry_data_MHD
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_int_vol_data
      use m_layering_ele_list
!
      use read_udt_4_snapshot
!
      use nod_phys_send_recv
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
      use check_deltat_by_prev_rms
!
      integer(kind=kint ), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      integer(kind=kint ), intent(inout) :: istep_psf, istep_iso
      integer(kind=kint ), intent(inout) :: istep_pvr, istep_fline
!
!     ---- Load field data --- 
!
      call reset_update_flag(nod_fld1)
      istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*) 'step: ', istep_max_dt
!
      if (i_step_output_rst .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot(node1, nod_fld1)
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
        call subtract_2_nod_scalars(node1, nod_fld1,                    &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(node1, nod_comm, nod_fld1)
!
      if (iflag_debug.eq.1)  write(*,*) 'update_fields'
      call update_fields(nod_comm, node1, ele1, surf1,                  &
     &    fluid1, conduct1, sf_grp1, iphys, iphys_ele, fld_ele1,        &
     &    jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,             &
     &    FEM1_elen, layer_tbl1, m1_lump, mhd_fem1_wk, fem1_wk,         &
     &    f1_l, f1_nl, nod_fld1)
!
!     ----- Evaluate model coefficients
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients                                   &
     &     (nod_comm, node1, ele1, surf1, sf_grp1, iphys,               &
     &      iphys_ele, fld_ele1, fluid1, conduct1, layer_tbl1,          &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,           &
     &      FEM1_elen, m1_lump, mhd_fem1_wk, fem1_wk,                   &
     &      f1_l, f1_nl, nod_fld1)
      end if
!
!     ========  Data output
!
      call lead_fields_by_FEM                                           &
     &   (nod_comm, node1, ele1, surf1, edge1, fluid1, conduct1,        &
     &    sf_grp1, iphys, iphys_ele, fld_ele1,                          &
     &    jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,             &
     &    FEM1_elen, layer_tbl1, m1_lump, mhd_fem1_wk, fem1_wk,         &
     &    f1_l, f1_nl, nod_fld1)
!
      if (iflag_debug.eq.1)  write(*,*) 'lead_specital_SGS'
      call lead_specital_SGS
!
!     -----Output monitor date
!
      if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
      call output_time_step_control                                     &
     &   (node1, ele1, fluid1, iphys, nod_fld1, iphys_ele, fld_ele1,    &
     &    jac1_3d_q, jac1_3d_l, fem1_wk, mhd_fem1_wk)
!
      if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
      call output_monitor_control(node1, nod_fld1)
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
      end subroutine FEM_analyze_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snap_tmp
!
      use m_t_step_parameter
      use m_cal_max_indices
!
!
      call finalize_output_ucd
      call deallocate_phys_range
!        call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snap_tmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine lead_specital_SGS
!
      use m_constants
      use m_phys_constants
      use m_phys_labels
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_address
!
      use copy_nodal_fields
      use cvt_sph_vector_2_xyz_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_sph_tensor_2_xyz_smp
      use cvt_xyz_tensor_2_sph_smp
      use coordinate_convert_4_sph
!
      use products_nodal_fields_smp
      use int_sgs_induction
      use cal_momentum_terms
      use cal_sgs_4_monitor
      use int_sgs_induction
!
!
!$omp parallel
      call overwrite_nodal_xyz_2_sph_smp(node1, nod_fld1%ntot_phys,     &
     &    iphys%i_SGS_m_flux, n_sym_tensor, nod_fld1%d_fld)
!$omp end parallel
!
      call clear_nodal_data(node1, nod_fld1,                            &
     &    n_sym_tensor, iphys%i_SGS_m_flux)
!
!$omp parallel
      call overwrite_nodal_sph_2_xyz_smp(node1, nod_fld1%ntot_phys,     &
     &   iphys%i_SGS_m_flux, n_sym_tensor, nod_fld1%d_fld)
!$omp end parallel
!
      if (iphys%i_SGS_div_m_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead radial', trim(fhd_div_SGS_m_flux)
        call cal_terms_4_momentum                                       &
     &     (iphys%i_SGS_div_m_flux, iak_diff_mf, iak_diff_lor,          &
     &      nod_comm, node1, ele1, surf1, fluid1, sf_grp1,              &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, jac1_sf_grp_2d_q,    &
     &      rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl,     &
     &      nod_fld1)
      end if
!
!$omp parallel
      if (iphys%i_reynolds_wk .gt. 0) then
        call cal_phys_dot_product(node1, nod_fld1,                      &
     &      iphys%i_velo, iphys%i_SGS_div_m_flux, iphys%i_reynolds_wk)
      end if
!
      call overwrite_nodal_xyz_2_sph_smp(node1, nod_fld1%ntot_phys,     &
     &   iphys%i_velo, n_vector, nod_fld1%d_fld)
!$omp end parallel

      call clear_nodal_data(node1, nod_fld1, n_vector, iphys%i_velo)
!
!$omp parallel
      call overwrite_nodal_sph_2_xyz_smp(node1, nod_fld1%ntot_phys,     &
     &    iphys%i_velo, n_vector, nod_fld1%d_fld)
!$omp end parallel
!
      if (iphys%i_SGS_vp_induct .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_vp_induct)
        call cal_sgs_uxb_2_monitor(icomp_sgs_uxb, ie_dvx,               &
     &     nod_comm, node1, ele1, conduct1, iphys, iphys_ele, fld_ele1, &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk,        &
     &     f1_l, f1_nl, nod_fld1)

      end if
!
      if (iphys%i_SGS_induction .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_induction)
        call int_vol_sgs_induction                                      &
     &     (nod_comm, node1, ele1, conduct1, iphys, jac1_3d_q,          &
     &      rhs_tbl1, mhd_fem1_wk, fem1_wk, f1_nl, nod_fld1)
      end if
!
!$omp parallel
      if (iphys%i_SGS_me_gen .gt. 0) then
        call cal_phys_dot_product(node1, nod_fld1,                      &
     &      iphys%i_magne, iphys%i_SGS_induction, iphys%i_SGS_me_gen)
      end if
!$omp end parallel
!
      end subroutine lead_specital_SGS
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snap_tmp
