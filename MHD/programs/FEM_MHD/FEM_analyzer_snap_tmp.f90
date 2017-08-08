!FEM_analyzer_snap_tmp.f90
!      module FEM_analyzer_snap_tmp
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_snap_tmp                              &
!!     &         (MHD_files, bc_FEM_IO, MHD_step, range, fem_ucd, fem_sq)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(IO_boundary), intent(in) :: bc_FEM_IO
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(maximum_informations), intent(inout) :: range
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!      subroutine FEM_analyze_snap_tmp                                 &
!!     &          (i_step, MHD_files, MHD_step, visval, fem_ucd, fem_sq)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(VIZ_step_params), intent(inout) :: MHD_step
!!      subroutine FEM_finalize_snap_tmp                                &
!!     &         (MHD_files, MHD_step, range, fem_ucd)
!
      module FEM_analyzer_snap_tmp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_SGS_control_parameter
      use m_mesh_data
      use t_ucd_file
      use t_time_data
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ucd_file
      use t_cal_max_indices
      use t_FEM_MHD_mean_square
!
      use calypso_mpi
!
      implicit none
!
      type(time_data), save, private :: SNAP_time_IO
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_initialize_snap_tmp                                &
     &         (MHD_files, bc_FEM_IO, MHD_step, range, fem_ucd, fem_sq)
!
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_layering_ele_list
      use m_physical_property
      use m_element_phys_data
      use m_SGS_control_parameter
      use t_boundary_field_IO
!
      use initialize_4_snapshot
      use FEM_MHD_ucd_data
!
      use node_monitor_IO
      use open_sgs_model_coefs
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(IO_boundary), intent(in) :: bc_FEM_IO
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(maximum_informations), intent(inout) :: range
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap                                           &
     &   (MHD_files%fst_file_IO, FEM_prm1, SGS_par1, bc_FEM_IO,         &
     &    MHD_step, mesh1, group1, ele_mesh1, MHD_mesh1, layer_tbl1,    &
     &    MHD_prop1, ak_MHD, Csims_FEM_MHD1, iphys, nod_fld1,           &
     &    SNAP_time_IO, MHD_step%rst_step, fem_sq, label_sim)
!
      call output_grd_file_w_org_connect(MHD_step%ucd_step,             &
     &    mesh1, MHD_mesh1, nod_fld1, MHD_files%ucd_file_IO, fem_ucd)
!
      call alloc_phys_range(nod_fld1%ntot_phys_viz, range)
!
      end subroutine FEM_initialize_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snap_tmp                                   &
     &          (i_step, MHD_files, MHD_step, visval, fem_ucd, fem_sq)
!
      use m_physical_property
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
      use m_filter_elength
      use m_work_4_dynamic_model
      use m_3d_filter_coef_MHD
      use m_layering_ele_list
      use m_bc_data_velo
      use m_flexible_time_step
      use m_fem_mhd_restart
!
      use input_control
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
      use output_viz_file_control
!
      use check_deltat_by_prev_rms
      use output_viz_file_control
!
      integer(kind=kint ), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      integer(kind=kint ), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind = kint) :: iflag
!
!     ---- Load field data --- 
!
      call reset_update_flag(nod_fld1,                                  &
     &    Csims_FEM_MHD1%sgs_coefs, Csims_FEM_MHD1%diff_coefs)
      flex_p1%istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*) 'step: ', flex_p1%istep_max_dt
!
      if (MHD_step%rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot                                   &
     &     (flex_p1%istep_max_dt, MHD_files%fst_file_IO,                &
     &      mesh1%node, nod_fld1, SNAP_time_IO, MHD_step%rst_step)
!
      else if (MHD_step%ucd_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
        call read_udt_4_snap                                            &
     &     (flex_p1%istep_max_dt, MHD_files%org_ucd_file_IO,            &
     &      nod_fld1, SNAP_time_IO, MHD_step%ucd_step)
!
        MHD_step%time_d%time = MHD_step%init_d%time                     &
     &             + MHD_step%time_d%dt * dble(flex_p1%istep_max_dt)
        MHD_step%time_d%i_time_step = flex_p1%istep_max_dt
      end if
!
!     ---- magnetic field update
!
      if (MHD_prop1%ref_param_T%iflag_reference                         &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_temp'
        call subtract_2_nod_scalars(nod_fld1,                           &
     &      iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
      end if
      if (MHD_prop1%ref_param_C%iflag_reference                         &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_comp'
        call subtract_2_nod_scalars(nod_fld1,                           &
     &      iphys%i_light, iphys%i_ref_c, iphys%i_par_light)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(mesh1%nod_comm, nod_fld1)
!
      if (iflag_debug.eq.1)  write(*,*) 'update_fields'
      call update_fields                                                &
     &   (MHD_step%time_d, FEM_prm1, SGS_par1, mesh1, group1,           &
     &    ele_mesh1, MHD_mesh1, nod1_bcs, sf1_bcs, iphys, iphys_ele,    &
     &    fem_int1, FEM1_elen, filtering1, wide_filtering, layer_tbl1,  &
     &    mk_MHD1, wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk, &
     &    rhs_mat1, nod_fld1, fld_ele1, Csims_FEM_MHD1)
!
!     ----- Evaluate model coefficients
!
      if (SGS_par1%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
        call s_cal_model_coefficients                                   &
     &     (MHD_step%time_d, FEM_prm1, SGS_par1,                        &
     &      mesh1, group1, ele_mesh1, MHD_mesh1, MHD_prop1,             &
     &      layer_tbl1, nod1_bcs, sf1_bcs, iphys, iphys_ele, fld_ele1,  &
     &      fem_int1, FEM1_elen, filtering1, wide_filtering, mk_MHD1,   &
     &      wk_cor1, wk_lsq1, wk_sgs1, wk_diff1, wk_filter1,            &
     &      mhd_fem1_wk, rhs_mat1, nod_fld1, Csims_FEM_MHD1)
      end if
!
!     ========  Data output
!
      iflag = lead_field_data_flag(flex_p1%istep_max_dt, MHD_step)
      if(iflag .eq. 0) then
        call lead_fields_by_FEM                                         &
     &     (MHD_step%time_d, FEM_prm1, SGS_par1, mesh1, group1,         &
     &      ele_mesh1, MHD_mesh1, MHD_prop1, nod1_bcs, sf1_bcs,         &
     &      iphys, iphys_ele, ak_MHD, fem_int1, FEM1_elen,              &
     &      filtering1, wide_filtering, layer_tbl1, mk_MHD1,            &
     &      wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk,        &
     &     rhs_mat1, nod_fld1, fld_ele1, Csims_FEM_MHD1)
      end if
!
      if (iflag_debug.eq.1)  write(*,*) 'lead_specital_SGS'
      call lead_specital_SGS
!
!     -----Output monitor date
!
      iflag = output_IO_flag(flex_p1%istep_max_dt, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control                                   &
     &     (FEM_prm1, MHD_step%time_d, mesh1, MHD_mesh1,                &
     &      MHD_prop1%fl_prop, MHD_prop1%cd_prop, iphys,                &
     &      nod_fld1, iphys_ele, fld_ele1, fem_int1%jcs,                &
     &      fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq,                   &
     &      rhs_mat1%fem_wk, mhd_fem1_wk, fem_sq%msq)
      end if
!
      iflag = output_IO_flag(flex_p1%istep_max_dt, MHD_step%point_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
        call output_monitor_control                                     &
     &     (MHD_step%time_d, mesh1%node, nod_fld1)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
      call s_output_sgs_model_coefs(flex_p1%istep_max_dt, MHD_step,     &
     &    SGS_par1, MHD_prop1%cd_prop, wk_sgs1, wk_diff1)
!
!     ---- Output voulme field data
!
      if (iflag_debug.eq.1) write(*,*) 's_output_ucd_file_control'
      call s_output_ucd_file_control                                    &
     &   (MHD_files%ucd_file_IO, flex_p1%istep_max_dt,                  &
     &    MHD_step%time_d, MHD_step%ucd_step, fem_ucd)
!
!     ----
!
      if     (flex_p1%iflag_flexible_step .eq. iflag_flex_step) then
        visval = viz_file_step_4_flex(MHD_step%time_d,                  &
     &                                MHD_step%viz_step)
      else
        visval = viz_file_step_4_fix(flex_p1%istep_max_dt,              &
     &                               MHD_step%viz_step)
      end if
!
      end subroutine FEM_analyze_snap_tmp
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snap_tmp                                  &
     &         (MHD_files, MHD_step, range, fem_ucd)
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
      type(maximum_informations), intent(inout) :: range
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call finalize_output_ucd(MHD_files%ucd_file_IO, fem_ucd)
        call dealloc_phys_range(range)
      end if
!      call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snap_tmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine lead_specital_SGS
!
      use m_MHD_step_parameter
      use m_physical_property
      use m_phys_constants
      use m_phys_labels
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
      use m_filter_elength
      use m_3d_filter_coef_MHD
      use m_bc_data_velo
!
      use copy_nodal_fields
      use cvt_sph_vector_2_xyz_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_sph_tensor_2_xyz_smp
      use cvt_xyz_tensor_2_sph_smp
      use coordinate_convert_4_sph
!
      use products_nodal_fields_smp
      use cal_momentum_terms
      use cal_sgs_4_monitor
      use int_sgs_induction
!
!
!$omp parallel
      call overwrite_nodal_xyz_2_sph_smp                                &
     &   (mesh1%node, nod_fld1%ntot_phys,                               &
     &    iphys%i_SGS_m_flux, n_sym_tensor, nod_fld1%d_fld)
!$omp end parallel
!
      call clear_field_data(nod_fld1, n_sym_tensor, iphys%i_SGS_m_flux)
!
!$omp parallel
      call overwrite_nodal_sph_2_xyz_smp                                &
     &   (mesh1%node, nod_fld1%ntot_phys,                               &
     &    iphys%i_SGS_m_flux, n_sym_tensor, nod_fld1%d_fld)
!$omp end parallel
!
      if (iphys%i_SGS_div_m_flux .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead radial', trim(fhd_div_SGS_m_flux)
        call cal_terms_4_momentum(iphys%i_SGS_div_m_flux,               &
     &      Csims_FEM_MHD1%ifld_diff%i_mom_flux,                        &
     &      Csims_FEM_MHD1%ifld_diff%i_lorentz, MHD_step1%time_d%dt,    &
     &      FEM_prm1, SGS_par1%model_p, SGS_par1%commute_p,             &
     &      mesh1%nod_comm, mesh1%node, mesh1%ele, ele_mesh1%surf,      &
     &      group1%surf_grp, MHD_mesh1%fluid,                           &
     &      MHD_prop1%fl_prop, MHD_prop1%cd_prop,                       &
     &      sf1_bcs%Vsf_bcs, sf1_bcs%Bsf_bcs, iphys, iphys_ele, ak_MHD, &
     &      fem_int1, FEM1_elen, Csims_FEM_MHD1%diff_coefs,             &
     &      mk_MHD1%mlump_fl, mhd_fem1_wk, rhs_mat1,                    &
     &      nod_fld1, fld_ele1)
      end if
!
!$omp parallel
      if (iphys%i_reynolds_wk .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_velo, iphys%i_SGS_div_m_flux, iphys%i_reynolds_wk,  &
     &      nod_fld1)
      end if
!
      call overwrite_nodal_xyz_2_sph_smp                                &
     &   (mesh1%node, nod_fld1%ntot_phys,                               &
     &    iphys%i_velo, n_vector, nod_fld1%d_fld)
!$omp end parallel

      call clear_field_data(nod_fld1, n_vector, iphys%i_velo)
!
!$omp parallel
      call overwrite_nodal_sph_2_xyz_smp                                &
     &   (mesh1%node, nod_fld1%ntot_phys,                               &
     &    iphys%i_velo, n_vector, nod_fld1%d_fld)
!$omp end parallel
!
      if (iphys%i_SGS_vp_induct .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_vp_induct)
        call cal_sgs_uxb_2_monitor                                      &
     &     (Csims_FEM_MHD1%icomp_sgs%i_induction,                       &
     &      Csims_FEM_MHD1%iphys_elediff%i_velo, MHD_step1%time_d%dt,   &
     &      FEM_prm1, SGS_par1%model_p, SGS_par1%filter_p,              &
     &      mesh1%nod_comm, mesh1%node, mesh1%ele,                      &
     &      MHD_mesh1%conduct, MHD_prop1%cd_prop,                       &
     &      iphys, iphys_ele, fld_ele1, fem_int1%jcs%jac_3d,            &
     &      fem_int1%rhs_tbl, FEM1_elen, filtering1,                    &
     &      Csims_FEM_MHD1%sgs_coefs, mk_MHD1%mlump_cd,                 &
     &      wk_filter1, mhd_fem1_wk, rhs_mat1%fem_wk,                   &
     &      rhs_mat1%f_l, rhs_mat1%f_nl, nod_fld1)

      end if
!
      if (iphys%i_SGS_induction .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &        'lead ', trim(fhd_SGS_induction)
        call int_vol_sgs_induction(FEM_prm1,                            &
     &      mesh1%nod_comm, mesh1%node, mesh1%ele, MHD_mesh1%conduct,   &
     &      iphys, fem_int1%jcs%jac_3d, fem_int1%rhs_tbl,               &
     &      mk_MHD1%mlump_cd, mhd_fem1_wk,                              &
     &      rhs_mat1%fem_wk, rhs_mat1%f_nl, nod_fld1)
      end if
!
!$omp parallel
      if (iphys%i_SGS_me_gen .gt. 0) then
        call cal_phys_dot_product                                       &
     &     (iphys%i_magne, iphys%i_SGS_induction, iphys%i_SGS_me_gen,   &
     &      nod_fld1)
      end if
!$omp end parallel
!
      end subroutine lead_specital_SGS
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snap_tmp
