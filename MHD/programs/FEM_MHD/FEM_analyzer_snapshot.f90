!FEM_analyzer_snapshot.f90
!      module FEM_analyzer_snapshot
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_snapshot                              &
!!     &          (MHD_files, bc_FEM_IO, MHD_step, fem_ucd)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(IO_boundary), intent(in) :: bc_FEM_IO
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!      subroutine FEM_analyze_snapshot                                 &
!!     &         (i_step, MHD_files, MHD_step, visval, fem_ucd)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!      subroutine FEM_finalize_snapshot(MHD_files, MHD_step, fem_ucd)
!
      module FEM_analyzer_snapshot
!
      use m_precision
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
      subroutine FEM_initialize_snapshot                                &
     &          (MHD_files, bc_FEM_IO, MHD_step, fem_ucd)
!
      use m_cal_max_indices
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
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(IO_boundary), intent(in) :: bc_FEM_IO
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap                                           &
     &   (MHD_files%fst_file_IO, FEM_prm1, SGS_par1, bc_FEM_IO,         &
     &    MHD_step, mesh1, group1, ele_mesh1, MHD_mesh1, layer_tbl1,    &
     &    MHD_prop1, ak_MHD, Csims_FEM_MHD1, iphys, nod_fld1,           &
     &    SNAP_time_IO, MHD_step%rst_step, label_sim)
!
      call output_grd_file_w_org_connect(MHD_step%ucd_step,             &
     &    mesh1, MHD_mesh1, nod_fld1, MHD_files%ucd_file_IO, fem_ucd)
!
      call alloc_phys_range(nod_fld1%ntot_phys_viz, range)
!
      end subroutine FEM_initialize_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_snapshot                                   &
     &         (i_step, MHD_files, MHD_step, visval, fem_ucd)
!
      use m_physical_property
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
      use m_filter_elength
      use m_3d_filter_coef_MHD
      use m_layering_ele_list
      use m_work_4_dynamic_model
      use m_bc_data_velo
      use m_flexible_time_step
      use m_fem_mhd_restart
!
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use cal_model_coefficients
      use chenge_step_4_dynamic
      use copy_nodal_fields
      use input_control
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
     &                + MHD_step%time_d%dt * dble(flex_p1%istep_max_dt)
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
     &    wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk,          &
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
     &      fem_int1, FEM1_elen, filtering1, wide_filtering,            &
     &      wk_cor1, wk_lsq1, wk_sgs1, wk_diff1, wk_filter1,            &
     &      mhd_fem1_wk, rhs_mat1, nod_fld1, Csims_FEM_MHD1)
      end if
!
!     ========  Data output
!
      iflag = lead_field_data_flag(flex_p1%istep_max_dt, MHD_step)
      if(iflag .eq. 0) then
        call lead_fields_by_FEM                                         &
     &     (MHD_step%time_d, FEM_prm1, SGS_par1,  mesh1, group1,        &
     &      ele_mesh1, MHD_mesh1, MHD_prop1, nod1_bcs, sf1_bcs,         &
     &      iphys, iphys_ele, ak_MHD, fem_int1, FEM1_elen,              &
     &      filtering1, wide_filtering, layer_tbl1,                     &
     &      wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk,        &
     &      rhs_mat1, nod_fld1, fld_ele1, Csims_FEM_MHD1)
      end if
!
!     -----Output monitor date
!
      call start_elapsed_time(4)
!
      iflag = output_IO_flag(flex_p1%istep_max_dt, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control                                   &
     &     (FEM_prm1, MHD_step%time_d, mesh1, MHD_mesh1,                &
     &      MHD_prop1%fl_prop, MHD_prop1%cd_prop,                       &
     &      iphys, nod_fld1, iphys_ele, fld_ele1,                       &
     &      fem_int1%jcs, rhs_mat1%fem_wk, mhd_fem1_wk)
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
      call end_elapsed_time(4)
!
      end subroutine FEM_analyze_snapshot
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_snapshot(MHD_files, MHD_step, fem_ucd)
!
      use m_cal_max_indices
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(ucd_file_data), intent(inout) :: fem_ucd
!
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call finalize_output_ucd(MHD_files%ucd_file_IO, fem_ucd)
        call dealloc_phys_range(range)
      end if
!      call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_snapshot
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_snapshot
