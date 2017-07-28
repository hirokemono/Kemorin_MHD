!
!      module FEM_analyzer_MHD
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_MHD(MHD_files, bc_FEM_IO, MHD_step)
!!      subroutine FEM_analyze_MHD(MHD_files, MHD_step, visval, retval)
!!      subroutine FEM_finalize_MHD(MHD_files, MHD_step)
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(IO_boundary), intent(in) :: bc_FEM_IO
!
      module FEM_analyzer_MHD
!
      use m_precision
      use m_work_time
      use m_machine_parameter
!
      use m_SGS_control_parameter
      use m_physical_property
      use m_mesh_data
      use m_ucd_data
      use m_sorted_node_MHD
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ucd_file
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
      subroutine FEM_initialize_MHD(MHD_files, bc_FEM_IO, MHD_step)
!
      use m_mesh_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
      use m_filter_elength
      use m_control_parameter
      use m_cal_max_indices
      use m_layering_ele_list
      use m_bc_data_velo
      use m_work_4_dynamic_model
      use m_flexible_time_step
      use m_type_AMG_data
      use t_boundary_field_IO
!
      use input_control
      use initialization_4_MHD
      use lead_physical_values
      use update_after_evolution
!
      use nod_phys_send_recv
      use cal_model_coefficients
      use check_deltat_by_prev_rms
      use construct_matrices
!
      use chenge_step_4_dynamic
      use output_viz_file_control
      use FEM_MHD_ucd_data
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(IO_boundary), intent(in) :: bc_FEM_IO
!
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!   matrix assembling
!
      call init_analyzer_fl                                             &
     &   (MHD_files, bc_FEM_IO, FEM_prm1, SGS_par1, MHD_step,           &
     &    mesh1, group1, ele_mesh1, MHD_mesh1, layer_tbl1,              &
     &    MHD_prop1, ak_MHD, Csims_FEM_MHD1,                            &
     &    iphys, nod_fld1, label_sim)
!
      call nod_fields_send_recv(mesh1%nod_comm, nod_fld1)
!
!   obtain elemental averages
!
      call reset_update_flag(nod_fld1,                                  &
     &    Csims_FEM_MHD1%sgs_coefs, Csims_FEM_MHD1%diff_coefs)
      if (iflag_debug.eq.1) write(*,*) 'update_fields'
      call update_fields                                                &
     &   (MHD_step%time_d, FEM_prm1, SGS_par1, mesh1, group1,           &
     &    ele_mesh1, MHD_mesh1, nod1_bcs, sf1_bcs, iphys, iphys_ele,    &
     &    fem_int1, FEM1_elen, filtering1, wide_filtering, layer_tbl1,  &
     &    wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk,          &
     &    rhs_mat1, nod_fld1, fld_ele1, Csims_FEM_MHD1)
!
      if (SGS_par1%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'copy_model_coef_2_previous'
        call copy_model_coef_2_previous(SGS_par1%commute_p,             &
     &     (wk_sgs1%nlayer, wk_sgs1%num_kinds, wk_sgs1%fld_coef,        &
     &      wk_diff1%nlayer, wk_diff1%num_kinds,                        &
     &      wk_diff1%fld_coef, wk_diff1%fld_whole,                      &
     &      wk_sgs1%coef_p, wk_diff1%coef_p, wk_diff1%coef_wp)
      end if
!
!   construct matrix for Poisson and diffusion terms
!
      if (iflag_debug.eq.1) write(*,*) 'set_data_4_const_matrices'
      call set_data_4_const_matrices                                    &
     &   (mesh1, MHD_mesh1, MHD_prop1, fem_int1,                        &
     &    MGCG_WK1, MHD1_mat_tbls, solver_pack1)
      if (iflag_debug.eq.1) write(*,*) 'set_aiccg_matrices'
      call set_aiccg_matrices(MHD_step%time_d%dt, FEM_prm1,             &
     &    SGS_par1%model_p, SGS_par1%commute_p, mesh1, group1,          &
     &    ele_mesh1, MHD_mesh1, nod1_bcs, sf1_bcs, MHD_prop1, ak_MHD,   &
     &    fem_int1, FEM1_elen, Csims_FEM_MHD1, MHD1_mat_tbls,           &
     &    rhs_mat1, mhd_fem1_wk, MHD1_matrices)
!
!   time evolution loop start!
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
      if (iflag_debug.eq.1) write(*,*) 'lead_fields_by_FEM'
      iflag = lead_field_data_flag(flex_p1%istep_max_dt, MHD_step)
      if(iflag .eq. 0) then
        call lead_fields_by_FEM                                         &
     &     (MHD_step%time_d, FEM_prm1, SGS_par1, mesh1, group1,         &
     &      ele_mesh1, MHD_mesh1, MHD_prop1, nod1_bcs, sf1_bcs,         &
     &      iphys, iphys_ele, ak_MHD, fem_int1, FEM1_elen,              &
     &      filtering1, wide_filtering, layer_tbl1,                     &
     &      wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk,        &
     &      rhs_mat1, nod_fld1, fld_ele1, Csims_FEM_MHD1)
      end if
!
!     ---------------------
!
      SGS_par1%iflag_SGS_initial = 0
!
      call s_check_deltat_by_prev_rms                                   &
     &   (flex_p1, MHD_step%time_d, mesh1, MHD_mesh1,                   &
     &    MHD_prop1%cd_prop, iphys, nod_fld1, fem_int1%jcs,             &
     &    rhs_mat1%fem_wk, flex_data)
!
!    Open monitor files
      call end_eleps_time(2)
      call start_eleps_time(4)
!
      call output_grd_file_w_org_connect(MHD_step%ucd_step, mesh1,      &
     &    MHD_mesh1, nod_fld1, ucd_param, fem_ucd1)
!
      call alloc_phys_range(nod_fld1%ntot_phys_viz, range)
!       call s_open_boundary_monitor(my_rank, group1%sf_grp)
      call end_eleps_time(4)
!
      end subroutine FEM_initialize_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_MHD(MHD_files, MHD_step, visval, retval)
!
      use m_control_parameter
!
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_finite_element_matrix
      use m_filter_elength
      use m_layering_ele_list
      use m_work_4_dynamic_model
      use m_type_AMG_data
      use m_flexible_time_step
!
      use construct_matrices
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
!
      use init_iccg_matrices
      use check_deltat_by_prev_rms
      use output_viz_file_control
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      integer(kind=kint), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind=kint ), intent(inout) :: retval
!
      integer(kind = kint) :: iflag
      real(kind = kreal) :: total_max
!
!
!     ---- step to next time!! --- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_time_and_step'
      call set_new_time_and_step(MHD_prop1%cd_prop, iphys, nod_fld1,    &
     &    flex_p1, MHD_step%time_d)
!
      if (iflag_debug.eq.1) write(*,*) 'fields_evolution_4_FEM_SPH'
      call fields_evolution_4_FEM_SPH                                   &
     &   (MHD_step%time_d, FEM_prm1, SGS_par1,                          &
     &    mesh1, group1, ele_mesh1, MHD_mesh1%fluid, MHD_prop1,         &
     &    nod1_bcs, sf1_bcs, iphys, iphys_ele, ak_MHD, fem_int1,        &
     &    FEM1_elen, filtering1, wide_filtering,                        &
     &    layer_tbl1, solver_pack1, MGCG_WK1, wk_cor1, wk_lsq1,         &
     &    wk_sgs1, wk_diff1, wk_filter1, mhd_fem1_wk, rhs_mat1,         &
     &    nod_fld1, fld_ele1, Csims_FEM_MHD1)
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
!     ---------------------
!
      if (flex_p1%iflag_flexible_step .eq. iflag_flex_step) then
        if (iflag_debug.eq.1) write(*,*) 's_check_flexible_time_step'
        call s_check_flexible_time_step                                 &
     &     (mesh1, MHD_mesh1, MHD_prop1%cd_prop, iphys, nod_fld1,       &
     &      fem_int1%jcs, rhs_mat1%fem_wk, flex_data,                   &
     &      flex_p1, MHD_step%time_d)
      end if
!
!     ========  Data output
!
      if(flex_p1%istep_flex_to_max .eq. 0) then
        iflag = lead_field_data_flag(flex_p1%istep_max_dt, MHD_step)
        if(iflag .eq. 0) then
          call lead_fields_by_FEM                                       &
     &       (MHD_step%time_d, FEM_prm1, SGS_par1, mesh1, group1,       &
     &        ele_mesh1, MHD_mesh1, MHD_prop1, nod1_bcs, sf1_bcs,       &
     &        iphys, iphys_ele, ak_MHD, fem_int1, FEM1_elen,            &
     &        filtering1, wide_filtering, layer_tbl1,                   &
     &        wk_cor1, wk_lsq1, wk_diff1, wk_filter1, mhd_fem1_wk,      &
     &        rhs_mat1, nod_fld1, fld_ele1, Csims_FEM_MHD1)
        end if
!
!     -----Output monitor date
!
        call end_eleps_time(3)
        call start_eleps_time(4)
!
        iflag = output_IO_flag(flex_p1%istep_max_dt, MHD_step%rms_step)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
          call output_time_step_control                                 &
     &       (FEM_prm1, MHD_step%time_d, mesh1, MHD_mesh1,              &
     &        MHD_prop1%fl_prop, MHD_prop1%cd_prop,                     &
     &        iphys, nod_fld1, iphys_ele, fld_ele1,                     &
     &        fem_int1%jcs, rhs_mat1%fem_wk, mhd_fem1_wk)
        end if
!
        iflag= output_IO_flag(flex_p1%istep_max_dt,MHD_step%point_step)
        if(iflag .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
          call output_monitor_control                                   &
     &       (MHD_step%time_d, mesh1%node, nod_fld1)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
        call s_output_sgs_model_coefs(flex_p1%istep_max_dt, MHD_step,   &
     &      SGS_par1, MHD_prop1%cd_prop, wk_sgs1, wk_diff1)
!
!     ---- Output voulme field data
!
        if (iflag_debug.eq.1) write(*,*) 's_output_ucd_file_control'
        call s_output_ucd_file_control(ucd_param, flex_p1%istep_max_dt, &
     &      MHD_step%time_d, MHD_step%ucd_step, fem_ucd1)
!
        call end_eleps_time(4)
        call start_eleps_time(3)
      end if
!
!
      total_time = MPI_WTIME() - total_start
      call MPI_allREDUCE (total_time, total_max, ione, CALYPSO_REAL,    &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if(iflag_debug.gt.0) write(*,*) 'total_time',                     &
     &                       total_time, MHD_step%finish_d%elapsed_time
!
!     ---- Output restart field data
!
      iflag = set_IO_step_flag(flex_p1%istep_max_dt,MHD_step%rst_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_MHD_restart_file_ctl'
        call output_MHD_restart_file_ctl                                &
     &     (SGS_par1, MHD_files, MHD_step%time_d, MHD_step%rst_step,    &
     &      mesh1%node, mesh1%nod_comm, iphys,                          &
     &      wk_sgs1, wk_diff1, nod_fld1)
       end if
!
!     ----
!
!   Finish by elapsed time
      if(MHD_step%finish_d%i_end_step .eq. -1) then
        if(total_max .gt. MHD_step%finish_d%elapsed_time) then
          MHD_step%rst_step%istep_file = MHD_step%finish_d%i_end_step
          retval = 0
          call start_eleps_time(4)
          call output_MHD_restart_file_ctl                              &
     &       (SGS_par1, MHD_files, MHD_step%time_d, MHD_step%rst_step,  &
     &        mesh1%node, mesh1%nod_comm, iphys,                        &
     &        wk_sgs1, wk_diff1, nod_fld1)
          call end_eleps_time(4)
        end if
!
!   Finish by specific time
      else
        if(flex_p1%iflag_flexible_step .eq. iflag_flex_step) then
          if(MHD_step%time_d%time .gt. flex_p1%time_to_finish)          &
     &        retval = 0
        else
          if(flex_p1%istep_max_dt                                       &
     &         .ge. MHD_step%finish_d%i_end_step) retval = 0
        end if
      end if
!
!   Set visualization flag
      if(flex_p1%iflag_flexible_step .eq. iflag_flex_step) then
        visval = viz_file_step_4_flex(MHD_step%time_d,                  &
     &                                MHD_step%viz_step)
      else
        visval = viz_file_step_4_fix(flex_p1%istep_max_dt,              &
     &                               MHD_step%viz_step)
      end if
!
!     --------------------- 
!
      if (SGS_par1%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (iflag_debug.eq.1) write(*,*) 's_chenge_step_4_dynamic'
        call s_chenge_step_4_dynamic                                    &
     &     (my_rank, MHD_step%time_d%i_time_step,                       &
     &      SGS_par1%model_p, SGS_par1%commute_p,                       &
     &      SGS_par1%i_step_sgs_coefs, wk_sgs1, wk_diff1)
      end if
!
      if ( retval .ne. 0 ) then
        if (iflag_debug.eq.1) write(*,*) 'update_matrices'
        call update_matrices(MHD_step%time_d, FEM_prm1, SGS_par1,       &
     &     mesh1, group1, ele_mesh1, MHD_mesh1, nod1_bcs, sf1_bcs,      &
     &     MHD_prop1, ak_MHD, fem_int1, FEM1_elen, Csims_FEM_MHD1,      &
     &     MHD1_mat_tbls, flex_p1, rhs_mat1, mhd_fem1_wk,               &
     &     MHD1_matrices)
      end if
!
      end subroutine FEM_analyze_MHD
!
! ----------------------------------------------------------------------
!
      subroutine FEM_finalize_MHD(MHD_files, MHD_step)
!
      use m_cal_max_indices
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
!
      if(MHD_step%ucd_step%increment .gt. 0) then
        call finalize_output_ucd(MHD_files%ucd_file_IO, fem_ucd1)
        call dealloc_phys_range(range)
      end if
!      call close_boundary_monitor(my_rank)
!
      end subroutine FEM_finalize_MHD
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_MHD
