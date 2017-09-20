!FEM_analyzer_filtered.f90
!      module FEM_analyzer_filtered
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_analyze_filtered(i_step, MHD_files,              &
!!     &          femmesh, ele_mesh, iphys_nod, iphys_ele, ak_MHD,      &
!!     &          MHD_step, visval, FEM_SGS, SGS_MHD_wk,                &
!!     &          nod_fld, ele_fld, fem_ucd, fem_sq)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(phys_address), intent(in) :: iphys_nod, iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_SGS_structure), intent(inout) :: FEM_SGS
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!        type(ucd_file_data), intent(inout) :: fem_ucd
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      module FEM_analyzer_filtered
!
      use m_precision
      use m_work_time
      use m_SGS_control_parameter
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_material_property
      use t_IO_step_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_ucd_file
      use t_FEM_MHD_mean_square
      use t_FEM_SGS_structure
      use t_work_FEM_SGS_MHD
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
      subroutine FEM_analyze_filtered(i_step, MHD_files,                &
     &          femmesh, ele_mesh, iphys_nod, iphys_ele, ak_MHD,        &
     &          MHD_step, visval, FEM_SGS, SGS_MHD_wk,                  &
     &          nod_fld, ele_fld, fem_ucd, fem_sq)
!
      use m_control_parameter
      use m_physical_property
      use m_geometry_data_MHD
      use m_finite_element_matrix
      use m_bc_data_velo
      use m_flexible_time_step
      use m_fem_mhd_restart
!
      use nod_phys_send_recv
      use lead_physical_values
      use update_after_evolution
      use FEM_MHD_evolution
      use chenge_step_4_dynamic
      use copy_nodal_fields
!
      use time_step_data_IO_control
      use node_monitor_IO
      use sgs_model_coefs_IO
      use output_viz_file_control
      use filter_all_fields
      use input_control
!
      use check_deltat_by_prev_rms
      use output_viz_file_control
!
      integer(kind=kint ), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_address), intent(in) :: iphys_nod, iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
!
      integer(kind=kint ), intent(inout) :: visval
      type(MHD_step_param), intent(inout) :: MHD_step
!
      type(phys_data), intent(inout) :: nod_fld, ele_fld
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!
      type(ucd_file_data), intent(inout) :: fem_ucd
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind = kint) :: iflag
!
!     ---- Load field data --- 
!
      call reset_update_flag                                            &
     &   (nod_fld, FEM_SGS%Csims%sgs_coefs, FEM_SGS%Csims%diff_coefs)
      flex_p1%istep_max_dt = i_step
      if (my_rank.eq.0) write(*,*) 'step: ', flex_p1%istep_max_dt
!
      if (MHD_step%rst_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'input_restart_4_snapshot'
        call input_restart_4_snapshot                                   &
     &     (flex_p1%istep_max_dt, MHD_files%fst_file_IO,                &
     &      femmesh%mesh%node, nod_fld, SNAP_time_IO,                   &
     &      MHD_step%rst_step)
!
      else if (MHD_step%ucd_step%increment .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
        call read_udt_4_snap                                            &
     &     (flex_p1%istep_max_dt, MHD_files%org_ucd_file_IO,            &
     &      nod_fld, SNAP_time_IO, MHD_step%ucd_step)
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
        call subtract_2_nod_scalars(nod_fld,                            &
     &     iphys_nod%i_temp, iphys_nod%i_ref_t, iphys_nod%i_par_temp)
      end if
      if (MHD_prop1%ref_param_C%iflag_reference                         &
     & .ne. id_no_ref_temp) then
        if (iflag_debug.eq.1)  write(*,*) 'set_2_perturbation_comp'
        call subtract_2_nod_scalars(nod_fld,                            &
     &     iphys_nod%i_light, iphys_nod%i_ref_c, iphys_nod%i_par_light)
      end if
!
!     ---------------------
!
      if (iflag_debug.eq.1)  write(*,*) 'phys_send_recv_all'
      call nod_fields_send_recv(femmesh%mesh, nod_fld)
!
      if (iflag_debug.eq.1)  write(*,*) 'update_FEM_fields'
      call update_FEM_fields(MHD_step%time_d,                           &
     &    FEM_prm1, FEM_SGS%SGS_par, femmesh, ele_mesh, MHD_mesh1,      &
     &    FEM_MHD1_BCs%nod_bcs, FEM_MHD1_BCs%surf_bcs,                  &
     &    iphys_nod, iphys_ele, fem_int1, FEM_SGS%FEM_filters, mk_MHD1, &
     &    SGS_MHD_wk, nod_fld, ele_fld, FEM_SGS%Csims)
!
!     ----- Evaluate model coefficients
!
      call cal_FEM_model_coefficients                                   &
     &   (MHD_step%time_d, FEM_prm1, FEM_SGS%SGS_par,                   &
     &    femmesh, ele_mesh, MHD_mesh1, MHD_prop1,                      &
     &    FEM_MHD1_BCs%nod_bcs, FEM_MHD1_BCs%surf_bcs,                  &
     &    iphys_nod, iphys_ele, ele_fld, fem_int1, FEM_SGS%FEM_filters, &
     &    mk_MHD1, SGS_MHD_wk, nod_fld, FEM_SGS%Csims)
!
!     ========  Data output
!
      iflag = lead_field_data_flag(flex_p1%istep_max_dt, MHD_step)
      if(iflag .eq. 0) then
        call lead_fields_by_FEM                                         &
     &    (MHD_step%time_d, FEM_prm1, FEM_SGS%SGS_par, femmesh,         &
     &     ele_mesh, MHD_mesh1, MHD_prop1, FEM_MHD1_BCs,                &
     &     iphys_nod, iphys_ele, ak_MHD, fem_int1, FEM_SGS%FEM_filters, &
     &     mk_MHD1, SGS_MHD_wk, nod_fld, ele_fld, FEM_SGS%Csims)
      end if
!
!     ----Filtering
      if (iflag_debug.eq.1) write(*,*) 'filtering_all_fields'
      call filtering_all_fields(FEM_SGS%SGS_par%filter_p,               &
     &    femmesh%mesh%nod_comm, femmesh%mesh%node,                     &
     &    FEM_SGS%FEM_filters%filtering,                                &
     &    SGS_MHD_wk%FEM_SGS_wk%wk_filter, nod_fld)
!
!     -----Output monitor date
!
      iflag = output_IO_flag(flex_p1%istep_max_dt, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control                                   &
     &     (FEM_prm1, MHD_step%time_d, femmesh%mesh, MHD_mesh1,         &
     &      MHD_prop1%fl_prop, MHD_prop1%cd_prop,                       &
     &      iphys_nod, nod_fld, iphys_ele, ele_fld, fem_int1%jcs,       &
     &      fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq,                   &
     &      SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq%msq)
      end if
!
      iflag = output_IO_flag(flex_p1%istep_max_dt,MHD_step%point_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_monitor_control'
        call output_monitor_control                                     &
     &     (MHD_step%time_d, femmesh%mesh%node, nod_fld)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 's_output_sgs_model_coefs'
      call s_output_sgs_model_coefs(flex_p1%istep_max_dt,               &
     &    MHD_step, FEM_SGS%SGS_par, MHD_prop1%cd_prop,                 &
     &    SGS_MHD_wk%FEM_SGS_wk)
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
      end subroutine FEM_analyze_filtered
!
! ----------------------------------------------------------------------
!
      end module FEM_analyzer_filtered
