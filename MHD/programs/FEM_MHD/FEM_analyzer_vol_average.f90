!FEM_analyzer_vol_average.f90
!      module FEM_analyzer_vol_average
!
!      modified by H. Matsui on June, 2005 
!
!!      subroutine FEM_initialize_vol_average                           &
!!     &         (MHD_files, bc_FEM_IO, MHD_step,                       &
!!     &          femmesh, ele_mesh, SGS_MHD_wk, fem_sq)
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(IO_boundary), intent(in) :: bc_FEM_IO
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!      subroutine FEM_analyze_vol_average(i_step, MHD_files,           &
!!     &          femmesh, MHD_step, SGS_MHD_wk, fem_sq)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      module FEM_analyzer_vol_average
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use m_SGS_control_parameter
      use t_mesh_data
      use t_time_data
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_ucd_file
      use t_FEM_MHD_mean_square
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
      subroutine FEM_initialize_vol_average                             &
     &         (MHD_files, bc_FEM_IO, MHD_step,                         &
     &          femmesh, ele_mesh, SGS_MHD_wk, fem_sq)
!
      use m_node_phys_data
      use m_control_parameter
      use m_layering_ele_list
      use m_geometry_data_MHD
      use m_physical_property
      use m_element_phys_data
      use m_SGS_control_parameter
      use m_finite_element_matrix
      use t_boundary_field_IO
!
      use initialize_4_snapshot
!
      use node_monitor_IO
      use open_sgs_model_coefs
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(IO_boundary), intent(in) :: bc_FEM_IO
!
      type(mesh_data), intent(inout) :: femmesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(MHD_step_param), intent(inout) :: MHD_step
!
!   matrix assembling
!
      if (iflag_debug.eq.1)  write(*,*) 'init_analyzer_snap'
      call init_analyzer_snap                                           &
     &   (MHD_files%fst_file_IO, FEM_prm1, SGS_par1, bc_FEM_IO,         &
     &    MHD_step, femmesh%mesh, femmesh%group, ele_mesh, MHD_mesh1,   &
     &    layer_tbl1, MHD_prop1, ak_MHD, Csims_FEM_MHD1,                &
     &    iphys, nod_fld1, SNAP_time_IO, MHD_step%rst_step,             &
     &    fem_int1, mk_MHD1, SGS_MHD_wk, fem_sq, label_sim)
!
      end subroutine FEM_initialize_vol_average
!
! ----------------------------------------------------------------------
!
      subroutine FEM_analyze_vol_average(i_step, MHD_files,             &
     &          femmesh, MHD_step, SGS_MHD_wk, fem_sq)
!
      use m_control_parameter
      use m_physical_property
      use m_node_phys_data
      use m_geometry_data_MHD
      use m_element_phys_data
      use m_finite_element_matrix
!
      use nod_phys_send_recv
      use lead_physical_values
      use copy_nodal_fields
      use input_control
!
      use time_step_data_IO_control
      use output_parallel_ucd_file
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(mesh_data), intent(in) :: femmesh
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!     ---- Load field data --- 
!
      if (my_rank.eq.0) write(*,*) 'step: ', i_step
!
      if (iflag_debug.eq.1)  write(*,*) 'read_udt_4_snap'
      call read_udt_4_snap(i_step, MHD_files%org_ucd_file_IO,           &
     &    nod_fld1, SNAP_time_IO, MHD_step%ucd_step)
      MHD_step%time_d%time = MHD_step%init_d%time                       &
     &                      + MHD_step%time_d%dt * dble(i_step)
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
      call nod_fields_send_recv(femmesh%mesh, nod_fld1)
!
!     -----Output monitor date
!
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if (iflag_debug.eq.1) write(*,*) 'output_time_step_control'
        call output_time_step_control                                   &
     &     (FEM_prm1, MHD_step%time_d, femmesh%mesh, MHD_mesh1,         &
     &      MHD_prop1%fl_prop, MHD_prop1%cd_prop,                       &
     &      iphys, nod_fld1, iphys_ele, fld_ele1, fem_int1%jcs,         &
     &      fem_sq%i_rms, fem_sq%j_ave, fem_sq%i_msq,                   &
     &      SGS_MHD_wk%rhs_mat, SGS_MHD_wk%mhd_fem_wk, fem_sq%msq)
      end if
!
      end subroutine FEM_analyze_vol_average
!
! ----------------------------------------------------------------------
!
!      subroutine FEM_finalize_vol_average
!
!      end subroutine FEM_finalize_vol_average
!
!-----------------------------------------------------------------------
!
      end module FEM_analyzer_vol_average
