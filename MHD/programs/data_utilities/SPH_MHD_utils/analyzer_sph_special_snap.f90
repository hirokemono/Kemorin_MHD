!>@file   analyzer_sph_special_snap.f90
!!@brief  module analyzer_sph_special_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!        including some special treatment
!!
!!@verbatim
!!      subroutine evolution_sph_special_snap
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!@endverbatim
!
      module analyzer_sph_special_snap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_MHD_step_parameter
      use m_SPH_MHD_model_data
      use m_SPH_SGS_structure
      use t_mesh_data
      use t_step_parameter
      use t_MHD_file_parameter
      use t_work_SPH_MHD
      use t_viz_sections
!
      implicit none
!
      private :: SPH_analyze_special_snap
      private :: SPH_to_FEM_bridge_special_snap
      private :: lead_special_fields_4_sph_mhd
      private :: set_special_rj_fields
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_special_snap
!
      use t_MHD_step_parameter
!
      use analyzer_sph_snap
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
      use output_viz_file_control
      use SGS_MHD_zonal_mean_viz
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call s_initialize_time_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
!
        iflag = output_IO_flag(MHD_step1%time_d%i_time_step,            &
     &                         MHD_step1%rst_step)
        if(iflag .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_special_snap'
        call SPH_analyze_special_snap(MHD_step1%time_d%i_time_step,     &
     &      MHD_files1, SPH_model1, MHD_step1, SPH_SGS1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        iflag = lead_field_data_flag(MHD_step1%time_d%i_time_step,      &
     &                               MHD_step1)
        if(iflag .eq. 0) then
          if(iflag_debug.eq.1)                                          &
     &       write(*,*) 'SPH_to_FEM_bridge_special_snap'
          call SPH_to_FEM_bridge_special_snap                           &
     &       (SPH_SGS1%sph, FEM_d1%geofem%mesh, SPH_WK1%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      FEM_d1%geofem, FEM_d1%field, MHD_step1, visval, MHD_IO1)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        FEM_d1%geofem, FEM_d1%field, next_tbl_VIZ1%neib_ele,      &
     &        jacobians_VIZ1, vizs1)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
!*
!*  ----------- Zonal means --------------
!*
          call SGS_MHD_zmean_sections                                   &
     &       (MHD_step1%viz_step, MHD_step1%time_d, SPH_SGS1%SGS_par,   &
     &        SPH_SGS1%sph, FEM_d1%geofem, SPH_WK1%trns_WK,             &
     &        FEM_d1%field, zmeans1)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, MHD_IO1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_special_snap(i_step, MHD_files,            &
     &          SPH_model, MHD_step, SPH_SGS, SPH_WK)
!
      use m_work_time
      use t_MHD_step_parameter
!
      use cal_SGS_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_SGS_MHD_rst_IO_control
      use input_control_sph_MHD
      use sph_SGS_mhd_monitor_data_IO
      use self_buoyancy_w_filter_sph
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files,                  &
     &    MHD_step%rst_step, MHD_step%init_d, SPH_SGS)

      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_SGS%sph%sph_rj, SPH_SGS%ipol, SPH_SGS%fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_SGS%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(SPH_SGS%sph%sph_rj, SPH_WK%r_2nd,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, &
     &    SPH_SGS%ipol, SPH_SGS%fld)
      if(iflag_debug .gt. 0) write(*,*) 'rot_self_filter_buoyancy_sph'
      call rot_self_filter_buoyancy_sph                                 &
     &   (SPH_SGS%sph%sph_rj, SPH_SGS%ipol, SPH_model%MHD_prop,         &
     &    SPH_model%sph_MHD_bc%sph_bc_U, SPH_SGS%fld)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_special_rj_fields                                        &
     &   (SPH_SGS%sph, SPH_SGS%ipol, SPH_SGS%fld)
!
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+4)
      call nonlinear_with_SGS(i_step, SPH_WK%r_2nd,SPH_model,           &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+4)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    SPH_SGS%sph%sph_rj, SPH_SGS%ipol, SPH_SGS%fld)
!*
      if(iflag_debug.gt.0) write(*,*) 'lead_special_fields_4_sph_mhd'
      call lead_special_fields_4_sph_mhd                                &
     &   (i_step, SPH_model%omega_sph, SPH_WK%monitor,                  &
     &    SPH_WK%r_2nd, SPH_model%MHD_prop, SPH_model%sph_MHD_bc,       &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_WK%MHD_mats,              &
     &    MHD_step, SPH_SGS)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  lead energy data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug .gt. 0)                                          &
     &                write(*,*) 'output_rms_sph_SGS_mhd_control'
        call output_rms_sph_SGS_mhd_control(MHD_step1%time_d, SPH_SGS,  &
     &      SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, SPH_WK%monitor)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  -----------  Output spectr data --------------
!*
      call output_spectr_4_snap(i_step, MHD_step1%time_d,               &
     &    MHD_files%sph_file_IO, SPH_SGS%fld, MHD_step%ucd_step)
!
      end subroutine SPH_analyze_special_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_special_snap(sph, mesh, WK)
!
      use FEM_analyzer_sph_MHD
      use sph_rtp_zonal_rms_data
!*
      type(sph_grids), intent(in) :: sph
      type(mesh_geometry), intent(in) :: mesh
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
!*  -----------  data transfer to FEM array --------------
!*
      call SPH_to_FEM_bridge_MHD(sph, WK, mesh, FEM_d1%field)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field                                     &
     &   (sph%sph_rtp, mesh%node, FEM_d1%field)
!
      end subroutine SPH_to_FEM_bridge_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine lead_special_fields_4_sph_mhd(i_step, omega_sph,       &
     &          monitor, r_2nd, MHD_prop, sph_MHD_bc,                   &
     &          trans_p, trns_WK, sph_MHD_mat, MHD_step, SPH_SGS)
!
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_poloidal_rotation
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_sph_transforms
      use t_radial_matrices_sph_MHD
      use output_viz_file_control
      use lead_fields_SPH_SGS_MHD
!
      use cal_zonal_mean_sph_spectr
      use sph_transforms_4_MHD
      use sph_transforms_snapshot
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_rotation), intent(in) :: omega_sph
      type(sph_mhd_monitor_data), intent(in) :: monitor
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: trns_WK
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!
      integer(kind = kint) :: iflag
!
!
      if(lead_field_data_flag(i_step, MHD_step) .eq. 0) then
        call lead_fields_4_SPH_SGS_MHD                                  &
     &     (monitor, r_2nd, MHD_prop, sph_MHD_bc, trans_p, sph_MHD_mat, &
     &      trns_WK, SPH_SGS%dynamic, SPH_SGS)
      end if
!
      call sph_back_trans_4_MHD(SPH_SGS%sph, SPH_SGS%comms,             &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, omega_sph, trans_p,    &
     &    trns_WK%gt_cor, SPH_SGS%fld, trns_WK%trns_MHD%b_trns,         &
     &    trns_WK%trns_MHD%backward, trns_WK%WK_sph,                    &
     &    trns_WK%trns_MHD%mul_FFTW, trns_WK%cor_rlm)
!
      call sph_forward_trans_snapshot_MHD                               &
     &   (SPH_SGS%sph, SPH_SGS%comms, trans_p,                          &
     &    trns_WK%trns_snap%forward,  trns_WK%WK_sph, SPH_SGS%fld)
!
! ----  Take zonal mean
!
      if (my_rank.eq.0) write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr(SPH_SGS%sph%sph_rj, SPH_SGS%fld)
!
      end subroutine lead_special_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_special_rj_fields(sph, ipol, rj_fld)
!
      use t_spheric_parameter
      use t_phys_address
      use t_phys_data
!
      use cal_zonal_mean_sph_spectr
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(inout) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer (kind =kint), allocatable :: ipick_degree(:)
      integer(kind = kint) :: ltr_half
      integer(kind = kint) :: l
!
!
      ltr_half = 1*(sph%sph_params%l_truncation + 1) / 2
      allocate(ipick_degree(ltr_half))
      do l = 1, ltr_half
        ipick_degree(l) = l-1
      end do
!
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,              &
!     &    ithree, ipol%base%i_velo, sph%sph_rj, rj_fld)
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,              &
!     &    ithree, ipol%base%i_magne)
!      deallocate(ipick_degree, sph%sph_rj, rj_fld)

      if (my_rank.eq.0) write(*,*) 'delete zonam mean velocity'
      call take_zonal_mean_rj_field                                     &
     &   (ithree, ipol%base%i_velo, sph%sph_rj, rj_fld)
      call take_zonal_mean_rj_field                                     &
     &   (ithree, ipol%base%i_vort, sph%sph_rj, rj_fld)
      if (my_rank.eq.0) write(*,*) 'delete zonam mean toroidal'
      call delete_zonal_mean_rj_field                                   &
     &   (ione, ipol%base%i_velo, sph%sph_rj, rj_fld)
      call delete_zonal_mean_rj_field                                   &
     &   (ione, (ipol%base%i_velo+1), sph%sph_rj, rj_fld)
      call delete_zonal_mean_rj_field                                   &
     &   (ione, (ipol%base%i_vort+2), sph%sph_rj, rj_fld)
!
      end subroutine set_special_rj_fields
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_special_snap
