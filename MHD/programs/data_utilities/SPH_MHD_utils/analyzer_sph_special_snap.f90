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
      use SPH_analyzer_SGS_snap
      use output_viz_file_control
      use SGS_MHD_zonal_mean_viz
      use set_time_step_params
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(MHD_step1%init_d, MHD_step1%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(MHD_step1%time_d)
        if(output_IO_flag(MHD_step1%time_d%i_time_step,                 &
     &                    MHD_step1%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_special_snap'
        call SPH_analyze_special_snap                                   &
     &     (MHD_step1%time_d%i_time_step,  MHD_files1, SPH_model1,      &
     &     MHD_step1, SPH_SGS1, SPH_MHD1, SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
        if(lead_field_data_flag(MHD_step1%time_d%i_time_step,           &
     &                          MHD_step1)) then
          if(iflag_debug.eq.1)                                          &
     &       write(*,*) 'SPH_to_FEM_bridge_special_snap'
          call SPH_to_FEM_bridge_special_snap                           &
     &       (SPH_MHD1%sph, FEM_d1%geofem, SPH_WK1%trns_WK,             &
     &        FEM_d1%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      FEM_d1%geofem, FEM_d1%field, MHD_step1,                     &
     &      MHD_IO1, FEM_d1%v_sol)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(MHD_step1%time_d%i_time_step,          &
     &                           MHD_step1%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(MHD_step1%time_d%i_time_step,         &
     &                          MHD_step1%viz_step)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        FEM_d1%geofem, FEM_d1%field, ele_4_nod_VIZ1,              &
     &        jacobians_VIZ1, vizs1)
!*
!*  ----------- Zonal means --------------
!*
          if(MHD_step1%viz_step%istep_psf .ge. 0) then
            call SGS_MHD_zmean_sections(MHD_step1%viz_step%istep_psf,   &
     &          MHD_step1%time_d, SPH_MHD1%sph, FEM_d1%geofem,          &
     &          SPH_WK1%trns_WK, SPH_SGS1, FEM_d1%field,                &
     &          zmeans1, FEM_d1%v_sol)
          end if
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
      call dealloc_FEM_mesh_field_items(FEM_d1)
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
     &          SPH_model, MHD_step, SPH_SGS, SPH_MHD, SPH_WK)
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
      use lead_fields_SPH_SGS_MHD
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files,                  &
     &    MHD_step%rst_step, MHD_step%init_d, SPH_MHD, SPH_SGS)

      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_MHD%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, &
     &    SPH_MHD%ipol, SPH_MHD%fld)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_special_rj_fields                                        &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+4)
      call nonlinear_with_SGS(i_step, SPH_WK%r_2nd,SPH_model,           &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS, SPH_MHD)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+4)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!*
      if(lead_field_data_flag(i_step, MHD_step)) then
        call lead_fields_4_SPH_SGS_MHD                                  &
     &     (SPH_SGS%SGS_par, SPH_WK%monitor, SPH_WK%r_2nd,              &
     &      SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p,   &
     &      SPH_SGS%ipol_LES, SPH_WK%MHD_mats, SPH_WK%trns_WK,          &
     &      SPH_SGS%trns_WK_LES, SPH_SGS%dynamic, SPH_MHD)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'lead_special_fields_4_sph_mhd'
      call lead_special_fields_4_sph_mhd                                &
     &   (SPH_model%omega_sph, SPH_model%MHD_prop,                      &
     &    SPH_model%sph_MHD_bc, SPH_WK%trans_p, SPH_WK%trns_WK,         &
     &    SPH_SGS, SPH_MHD)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  lead energy data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      if(output_IO_flag(i_step, MHD_step%rms_step)) then
        if(iflag_debug .gt. 0)                                          &
     &                write(*,*) 'output_rms_sph_SGS_mhd_control'
        call output_rms_sph_SGS_mhd_control                             &
     &     (MHD_step1%time_d, SPH_SGS, SPH_MHD,                         &
     &      SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, SPH_WK%monitor)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  -----------  Output spectr data --------------
!*
      call output_spectr_4_snap(i_step, MHD_step1%time_d,               &
     &    MHD_files%sph_file_IO, SPH_MHD%fld, MHD_step%ucd_step)
!
      end subroutine SPH_analyze_special_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_special_snap                         &
     &         (sph, geofem, WK, nod_fld)
!
      use FEM_analyzer_sph_MHD
      use sph_rtp_zonal_rms_data
!*
      type(sph_grids), intent(in) :: sph
      type(mesh_data), intent(in) :: geofem
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
      type(phys_data), intent(inout) :: nod_fld
!
!*  -----------  data transfer to FEM array --------------
!*
      call SPH_to_FEM_bridge_MHD(sph, WK, geofem, nod_fld)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field                                     &
     &   (sph%sph_rtp, geofem%mesh%node, nod_fld)
!
      end subroutine SPH_to_FEM_bridge_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine lead_special_fields_4_sph_mhd(omega_sph, MHD_prop,     &
     &          sph_MHD_bc, trans_p, trns_WK, SPH_SGS, SPH_MHD)
!
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_poloidal_rotation
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_radial_matrices_sph_MHD
!
      use cal_zonal_mean_sph_spectr
      use sph_transforms_4_MHD
      use sph_transforms_snapshot
      use output_viz_file_control
!
      type(sph_rotation), intent(in) :: omega_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: trns_WK
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      call sph_back_trans_4_MHD(SPH_MHD%sph, SPH_MHD%comms,             &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, omega_sph, trans_p,    &
     &    trns_WK%gt_cor, SPH_MHD%fld, trns_WK%trns_MHD%b_trns,         &
     &    trns_WK%trns_MHD%backward, trns_WK%WK_leg,                    &
     &    trns_WK%WK_FFTs_MHD, trns_WK%cor_rlm)
!
      call sph_forward_trans_snapshot_MHD                               &
     &   (SPH_MHD%sph, SPH_MHD%comms, trans_p,                          &
     &    trns_WK%trns_eflux%forward, trns_WK%WK_leg, trns_WK%WK_FFTs,  &
     &    SPH_MHD%fld)
      call sph_forward_trans_snapshot_MHD                               &
     &   (SPH_MHD%sph, SPH_MHD%comms, trans_p,                          &
     &    SPH_SGS%trns_WK_LES%trns_SGS_snap%forward,                    &
     &    trns_WK%WK_leg, trns_WK%WK_FFTs, SPH_MHD%fld)
!
! ----  Take zonal mean
!
      if (my_rank.eq.0) write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr(SPH_MHD%sph%sph_rj, SPH_MHD%fld)
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
