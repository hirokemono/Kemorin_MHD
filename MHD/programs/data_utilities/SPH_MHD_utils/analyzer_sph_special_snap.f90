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
      use m_MHD_step_parameter
      use m_SPH_MHD_model_data
      use m_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_mesh_data
      use t_SPH_SGS_structure
      use t_step_parameter
      use t_MHD_file_parameter
      use t_work_SPH_MHD
!
      implicit none
!
      private :: SPH_analyze_special_snap
      private :: SPH_to_FEM_bridge_special_snap
      private :: lead_special_fields_4_sph_mhd
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
      use m_SPH_mesh_field_data
      use m_mesh_data
      use m_node_phys_data
!
      use analyzer_sph_snap
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
      use sections_for_1st
      use output_viz_file_control
!
      integer(kind = kint) :: visval
      integer(kind = kint) :: iflag
!
!*  -----------  set initial step data --------------
!*
      call start_elapsed_time(3)
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
     &      MHD_files1, SPH_model1, MHD_step1, SPH_SGS1, SPH_MHD1,      &
     &      SPH_WK1)
!*
!*  -----------  output field data --------------
!*
        call start_elapsed_time(1)
        call start_elapsed_time(4)
        iflag = lead_field_data_flag(MHD_step1%time_d%i_time_step,      &
     &                               MHD_step1)
        if(iflag .eq. 0) then
          if(iflag_debug.eq.1)                                          &
     &       write(*,*) 'SPH_to_FEM_bridge_special_snap'
          call SPH_to_FEM_bridge_special_snap                           &
     &       (SPH_MHD1%sph, femmesh1%mesh, SPH_WK1%trns_WK)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1,                            &
     &      femmesh1%mesh, nod_fld1, MHD_step1, visval, fem_ucd1)
!
        call end_elapsed_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          call start_elapsed_time(12)
          call visualize_surface(MHD_step1%viz_step, MHD_step1%time_d,  &
     &        femmesh1, ele_mesh1, nod_fld1)
          call end_elapsed_time(12)
        end if
        call end_elapsed_time(1)
!
!*  -----------  exit loop --------------
!*
        if(MHD_step1%time_d%i_time_step                                 &
     &        .ge. MHD_step1%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      call end_elapsed_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize(MHD_files1, MHD_step1, range1, fem_ucd1)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_elaps(num_elapsed)
      call end_elapsed_time(1)
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
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files,                  &
     &    SPH_MHD, MHD_step%rst_step, MHD_step%init_d,                  &
     &    SPH_SGS%SGS_par%i_step_sgs_coefs, SPH_SGS%SGS_par%model_p,    &
     &    SPH_SGS%dynamic)

      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,     &
     &    SPH_model%MHD_prop, sph_MHD_bc1, SPH_WK%trans_p%leg,          &
     &    SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_special_rj_fields                                        &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor,       &
     &    SPH_MHD%fld)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_elapsed_time(8)
      call nonlinear_with_SGS(i_step, SPH_SGS%SGS_par, SPH_WK%r_2nd,   &
     &    SPH_model, sph_MHD_bc1, SPH_WK%trans_p, SPH_WK%trns_WK,      &
     &    SPH_SGS%dynamic, SPH_MHD)
      call end_elapsed_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_elapsed_time(9)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%fld)
!*
      if(iflag_debug.gt.0) write(*,*) 'lead_special_fields_4_sph_mhd'
      call lead_special_fields_4_sph_mhd(i_step,                        &
     &    SPH_model%omega_sph, SPH_WK%r_2nd, SPH_model%MHD_prop,        &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS%dynamic,              &
     &    SPH_WK%MHD_mats, MHD_step, SPH_MHD)
      call end_elapsed_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_elapsed_time(4)
      call start_elapsed_time(11)
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control(MHD_step1%time_d, SPH_MHD%sph,  &
     &      sph_MHD_bc1%sph_bc_U, SPH_WK%trans_p%leg,                   &
     &      SPH_MHD%ipol, SPH_MHD%fld, SPH_WK%monitor)
      end if
      call end_elapsed_time(11)
      call end_elapsed_time(4)
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
      subroutine SPH_to_FEM_bridge_special_snap(sph, mesh, WK)
!
      use m_node_phys_data
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use sph_rtp_zonal_rms_data
!*
      type(sph_grids), intent(in) :: sph
      type(mesh_geometry), intent(in) :: mesh
      type(works_4_sph_trans_MHD), intent(in) :: WK
!
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp                                  &
     &   (sph%sph_params, sph%sph_rtp, WK%trns_MHD,                     &
     &    mesh%node, iphys_nod1, nod_fld1)
      call copy_snap_vec_fld_from_trans                                 &
     &   (sph%sph_params%m_folding, sph%sph_rtp, WK%trns_snap,          &
     &    mesh%node, iphys_nod1, nod_fld1)
      call copy_snap_vec_force_from_trans                               &
     &   (sph%sph_params%m_folding, sph%sph_rtp, WK%trns_snap,          &
     &    mesh%node, iphys_nod1, nod_fld1)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field(sph%sph_rtp, mesh%node, nod_fld1)
!
      end subroutine SPH_to_FEM_bridge_special_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine lead_special_fields_4_sph_mhd                          &
     &         (i_step, omega_sph, r_2nd, MHD_prop, trans_p, trns_WK,   &
     &          dynamic_SPH, sph_MHD_mat, MHD_step, SPH_MHD)
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
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: trns_WK
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      integer(kind = kint) :: iflag
!
!
      iflag = lead_field_data_flag(i_step, MHD_step)
      if(iflag .eq. 0) then
        call lead_fields_4_SPH_SGS_MHD(SPH_SGS1%SGS_par,                &
     &      r_2nd, MHD_prop, sph_MHD_bc1, trans_p, sph_MHD_mat,         &
     &      trns_WK, dynamic_SPH, SPH_MHD)
      end if
!
      call sph_back_trans_4_MHD(SPH_MHD%sph, SPH_MHD%comms,             &
     &    MHD_prop%fl_prop, sph_MHD_bc1%sph_bc_U, omega_sph, trans_p,   &
     &    trns_WK%gt_cor, SPH_MHD%ipol, SPH_MHD%fld, trns_WK%trns_MHD,  &
     &    trns_WK%WK_sph, trns_WK%MHD_mul_FFTW, trns_WK%cor_rlm)
!
      call sph_forward_trans_snapshot_MHD                               &
     &   (SPH_MHD%sph, SPH_MHD%comms, trans_p, trns_WK%trns_snap,       &
     &     SPH_MHD%ipol, trns_WK%WK_sph, SPH_MHD%fld)
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
      subroutine set_special_rj_fields(sph, ipol, idpdr, itor, rj_fld)
!
      use t_spheric_parameter
      use t_phys_address
      use t_phys_data
!
      use cal_zonal_mean_sph_spectr
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(inout) :: ipol, idpdr, itor
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
!     &    ithree, ipol%i_velo, sph%sph_rj, rj_fld)
!      call pick_degree_sph_spectr(ltr_half, ipick_degree,              &
!     &    ithree, ipol%i_magne)
!      deallocate(ipick_degree, sph%sph_rj, rj_fld)

      if (my_rank.eq.0) write(*,*) 'delete zonam mean velocity'
      call take_zonal_mean_rj_field                                     &
     &   (ithree, ipol%i_velo, sph%sph_rj, rj_fld)
      call take_zonal_mean_rj_field                                     &
     &   (ithree, ipol%i_vort, sph%sph_rj, rj_fld)
      if (my_rank.eq.0) write(*,*) 'delete zonam mean toroidal'
      call delete_zonal_mean_rj_field                                   &
     &   (ione, ipol%i_velo, sph%sph_rj, rj_fld)
      call delete_zonal_mean_rj_field                                   &
     &   (ione, idpdr%i_velo, sph%sph_rj, rj_fld)
      call delete_zonal_mean_rj_field                                   &
     &   (ione, itor%i_vort, sph%sph_rj, rj_fld)
!
      end subroutine set_special_rj_fields
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_special_snap
