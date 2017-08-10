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
      use m_SGS_control_parameter
      use m_physical_property
      use t_mesh_data
      use t_sph_filtering_data
      use t_step_parameter
      use t_MHD_file_parameter
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
        call SPH_analyze_special_snap                                   &
     &     (MHD_step1%time_d%i_time_step, MHD_files1, MHD_step1)
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
          call SPH_to_FEM_bridge_special_snap(femmesh1%mesh)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD(MHD_files1%ucd_file_IO,                &
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
      call FEM_finalize                                                 &
     &   (MHD_files1%ucd_file_IO, MHD_step1, range1, fem_ucd1)
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
      subroutine SPH_analyze_special_snap(i_step, MHD_files, MHD_step)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
      use m_boundary_data_sph_MHD
      use m_radial_matrices_sph
      use t_MHD_step_parameter
!
      use cal_SGS_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_SGS_MHD_rst_IO_control
      use sph_mhd_rms_IO
      use input_control_sph_MHD
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files, sph1%sph_rj,     &
     &    ipol, rj_fld1, MHD_step%rst_step, MHD_step1%init_d,           &
     &    SGS_par1%i_step_sgs_coefs, SGS_par1%model_p, dynamic_SPH1)

      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(ref_temp1, ref_comp1, MHD_prop1,   &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(sph1%sph_rj, r_2nd,                   &
     &    MHD_prop1, sph_MHD_bc1, trans_p1%leg, ipol, itor, rj_fld1)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_special_rj_fields(sph1, ipol, idpdr, itor, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_elapsed_time(8)
      call nonlinear_w_SGS(i_step, SGS_par1, sph1, comms_sph1,          &
     &    omega_sph1, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,          &
     &    ref_temp1, ref_comp1, ipol, itor,                             &
     &    trns_WK1, dynamic_SPH1, rj_fld1)
      call end_elapsed_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_elapsed_time(9)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(ref_temp1, ref_comp1, MHD_prop1,  &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 'lead_special_fields_4_sph_mhd'
      call lead_special_fields_4_sph_mhd(i_step, sph1, comms_sph1,      &
     &    omega_sph1, r_2nd, ipol, trns_WK1, dynamic_SPH1,              &
     &    sph_MHD_mat1, rj_fld1, MHD_step)
      call end_elapsed_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_elapsed_time(4)
      call start_elapsed_time(11)
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control                                 &
     &     (MHD_step1%time_d, sph1%sph_params, sph1%sph_rj,             &
     &      sph_MHD_bc1%sph_bc_U, trans_p1%leg, ipol, rj_fld1,          &
     &      pwr1, WK_pwr)
      end if
      call end_elapsed_time(11)
      call end_elapsed_time(4)
!
!*  -----------  Output spectr data --------------
!*
      call output_spectr_4_snap(i_step, MHD_step1%time_d,               &
     &    MHD_files%sph_file_IO, rj_fld1, MHD_step%ucd_step)
!
      end subroutine SPH_analyze_special_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_special_snap(mesh)
!
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_trans_arrays_MHD
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use sph_rtp_zonal_rms_data
!*
      type(mesh_geometry), intent(in) :: mesh
!
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp                                  &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_MHD,   &
     &    mesh%node, iphys, nod_fld1)
      call copy_snap_vec_fld_from_trans                                 &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_snap,  &
     &    mesh%node, iphys, nod_fld1)
      call copy_snap_vec_force_from_trans                               &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_snap,  &
     &    mesh%node, iphys, nod_fld1)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field(sph1%sph_rtp, mesh%node, nod_fld1)
!
      end subroutine SPH_to_FEM_bridge_special_snap
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
      subroutine lead_special_fields_4_sph_mhd(i_step, sph, comms_sph,  &
     &          omega_sph, r_2nd, ipol, trns_WK, dynamic_SPH,           &
     &          sph_MHD_mat, rj_fld, MHD_step)
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
      use m_physical_property
      use m_boundary_data_sph_MHD
      use m_schmidt_poly_on_rtm
      use output_viz_file_control
      use lead_fields_SPH_SGS_MHD
!
      use cal_zonal_mean_sph_spectr
      use sph_transforms_4_MHD
      use sph_transforms_snapshot
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(inout) :: omega_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol
      type(works_4_sph_trans_MHD), intent(inout) :: trns_WK
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: iflag
!
!
      iflag = lead_field_data_flag(i_step, MHD_step)
      if(iflag .eq. 0) then
        call lead_fields_4_SPH_SGS_MHD(SGS_par1%model_p, sph,           &
     &      comms_sph, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,         &
     &      ipol, sph_MHD_mat, trns_WK, dynamic_SPH, rj_fld)
      end if
!
      call sph_back_trans_4_MHD(sph, comms_sph,                         &
     &    MHD_prop1%fl_prop, sph_MHD_bc1%sph_bc_U, omega_sph, trans_p1, &
     &    trns_WK%gt_cor, ipol, rj_fld, trns_WK%trns_MHD,               &
     &    trns_WK%WK_sph, trns_WK%MHD_mul_FFTW, trns_WK%cor_rlm)
!
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p1, trns_WK%trns_snap, ipol,            &
     &    trns_WK%WK_sph, rj_fld)
!
! ----  Take zonal mean
!
      if (my_rank.eq.0) write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr(sph%sph_rj, rj_fld)
!
      end subroutine lead_special_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_special_snap
