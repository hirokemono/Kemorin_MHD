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
!!@endverbatim
!
      module analyzer_sph_special_snap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_SGS_control_parameter
      use t_sph_filtering_data
!
      implicit none
!
      private :: SPH_analyze_special_snap
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
      use m_t_int_parameter
      use m_t_step_parameter
      use m_mesh_data
      use m_node_phys_data
!
      use analyzer_sph_snap
      use FEM_analyzer_sph_MHD
      use SPH_analyzer_snap
      use sections_for_1st
!
      integer(kind = kint) :: visval
!
!     ---------------------
!
      call start_eleps_time(3)
!
!*  -----------  set initial step data --------------
!*
      i_step_MHD = i_step_init - 1
!*
!*  -------  time evelution loop start -----------
!*
      do
        i_step_MHD = i_step_MHD + 1
        istep_max_dt = i_step_MHD
!
        if( mod(i_step_MHD,i_step_output_rst) .ne. 0) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_special_snap'
        call SPH_analyze_special_snap(i_step_MHD)
!*
!*  -----------  output field data --------------
!*
        call start_eleps_time(1)
        call start_eleps_time(4)
!
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'SPH_to_FEM_bridge_special_snap'
        call SPH_to_FEM_bridge_special_snap
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_MHD'
        call FEM_analyze_sph_MHD                                        &
     &     (i_step_MHD, mesh1, nod_fld1, viz_step1, visval)
!
        call end_eleps_time(4)
!
!*  ----------- Visualization --------------
!*
        if(visval .eq. 0) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_surface'
          call start_eleps_time(12)
          call visualize_surface(viz_step1, mesh1, ele_mesh1, nod_fld1)
          call end_eleps_time(12)
        end if
        call end_eleps_time(1)
!
!*  -----------  exit loop --------------
!*
        if(i_step_MHD .ge. i_step_number) exit
      end do
!
!  time evolution end
!
      call end_eleps_time(3)
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize'
      call FEM_finalize
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
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
      subroutine SPH_analyze_special_snap(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use input_control_sph_MHD
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap                                    &
     &   (i_step, MHD1_org_files%rj_file_param, sph1%sph_rj,            &
     &    ipol, rj_fld1)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (ref_param_T1, ref_param_C1, ref_temp1, ref_comp1,             &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (sph1%sph_rj, r_2nd, trans_p1%leg, ipol, itor, rj_fld1)
!
!*  ----------------Modify spectr data ... ----------
!*
      call set_special_rj_fields(sph1, ipol, idpdr, itor, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear(SGS_par1%model_p,                                  &
     &    sph1, comms_sph1, omega_sph1, r_2nd, trans_p1,                &
     &    ref_temp1, ref_comp1, ipol, itor, trns_WK1, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
!
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (ref_param_T1, ref_param_C1, ref_temp1, ref_comp1,             &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 'lead_special_fields_4_sph_mhd'
      call lead_special_fields_4_sph_mhd                                &
     &   (sph1, comms_sph1, omega_sph1, r_2nd, ipol, trns_WK1, rj_fld1)
      call end_eleps_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control(sph1%sph_params, sph1%sph_rj,     &
     &    trans_p1%leg, ipol, rj_fld1, pwr1, WK_pwr)
      call end_eleps_time(11)
      call end_eleps_time(4)
!
!*  -----------  Output spectr data --------------
!*
      call output_spectr_4_snap(i_step, sph_file_param1, rj_fld1)
!
      end subroutine SPH_analyze_special_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_special_snap
!
      use m_mesh_data
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_trans_arrays_MHD
      use output_viz_file_control
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use sph_rtp_zonal_rms_data
!
!
      if(lead_field_data_flag() .ne. 0) return
!*
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp                                  &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_MHD,   &
     &    mesh1%node, iphys, nod_fld1)
      call copy_snap_vec_fld_from_trans                                 &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_snap,  &
     &    mesh1%node, iphys, nod_fld1)
      call copy_snap_vec_force_from_trans                               &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_snap,  &
     &    mesh1%node, iphys, nod_fld1)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field(sph1%sph_rtp, mesh1%node, nod_fld1)
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
      subroutine lead_special_fields_4_sph_mhd(sph, comms_sph,          &
     &          omega_sph, r_2nd, ipol, trns_WK, rj_fld)
!
      use t_spheric_parameter
      use t_poloidal_rotation
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use m_physical_property
      use m_schmidt_poly_on_rtm
      use output_viz_file_control
      use lead_fields_4_sph_mhd
!
      use cal_zonal_mean_sph_spectr
      use sph_transforms_4_MHD
      use sph_transforms_snapshot
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(inout) :: omega_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol
      type(works_4_sph_trans_MHD), intent(inout) :: trns_WK
      type(phys_data), intent(inout) :: rj_fld
!
!
      call s_lead_fields_4_sph_mhd(SGS_par1%model_p, sph,               &
     &    comms_sph, r_2nd, fl_prop1, cd_prop1, ht_prop1, cp_prop1,     &
     &    trans_p1, ipol, rj_fld, trns_WK)
!
      call sph_back_trans_4_MHD                                         &
     &   (sph, comms_sph, fl_prop1, omega_sph, trans_p1,                &
     &    ipol, rj_fld, trns_WK%trns_MHD, trns_WK%MHD_mul_FFTW)
!
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p1, trns_WK%trns_snap, ipol, rj_fld)
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
