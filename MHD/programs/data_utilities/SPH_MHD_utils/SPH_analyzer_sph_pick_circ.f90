!SPH_analyzer_sph_pick_circ.f90
!     module SPH_analyzer_sph_pick_circ
!
!      Written by H. Matsui
!
!>@file   SPH_analyzer_d_bench.f90
!!        module SPH_analyzer_d_bench
!!
!!@author H. Matsui
!!@date   Programmed in 2012
!!@n      modified in 2013
!
!>@brief spherical harmonics part of 
!!       Initialzation and evolution loop to pick up data on circle
!!
!!@verbatim
!!      subroutine SPH_init_sph_pick_circle(MHD_files, bc_IO, iphys)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_pick_circle(i_step, MHD_files)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!      subroutine SPH_finalize_pick_circle
!!@endverbatim
!
      module SPH_analyzer_sph_pick_circ
!
      use m_precision
      use m_SGS_control_parameter
      use m_MHD_step_parameter
      use m_physical_property
      use m_boundary_data_sph_MHD
      use m_radial_matrices_sph
      use t_phys_address
      use t_MHD_file_parameter
      use t_sph_filtering
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_pick_circle(MHD_files, bc_IO, iphys)
!
      use m_constants
      use m_array_for_send_recv
      use calypso_mpi
      use m_machine_parameter
!
      use m_mesh_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_rms_4_sph_spectr
      use m_physical_property
      use m_sph_trans_arrays_MHD
      use m_bc_data_list
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_sph_phys_address
      use const_fdm_coefs
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use sph_transforms_4_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use cal_rms_fields_by_sph
      use r_interpolate_sph_data
      use sph_mhd_rst_IO_control
      use cal_SGS_nonlinear
      use init_sph_trans_SGS_MHD
      use sph_MHD_circle_transform
      use nod_phys_send_recv
      use sph_filtering
      use check_dependency_SGS_MHD
      use input_control_sph_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(boundary_spectra), intent(in) :: bc_IO
      type(phys_address), intent(in) :: iphys
!
!
!   Allocate spectr field data
!
      call set_sph_SGS_MHD_sprctr_data(SGS_par1%model_p, sph1%sph_rj,   &
     &    MHD_prop1, ipol, idpdr, itor, rj_fld1)
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, sph1%sph_rtp%nnod_rtp)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(mesh1%nod_comm)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr(sph1%sph_params,                       &
     &    sph1%sph_rj, rj_fld1, pwr1, WK_pwr)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo                                     &
     &   (bc_IO, sph_grps1, MHD_BC1, ipol, sph1,                        &
     &    omega_sph1, ref_temp1, ref_comp1, MHD_prop1, sph_MHD_bc1,     &
     &    r_2nd, rj_fld1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_SGS_MHD'
      call init_sph_transform_SGS_MHD                                   &
     &   (SGS_par1%model_p, MHD_prop1, sph_MHD_bc1,                     &
     &    ipol, idpdr, itor, iphys, sph1, comms_sph1, omega_sph1,       &
     &    trans_p1, trns_WK1, rj_fld1)
!
! ---------------------------------
!
      if(SGS_par1%model_p%iflag_SGS .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)' init_SGS_model_sph_mhd'
        call init_SGS_model_sph_mhd(SGS_par1, sph1, sph_grps1,          &
     &      MHD_prop1, dynamic_SPH1)
      end if
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap(MHD_prop1, sph_MHD_bc1,            &
     &    sph1%sph_rj, r_2nd, trans_p1%leg, sph_MHD_mat1)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation                                &
     &   (MHD_files%org_rj_file_IO, sph1%sph_params, sph1%sph_rj)
!
!* -----  find mid-equator point -----------------
!
      call const_circle_point_global                                    &
     &   (sph1%sph_params%l_truncation, sph1%sph_rtp, sph1%sph_rj)
!
      end subroutine SPH_init_sph_pick_circle
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_pick_circle(i_step, MHD_files)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_field_on_circle
      use m_sph_trans_arrays_MHD
!
      use cal_SGS_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_SPH_SGS_MHD
      use sph_mhd_rst_IO_control
      use sph_MHD_circle_transform
      use input_control_sph_MHD
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_4_snap(i_step,                            &
     &    MHD_files%org_rj_file_IO, MHD_files%fst_file_IO, sph1%sph_rj, &
     &    ipol, rj_fld1, MHD_step1%rst_step, MHD_step1%init_d)
      call copy_time_data(MHD_step1%init_d, MHD_step1%time_d)
!
      call sync_temp_by_per_temp_sph(ref_temp1, ref_comp1, MHD_prop1,   &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(sph1%sph_rj, r_2nd,                   &
     &    MHD_prop1, sph_MHD_bc1, trans_p1%leg, ipol, itor, rj_fld1)
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
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(ref_temp1, ref_comp1, MHD_prop1,  &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      iflag = lead_field_data_flag(i_step, MHD_step1)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead_fields_4_SPH_SGS_MHD'
        call lead_fields_4_SPH_SGS_MHD(SGS_par1%model_p, sph1,          &
     &      comms_sph1, r_2nd, MHD_prop1, sph_MHD_bc1, trans_p1,        &
     &      ipol, sph_MHD_mat1, trns_WK1, dynamic_SPH1, rj_fld1)
      end if
      call end_elapsed_time(9)
!
!*  -----------  lead mid-equator field --------------
!*
      call start_elapsed_time(4)
      if(iflag_debug.gt.0)  write(*,*) 'sph_transfer_on_circle'
      call sph_transfer_on_circle(sph1%sph_rj, rj_fld1)
      call write_field_data_on_circle(i_step, MHD_step1%time_d%time)
      call end_elapsed_time(4)
!
      end subroutine SPH_analyze_pick_circle
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_pick_circle
!
!      end subroutine SPH_finalize_pick_circle
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_sph_pick_circ
