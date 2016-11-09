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
!!      subroutine SPH_init_sph_pick_circle(iphys)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_pick_circle(i_step)
!!      subroutine SPH_finalize_pick_circle
!!@endverbatim
!
      module SPH_analyzer_sph_pick_circ
!
      use m_precision
      use t_phys_address
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_pick_circle(iphys)
!
      use m_constants
      use m_array_for_send_recv
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
!
      use m_mesh_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_rms_4_sph_spectr
      use m_node_id_spherical_IO
      use m_physical_property
      use m_sph_trans_arrays_MHD
      use m_boundary_params_sph_MHD
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
      use init_sphrical_transform_MHD
      use sph_MHD_circle_transform
      use nod_phys_send_recv
      use sph_filtering
!
      type(phys_address), intent(in) :: iphys
!
!
!   Allocate spectr field data
!
      call set_sph_sprctr_data_address                                  &
     &   (sph1%sph_rj, ipol, idpdr, itor, rj_fld1)
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, sph1%sph_rtp%nnod_rtp)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(mesh1%nod_comm)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_rms_4_sph_spectr'
      call init_rms_4_sph_spectr(sph1%sph_params%l_truncation,          &
     &    sph1%sph_rj, rj_fld1, pwr1, WK_pwr)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(sph_grps1, ipol, sph1,              &
     &    omega_sph1, ref_temp1, r_2nd, rj_fld1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(ipol, idpdr, itor, iphys,             &
     &    sph1, comms_sph1, omega_sph1, trans_p1, trns_WK1, rj_fld1)
!
! ---------------------------------
!
      if(iflag_SGS_model .gt. 0) then
      if(iflag_debug.gt.0) write(*,*)' init_SGS_model_sph_mhd'
        call init_SGS_model_sph_mhd                                     &
     &     (sph1, sph_grps1, trns_WK1%dynamic_SPH)
      end if
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap(sph1%sph_rj, r_2nd, trans_p1%leg)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation(sph1%sph_params, sph1%sph_rj)
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
      subroutine SPH_analyze_pick_circle(i_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_node_id_spherical_IO
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_schmidt_poly_on_rtm
      use m_field_on_circle
      use m_sph_trans_arrays_MHD
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_MHD_circle_transform
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap                                    &
     &   (i_step, sph1%sph_rj, ipol, rj_fld1)
!
      call sync_temp_by_per_temp_sph                                    &
     &   (ref_temp1%t_rj, sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (sph1%sph_rj, r_2nd, trans_p1%leg, ipol, itor, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear(sph1, comms_sph1, omega_sph1, r_2nd, trans_p1,     &
     &    ref_temp1%t_rj, ipol, itor, trns_WK1, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (ref_temp1%t_rj, sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd                                      &
     &   (sph1, comms_sph1, r_2nd, trans_p1, ipol, rj_fld1, trns_WK1)
      call end_eleps_time(9)
!
!*  -----------  lead mid-equator field --------------
!*
      call start_eleps_time(4)
      if(iflag_debug.gt.0)  write(*,*) 'sph_transfer_on_circle'
      call sph_transfer_on_circle(sph1%sph_rj, rj_fld1)
      call write_field_data_on_circle(i_step, time)
      call end_eleps_time(4)
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
