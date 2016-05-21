!>@file   SPH_analyzer_zm_snap.f90
!!@brief  module SPH_analyzer_zm_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  main routines to evaluate zonal mean field
!!
!!@verbatim
!!      subroutine SPH_analyze_zm_snap(i_step)
!!@endverbatim
!!
!!@param i_step  time step number
!
      module SPH_analyzer_zm_snap
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_snap(i_step)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_node_id_spherical_IO
      use m_sph_trans_arrays_MHD
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      use cal_zonal_mean_sph_spectr
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step, sph1%sph_rj, rj_fld1)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(reftemp_rj, sph1%sph_rj, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(sph1%sph_rj, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear                                                    &
     &   (sph1, comms_sph1, reftemp_rj, trns_WK1%trns_MHD, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(reftemp_rj, sph1%sph_rj, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd(sph1, comms_sph1, rj_fld1, trns_WK1)
      call end_eleps_time(9)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_sph_spectr'
      call zonal_mean_all_sph_spectr(sph1%sph_rj, rj_fld1)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control                                   &
     &   (sph1%sph_params, sph1%sph_rj, rj_fld1)
      call end_eleps_time(11)
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_zm_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_zm_snap
!
      use m_mesh_data
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
!
      use m_sph_trans_arrays_MHD
!
      use output_viz_file_control
      use lead_pole_data_4_sph_mhd
      use nod_phys_send_recv
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use sph_rtp_zonal_rms_data
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
      if(iflag .ne. 0) return
!*
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp                                  &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp,                      &
     &    trns_WK1%trns_MHD%f_trns, trns_WK1%trns_MHD%ncomp_rtp_2_rj,   &
     &    mesh1%node, iphys, trns_WK1%frm_rtp, nod_fld1)
      call copy_snap_vec_fld_from_trans                                 &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_snap,  &
     &    mesh1%node, iphys, nod_fld1)
      call copy_snap_vec_fld_to_trans                                   &
     &   (sph1%sph_params%m_folding, sph1%sph_rtp, trns_WK1%trns_snap,  &
     &    mesh1%node, iphys, nod_fld1)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_mean_all_rtp_field'
      call zonal_mean_all_rtp_field(sph1%sph_rtp, mesh1%node, nod_fld1)
!
!*  ----------- transform field at pole and center --------------
!*
      call lead_pole_fields_4_sph_mhd(sph1%sph_params, sph1%sph_rtp,    &
     &    trns_WK1%trns_snap, trns_WK1%fls_pl,                          &
     &    mesh1%node, iphys, nod_fld1)
!
      call nod_fields_send_recv(mesh1%node, mesh1%nod_comm, nod_fld1)
!
      end subroutine SPH_to_FEM_bridge_zm_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_zm_snap
