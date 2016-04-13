!
!     module SPH_analyzer_zrms_snap
!
!      Written by H. Matsui
!
!>@file   SPH_analyzer_zrms_snap.f90
!!@brief  module SPH_analyzer_zrms_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  main routines to evaluate zonal root mean square field
!!
!!@verbatim
!!      subroutine SPH_analyze_zRMS_snap(i_step)
!!      subroutine SPH_to_FEM_bridge_zRMS_snap
!!@endverbatim
!!
!!@param i_step  time step number
!
      module SPH_analyzer_zrms_snap
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
      subroutine SPH_analyze_zRMS_snap(i_step)
!
      use m_work_time
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_node_id_spherical_IO
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step, rj_fld1)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(reftemp_rj, rj_fld1)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear(reftemp_rj, rj_fld1)
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(reftemp_rj, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd(rj_fld1)
      call end_eleps_time(9)
!
      end subroutine SPH_analyze_zRMS_snap
!
!-----------------------------------------------------------------------
!
      subroutine SPH_to_FEM_bridge_zRMS_snap
!
      use m_mesh_data
      use m_node_phys_data
      use m_sph_spectr_data
      use output_viz_file_control
      use lead_pole_data_4_sph_mhd
      use nod_phys_send_recv
      use copy_snap_4_sph_trans
      use copy_MHD_4_sph_trans
      use sph_rtp_zonal_rms_data
      use m_sph_spectr_data
!
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
      if(iflag .ne. 0) return
!*
!*  -----------  data transfer to FEM array --------------
!*
      call copy_forces_to_snapshot_rtp(mesh1%node, iphys, nod_fld1)
      call copy_snap_vec_fld_from_trans(mesh1%node, iphys, nod_fld1)
      call copy_snap_vec_fld_to_trans(mesh1%node, iphys, nod_fld1)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_cyl_rms_all_rtp_field'
!      call zonal_rms_all_rtp_field(mesh1%node, nod_fld1)
      call zonal_cyl_rms_all_rtp_field(mesh1%node, nod_fld1)
!
!*  ----------- transform field at pole and center --------------
!*
      call lead_pole_fields_4_sph_mhd(mesh1%node, iphys, nod_fld1)
!
      call nod_fields_send_recv(mesh1%node, mesh1%nod_comm, nod_fld1)
!
      end subroutine SPH_to_FEM_bridge_zRMS_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_zrms_snap
