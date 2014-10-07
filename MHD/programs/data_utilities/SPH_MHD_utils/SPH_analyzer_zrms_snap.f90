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
      use m_t_step_parameter
      use m_node_id_spherical_IO
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use set_reference_sph_mhd
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
!
      use sph_rtp_zonal_rms_data
!
      integer(kind = kint), intent(in) :: i_step
!
!
      call read_alloc_sph_rst_4_snap(i_step)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start
!
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear
      call end_eleps_time(8)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd
      call end_eleps_time(9)
!
! ----  Take zonal mean
!
      if (iflag_debug.eq.1) write(*,*) 'zonal_cyl_rms_all_rtp_field'
!      call zonal_rms_all_rtp_field
      call zonal_cyl_rms_all_rtp_field
!
      end subroutine SPH_analyze_zRMS_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_zrms_snap
