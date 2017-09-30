!>@file   SPH_analyzer_correle_all.f90
!!@brief  module SPH_analyzer_correle_all
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_analyze_correlate_all                            &
!!     &         (time_d, MHD_files, MHD_step, SPH_MHD, SPH_WK)
!!        type(time_data), intent(in) :: time_d
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!@endverbatim
!
      module SPH_analyzer_correle_all
!
      use m_precision
      use calypso_mpi
      use t_time_data
      use t_phys_address
      use t_file_IO_parameter
      use t_phys_data
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_mesh_field_data
      use t_work_SPH_MHD
!
      use SPH_analyzer_back_trans
!
      implicit none
!
      type(field_IO_params), save :: sph_file_param2
      type(phys_data), save :: ref_rj_fld
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_correlate_all                              &
     &         (time_d, MHD_files, MHD_step, SPH_MHD, SPH_WK)
!
      use m_work_time
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use input_control_sph_MHD
!
      use cal_correlations_by_spectr
!
      type(time_data), intent(in) :: time_d
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!       read first data
!
      call read_alloc_sph_spectr(time_d%i_time_step,                    &
     &    MHD_files%org_rj_file_IO, MHD_files%sph_file_IO,              &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld,                &
     &    MHD_step%ucd_step, MHD_step%init_d)
!
      call copy_field_name_type(SPH_MHD%fld, ref_rj_fld)
      call copy_field_data_type(SPH_MHD%fld, ref_rj_fld)
!
!       read second data
!
      call read_alloc_sph_spectr                                        &
     &   (time_d%i_time_step, MHD_files%org_rj_file_IO,                 &
     &    sph_file_param2, SPH_MHD%sph%sph_rj, SPH_MHD%ipol,            &
     &    SPH_MHD%fld, MHD_step%ucd_step, MHD_step%init_d)
      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
!
      call back_trans_4_correlate(SPH_MHD%sph, SPH_MHD%comms,           &
     &    ref_rj_fld, SPH_MHD%fld, SPH_WK%trans_p,                      &
     &    SPH_WK%trns_WK%trns_MHD, SPH_WK%trns_WK%WK_sph)
!
      call cal_sph_correlations                                         &
     &   (SPH_MHD%sph, SPH_MHD%ipol, ref_rj_fld, SPH_MHD%fld,           &
     &    SPH_WK%trans_p, SPH_WK%monitor%pwr, SPH_WK%monitor%WK_pwr)
!
      call dealloc_phys_data_type(ref_rj_fld)
      call dealloc_phys_name_type(ref_rj_fld)
!
      call write_sph_vol_ms_file                                        &
     &   (my_rank, time_d, SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,  &
     &    SPH_WK%monitor%pwr)
      call write_sph_layer_ms_file(my_rank, time_d,                     &
     &   SPH_MHD%sph%sph_params, SPH_WK%monitor%pwr)
!
      end subroutine SPH_analyze_correlate_all
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_snap
!
!      end subroutine SPH_finalize_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_correle_all
