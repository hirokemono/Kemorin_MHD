!>@file   sph_SGS_mhd_monitor_data_IO.f90
!!@brief  module sph_SGS_mhd_monitor_data_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine open_sph_vol_rms_file_SGS_mhd                        &
!!     &         (sph, ipol, rj_fld, monitor)
!!      subroutine output_rms_sph_SGS_mhd_control                       &
!!     &         (time_d, SPH_SGS, SPH_MHD, sph_MHD_bc, leg, monitor)
!!      subroutine init_rms_4_sph_spectr_SGS_mhd(sph, rj_fld, monitor)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(SPH_SGS_structure), intent(in) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!@endverbatim
!
      module sph_SGS_mhd_monitor_data_IO
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_schmidt_poly_on_rtm
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
      use t_IO_step_parameter
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
!
      use t_sph_mhd_monitor_data_IO
!
      use pickup_sph_spectr_data
      use output_sph_m_square_file
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_SGS_mhd                          &
     &         (sph, ipol, rj_fld, monitor)
!
      use m_error_IDs
      use pickup_gauss_coefficients
      use cal_rms_fields_by_sph
      use output_sph_m_square_file
      use MPI_sph_gauss_coefs_IO
      use calypso_mpi_int
      use calypso_mpi_int
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_rms_4_sph_spectr_SGS_mhd'
      call init_rms_4_sph_spectr_SGS_mhd(sph, rj_fld, monitor)
!
      if(iflag_debug .gt. 0) write(*,*) 'check_sph_vol_ms_file'
      iflag = check_sph_vol_ms_file(my_rank, monitor%ene_labels,        &
     &                              sph%sph_params, sph%sph_rj,         &
     &                              monitor%pwr)
      call calypso_mpi_bcast_one_int(iflag, 0)
      if(iflag .gt. 0) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_sph_spec_4_monitor'
      call init_sph_spec_4_monitor(sph%sph_params, sph%sph_rj,          &
     &    rj_fld, monitor%pick_list, monitor%pick_coef)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'init_gauss_coefs_4_monitor'
      call init_gauss_coefs_4_monitor(sph%sph_params, sph%sph_rj,       &
     &    ipol, monitor%gauss_list, monitor%gauss_coef)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'check_gauss_coefs_num'
      iflag = check_gauss_coefs_num(monitor%gauss_coef)
      call calypso_mpi_bcast_one_int(iflag, 0)
      if(iflag .gt. 0) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      end subroutine open_sph_vol_rms_file_SGS_mhd
!
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_SGS_mhd_control                         &
     &         (time_d, SPH_SGS, SPH_MHD, sph_MHD_bc, leg, monitor)
!
      use t_time_data
      use t_boundary_data_sph_MHD
      use m_machine_parameter
!
      use cal_write_sph_monitor_data
      use cal_SGS_sph_rms_data
!
      type(time_data), intent(in) :: time_d
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(SPH_SGS_structure), intent(in) :: SPH_SGS
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      call cal_SGS_sph_monitor_data                                     &
     &   (SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,                   &
     &    sph_MHD_bc%sph_bc_U, leg, SPH_MHD%ipol, SPH_SGS%ipol_LES,     &
     &    SPH_MHD%fld, monitor%pwr, monitor%WK_pwr, monitor%Nusselt)
!
      call output_sph_monitor_data                                      &
     &   (monitor%ene_labels, time_d, SPH_MHD%sph%sph_params,           &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld,                &
     &    monitor%pwr, monitor%pick_coef, monitor%gauss_coef,           &
     &    monitor%Nusselt)
!
      end subroutine output_rms_sph_SGS_mhd_control
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr_SGS_mhd(sph, rj_fld, monitor)
!
      use cal_rms_fields_by_sph
      use init_energy_labels_sph_SGS
!
      type(sph_grids), intent(in) :: sph
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_energy_labels_w_filter'
      call init_energy_labels_w_filter(monitor%ene_labels)
      call init_rms_4_sph_spectr                                        &
     &   (sph%sph_params, sph%sph_rj, rj_fld,                           &
     &    monitor%pwr, monitor%WK_pwr)
!
      end subroutine init_rms_4_sph_spectr_SGS_mhd
!
!  --------------------------------------------------------------------
!
      end module sph_SGS_mhd_monitor_data_IO
