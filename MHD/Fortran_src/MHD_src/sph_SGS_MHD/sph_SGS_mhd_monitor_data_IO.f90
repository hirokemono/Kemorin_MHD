!>@file   sph_SGS_mhd_monitor_data_IO.f90
!!@brief  module sph_SGS_mhd_monitor_data_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      subroutine init_rms_sph_SGS_mhd_control(MHD_prop, sph_MHD_bc,   &
!!     &          r_2nd, SPH_MHD, MHD_mats, monitor, SR_sig)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(MHD_radial_matrices), intent(inout) :: MHD_mats
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine output_rms_sph_SGS_mhd_control                       &
!!     &         (time_d, SPH_SGS, SPH_MHD, sph_MHD_bc, r_2nd, leg,     &
!!     &          MHD_mats, monitor, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(SPH_SGS_structure), intent(in) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(MHD_radial_matrices), intent(in) :: MHD_mats
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module sph_SGS_mhd_monitor_data_IO
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_control_parameter
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_schmidt_poly_on_rtm
      use t_pickup_sph_spectr_data
      use t_no_heat_Nusselt
      use t_IO_step_parameter
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_sph_mhd_monitor_data_IO
      use t_fdm_coefs
!
      use pickup_sph_spectr_data
!
      implicit none
!
      private :: open_sph_vol_rms_file_SGS_mhd
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine init_rms_sph_SGS_mhd_control(MHD_prop, sph_MHD_bc,     &
     &          r_2nd, SPH_MHD, MHD_mats, monitor, SR_sig)
!
      use t_solver_SR
      use t_time_data
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
!
      use cal_heat_source_Nu
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(MHD_radial_matrices), intent(inout) :: MHD_mats
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
      character(len=kchara) :: mat_name
!
!
     if(monitor%heat_Nusselt%iflag_Nusselt .eq. iflag_source_Nu) then
        write(mat_name,'(a)') 'Diffusive_Temperature'
        call init_poisson_matrix_for_Nu                                 &
     &     (mat_name, SPH_MHD%sph, r_2nd, MHD_prop%ht_prop,             &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center,                &
     &      MHD_mats%band_T00_poisson_fixT, monitor%heat_Nusselt)
      end if
!
     if(monitor%comp_Nusselt%iflag_Nusselt .eq. iflag_source_Nu) then
        write(mat_name,'(a)') 'Diffusive_Composition'
        call init_poisson_matrix_for_Nu                                 &
     &     (mat_name, SPH_MHD%sph, r_2nd, MHD_prop%cp_prop,             &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center,                &
     &      MHD_mats%band_C00_poisson_fixC, monitor%comp_Nusselt)
      end if
!
      call open_sph_vol_rms_file_SGS_mhd                                &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld, monitor, SR_sig)
!
      end subroutine init_rms_sph_SGS_mhd_control
!
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_SGS_mhd_control                         &
     &         (time_d, SPH_SGS, SPH_MHD, MHD_prop, sph_MHD_bc,         &
     &          r_2nd, leg, MHD_mats, monitor, SR_sig)
!
      use t_solver_SR
      use t_time_data
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
      use m_machine_parameter
!
      use cal_write_sph_monitor_data
      use cal_SGS_sph_rms_data
!
      type(time_data), intent(in) :: time_d
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(SPH_SGS_structure), intent(in) :: SPH_SGS
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(MHD_radial_matrices), intent(in) :: MHD_mats
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      call cal_SGS_sph_monitor_data                                     &
     &   (SPH_MHD%sph, MHD_prop, sph_MHD_bc, r_2nd, leg, MHD_mats,      &
     &    SPH_MHD%ipol, SPH_SGS%ipol_LES, SPH_MHD%fld, monitor)
!
      call output_sph_monitor_data                                      &
     &   (time_d, SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,           &
     &    SPH_MHD%ipol, SPH_MHD%fld, monitor, SR_sig)
!
      end subroutine output_rms_sph_SGS_mhd_control
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_SGS_mhd                          &
     &         (sph, ipol, rj_fld, monitor, SR_sig)
!
      use m_error_IDs
      use pickup_gauss_coefficients
      use cal_rms_fields_by_sph
      use output_sph_pwr_volume_file
      use write_sph_gauss_coefs
!
      use t_solver_SR
      use calypso_mpi_int
      use init_energy_labels_sph_SGS
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: iflag
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_energy_labels_w_filter'
      call init_energy_labels_w_filter(monitor%ene_labels)
      call init_rms_4_sph_spectr                                        &
     &   (sph%sph_params, sph%sph_rj, rj_fld,                           &
     &    monitor%pwr, monitor%WK_pwr, monitor%dip)
!
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
     &    ipol, monitor%gauss_list, monitor%gauss_coef, SR_sig)
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
      end module sph_SGS_mhd_monitor_data_IO
