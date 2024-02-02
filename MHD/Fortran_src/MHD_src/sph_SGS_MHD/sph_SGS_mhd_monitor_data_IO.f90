!>@file   sph_SGS_mhd_monitor_data_IO.f90
!!@brief  module sph_SGS_mhd_monitor_data_IO
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!
!!      subroutine init_rms_sph_SGS_mhd_control(MHD_prop, sph_MHD_bc,   &
!!     &          r_2nd, trans_p, nod_fld, SPH_SGS, SPH_MHD, MHD_mats,  &
!!     &          monitor, SR_sig, SR_r)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_data), intent(in) :: nod_fld
!!        type(SPH_SGS_structure), intent(in) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(MHD_radial_matrices), intent(inout) :: MHD_mats
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine output_rms_sph_SGS_mhd_control                       &
!!     &         (time_d, SPH_SGS, SPH_MHD, sph_MHD_bc, r_2nd, leg,     &
!!     &          r_2nd, trans_p, MHD_mats, monitor, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
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
      use t_work_4_sph_trans
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
      private :: init_sph_lorentz_spectr_data
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine init_rms_sph_SGS_mhd_control(MHD_prop, sph_MHD_bc,     &
     &          r_2nd, trans_p, nod_fld, SPH_SGS, SPH_MHD, MHD_mats,    &
     &          monitor, SR_sig, SR_r)
!
      use t_solver_SR
      use t_time_data
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
!
      use cal_heat_source_Nu
      use field_at_mid_equator
      use const_data_4_dynamobench
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: nod_fld
      type(SPH_SGS_structure), intent(in) :: SPH_SGS
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(MHD_radial_matrices), intent(inout) :: MHD_mats
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      character(len=kchara) :: mat_name
      integer(kind = kint) :: i
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
     &   (SPH_MHD%sph, sph_MHD_bc%sph_bc_U,                             &
     &    SPH_MHD%ipol, SPH_SGS%ipol_LES, SPH_MHD%fld,                  &
     &    monitor, SR_sig)
!
      if(monitor%bench%iflag_dynamobench .gt. 0) then
        call init_circle_field_name_dbench(SPH_MHD%ipol,                &
     &      monitor%circ_mid_eq%d_circle, monitor%bench)
        call init_mid_equator_point_global(SPH_MHD%sph,                 &
     &                                     monitor%circ_mid_eq)
        call init_circle_point_global                                   &
     &     (SPH_MHD%sph, SPH_MHD%comms, trans_p,                        &
     &      monitor%circ_mid_eq, SR_sig, SR_r)
        call alloc_dynamobench_monitor(monitor%circ_mid_eq%d_circle,    &
     &                                 monitor%bench)
      end if
!
      do i = 1, monitor%mul_circle%num_circles
        call dup_phys_name                                              &
     &     (nod_fld, monitor%mul_circle%cdat(i)%d_circle)
        call init_circle_point_global                                   &
     &     (SPH_MHD%sph, SPH_MHD%comms, trans_p,                        &
     &      monitor%mul_circle%cdat(i), SR_sig, SR_r)
        call set_circle_transfer_address(nod_fld, SPH_MHD%fld,          &
     &                                   monitor%mul_circle%cdat(i))
      end do
!
      end subroutine init_rms_sph_SGS_mhd_control
!
!  --------------------------------------------------------------------
!
      subroutine output_rms_sph_SGS_mhd_control                         &
     &         (time_d, SPH_SGS, SPH_MHD, MHD_prop, sph_MHD_bc,         &
     &          r_2nd, trans_p, MHD_mats, monitor, SR_sig)
!
      use t_solver_SR
      use t_time_data
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
      use m_machine_parameter
!
      use cal_write_sph_monitor_data
      use cal_SGS_sph_rms_data
      use cal_write_sph_monitor_data
!
      type(time_data), intent(in) :: time_d
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SPH_SGS_structure), intent(in) :: SPH_SGS
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(MHD_radial_matrices), intent(in) :: MHD_mats
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      call cal_SGS_sph_monitor_data(time_d, SPH_MHD%sph, MHD_prop,      &
     &    sph_MHD_bc, r_2nd, trans_p, MHD_mats, SPH_MHD%ipol,           &
     &    SPH_SGS%ipol_LES, SPH_MHD%fld, monitor)
!
      call output_sph_monitor_data                                      &
     &   (time_d, SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,           &
     &    sph_MHD_bc, SPH_MHD%ipol, SPH_MHD%fld, monitor, SR_sig)
      call output_sph_mean_square_files(monitor%ene_labels, time_d,     &
     &    SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,                   &
     &    monitor%lor_spectr)
!
      end subroutine output_rms_sph_SGS_mhd_control
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file_SGS_mhd                          &
     &         (sph, sph_bc_U, ipol, ipol_LES, rj_fld, monitor, SR_sig)
!
      use m_error_IDs
      use pickup_gauss_coefficients
      use cal_rms_fields_by_sph
      use output_sph_pwr_volume_file
      use write_sph_gauss_coefs
      use write_picked_sph_spectr
      use cal_CMB_dipolarity
      use init_rms_4_sph_spectr
!
      use t_solver_SR
      use calypso_mpi_int
      use calypso_mpi_logical
      use init_energy_labels_sph_SGS
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: ierr_lc, ierr_gl
      logical :: flag
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_energy_labels_w_filter'
      call init_energy_labels_w_filter(monitor%ene_labels)
      call init_sph_spectr_data_and_file(sph, rj_fld, monitor)
      call init_sph_Lorentz_spectr_data(sph, ipol, ipol_LES,            &
     &                                  rj_fld, monitor)
!
      call init_dipolarity_4_sph_spectr(sph%sph_params, monitor%pwr,    &
     &                                  monitor%dip)
!
      if(iflag_debug .gt. 0) write(*,*) 'init_sph_spec_4_monitor'
      call init_sph_spec_4_monitor(sph%sph_params, sph%sph_rj,          &
     &    rj_fld, monitor%pick_list, monitor%pick_coef)
      ierr_lc =  error_picked_spectr_files(sph%sph_params,              &
     &                                     monitor%pick_coef)
!
      call calypso_mpi_allreduce_one_int(ierr_lc, ierr_gl, MPI_SUM)
      if(ierr_gl .gt. 0) then
        write(e_message,*) ierr_gl,                                     &
     &      ' pickup mode files have wrong header. Check field defs.'
        call calypso_mpi_barrier()
        call calypso_MPI_abort(ierr_file, e_message)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'init_gauss_coefs_data_and_file'
      call init_gauss_coefs_data_and_file(sph, ipol,                    &
     &    monitor%gauss_list, monitor%gauss_coef, SR_sig)
!
      if(iflag_debug .gt. 0) write(*,*) 'error_gauss_coefs_header'
      flag = error_gauss_coefs_header(sph%sph_params, sph%sph_rj,       &
     &                                monitor%gauss_coef)
      call calypso_mpi_bcast_one_logical(flag, 0)
      if(flag) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      call init_l_scale_data_and_file(sph, sph_bc_U,                    &
     &                                rj_fld, monitor)
!
      end subroutine open_sph_vol_rms_file_SGS_mhd
!
!  --------------------------------------------------------------------
!
      subroutine init_sph_lorentz_spectr_data(sph, ipol, ipol_LES,      &
     &                                        rj_fld, monitor)
!
      use m_error_IDs
      use t_energy_label_parameters
      use cal_rms_fields_by_sph
      use init_sph_lorentz_spectr
      use calypso_mpi_logical
      use output_sph_pwr_volume_file
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      logical :: flag
!
!
      if(iflag_debug .gt. 0) write(*,*) 's_init_rms_4_sph_spectr'
      call s_init_sph_lorentz_spectr                                    &
     &   (sph%sph_params, sph%sph_rj, ipol, ipol_LES, rj_fld,           &
     &    monitor%lor_spectr, monitor%WK_lor_spectr)
!
      if ( iflag_debug.gt.0 ) write(*,*) 'error_sph_vol_ms_file'
      flag = error_sph_vol_ms_file(my_rank, monitor%ene_labels,         &
     &                             sph%sph_params, sph%sph_rj,          &
     &                             monitor%lor_spectr%v_spectr(1))
      call calypso_mpi_bcast_one_logical                                &
     &  (flag, monitor%lor_spectr%v_spectr(1)%irank_m)
      if(flag) then
        call calypso_mpi_barrier
        call calypso_mpi_abort(ierr_file,                               &
     &     'Field information might be updated.')
      end if
!
      end subroutine init_sph_lorentz_spectr_data
!
!  --------------------------------------------------------------------
!
      end module sph_SGS_mhd_monitor_data_IO
