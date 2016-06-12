!>@file   lead_fields_4_sph_mhd.f90
!!@brief  module lead_fields_4_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine s_lead_fields_4_sph_mhd                              &
!!     &         (sph, comms_sph, trans_p, rj_fld, trns_WK)
!!      subroutine pressure_4_sph_mhd(sph_rj, rj_fld)
!!      subroutine enegy_fluxes_4_sph_mhd(sph, comms_sph, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: trns_WK
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module lead_fields_4_sph_mhd
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_data
      use t_addresses_sph_transform
      use t_sph_trans_arrays_MHD
      use t_sph_matrices
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
!
      implicit none
!
      private :: pressure_4_sph_mhd
      private :: gradients_of_vectors_sph, enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_lead_fields_4_sph_mhd                                &
     &         (sph, comms_sph, trans_p, rj_fld, trns_WK)
!
      use m_control_parameter
      use m_t_step_parameter
      use m_radial_matrices_sph
      use output_viz_file_control
      use copy_MHD_4_sph_trans
      use cal_energy_flux_rtp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(works_4_sph_trans_MHD), intent(inout) :: trns_WK
      type(phys_data), intent(inout) :: rj_fld
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( (iflag*mod(istep_max_dt,i_step_output_rst)) .eq.0 ) then
        if(iflag_t_evo_4_velo .gt. id_no_evolution) then
          call pressure_4_sph_mhd                                       &
     &       (sph%sph_rj, trans_p%leg, band_p_poisson, rj_fld)
        end if
      end if
!
      if(iflag .gt. 0) return
!
      call select_mhd_field_from_trans                                  &
     &   (sph%sph_rtp, trns_WK%trns_MHD%f_trns,                         &
     &    trns_WK%trns_MHD%ncomp_rtp_2_rj, trns_WK%trns_MHD%frc_rtp,    &
     &    trns_WK%frm_rtp)
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     & then
        call cal_nonlinear_pole_MHD(sph%sph_rtp,                        &
     &      trns_WK%trns_MHD%f_trns, trns_WK%trns_snap%b_trns,          &
     &      trns_WK%trns_snap%ncomp_rj_2_rtp,                           &
     &      trns_WK%trns_MHD%ncomp_rtp_2_rj,                            &
     &      trns_WK%fls_pl, trns_WK%frm_pl)
      end if
!
      call gradients_of_vectors_sph(sph, comms_sph, trans_p,            &
     &    trns_WK%trns_MHD, trns_WK%trns_tmp, rj_fld)
      call enegy_fluxes_4_sph_mhd(sph, comms_sph, trans_p,              &
     &    trns_WK%trns_MHD, trns_WK%trns_snap, rj_fld,                  &
     &    trns_WK%frm_rtp, trns_WK%flc_pl, trns_WK%fls_pl)
!
      end subroutine s_lead_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine pressure_4_sph_mhd                                     &
     &         (sph_rj, leg, band_p_poisson, rj_fld)
!
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use cal_sol_sph_fluid_crank
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use const_sph_radial_grad
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(band_matrices_type), intent(in) :: band_p_poisson
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2(sph_rj, leg%g_sph_rj, rj_fld)
!
      call s_const_radial_forces_on_bc(sph_rj, leg%g_sph_rj, rj_fld)
!
      call sum_div_of_forces(rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v(sph_rj, band_p_poisson, rj_fld)
!
      if(ipol%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient(sph_rj, sph_bc_U, leg%g_sph_rj,    &
     &     ipol%i_press, ipol%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_4_sph_mhd                                 &
     &          (sph, comms_sph, trans_p, trns_MHD, trns_snap, rj_fld,  &
     &           frm_rtp, flc_pl, fls_pl)
!
      use m_sph_phys_address
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use cal_energy_flux_rj
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(phys_data), intent(inout) :: rj_fld
      real(kind = kreal), intent(inout)                                 &
     &       :: frm_rtp(sph%sph_rtp%nnod_rtp,trns_snap%ncomp_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &       :: flc_pl(sph%sph_rtp%nnod_pole,trns_snap%ncomp_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &       :: fls_pl(sph%sph_rtp%nnod_pole,trns_snap%ncomp_rj_2_rtp)
!
!
!      Evaluate fields for output in spectrum space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rj'
      call s_cal_energy_flux_rj(sph%sph_rj, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD                                  &
     &   (sph, comms_sph, trans_p, rj_fld, trns_snap, flc_pl, fls_pl)
!
!      Evaluate fields for output in grid space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rtp'
      call s_cal_energy_flux_rtp(sph%sph_rtp, trns_MHD%f_trns,          &
     &    trns_snap%b_trns, trns_snap%f_trns, trns_MHD%ncomp_rtp_2_rj,  &
     &    trns_snap%ncomp_rj_2_rtp, trns_snap%ncomp_rtp_2_rj,           &
     &    frm_rtp, trns_snap%fld_rtp, trns_snap%frc_rtp)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p, trns_snap, rj_fld)
!
      end subroutine enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine gradients_of_vectors_sph                               &
     &         (sph, comms_sph, trans_p, trns_MHD, trns_tmp, rj_fld)
!
      use m_sph_phys_address
      use sph_transforms_4_MHD
      use sph_poynting_flux_smp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_velo_to_grad_v_rtp'
      call copy_velo_to_grad_v_rtp                                      &
     &   (sph%sph_rtp, trns_MHD%b_trns, trns_tmp%f_trns,                &
     &    trns_MHD%ncomp_rj_2_rtp, trns_tmp%ncomp_rtp_2_rj,             &
     &    trns_MHD%fld_rtp, trns_tmp%frc_rtp)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_tmp_snap_MHD'
      call sph_forward_trans_tmp_snap_MHD                               &
     &   (sph, comms_sph, trans_p, trns_tmp, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_grad_of_velocities_sph'
      call cal_grad_of_velocities_sph                                   &
     &   (sph%sph_rj, trans_p%leg%g_sph_rj, rj_fld)
!
      end subroutine gradients_of_vectors_sph
!
! ----------------------------------------------------------------------
!
      end module lead_fields_4_sph_mhd
