!>@file   lead_fields_4_sph_mhd.f90
!!@brief  module lead_fields_4_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine s_lead_fields_4_sph_mhd(rj_fld)
!!      subroutine pressure_4_sph_mhd(rj_fld)
!!      subroutine enegy_fluxes_4_sph_mhd(rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module lead_fields_4_sph_mhd
!
      use m_precision
      use m_machine_parameter
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_data
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
      subroutine s_lead_fields_4_sph_mhd(rj_fld)
!
      use m_control_parameter
      use m_t_step_parameter
      use output_viz_file_control
      use copy_MHD_4_sph_trans
      use cal_energy_flux_rtp
!
      integer (kind =kint) :: iflag
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( (iflag*mod(istep_max_dt,i_step_output_rst)) .eq.0 ) then
        if(iflag_t_evo_4_velo .gt. id_no_evolution) then
          call pressure_4_sph_mhd(rj_fld)
        end if
      end if
!
      if(iflag .gt. 0) return
!
      call select_mhd_field_from_trans(sph_rtp1)
      if    (sph_param1%iflag_shell_mode .eq. iflag_MESH_w_pole         &
     &  .or. sph_param1%iflag_shell_mode .eq. iflag_MESH_w_center) then
        call cal_nonlinear_pole_MHD
      end if
!
      call gradients_of_vectors_sph(rj_fld)
      call enegy_fluxes_4_sph_mhd(rj_fld)
!
      end subroutine s_lead_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine pressure_4_sph_mhd(rj_fld)
!
      use m_spheric_parameter
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use cal_sol_sph_fluid_crank
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use const_sph_radial_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2(sph_rj1, rj_fld)
!
      call s_const_radial_forces_on_bc(sph_rj1, rj_fld)
!
      call sum_div_of_forces(rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v(sph_rj1, rj_fld)
!
      if(ipol%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient(sph_rj1, sph_bc_U,                 &
     &     ipol%i_press, ipol%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_4_sph_mhd(rj_fld)
!
      use m_spheric_parameter
      use m_sph_phys_address
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use cal_energy_flux_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!      Evaluate fields for output in spectrum space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rj'
      call s_cal_energy_flux_rj(sph_rj1, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD                                  &
     &   (sph_param1, sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1,            &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1, rj_fld)
!
!      Evaluate fields for output in grid space
      if (iflag_debug.eq.1) write(*,*) 's_cal_energy_flux_rtp'
      call s_cal_energy_flux_rtp(sph_rtp1)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD(sph_rtp1, sph_rtm1, sph_rlm1, &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1,rj_fld)
!
      end subroutine enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine gradients_of_vectors_sph(rj_fld)
!
      use m_sph_phys_address
      use sph_transforms_4_MHD
      use sph_poynting_flux_smp
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_velo_to_grad_v_rtp'
      call copy_velo_to_grad_v_rtp(sph_rtp1)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_tmp_snap_MHD'
      call sph_forward_trans_tmp_snap_MHD(sph_rtp1, sph_rtm1, sph_rlm1, &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_grad_of_velocities_sph'
      call cal_grad_of_velocities_sph(sph_rj1, rj_fld)
!
      end subroutine gradients_of_vectors_sph
!
! ----------------------------------------------------------------------
!
      end module lead_fields_4_sph_mhd
