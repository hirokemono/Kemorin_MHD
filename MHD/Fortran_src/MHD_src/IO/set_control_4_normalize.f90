!>@file   set_control_4_normalize.f90
!!@brief  module set_control_4_normalize
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set normalizatios for MHD simulation from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_normalize
!!@endverbatim
!
      module set_control_4_normalize
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_error_IDs
!
      use t_normalize_parameter
!
      implicit  none
!
      private :: set_dimensionless_numbers
      private :: set_coefs_4_thermal_eq, set_coefs_4_momentum_eq
      private :: set_coefs_4_induction_eq, set_coefs_4_composition_eq
      private :: copy_dimless_from_ctl, copy_power_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_normalize
!
      use m_control_parameter
      use m_normalize_parameter
!
      integer (kind = kint) :: i
!
!
!   set dimensionless numbers
!
      call set_dimensionless_numbers
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'num_dimless ', MHD_coef_list%dimless_list%num
        do i = 1, MHD_coef_list%dimless_list%num
          write(*,*) i, trim(MHD_coef_list%dimless_list%name(i)),      &
     &              ': ', MHD_coef_list%dimless_list%value(i)
        end do
      end if
!
!    set normalization for thermal
!
      if (iflag_t_evo_4_temp .eq. id_no_evolution) then
        MHD_coef_list%coefs_termal%num =    0
        MHD_coef_list%coefs_t_diffuse%num = 0
        MHD_coef_list%coefs_h_source%num =  0
      else
        call set_coefs_4_thermal_eq
      end if
!
!    set coefficients for momentum equation
!
      if (evo_velo%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_momentum%num =  0
        MHD_coef_list%coefs_pressure%num =  0
        MHD_coef_list%coefs_v_diffuse%num = 0
        MHD_coef_list%coefs_buoyancy%num =  0
        MHD_coef_list%coefs_comp_buo%num =  0
        MHD_coef_list%coefs_Coriolis%num =  0
        MHD_coef_list%coefs_Lorentz%num =   0
      else
        call set_coefs_4_momentum_eq
      end if
!
!
!    coefficients for inducition equation
!
      if (evo_magne%iflag_scheme .eq. id_no_evolution                   &
     &  .and. evo_vect_p%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_magnetic%num =  0
        MHD_coef_list%coefs_magne_p%num =   0
        MHD_coef_list%coefs_m_diffuse%num = 0
        MHD_coef_list%coefs_induction%num = 0
      else
        call set_coefs_4_induction_eq
      end if
!
!    set normalization for composition
!
      if (evo_comp%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_composition%num = 0
        MHD_coef_list%coefs_c_diffuse%num =   0
        MHD_coef_list%coefs_c_source%num =    0
      else
        call set_coefs_4_composition_eq
      end if
!
      end subroutine s_set_control_4_normalize
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_dimensionless_numbers
!
      use m_control_parameter
      use m_ctl_data_mhd_normalize
      use m_normalize_parameter
!
!
      if (coef_4_dimless_ctl%icou .eq. 0) then
          e_message =                                                   &
     &     'Set dimensionless numbers'
          call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%dimless_list%num = coef_4_dimless_ctl%num
      end if
!
      call copy_dimless_from_ctl                                        &
     &   (coef_4_dimless_ctl, MHD_coef_list%dimless_list)
!
      end subroutine set_dimensionless_numbers
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_thermal_eq
!
      use m_control_parameter
      use m_normalize_parameter
      use m_ctl_data_termal_norm
!
!
      if (coef_4_heat_flux_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for temperature'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_termal%num = coef_4_heat_flux_ctl%num
      end if
!
      if (coef_4_t_diffuse_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for thermal diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_t_diffuse%num = coef_4_t_diffuse_ctl%num
      end if
!
      if (coef_4_heat_src_ctl%icou .gt. 0) then
        MHD_coef_list%coefs_h_source%num = coef_4_heat_src_ctl%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_heat_flux_ctl, MHD_coef_list%coefs_termal)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_t_diffuse_ctl, MHD_coef_list%coefs_t_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_heat_src_ctl, MHD_coef_list%coefs_h_source)
!
      end subroutine set_coefs_4_thermal_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_momentum_eq
!
      use m_control_parameter
      use m_normalize_parameter
      use m_ctl_data_momentum_norm
!
!
      if (coef_4_intertia_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for velocity'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_momentum%num = coef_4_intertia_ctl%num
      end if
!
      if (coef_4_grad_p_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for pressure gradient'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_pressure%num = coef_4_grad_p_ctl%num
      end if
!
      if (coef_4_viscous_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for viscosity'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_v_diffuse%num = coef_4_viscous_ctl%num
      end if
!
      if(iflag_4_gravity .eq. id_turn_OFF                               &
     &      .and. iflag_4_filter_gravity .eq. id_turn_OFF) then
        MHD_coef_list%coefs_buoyancy%num = 0
      else
        if (coef_4_termal_buo_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for buoyancy'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_buoyancy%num = coef_4_termal_buo_ctl%num
        end if
      end if
!
      if (iflag_4_composit_buo .eq. id_turn_OFF) then
        MHD_coef_list%coefs_comp_buo%num = 0
      else
        if (coef_4_comp_buo_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for compiositional buoyancy'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_comp_buo%num = coef_4_comp_buo_ctl%num
        end if
      end if
!
      if (iflag_4_coriolis .eq. id_turn_OFF) then
        MHD_coef_list%coefs_Coriolis%num = 0
      else
        if (coef_4_Coriolis_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for Coriolis force'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_Coriolis%num = coef_4_Coriolis_ctl%num
        end if
      end if
!
      if (iflag_4_lorentz .eq. id_turn_OFF) then
        MHD_coef_list%coefs_Lorentz%num = 0
      else
        if (coef_4_Loreantz_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for Lorentz force'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_Lorentz%num = coef_4_Loreantz_ctl%num
        end if
      end if
!
!
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_intertia_ctl, MHD_coef_list%coefs_momentum)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_grad_p_ctl, MHD_coef_list%coefs_pressure)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_viscous_ctl, MHD_coef_list%coefs_v_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_termal_buo_ctl, MHD_coef_list%coefs_buoyancy)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_comp_buo_ctl, MHD_coef_list%coefs_comp_buo)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_Coriolis_ctl, MHD_coef_list%coefs_Coriolis)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_Loreantz_ctl, MHD_coef_list%coefs_Lorentz)
!
      end subroutine set_coefs_4_momentum_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_induction_eq
!
      use m_control_parameter
      use m_normalize_parameter
      use m_ctl_data_induct_norm
!
!
      if (coef_4_magne_evo_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic field'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_magnetic%num = coef_4_magne_evo_ctl%num
      end if
!
      if (coef_4_mag_potential_ctl%icou .eq. 0                          &
     &       .and. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic potential'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_magne_p%num = coef_4_mag_potential_ctl%num
      end if
!
      if (coef_4_mag_diffuse_ctl%icou .eq. 0) then
        e_message = 'Set coefficients for magnetic diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_m_diffuse%num = coef_4_mag_diffuse_ctl%num
      end if
!
      if (coef_4_induction_ctl%icou .eq. 0) then
        e_message = 'Set coefficients for induction term'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_induction%num = coef_4_induction_ctl%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_magne_evo_ctl, MHD_coef_list%coefs_magnetic)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_mag_potential_ctl, MHD_coef_list%coefs_magne_p)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_mag_diffuse_ctl, MHD_coef_list%coefs_m_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_induction_ctl, MHD_coef_list%coefs_induction)
!
      end subroutine set_coefs_4_induction_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_composition_eq
!
      use m_normalize_parameter
      use m_ctl_data_composite_norm
!
!
      if (coef_4_comp_flux_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for composition scalar'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_composition%num = coef_4_comp_flux_ctl%num
      end if
!
      if (coef_4_c_diffuse_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for scalar diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_c_diffuse%num = coef_4_c_diffuse_ctl%num
      end if
!
      if (coef_4_comp_src_ctl%icou .gt. 0) then
        MHD_coef_list%coefs_c_source%num = coef_4_comp_src_ctl%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_comp_flux_ctl, MHD_coef_list%coefs_composition)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_c_diffuse_ctl, MHD_coef_list%coefs_c_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (coef_4_comp_src_ctl, MHD_coef_list%coefs_c_source)
!
      end subroutine set_coefs_4_composition_eq
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_dimless_from_ctl(coef_ctl, dimless_list)
!
      use t_read_control_arrays
!
      type(ctl_array_cr), intent(inout) :: coef_ctl
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      call alloc_dimless_list(dimless_list)
      if (dimless_list%num .le. 0) return
!
      dimless_list%name(1:dimless_list%num)                            &
     &             = coef_ctl%c_tbl(1:dimless_list%num)
      dimless_list%value(1:dimless_list%num)                           &
     &             = coef_ctl%vect(1:dimless_list%num)
!
      call dealloc_control_array_c_r(coef_ctl)
!
      end subroutine copy_dimless_from_ctl
!
! -----------------------------------------------------------------------
!
      subroutine copy_power_and_names_from_ctl(coef_ctl, coef_list)
!
      use t_read_control_arrays
!
      type(ctl_array_cr), intent(inout) :: coef_ctl
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      call alloc_coef_power_list(coef_list)
      if (coef_list%num .le. 0) return
!
      coef_list%name(1:coef_list%num) = coef_ctl%c_tbl(1:coef_list%num)
      coef_list%power(1:coef_list%num) = coef_ctl%vect(1:coef_list%num)
!
      call dealloc_control_array_c_r(coef_ctl)
!
      end subroutine copy_power_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      end module set_control_4_normalize
