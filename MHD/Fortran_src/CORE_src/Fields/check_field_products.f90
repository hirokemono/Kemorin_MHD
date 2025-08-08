!>@file   check_field_products.f90
!!        module check_field_products
!!
!!@author H. Matsui (UC Davis)
!!@n      and T. Kera (Tohoku University)
!!
!!@date   Programmed in Jan., 2020
!!@n      Modified in July, 2021
!!
!!
!>@brief Check Dependecies for products of fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_field_products(field_ctl)
!!      subroutine add_field_ctl_4_diffusions(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_field_products
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_field_products(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use m_base_force_labels
      use m_field_product_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(rest_of_geostrophic, field_ctl)) then
        call add_phys_name_ctl(Coriolis_force, field_ctl)
        call add_phys_name_ctl(pressure_gradient, field_ctl)
      end if
!
      if(check_field_list_ctl(poynting_flux, field_ctl)) then
        call add_phys_name_ctl(electric_field, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
      if(check_field_list_ctl(electric_field, field_ctl)) then
        call add_phys_name_ctl(vecp_induction, field_ctl)
        call add_phys_name_ctl(current_density, field_ctl)
      end if
!
      if(     check_field_list_ctl(truncated_magnetic_field, field_ctl) &
     &   .or. check_field_list_ctl(magnetic_intensity, field_ctl)       &
     &   .or. check_field_list_ctl(declination, field_ctl)              &
     &   .or. check_field_list_ctl(inclination, field_ctl)              &
     &   .or. check_field_list_ctl(vgp_latitude, field_ctl)             &
     &   .or. check_field_list_ctl(vgp_longigude, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(check_field_list_ctl(Lorentz_work_dipole, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Lorentz_force_dipole, field_ctl)
      end if
      if(check_field_list_ctl(Lorentz_force_dipole, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(current_for_dipole, field_ctl)
      end if
!
      if(check_field_list_ctl(kinetic_helicity, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(vorticity, field_ctl)
      end if
      if(check_field_list_ctl(magnetic_helicity, field_ctl)) then
        call add_phys_name_ctl(vector_potential, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
      if(check_field_list_ctl(current_helicity, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(current_density, field_ctl)
      end if
      if(check_field_list_ctl(cross_helicity, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(      check_field_list_ctl(square_velocity, field_ctl)         &
     &    .or. check_field_list_ctl(velocity_scale, field_ctl)          &
     &    .or. check_field_list_ctl(stream_pol_velocity, field_ctl))    &
     &   call add_phys_name_ctl(velocity, field_ctl)
      if(check_field_list_ctl(square_vorticity, field_ctl))             &
     &   call add_phys_name_ctl(vorticity, field_ctl)
      if(      check_field_list_ctl(square_magne, field_ctl)            &
     &    .or. check_field_list_ctl(magnetic_scale, field_ctl)          &
     &    .or. check_field_list_ctl(stream_pol_magne, field_ctl)        &
     &    .or. check_field_list_ctl(magnetic_dipole, field_ctl))        &
     &   call add_phys_name_ctl(magnetic_field, field_ctl)
      if(check_field_list_ctl(square_vector_potential, field_ctl))      &
     &   call add_phys_name_ctl(vector_potential, field_ctl)
      if(      check_field_list_ctl(square_current, field_ctl)          &
     &    .or. check_field_list_ctl(current_for_dipole, field_ctl))     &
     &   call add_phys_name_ctl(current_density, field_ctl)
      if(      check_field_list_ctl(square_temperature, field_ctl)      &
     &    .or. check_field_list_ctl(temperature_scale, field_ctl))      &
     &   call add_phys_name_ctl(temperature, field_ctl)
      if(      check_field_list_ctl(square_composition, field_ctl)      &
     &    .or. check_field_list_ctl(composition_scale, field_ctl))      &
     &   call add_phys_name_ctl(composition, field_ctl)
!
      end subroutine add_field_ctl_4_field_products
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diffusions(field_ctl)
!
      use t_control_array_character3
      use m_diffusion_term_labels
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_viscousity, field_ctl))               &
     &   call add_phys_name_ctl(viscous_diffusion, field_ctl)
!
      if(check_field_list_ctl(viscous_diffusion, field_ctl))            &
     &   call add_phys_name_ctl(velocity, field_ctl)
      if(check_field_list_ctl(vorticity_diffusion, field_ctl))          &
     &   call add_phys_name_ctl(vorticity, field_ctl)
      if(check_field_list_ctl(magnetic_diffusion, field_ctl))           &
     &   call add_phys_name_ctl(magnetic_field, field_ctl)
      if(check_field_list_ctl(vector_potential_diffusion, field_ctl))   &
     &   call add_phys_name_ctl(vector_potential, field_ctl)
      if(check_field_list_ctl(thermal_diffusion, field_ctl))            &
     &   call add_phys_name_ctl(temperature, field_ctl)
      if(check_field_list_ctl(composition_diffusion, field_ctl))        &
     &   call add_phys_name_ctl(composition, field_ctl)
!
      end subroutine add_field_ctl_4_diffusions
!
! -----------------------------------------------------------------------
!
      end module check_field_products
