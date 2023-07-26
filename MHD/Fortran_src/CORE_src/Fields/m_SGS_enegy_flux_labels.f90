!>@file   m_SGS_enegy_flux_labels.f90
!!        module m_SGS_enegy_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_SGS_ene_fluxes(field_name)
!!
!!      subroutine set_SGS_energy_flux_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!! !!!!!  energy flux by SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   Reynolds_work [i_reynolds_wk]:   Reynolds stress
!!   SGS_Lorentz_work [i_SGS_Lor_wk]: work of SGS Lorentz force
!!   SGS_buoyancy_flux [i_SGS_buo_wk]: SGS buoyancy flux
!!   SGS_comp_buoyancy_flux [i_SGS_comp_buo_wk]: 
!!                          SGS compositional buoyancy flux
!!
!!   SGS_mag_induction_flux [i_SGS_me_gen]:
!!                           magnetic energy flux by SGS induction
!!
!!   SGS_temp_flux_gen [i_SGS_temp_gen]:
!!                           temperature change by SGS heat flux
!!   SGS_comp_flux_gen [i_SGS_comp_gen]:
!!                           temperature change by SGS composition flux
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_SGS_enegy_flux_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!  energy flux by SGS terms
!>        Field label for work of SGS Reynolds stress
!!         @f$ -u_{i} e_{ijk} (\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k}) @f$
      type(field_def), parameter :: Reynolds_work                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Reynolds_work',                           &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for work of SGS Lorentz force
!!         @f$  u_{i} e_{ijk} (\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k}) @f$
      type(field_def), parameter :: SGS_Lorentz_work                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_Lorentz_work',                        &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for work of SGS buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{T} g_{i} I_{Ti} @f$
      type(field_def), parameter :: SGS_buoyancy_flux                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_buoyancy_flux',                       &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for work of SGS compositional buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{C} g_{i} I_{Ci} @f$
      type(field_def), parameter :: SGS_comp_buoyancy_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_comp_buoyancy_flux',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!
!>        Field label for energy flux of SGS induction
!!         @f$ B_{i} e_{ijk} \partial_{j} e_{klm} 
!!            (\widetilde{u_{l}B_{m}} - \tilde{u}_{l}\tilde{B}_{m} ) @f$
      type(field_def), parameter :: SGS_mag_induction_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_mag_induction_flux',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!
!>        Field label for temperature generation by SGS heat flux
!!         @f$ T \partial_{i} \left( \widetilde{u_{i}T}
!!            - \tilde{u}_{i}\tilde{T} \right) @f$
      type(field_def), parameter :: SGS_temp_flux_gen                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_temp_flux_gen',                       &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for composition generation by SGS composition flux
!!         @f$ C \partial_{i} \left( \widetilde{u_{i}C}
!!            - \tilde{u}_{i}\tilde{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_gen =      'SGS_comp_flux_gen'
      type(field_def), parameter :: SGS_comp_flux_gen                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_comp_flux_gen',                       &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_ene_fluxes(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_ene_fluxes                                              &
     &   =    (field_name .eq. Reynolds_work%name)                      &
     &   .or. (field_name .eq. SGS_Lorentz_work%name)                   &
     &   .or. (field_name .eq. SGS_buoyancy_flux%name)                  &
     &   .or. (field_name .eq. SGS_comp_buoyancy_flux%name)             &
!
     &   .or. (field_name .eq. SGS_mag_induction_flux%name)             &
     &   .or. (field_name .eq. SGS_temp_flux_gen%name)                  &
     &   .or. (field_name .eq. SGS_comp_flux_gen%name)
!
      end function check_SGS_ene_fluxes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_SGS_energy_flux_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(Reynolds_work,          array_c2i)
      call set_field_label_to_ctl(SGS_Lorentz_work,       array_c2i)
      call set_field_label_to_ctl(SGS_buoyancy_flux,      array_c2i)
      call set_field_label_to_ctl(SGS_comp_buoyancy_flux, array_c2i)
      call set_field_label_to_ctl(SGS_mag_induction_flux, array_c2i)
      call set_field_label_to_ctl(SGS_temp_flux_gen,      array_c2i)
      call set_field_label_to_ctl(SGS_comp_flux_gen,      array_c2i)
!
      end subroutine set_SGS_energy_flux_names
!
! ----------------------------------------------------------------------
!
      end module m_SGS_enegy_flux_labels
