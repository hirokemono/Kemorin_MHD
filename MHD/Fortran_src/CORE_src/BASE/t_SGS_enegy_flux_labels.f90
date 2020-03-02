!>@file   t_SGS_enegy_flux_labels.f90
!!        module t_SGS_enegy_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!
!!      integer(kind = kint) function num_SGS_energy_fluxes()
!!      subroutine set_SGS_energy_flux_labels(n_comps, names, maths)
!!
!! !!!!!  product of fields names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   fhd_Reynolds_work []: 
!!   fhd_SGS_Lorentz_work []: 
!!   fhd_SGS_buo_flux []: 
!!   fhd_SGS_comp_buo_flux []: 
!!
!!   fhd_SGS_m_ene_gen []: 
!!
!!   fhd_SGS_temp_gen []: 
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_enegy_flux_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nSGS_e_flux = 7
!
!
!  energy flux by SGS terms
!>        Field label for work of SGS Reynolds stress
!!         @f$ -u_{i} e_{ijk} (\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work =     'Reynolds_work'
      type(field_def), parameter :: Reynolds_work                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Reynolds_work',                           &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!>        Field label for work of SGS Lorentz force
!!         @f$  u_{i} e_{ijk} (\overline{J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_work =  'SGS_Lorentz_work'
      type(field_def), parameter :: SGS_Lorentz_work                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_Lorentz_work',                        &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!>        Field label for work of SGS buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{T} g_{i} I_{Ti} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_buo_flux =      'SGS_buoyancy_flux'
      type(field_def), parameter :: SGS_buoyancy_flux                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_buoyancy_flux',                       &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!>        Field label for work of SGS compositional buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{C} g_{i} I_{Ci} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_buo_flux = 'SGS_comp_buoyancy_flux'
      type(field_def), parameter :: SGS_comp_buoyancy_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_comp_buoyancy_flux',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!
!>        Field label for energy flux of SGS induction
!!         @f$ B_{i} e_{ijk} \partual_{j} e_{klm} 
!!            (\overline{u_{l}B_{m}} - \bar{u}_{l}\bar{B}_{m} ) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen = 'SGS_mag_induction_flux'
      type(field_def), parameter :: SGS_mag_induction_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_mag_induction_flux',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!
!>        Field label for temperature generation by SGS heat flux
!!         @f$ T \partial_{i} \left( \overline{u_{i}T}
!!            - \bar{u}_{i}\bar{T} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen =      'SGS_temp_flux_gen'
      type(field_def), parameter :: SGS_temp_flux_gen                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_temp_flux_gen',                       &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!>        Field label for composition generation by SGS composition flux
!!         @f$ C \partial_{i} \left( \overline{u_{i}C}
!!            - \bar{u}_{i}\bar{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_gen =      'SGS_comp_flux_gen'
      type(field_def), parameter :: SGS_comp_flux_gen                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_comp_flux_gen',                       &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!
!
!>       Structure of start address for energy flux by SGS terms
      type SGS_flux_address
!>        Field address for work of SGS Reynolds stress
!!         @f$ -u_{i} e_{ijk} (\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k}) @f$
        integer (kind=kint) :: i_reynolds_wk =     izero
!>        Field address for work of SGS Lorentz force
!!         @f$  u_{i} e_{ijk} (\overline{J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k}) @f$
        integer (kind=kint) :: i_SGS_Lor_wk =      izero
!>        Field address for work of SGS buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{T} g_{i} I_{Ti} @f$
        integer (kind=kint) :: i_SGS_buo_wk =      izero
!>        Field address for work of SGS compositional buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{C} g_{i} I_{Ci} @f$
        integer (kind=kint) :: i_SGS_comp_buo_wk = izero
!
!>        Field address for energy flux of SGS induction
!!         @f$ B_{i} e_{ijk} \partual_{j} e_{klm} 
!!            (\overline{u_{l}B_{m}} - \bar{u}_{l}\bar{B}_{m} ) @f$
        integer (kind=kint) :: i_SGS_me_gen =      izero
!
!>        Field address for temperature generation by SGS heat flux
!!         @f$ T \partial_{i} \left( \overline{u_{i}T}
!!            - \bar{u}_{i}\bar{T} \right) @f$
        integer (kind=kint) :: i_SGS_temp_gen =    izero
!>        Field address for composition generation
!!           by SGS composition flux
!!         @f$ C \partial_{i} \left( \overline{u_{i}C}
!!            - \bar{u}_{i}\bar{C} \right) @f$
        integer (kind=kint) :: i_SGS_comp_gen =    izero
      end type SGS_flux_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_SGS_energy_fluxes()
      num_SGS_energy_fluxes = nSGS_e_flux
      return
      end function num_SGS_energy_fluxes
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_energy_flux_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(nSGS_e_flux)
      character(len = kchara), intent(inout) :: names(nSGS_e_flux)
      character(len = kchara), intent(inout) :: maths(nSGS_e_flux)
!
!
      call set_field_labels(Reynolds_work,                              &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(SGS_Lorentz_work,                           &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(SGS_buoyancy_flux,                          &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(SGS_comp_buoyancy_flux,                     &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(SGS_mag_induction_flux,                     &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(SGS_temp_flux_gen,                          &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(SGS_comp_flux_gen,                          &
     &    n_comps( 7), names( 7), maths( 7))
!
      end subroutine set_SGS_energy_flux_labels
!
! ----------------------------------------------------------------------
!
      end module t_SGS_enegy_flux_labels
