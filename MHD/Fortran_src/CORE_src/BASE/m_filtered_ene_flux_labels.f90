!>@file   m_filtered_ene_flux_labels.f90
!!        module m_filtered_ene_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_enegy_fluxes(field_name)
!!      subroutine set_enegy_fluxe_addresses                            &
!!     &         (i_phys, field_name, base_force, flag)
!!        type(energy_flux_address), intent(inout) :: ene_flux
!!
!!      subroutine energy_fluxes_monitor_address                        &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_ene_flux, ave_ene_flux, flag)
!!        type(energy_flux_address), intent(inout) :: rms_ene_flux
!!        type(energy_flux_address), intent(inout) :: ave_ene_flux
!!
!!      integer(kind = kint) function num_energy_fluxes()
!!      subroutine set_energy_flux_names(field_names)
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   inertia_work_by_filtered:  Work of Reynolds stress
!!                             u \cdot (\omega \times u)
!!   Lorentz_work_by_filtered:  Work of Lorentz force
!!                             u \cdot (J \times B)
!!   work_against_Lorentz_by_filtered:  Work against Lorentz force
!!                                           -u \cdot (J \times B)
!!   mag_tension_work_by_filtered: Work of magnetic tension
!!                                            u \cdot( (B \nabla) B)
!!   filtered_buoyancy_flux:       Thermal buoyancy flux
!!                                           -u \cdot (\alpha_{T} g T)
!!   filtered_comp_buoyancy_flux: Compositional buoyancy flux
!!                                           -u \cdot (\alpha_{C} g C)
!!
!!   magnetic_ene_generation_by_filtered:
!!                    energy flux by magneitic induction
!!                              B \cdot (\nabla \times (u \times B))
!!   magnetic_stretch_flux_by_filtered:
!!                    energy flux by magneitic streatch 
!!                              B \cdot ((B \nabla) u)
!!
!!   temp_generation_by_filtered: heat advection flux
!!                              T (u \cdot \nabla) T
!!   part_temp_gen_by_filtered:  perturbation of heat advection flux
!!                                     \Theta (u \cdot \nabla) \Theta
!!   comp_generation_by_filtered:    composition advection flux 
!!                                     C (u \cdot \nabla) C
!!   part_comp_gen_by_filtered: 
!!                perturbation of composition advection flux
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_ene_flux_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use t_base_force_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private                          &
     &                   :: nene_flux_by_filtered = 11
!
!>        Field label of work of inertia
!!         @f$ u_{i} (u_{j} \partial_{j} u_{i}) @f$
      character(len=kchara), parameter :: fhd_inertia_work_by_filter    &
     &                      = 'inertia_work_by_filtered'
!
!>        Field label of work against Lorentz force
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter :: fhd_wk_agst_Lorentz_by_filter &
     &                      = 'work_against_Lorentz_by_filtered'
!>        Field label of work of Lorentz force
!!         @f$ u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter :: fhd_Lorentz_work_by_filter    &
     &                      = 'Lorentz_work_by_filtered'
!>        Field address of work of magnetic tension
!!         @f$ u_{i} (B_{j} \partial_{j}) B_{i} @f$
      character(len=kchara), parameter :: fhd_m_tension_work_by_filter  &
     &                      = 'mag_tension_work_by_filtered'
!
!>        Field label for filtered buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} \tilde{T} @f$
      character(len=kchara), parameter :: fhd_filter_buo_flux           &
     &                      =   'filtered_buoyancy_flux'
!>        Field label of compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} \tilde{C} @f$
      character(len=kchara), parameter :: fhd_filter_comp_buo_flux      &
     &                      = 'filtered_comp_buoyancy_flux'
!!
!>        Field label of magnetic energy flux
!>       @f$ B_{i}e_{ijk} \partial_{j} \left(e_{klm}u_{l}B_{m}\right) @f$
      character(len=kchara), parameter :: fhd_mag_ene_gen_by_filter     &
     &                      = 'magnetic_ene_generation_by_filtered'
!>        Field label of energy flux of magnetic stretch term
!!       @f$ u_{i} \left(B_{i} \partial_{k} u_{k} \right)@f$
      character(len=kchara), parameter :: fhd_m_stretch_flux_by_filter  &
     &                      = 'magnetic_stretch_flux_by_filtered'
!
!>        Field label of temperature flux
!!         @f$ T (u_{i} \partial_{i}) T @f$
      character(len=kchara), parameter :: fhd_temp_gen_by_filter        &
     &                      =   'temp_generation_by_filtered'
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (u_{i} \partial_{i}) \Theta @f$
      character(len=kchara), parameter :: fhd_part_temp_gen_by_filter   &
     &                      =     'part_temp_gen_by_filtered'
!>        Field label of composition flux
!!         @f$ C (u_{i} \partial_{i}) @f$
      character(len=kchara), parameter :: fhd_comp_gen_by_filter        &
     &                      =   'comp_generation_by_filtered'
!>        Field label of perturbation composition flux
!!         @f$ \tilde{\Theta}_{C} (u_{i} \partial_{i}) 
!!            \tilde{\Theta}_{C} @f$
      character(len=kchara), parameter :: fhd_part_comp_gen_by_filter   &
     &                      =     'part_comp_gen_by_filtered'
!
      end module m_filtered_ene_flux_labels
