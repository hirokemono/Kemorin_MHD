!>@file   t_energy_flux_labels.f90
!!        module t_energy_flux_labels
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
!!   inertia_work:  Work of Reynolds stress   u \cdot (\omega \times u)
!!   Lorentz_work:  Work of Lorentz force     u \cdot (J \times B)
!!   work_against_Lorentz:  Work against Lorentz force
!!                                           -u \cdot (J \times B)
!!   mag_tension_work: Work of magnetic tension
!!                                            u \cdot( (B \nabla) B)
!!   buoyancy_flux:       Thermal buoyancy flux
!!                                           -u \cdot (\alpha_{T} g T)
!!   composite_buoyancy_flux: Compositional buoyancy flux
!!                                           -u \cdot (\alpha_{C} g C)
!!
!!   magnetic_ene_generation: energy flux by magneitic induction
!!                              B \cdot (\nabla \times (u \times B))
!!   magnetic_stretch_flux:   energy flux by magneitic streatch 
!!                              B \cdot ((B \nabla) u)
!!
!!   temp_generation: heat advection flux   T (u \cdot \nabla) T
!!   part_temp_gen:  perturbation of heat advection flux
!!                                     \Theta (u \cdot \nabla) \Theta
!!   composition_gen:    composition advection flux 
!!                                     C (u \cdot \nabla) C
!!   pert_comp_advect:   perturbation of composition advection flux
!!                                     C (u \cdot \nabla) (C-C_0)
!!
!!   vis_ene_diffuse:  Energy dissipation by Viscousity
!!                                     u ( \nabla^{2} u)
!!   mag_ene_diffuse:  Energy dissipation by Ohmic dissipation
!!                                     B ( \nabla^{2} B)
!!
!!   pressure_work:  work of pressure gradient
!!                                     u ( \nabla p)
!!   m_potential_work: energy flux of scalar potential
!!                                     B ( \nabla \phi)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_energy_flux_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use t_base_force_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nene_flux = 16
!
!>        Field label of work of inertia
!!         @f$ u_{i} (u_{j} \partial_{j} u_{i}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_inertia_work = 'inertia_work'
!
!>        Field label of work against Lorentz force
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_work_agst_Lorentz = 'work_against_Lorentz'
!>        Field label of work of Lorentz force
!!         @f$ u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz_work =      'Lorentz_work'
!>        Field address of work of magnetic tension
!!         @f$ u_{i} (B_{j} \partial_{j}) B_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_tension_work =  'mag_tension_work'
!
!>        Field label of buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy_flux =     'buoyancy_flux'
!>        Field label of compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_comp_buo_flux =     'composite_buoyancy_flux'
!!
!>        Field label of magnetic energy flux
!>       @f$ B_{i}e_{ijk} \partial_{j} \left(e_{klm}u_{l}B_{m}\right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_ene_gen =       'magnetic_ene_generation'
!>        Field label of energy flux of magnetic stretch term
!!       @f$ u_{i} \left(B_{j} \partial_{j} u_{i} \right)@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_stretch_flux =   'magnetic_stretch_flux'
!
!>        Field label of temperature flux
!!         @f$ T (u_{i} \partial_{i}) T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_temp_generation =   'temp_generation'
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (u_{i} \partial_{i}) \Theta @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_temp_gen =     'part_temp_gen'
!>        Field label of composition flux
!!         @f$ C (u_{i} \partial_{i}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_comp_generation =   'comp_generation'
!>        Field label of perturbation composition flux
!!         @f$ (C - C_0) (u_{i} \partial_{i}) (C - C_0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_comp_gen =     'part_comp_gen'
!
!>        Field label of energy flux by viscous diffusion
!!         @f$ u_{i} \left( \partial_{j}\partial_{j} u_{i} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vis_ene_diffuse =   'vis_ene_diffuse'
!>        Field label of energy flux by magnetic diffusion
!!         @f$ B_{i} \left( \partial_{j}\partial_{j} B_{i} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_ene_diffuse =   'mag_ene_diffuse'
!
!>        Field label for potential in momentum euqaion
!!         @f$  \varphi @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_press_work =      'pressure_work'
!>        Field label for potential in induction euqaion
!!         @f$  \varphi @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_m_potential_work = 'm_potential_work'
!
!   --------------------------------------------------------------------
!
!>        Old Field label for buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy_work =     'buoyancy_work'
!
!>       Structure of start address of base forces
      type energy_flux_address
!>        Field address of work of inertia
!!         @f$ u_{i} (u_{j} \partial_{j} u_{i}) @f$
        integer (kind=kint) :: i_m_advect_work =   izero
!>        Field address of work against Lorentz force
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
        integer (kind=kint) :: i_nega_ujb =        izero
!>        Field address of work of Lorentz force
!!         @f$ u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
        integer (kind=kint) :: i_ujb =             izero
!>        Field address of work of magnetic tension
!!         @f$ u_{i} (B_{j} \partial_{j}) B_{i} @f$
        integer (kind=kint) :: i_m_tension_wk  =   izero
!>        Field address of buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
        integer (kind=kint) :: i_buo_gen =         izero
!>        Field address of compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} C @f$
        integer (kind=kint) :: i_c_buo_gen =       izero
!!
!>        Field address of magnetic energy flux
!>       @f$ B_{i}e_{ijk} \partial_{j} \left(e_{klm}u_{l}B_{m}\right) @f$
        integer (kind=kint) :: i_me_gen =           izero
!>        Field label of energy flux of magnetic stretch term
!!       @f$ u_{i} \left(B_{j} \partial_{j} u_{i} \right)@f$
        integer (kind=kint) :: i_mag_stretch_flux = izero
!
!>        Field address of temperature flux
!!         @f$ T (u_{i} \partial_{i}) T @f$
        integer (kind=kint) :: i_temp_gen =          izero
!>        Field address of perturbation temperature flux
!!         @f$ \Theta (u_{i} \partial_{i}) \Theta @f$
        integer (kind=kint) :: i_par_t_gen =         izero
!>        Field address of composition flux
!!         @f$ C (u_{i} \partial_{i}) @f$
        integer (kind=kint) :: i_comp_gen =          izero
!>        Field address of perturbation composition flux
!!         @f$ (C - C_0) (u_{i} \partial_{i}) (C - C_0) @f$
        integer (kind=kint) :: i_par_c_gen =         izero
!
!>        Field address of energy flux by viscous diffusion
!!         @f$ u_{i} \left( \partial_{j}\partial_{j} u_{i} \right) @f$
        integer (kind=kint) :: i_vis_e_diffuse =   izero
!>        Field address of energy flux by magnetic diffusion
!!         @f$ B_{i} \left( \partial_{j}\partial_{j} B_{i} \right) @f$
        integer (kind=kint) :: i_mag_e_diffuse =   izero
      end type energy_flux_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_enegy_fluxes(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_enegy_fluxes = .FALSE.
      if (    (field_name .eq. fhd_inertia_work)                        &
     &   .or. (field_name .eq. fhd_work_agst_Lorentz)                   &
     &   .or. (field_name .eq. fhd_Lorentz_work)                        &
     &   .or. (field_name .eq. fhd_mag_tension_work)                    &
     &   .or. (field_name .eq. fhd_buoyancy_flux)                       &
     &   .or. (field_name .eq. fhd_comp_buo_flux)                       &
     &   .or. (field_name .eq. fhd_mag_ene_gen)                         &
     &   .or. (field_name .eq. fhd_mag_stretch_flux)                    &
     &   .or. (field_name .eq. fhd_temp_generation)                     &
     &   .or. (field_name .eq. fhd_part_temp_gen)                       &
     &   .or. (field_name .eq. fhd_comp_generation)                     &
     &   .or. (field_name .eq. fhd_part_comp_gen)                       &
!     &   .or. (field_name .eq. fhd_vis_ene_diffuse)                    &
!     &   .or. (field_name .eq. fhd_mag_ene_diffuse)                    &
     &      )   check_enegy_fluxes = .TRUE.
!
      end function check_enegy_fluxes
!
! ----------------------------------------------------------------------
!
      subroutine set_enegy_fluxe_addresses                              &
     &         (i_phys, field_name, ene_flux, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: ene_flux
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes(field_name)
      if(flag) then
        if (field_name .eq. fhd_inertia_work) then
          ene_flux%i_m_advect_work = i_phys
        else if (field_name .eq. fhd_work_agst_Lorentz) then
          ene_flux%i_nega_ujb =      i_phys
        else if (field_name .eq. fhd_Lorentz_work) then
          ene_flux%i_ujb =           i_phys
        else if (field_name .eq. fhd_mag_tension_work) then
          ene_flux%i_m_tension_wk =  i_phys
!
        else if (field_name .eq. fhd_buoyancy_flux) then
          ene_flux%i_buo_gen =       i_phys
        else if (field_name .eq. fhd_comp_buo_flux) then
          ene_flux%i_c_buo_gen =     i_phys
!
        else if (field_name .eq. fhd_mag_ene_gen) then
          ene_flux%i_me_gen =           i_phys
        else if (field_name .eq. fhd_mag_stretch_flux) then
          ene_flux%i_mag_stretch_flux = i_phys
!
        else if (field_name .eq. fhd_temp_generation) then
          ene_flux%i_temp_gen =  i_phys
        else if (field_name .eq. fhd_part_temp_gen) then
          ene_flux%i_par_t_gen = i_phys
!
        else if (field_name .eq. fhd_comp_generation) then
          ene_flux%i_comp_gen =  i_phys
        else if (field_name .eq. fhd_part_comp_gen) then
          ene_flux%i_par_c_gen = i_phys
!
!        else if (field_name .eq. fhd_vis_ene_diffuse) then
!          ene_flux%i_vis_e_diffuse = i_phys
!        else if (field_name .eq. fhd_mag_ene_diffuse) then
!          ene_flux%i_mag_e_diffuse = i_phys
        end if
      end if
!
      end subroutine set_enegy_fluxe_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine energy_fluxes_monitor_address                          &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_ene_flux, ave_ene_flux, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(energy_flux_address), intent(inout) :: rms_ene_flux
      type(energy_flux_address), intent(inout) :: ave_ene_flux
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_enegy_fluxe_addresses                                    &
     &   ((numrms+1), field_name, rms_ene_flux, flag_r)
      call set_enegy_fluxe_addresses                                    &
     &   ((numave+1), field_name, ave_ene_flux, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine energy_fluxes_monitor_address
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_energy_fluxes()
      num_energy_fluxes = nene_flux
      return
      end function num_energy_fluxes
!
! ----------------------------------------------------------------------
!
      subroutine set_energy_flux_names(field_names)
!
      character(len = kchara), intent(inout) :: field_names(nene_flux)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_inertia_work), CHAR(0)
      write(field_names( 2),'(a,a1)')                                   &
     &                   trim(fhd_work_agst_Lorentz), CHAR(0)
      write(field_names( 3),'(a,a1)') trim(fhd_Lorentz_work), CHAR(0)
      write(field_names( 4),'(a,a1)')                                   &
     &                   trim(fhd_mag_tension_work), CHAR(0)
!
      write(field_names( 5),'(a,a1)') trim(fhd_buoyancy_flux), CHAR(0)
      write(field_names( 6),'(a,a1)') trim(fhd_comp_buo_flux), CHAR(0)
!
      write(field_names( 7),'(a,a1)') trim(fhd_mag_ene_gen), CHAR(0)
      write(field_names( 8),'(a,a1)')                                   &
     &                   trim(fhd_mag_stretch_flux), CHAR(0)
!
      write(field_names( 9),'(a,a1)')                                   &
     &                   trim(fhd_temp_generation), CHAR(0)
      write(field_names(10),'(a,a1)') trim(fhd_part_temp_gen), CHAR(0)
      write(field_names(11),'(a,a1)')                                   &
     &                   trim(fhd_comp_generation), CHAR(0)
      write(field_names(12),'(a,a1)') trim(fhd_part_comp_gen), CHAR(0)
!
      write(field_names(13),'(a,a1)') trim(fhd_vis_ene_diffuse), CHAR(0)
      write(field_names(14),'(a,a1)')                                   &
     &                   trim(fhd_mag_ene_diffuse), CHAR(0)
!
      write(field_names(15),'(a,a1)') trim(fhd_press_work), CHAR(0)
      write(field_names(16),'(a,a1)')                                   &
     &                   trim(fhd_m_potential_work), CHAR(0)
!
      end subroutine set_energy_flux_names
!
! ----------------------------------------------------------------------
!
      end module t_energy_flux_labels
