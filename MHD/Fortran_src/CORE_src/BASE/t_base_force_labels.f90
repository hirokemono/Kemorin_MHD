!>@file   t_base_force_labels.f90
!!        module t_base_force_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_base_forces(field_name)
!!      subroutine set_base_force_addresses                             &
!!     &         (i_phys, field_name, base_force, flag)
!!      integer(kind = kint) function check_base_force_id               &
!!     &                    (i_field, field_name, base_fld, base_force)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: base_force
!!
!!      subroutine base_force_monitor_address                           &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_force, ave_force, flag)
!!        type(base_force_address), intent(inout) :: rms_force
!!        type(base_force_address), intent(inout) :: ave_force
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   pressure_gradient:     pressure gradient    u
!!   inertia:        inertia (\omega \times u)
!!   momentum_flux:  momentum flux     u_{i} u_{j}
!!   Lorentz_force:  Lorentz force     J \times B
!!   magnetic_tension:  magnetic tension   (B \nabla) B
!!   maxwell_tensor:  maxwell tensor       B_{i} B_{j}
!!   Coriolis_force:  Coriolis force     2 \Omega \times u
!!   buoyancy:   Thermal buoyancy       - \alpha_{T} g T
!!   composite_buoyancy:   compositional buoyancy  - \alpha_{C} g C
!!
!!   vecp_induction:     induction                           u \times B
!!   magnetic_induction:  magneitic induction \nabla \times (u \times B)
!!   magnetic_stretch:    magneitic streatch         (B \nabla) u
!!   induction_tensor:    induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!   electric_field:   electric field              E
!!   poynting_flux:    poynting flux      E \times B
!!
!!   heat_advect:    heat advection     (u \cdot \nabla) T
!!   part_h_advect:  perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   heat_flux:    heat flux                   uT
!!   part_h_flux:  perturbation of heat flux   u\Theta
!!
!!   composition_advect:    composition advection     (u \cdot \nabla) C
!!   part_c_advect:  perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!   composite_flux:    composition flux                   uC
!!   part_c_flux:  perturbation of composition flux   u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_base_force_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
!>        Field label for pressure gradient
!!         @f$ \partial_{i} p @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_press_grad = 'pressure_gradient'
!>        Field label for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i} @f$
      character(len=kchara), parameter :: fhd_inertia = 'inertia'
!>        Field label for momentum flux
!!         @f$ u_{i} u_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mom_flux =      'momentum_flux'
!>        Field label for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz =       'Lorentz_force'
!>        start address for magnetic tension
!!         @f$ B_{j} \partial_{j} B_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_tension = 'magnetic_tension'
!>        Field label for Maxwell tensor
!!         @f$ B_{i} B_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_maxwell_t =     'maxwell_tensor'
!>        Field label for Coriolis force
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Coriolis =      'Coriolis_force'
!>        Field label for buoyancy
!!         @f$ -\alpha_{T} g_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy =      'buoyancy'
!>        Field label for compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_comp_buo =      'composite_buoyancy'
!!
!>        Field label for induction for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vp_induct =     'vecp_induction'
!>        Field label for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_induct =    'magnetic_induction'
!>        Field label for magnetic stretch term
!!         @f$ \left(B_{i} \partial_{k} u_{k} \right)@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_stretch =   'magnetic_stretch'
!>        Field label for Tensor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_induct_t =      'induction_tensor'
!>        Field label for electric field
!!         @f$ E_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_e_field = 'electric_field'
!>        Field label for poynting flux
!!         @f$  e_{ijk} E_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_poynting = 'poynting_flux'
!
!>        Field label for advection for temperature
!!         @f$ u_{i} \partial_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_heat_advect =       'heat_advect'
!>        Field label for advection for perturbation of temperature
!!         @f$ u_{i} \partial_{i} \Theta @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_h_advect =     'part_h_advect'
!>        Field label for heat flux
!!         @f$ u_{i} T @f$
      character(len=kchara), parameter :: fhd_h_flux = 'heat_flux'
!>        Field label for perturbation of heat flux
!!         @f$ u_{i} \Theta @f$
      character(len=kchara), parameter :: fhd_ph_flux = 'part_h_flux'
!
!>        Field label for advection for composition
!!         @f$ u_{i} \partial_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_composit_advect =    'composition_advect'
!>        Field label for advection for perturbation of composition
!!         @f$ u_{i} \partial_{i} \Theta_C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_c_advect =     'part_c_advect'
!>        Field label for compositinoal flux
!!         @f$ u_{i} C @f$
      character(len=kchara), parameter :: fhd_c_flux = 'composite_flux'
!>        Field label for perturbation of composition flux
!!         @f$ u_{i} \Theta_C @f$
      character(len=kchara), parameter :: fhd_pc_flux = 'part_c_flux'
!
!>       Structure for start address for base forces
      type base_force_address
!>        Start address for pressure gradient
!!         @f$ \partial_{i} p @f$
        integer (kind=kint) :: i_press_grad  =     izero
!>        start address for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i} @f$
        integer (kind=kint) :: i_m_advect =        izero
!>        start address for magnetic tension
!!         @f$ B_{j} \partial_{j} B_{i} @f$
        integer (kind=kint) :: i_m_tension =       izero
!>        start address for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k} @f$
        integer (kind=kint) :: i_lorentz =         izero
!>        start address for Coriolis force
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} @f$
        integer (kind=kint) :: i_coriolis =        izero
!>        start address for buoyancy
!!         @f$ -\alpha_{T} g_{i} T @f$
        integer (kind=kint) :: i_buoyancy =        izero
!>        start address for compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C @f$
        integer (kind=kint) :: i_comp_buo =        izero
!
!>        start address for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
        integer (kind=kint) :: i_induction =       izero
!>        start address for induction for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
        integer (kind=kint) :: i_vp_induct =       izero
!>        start address for magnetic stretch term
!!         @f$ B_{i} \partial_{k} \u_{k} \right)@f$
        integer (kind=kint) :: i_mag_stretch =     izero
!>        start address for electric field
!!         @f$ E_{i} @f$
        integer (kind=kint) :: i_electric =        izero
!>        start address for poynting flux
!!         @f$  e_{ijk} E_{j} B_{k} @f$
        integer (kind=kint) :: i_poynting =        izero
!
!>        start address for advection for temperature
!!         @f$ u_{i} \partial_{i} T @f$
        integer (kind=kint) :: i_h_advect =        izero
!>        start address for advection for perturbation of temperature
!!         @f$ u_{i} \partial_{i} \Theta @f$
        integer (kind=kint) :: i_ph_advect =       izero
!>        start address for advection for composition
!!         @f$ u_{i} \partial_{i} C @f$
        integer (kind=kint) :: i_c_advect =        izero
!>        start address for advection for perturbation of composition
!!         @f$ u_{i} \partial_{i} \Theta_C @f$
        integer (kind=kint) :: i_pc_advect =       izero
!!
!>        start address for heat flux
!!         @f$ u_{i} T @f$
        integer (kind=kint) :: i_h_flux =          izero
!>        start address for perturbation of heat flux
!!         @f$ u_{i} \Theta @f$
        integer (kind=kint) :: i_ph_flux =         izero
!>        start address for compositinoal flux
!!         @f$ u_{i} C @f$
        integer (kind=kint) :: i_c_flux =          izero
!>        start address for compositinoal flux
!!         @f$ u_{i} \Theta_{C} @f$
        integer (kind=kint) :: i_pc_flux =          izero
!>        start address for momentum flux
!!         @f$ u_{i} u_{j} @f$
        integer (kind=kint) :: i_m_flux =          izero
!>        start address for Maxwell tensor
!!         @f$ B_{i} B_{j} @f$
        integer (kind=kint) :: i_maxwell =         izero
!>        start address for TEnsor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
        integer (kind=kint) :: i_induct_t =        izero
      end type base_force_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_base_forces(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_forces = .FALSE.
      if (    (field_name .eq. fhd_press_grad)                          &
     &   .or. (field_name .eq. fhd_inertia)                             &
     &   .or. (field_name .eq. fhd_mom_flux)                            &
     &   .or. (field_name .eq. fhd_Coriolis)                            &
     &   .or. (field_name .eq. fhd_Lorentz)                             &
     &   .or. (field_name .eq. fhd_mag_tension)                         &
     &   .or. (field_name .eq. fhd_maxwell_t)                           &
     &   .or. (field_name .eq. fhd_e_field)                             &
     &   .or. (field_name .eq. fhd_poynting)                            &
     &   .or. (field_name .eq. fhd_buoyancy)                            &
     &   .or. (field_name .eq. fhd_comp_buo)                            &
     &   .or. (field_name .eq. fhd_vp_induct)                           &
     &   .or. (field_name .eq. fhd_mag_induct)                          &
     &   .or. (field_name .eq. fhd_mag_stretch)                         &
     &   .or. (field_name .eq. fhd_induct_t)                            &
     &   .or. (field_name .eq. fhd_heat_advect)                         &
     &   .or. (field_name .eq. fhd_part_h_advect)                       &
     &   .or. (field_name .eq. fhd_h_flux)                              &
     &   .or. (field_name .eq. fhd_ph_flux)                             &
     &   .or. (field_name .eq. fhd_composit_advect)                     &
     &   .or. (field_name .eq. fhd_part_c_advect)                       &
     &   .or. (field_name .eq. fhd_c_flux)                              &
     &   .or. (field_name .eq. fhd_pc_flux)                             &
     &      )   check_base_forces = .TRUE.
!
      end function check_base_forces
!
! ----------------------------------------------------------------------
!
      subroutine set_base_force_addresses                               &
     &         (i_phys, field_name, base_force, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: base_force
      logical, intent(inout) :: flag
!
!
      flag = check_base_forces(field_name)
      if(flag) then
        if (field_name .eq. fhd_press_grad) then
          base_force%i_press_grad = i_phys
        else if (field_name .eq. fhd_inertia) then
          base_force%i_m_advect =   i_phys
        else if (field_name .eq. fhd_mom_flux ) then
          base_force%i_m_flux =     i_phys
        else if (field_name .eq. fhd_Coriolis) then
          base_force%i_coriolis =   i_phys
        else if (field_name .eq. fhd_Lorentz) then
          base_force%i_lorentz =    i_phys
        else if (field_name .eq. fhd_mag_tension) then
          base_force%i_m_tension =  i_phys
        else if (field_name .eq. fhd_maxwell_t ) then
          base_force%i_maxwell =    i_phys
!
        else if (field_name .eq. fhd_e_field) then
          base_force%i_electric = i_phys
        else if (field_name .eq. fhd_poynting) then
          base_force%i_poynting = i_phys
!
        else if (field_name .eq. fhd_buoyancy) then
          base_force%i_buoyancy =   i_phys
        else if (field_name .eq. fhd_comp_buo) then
          base_force%i_comp_buo =   i_phys
!
        else if (field_name .eq. fhd_vp_induct) then
          base_force%i_vp_induct =   i_phys
        else if (field_name .eq. fhd_mag_induct) then
          base_force%i_induction =   i_phys
        else if (field_name .eq. fhd_mag_stretch) then
          base_force%i_mag_stretch = i_phys
        else if (field_name .eq. fhd_induct_t ) then
          base_force%i_induct_t =    i_phys
!
        else if (field_name .eq. fhd_heat_advect) then
          base_force%i_h_advect =  i_phys
        else if (field_name .eq. fhd_part_h_advect) then
          base_force%i_ph_advect = i_phys
        else if (field_name .eq. fhd_h_flux) then
          base_force%i_h_flux =    i_phys
        else if (field_name .eq. fhd_ph_flux) then
          base_force%i_ph_flux =   i_phys
!
        else if (field_name .eq. fhd_composit_advect) then
          base_force%i_c_advect =  i_phys
        else if (field_name .eq. fhd_part_c_advect) then
          base_force%i_pc_advect = i_phys
        else if (field_name .eq. fhd_c_flux) then
          base_force%i_c_flux =    i_phys
        else if (field_name .eq. fhd_pc_flux) then
          base_force%i_pc_flux =   i_phys
        end if
      end if
!
      end subroutine set_base_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_base_force_id                 &
     &                    (i_field, field_name, base_fld, base_force)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: base_force
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. base_force%i_press_grad)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. base_force%i_m_advect)                     &
     &    .or. (i_field .eq. base_force%i_m_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_vort, fhd_vort)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. base_force%i_lorentz)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_current, fhd_current)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. base_force%i_maxwell)                      &
     &    .or. (i_field .eq. base_force%i_m_tension)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. base_force%i_coriolis)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. base_force%i_buoyancy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. base_force%i_comp_buo)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_light, fhd_light)
!
      else if( (i_field .eq. base_force%i_vp_induct)                    &
     &    .or. (i_field .eq. base_force%i_mag_stretch)                  &
     &    .or. (i_field .eq. base_force%i_induct_t)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. base_force%i_induction)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_vp_induct, fhd_vp_induct)
      else if( (i_field .eq. base_force%i_electric)                     &
     &    .or. (i_field .eq. base_force%i_poynting)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_vp_induct, fhd_vp_induct)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_current, fhd_current)
!
      else if( (i_field .eq. base_force%i_h_advect)                     &
     &    .or. (i_field .eq. base_force%i_h_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. base_force%i_ph_advect)                    &
     &    .or. (i_field .eq. base_force%i_ph_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                           base_fld%i_par_temp, fhd_part_temp)
      else if( (i_field .eq. base_force%i_c_advect)                     &
     &    .or. (i_field .eq. base_force%i_c_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_light, fhd_light)
      else if( (i_field .eq. base_force%i_pc_advect)                    &
     &    .or. (i_field .eq. base_force%i_pc_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                           base_fld%i_par_light, fhd_part_light)
      end if
      check_base_force_id = iflag
      return
!
      end function check_base_force_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine base_force_monitor_address                             &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_force, ave_force, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_force_address), intent(inout) :: rms_force
      type(base_force_address), intent(inout) :: ave_force
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_base_force_addresses                                     &
     &   ((numrms+1), field_name, rms_force, flag_r)
      call set_base_force_addresses                                     &
     &   ((numave+1), field_name, ave_force, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine base_force_monitor_address
!
! ----------------------------------------------------------------------
!
      end module t_base_force_labels
