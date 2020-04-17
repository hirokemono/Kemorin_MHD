!>@file   m_energy_flux_w_sym_labels.f90
!!        module m_energy_flux_w_sym_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_ene_fluxes_w_sym(field_name)
!!      subroutine set_ene_flux_w_sym_addresses(i_phys, field_name,     &
!!     &          eflux_sym1_sym2, eflux_asym1_asym2,                   &
!!     &          eflux_sym1_asym2, eflux_asym1_sym2, flag)
!!        type(energy_flux_address), intent(inout) :: eflux_sym1_sym2
!!        type(energy_flux_address), intent(inout) :: eflux_asym1_asym2
!!        type(energy_flux_address), intent(inout) :: eflux_sym1_asym2
!!        type(energy_flux_address), intent(inout) :: eflux_asym1_sym2
!!
!!      subroutine ene_flux_w_sym_monitor_address                       &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_eflux_sym1_sym2, rms_eflux_asym1_asym2,           &
!!     &          rms_eflux_sym1_asym2, rms_eflux_asym1_sym2,           &
!!     &          ave_eflux_sym1_sym2, ave_eflux_asym1_asym2,           &
!!     &          ave_eflux_sym1_asym2, ave_eflux_asym1_sym2, flag)
!!        type(energy_flux_address),intent(inout):: rms_eflux_sym1_sym2
!!        type(energy_flux_address),intent(inout):: rms_eflux_asym1_asym2
!!        type(energy_flux_address),intent(inout):: rms_eflux_sym1_asym2
!!        type(energy_flux_address),intent(inout):: rms_eflux_asym1_sym2
!!        type(energy_flux_address),intent(inout):: ave_eflux_sym1_sym2
!!        type(energy_flux_address),intent(inout):: ave_eflux_asym1_asym2
!!        type(energy_flux_address),intent(inout):: ave_eflux_sym1_asym2
!!        type(energy_flux_address),intent(inout):: ave_eflux_asym1_sym2
!!
!!      integer(kind = kint) function num_ene_fluxes_w_symmetry()
!!      subroutine set_ene_flux_w_symmetry_names(field_names)
!!
!! !!!!!  energy flux names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   u_dot_wsym_x_usym, u_dot_wasym_x_uasym,
!!   u_dot_wsym_x_uasym, u_dot_wasym_x_usym:
!!          Work of Reynolds stress   u \cdot (\omega \times u)
!!   rev_u_dot_Jsym_x_Bsym, rev_u_dot_Jasym_x_Basym,
!!   rev_u_dot_Jsym_x_Basym, rev_u_dot_Jasym_x_Bsym:
!!          Work against Lorentz force       -u \cdot (J \times B)
!!   u_dot_Jsym_x_Bsym, u_dot_Jasym_x_Basym,
!!   u_dot_Jsym_x_Basym, u_dot_Jasym_x_Bsym:
!!          Work of Lorentz force             u \cdot (J \times B)
!!   u_dot_Bsym_nabla_Bsym, u_dot_Basym_nabla_Basym,
!!   u_dot_Bsym_nabla_Basym, u_dot_Basym_nabla_Bsym:
!!          Work of magnetic tension          u \cdot( (B \nabla) B)
!!
!!   sym_termal_buo_flux, asym_termal_buo_flux:
!!          Thermal buoyancy flux            -u \cdot (\alpha_{T} g T)
!!   sym_composite_buo_flux, asym_composite_buo_flux:
!!          Compositional buoyancy flux      -u \cdot (\alpha_{C} g C)
!!
!!   B_rot_Bsym_x_usym, B_rot_Basym_x_uasym,
!!   B_rot_Bsym_x_uasym, B_rot_Basym_x_usym:
!!         Energy flux by magneitic induction
!!                              B \cdot (\nabla \times (u \times B))
!!   B_dot_Bsym_nabla_usym, B_dot_Basym_nabla_uasym,
!!   B_dot_Bsym_nabla_uasym, B_dot_Basym_nabla_usym:
!!        Energy flux by magneitic streatch    B \cdot ((B \nabla) u)
!!
!!   T_usym_nabla_Tsym, T_uasym_nabla_Tasym,
!!   T_usym_nabla_Tasym, T_uasym_nabla_Tsym:
!!                       Heat advection flux   T (u \cdot \nabla) T
!!   pT_usym_nabla_pTsym, pT_uasym_nabla_pTasym,
!!   pT_usym_nabla_pTasym, pT_uasym_nabla_pTsym:
!!       Perturbation of heat advection flux
!!                                     \Theta (u \cdot \nabla) \Theta
!!   C_usym_nabla_Csym, C_uasym_nabla_Casym,
!!   C_usym_nabla_Casym, C_uasym_nabla_Csym:
!!       Composition advection flux            C (u \cdot \nabla) C
!!   pC_usym_nabla_pCsym, pC_uasym_nabla_pCasym,
!!   pC_usym_nabla_pCasym, pC_uasym_nabla_pCsym:
!!   pert_comp_advect:      perturbation of composition advection flux
!!                                   (C-C_0) (u \cdot \nabla) (C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_energy_flux_w_sym_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use t_base_force_labels
      use t_energy_flux_labels
      use m_field_w_symmetry_labels
      use m_force_w_sym_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: neflux_w_sym = 44
!
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_wsym_x_usym = 'u_dot_wsym_x_usym'
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_wasym_x_uasym = 'u_dot_wasym_x_uasym'
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_wsym_x_uasym = 'u_dot_wsym_x_uasym'
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_wasym_x_usym = 'u_dot_wasym_x_usym'
!
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_urev_Jsym_x_Bsym =    'rev_u_dot_Jsym_x_Bsym'
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_urev_Jasym_x_Basym =   'rev_u_dot_Jasym_x_Basym'
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_urev_Jsym_x_Basym =    'rev_u_dot_Jsym_x_Basym'
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_urev_Jasym_x_Bsym =    'rev_u_dot_Jasym_x_Bsym'
!
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Jsym_x_Bsym =        'u_dot_Jsym_x_Bsym'
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Jasym_x_Basym =      'u_dot_Jasym_x_Basym'
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Jsym_x_Basym =       'u_dot_Jsym_x_Basym'
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Jasym_x_Bsym =       'u_dot_Jasym_x_Bsym'
!
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Bsym_nabla_Bsym = 'u_dot_Bsym_nabla_Bsym'
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Basym_nabla_Basym = 'u_dot_Basym_nabla_Basym'
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Bsym_nabla_Basym = 'u_dot_Bsym_nabla_Basym'
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_u_dot_Basym_nabla_Bsym = 'u_dot_Basym_nabla_Bsym'
!
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_sym_buo_flux =    'sym_termal_buo_flux'
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_asym_buo_flux =   'asym_termal_buo_flux'
!
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_sym_comp_buo_flux =   'sym_composite_buo_flux'
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_asym_comp_buo_flux =  'asym_composite_buo_flux'
!
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{sym} \times  u_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_B_rot_Bsym_x_usym =   'B_rot_Bsym_x_usym'
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{asym} \times  u_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_B_rot_Basym_x_uasym = 'B_rot_Basym_x_uasym'
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{sym} \times  u_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_B_rot_Bsym_x_uasym =  'B_rot_Bsym_x_uasym'
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{asym} \times  u_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_B_rot_Basym_x_usym =  'B_rot_Basym_x_usym'
!
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{sym} \nabla) u_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_Bdot_Bsym_nabla_usym =   'B_dot_Bsym_nabla_usym'
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{a} \nabla) u_{asym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_Bdot_Basym_nabla_uasym = 'B_dot_Basym_nabla_uasym'
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{sym} \nabla) u_{asym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_Bdot_Bsym_nabla_uasym =  'B_dot_Bsym_nabla_uasym'
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{asym} \nabla) u_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_Bdot_Basym_nabla_usym =  'B_dot_Basym_nabla_usym'
!
!>        Field label of temperature flux
!!         @f$ T (u_{sim} \nabla) T_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_T_usym_nabla_Tsym =     'T_usym_nabla_Tsym'
!>        Field label of temperature flux
!!         @f$ T (u_{asym} \nabla) T_{asym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_T_uasym_nabla_Tasym =   'T_uasym_nabla_Tasym'
!>        Field label of temperature flux
!!         @f$ T (u_{sym} \nabla) T_{asym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_T_usym_nabla_Tasym =    'T_usym_nabla_Tasym'
!>        Field label of temperature flux
!!         @f$ T (u_{asym} \nabla) T_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_T_uasym_nabla_Tsym =    'T_uasym_nabla_Tsym'
!
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (u_{sym} \partial_{i}) \Theta_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pT_usym_nabla_pTsym =    'pT_usym_nabla_pTsym'
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (u_{asym} \partial_{i}) \Theta_{asym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pT_uasym_nabla_pTasym =  'pT_uasym_nabla_pTasym'
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (u_{sym} \partial_{i}) \Theta_{asym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pT_usym_nabla_pTasym =   'pT_usym_nabla_pTasym'
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (u_{asym} \partial_{i}) \Theta_{sym} @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pT_uasym_nabla_pTsym =   'pT_uasym_nabla_pTsym'
!
!>        Field label of composition flux
!!         @f$ C (u_{sim} \nabla C_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_C_usym_nabla_Csym =     'C_usym_nabla_Csym'
!>        Field label of composition flux
!!         @f$ C (u_{asym} \nabla C_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_C_uasym_nabla_Casym =   'C_uasym_nabla_Casym'
!>        Field label of composition flux
!!         @f$ C (u_{sym} \nabla C_{asym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_C_usym_nabla_Casym =    'C_usym_nabla_Casym'
!>        Field label of composition flux
!!         @f$ C (u_{asym} \nabla C_{sym}) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_C_uasym_nabla_Csym =    'C_uasym_nabla_Csym'
!
!>        Field label of composition flux
!!         @f$ (C - C_0) (u_{sym} \partial_{i}) (C_{sym} - C_0) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pC_usym_nabla_pCsym =    'pC_usym_nabla_pCsym'
!>        Field label of composition flux
!!         @f$ (C - C_0) (u_{asym} \partial_{i}) (C_{asym} - C_0) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pC_uasym_nabla_pCasym =  'pC_uasym_nabla_pCasym'
!>        Field label of composition flux
!!         @f$ (C - C_0) (u_{sym} \partial_{i} )(C_{asym} - C_0) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pC_usym_nabla_pCasym =   'pC_usym_nabla_pCasym'
!>        Field label of composition flux
!!         @f$ (C - C_0) (u_{asym} \partial_{i}) (C_{sym} - C_0) @f$
      character(len=kchara), parameter                                  &
     &     :: fhd_pC_uasym_nabla_pCsym =   'pC_uasym_nabla_pCsym'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_ene_fluxes_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_ene_fluxes_w_sym = .FALSE.
      if (    (field_name .eq. fhd_u_dot_wsym_x_usym)                   &
     &   .or. (field_name .eq. fhd_u_dot_wasym_x_uasym)                 &
     &   .or. (field_name .eq. fhd_u_dot_wsym_x_uasym)                  &
     &   .or. (field_name .eq. fhd_u_dot_wasym_x_usym)                  &
!
     &   .or. (field_name .eq. fhd_urev_Jsym_x_Bsym)                    &
     &   .or. (field_name .eq. fhd_urev_Jasym_x_Basym)                  &
     &   .or. (field_name .eq. fhd_urev_Jsym_x_Basym)                   &
     &   .or. (field_name .eq. fhd_urev_Jasym_x_Bsym)                   &
!
     &   .or. (field_name .eq. fhd_u_dot_Jsym_x_Bsym)                   &
     &   .or. (field_name .eq. fhd_u_dot_Jasym_x_Basym)                 &
     &   .or. (field_name .eq. fhd_u_dot_Jsym_x_Basym)                  &
     &   .or. (field_name .eq. fhd_u_dot_Jasym_x_Bsym)                  &
!
     &   .or. (field_name .eq. fhd_u_dot_Bsym_nabla_Bsym)               &
     &   .or. (field_name .eq. fhd_u_dot_Basym_nabla_Basym)             &
     &   .or. (field_name .eq. fhd_u_dot_Bsym_nabla_Basym)              &
     &   .or. (field_name .eq. fhd_u_dot_Basym_nabla_Bsym)              &
!
     &   .or. (field_name .eq. fhd_sym_buo_flux)                        &
     &   .or. (field_name .eq. fhd_asym_buo_flux)                       &
!
     &   .or. (field_name .eq. fhd_sym_comp_buo_flux)                   &
     &   .or. (field_name .eq. fhd_asym_comp_buo_flux)                  &
!
     &   .or. (field_name .eq. fhd_B_rot_Bsym_x_usym)                   &
     &   .or. (field_name .eq. fhd_B_rot_Basym_x_uasym)                 &
     &   .or. (field_name .eq. fhd_B_rot_Bsym_x_uasym)                  &
     &   .or. (field_name .eq. fhd_B_rot_Basym_x_usym)                  &
!
     &   .or. (field_name .eq. fhd_Bdot_Bsym_nabla_usym)                &
     &   .or. (field_name .eq. fhd_Bdot_Basym_nabla_uasym)              &
     &   .or. (field_name .eq. fhd_Bdot_Bsym_nabla_uasym)               &
     &   .or. (field_name .eq. fhd_Bdot_Basym_nabla_usym)               &
!
     &   .or. (field_name .eq. fhd_T_usym_nabla_Tsym)                   &
     &   .or. (field_name .eq. fhd_T_uasym_nabla_Tasym)                 &
     &   .or. (field_name .eq. fhd_T_usym_nabla_Tasym)                  &
     &   .or. (field_name .eq. fhd_T_uasym_nabla_Tsym)                  &
!
     &   .or. (field_name .eq. fhd_pT_usym_nabla_pTsym)                 &
     &   .or. (field_name .eq. fhd_pT_uasym_nabla_pTasym)               &
     &   .or. (field_name .eq. fhd_pT_usym_nabla_pTasym)                &
     &   .or. (field_name .eq. fhd_pT_uasym_nabla_pTsym)                &
!
     &   .or. (field_name .eq. fhd_C_usym_nabla_Csym)                   &
     &   .or. (field_name .eq. fhd_C_uasym_nabla_Casym)                 &
     &   .or. (field_name .eq. fhd_C_usym_nabla_Casym)                  &
     &   .or. (field_name .eq. fhd_C_uasym_nabla_Csym)                  &
!
     &   .or. (field_name .eq. fhd_pC_usym_nabla_pCsym)                 &
     &   .or. (field_name .eq. fhd_pC_uasym_nabla_pCasym)               &
     &   .or. (field_name .eq. fhd_pC_usym_nabla_pCasym)                &
     &   .or. (field_name .eq. fhd_pC_uasym_nabla_pCsym)                &
     &      )   check_ene_fluxes_w_sym = .TRUE.
!
      end function check_ene_fluxes_w_sym
!
! ----------------------------------------------------------------------
!
      subroutine set_ene_flux_w_sym_addresses(i_phys, field_name,       &
     &          eflux_sym1_sym2, eflux_asym1_asym2,                     &
     &          eflux_sym1_asym2, eflux_asym1_sym2, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_sym1_sym2
      type(energy_flux_address), intent(inout) :: eflux_asym1_asym2
      type(energy_flux_address), intent(inout) :: eflux_sym1_asym2
      type(energy_flux_address), intent(inout) :: eflux_asym1_sym2
      logical, intent(inout) :: flag
!
!
      flag = check_ene_fluxes_w_sym(field_name)
      if(flag) then
        if      (field_name .eq. fhd_u_dot_wsym_x_usym) then
          eflux_sym1_sym2%i_m_advect_work =   i_phys
        else if (field_name .eq. fhd_u_dot_wasym_x_uasym) then
          eflux_asym1_asym2%i_m_advect_work = i_phys
        else if (field_name .eq. fhd_u_dot_wsym_x_uasym) then
          eflux_sym1_asym2%i_m_advect_work =  i_phys
        else if (field_name .eq. fhd_u_dot_wasym_x_usym) then
          eflux_asym1_sym2%i_m_advect_work =  i_phys
!
        else if (field_name .eq. fhd_urev_Jsym_x_Bsym) then
          eflux_sym1_sym2%i_nega_ujb =     i_phys
        else if (field_name .eq. fhd_urev_Jasym_x_Basym) then
          eflux_asym1_asym2%i_nega_ujb =   i_phys
        else if (field_name .eq. fhd_urev_Jsym_x_Basym) then
          eflux_sym1_asym2%i_nega_ujb =    i_phys
        else if (field_name .eq. fhd_urev_Jasym_x_Bsym) then
          eflux_asym1_sym2%i_nega_ujb =    i_phys
!
        else if (field_name .eq. fhd_u_dot_Jsym_x_Bsym) then
          eflux_sym1_sym2%i_ujb =     i_phys
        else if (field_name .eq. fhd_u_dot_Jasym_x_Basym) then
          eflux_asym1_asym2%i_ujb =   i_phys
        else if (field_name .eq. fhd_u_dot_Jsym_x_Basym) then
          eflux_sym1_asym2%i_ujb =    i_phys
        else if (field_name .eq. fhd_u_dot_Jasym_x_Bsym) then
          eflux_asym1_sym2%i_ujb =    i_phys
!
        else if (field_name .eq. fhd_u_dot_Bsym_nabla_Bsym) then
          eflux_sym1_sym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. fhd_u_dot_Basym_nabla_Basym) then
          eflux_asym1_asym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. fhd_u_dot_Bsym_nabla_Basym) then
          eflux_sym1_asym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. fhd_u_dot_Basym_nabla_Bsym) then
          eflux_asym1_sym2%i_m_tension_wk =  i_phys
!
        else if (field_name .eq. fhd_sym_buo_flux) then
          eflux_sym1_sym2%i_buo_gen =   i_phys
        else if (field_name .eq. fhd_asym_buo_flux) then
          eflux_asym1_asym2%i_buo_gen =   i_phys
!
        else if (field_name .eq. fhd_sym_comp_buo_flux) then
          eflux_sym1_sym2%i_c_buo_gen =   i_phys
        else if (field_name .eq. fhd_asym_comp_buo_flux) then
          eflux_asym1_asym2%i_c_buo_gen =   i_phys
!
        else if (field_name .eq. fhd_B_rot_Bsym_x_usym) then
          eflux_sym1_sym2%i_me_gen =   i_phys
        else if (field_name .eq. fhd_B_rot_Basym_x_uasym) then
          eflux_asym1_asym2%i_me_gen = i_phys
        else if (field_name .eq. fhd_B_rot_Bsym_x_uasym) then
          eflux_sym1_asym2%i_me_gen =  i_phys
        else if (field_name .eq. fhd_B_rot_Basym_x_usym) then
          eflux_asym1_sym2%i_me_gen =  i_phys
!
        else if (field_name .eq. fhd_Bdot_Bsym_nabla_usym) then
          eflux_sym1_sym2%i_mag_stretch_flux =   i_phys
        else if (field_name .eq. fhd_Bdot_Basym_nabla_uasym) then
          eflux_asym1_asym2%i_mag_stretch_flux = i_phys
        else if (field_name .eq. fhd_Bdot_Bsym_nabla_uasym) then
          eflux_sym1_asym2%i_mag_stretch_flux =  i_phys
        else if (field_name .eq. fhd_Bdot_Basym_nabla_usym) then
          eflux_asym1_sym2%i_mag_stretch_flux =  i_phys
!
        else if (field_name .eq. fhd_T_usym_nabla_Tsym) then
          eflux_sym1_sym2%i_temp_gen =  i_phys
        else if (field_name .eq. fhd_T_uasym_nabla_Tasym) then
          eflux_asym1_asym2%i_temp_gen =  i_phys
        else if (field_name .eq. fhd_T_usym_nabla_Tasym) then
          eflux_sym1_asym2%i_temp_gen =  i_phys
        else if (field_name .eq. fhd_T_uasym_nabla_Tsym) then
          eflux_asym1_sym2%i_temp_gen =  i_phys
!
        else if (field_name .eq. fhd_pT_usym_nabla_pTsym) then
          eflux_sym1_sym2%i_par_t_gen = i_phys
        else if (field_name .eq. fhd_pT_uasym_nabla_pTasym) then
          eflux_asym1_asym2%i_par_t_gen = i_phys
        else if (field_name .eq. fhd_pT_usym_nabla_pTasym) then
          eflux_sym1_asym2%i_par_t_gen = i_phys
        else if (field_name .eq. fhd_pT_uasym_nabla_pTsym) then
          eflux_asym1_sym2%i_par_t_gen = i_phys
!
        else if (field_name .eq. fhd_C_usym_nabla_Csym) then
          eflux_sym1_sym2%i_comp_gen =    i_phys
        else if (field_name .eq. fhd_C_uasym_nabla_Casym) then
          eflux_asym1_asym2%i_comp_gen =  i_phys
        else if (field_name .eq. fhd_C_usym_nabla_Casym) then
          eflux_sym1_asym2%i_comp_gen =   i_phys
        else if (field_name .eq. fhd_C_uasym_nabla_Csym) then
          eflux_asym1_sym2%i_comp_gen =   i_phys
!
        else if (field_name .eq. fhd_pC_usym_nabla_pCsym) then
          eflux_sym1_sym2%i_par_c_gen =   i_phys
        else if (field_name .eq. fhd_pC_uasym_nabla_pCasym) then
          eflux_asym1_asym2%i_par_c_gen = i_phys
        else if (field_name .eq. fhd_pC_usym_nabla_pCasym) then
          eflux_sym1_asym2%i_par_c_gen =  i_phys
        else if (field_name .eq. fhd_pC_uasym_nabla_pCsym) then
          eflux_asym1_sym2%i_par_c_gen =  i_phys
        end if
      end if
!
      end subroutine set_ene_flux_w_sym_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ene_flux_w_sym_monitor_address                         &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_eflux_sym1_sym2, rms_eflux_asym1_asym2,             &
     &          rms_eflux_sym1_asym2, rms_eflux_asym1_sym2,             &
     &          ave_eflux_sym1_sym2, ave_eflux_asym1_asym2,             &
     &          ave_eflux_sym1_asym2, ave_eflux_asym1_sym2, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(energy_flux_address), intent(inout) :: rms_eflux_sym1_sym2
      type(energy_flux_address), intent(inout) :: rms_eflux_asym1_asym2
      type(energy_flux_address), intent(inout) :: rms_eflux_sym1_asym2
      type(energy_flux_address), intent(inout) :: rms_eflux_asym1_sym2
      type(energy_flux_address), intent(inout) :: ave_eflux_sym1_sym2
      type(energy_flux_address), intent(inout) :: ave_eflux_asym1_asym2
      type(energy_flux_address), intent(inout) :: ave_eflux_sym1_asym2
      type(energy_flux_address), intent(inout) :: ave_eflux_asym1_sym2
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_ene_flux_w_sym_addresses((numrms+1), field_name,         &
     &    rms_eflux_sym1_sym2, rms_eflux_asym1_asym2,                   &
     &    rms_eflux_sym1_asym2, rms_eflux_asym1_sym2, flag_r)
      call set_ene_flux_w_sym_addresses((numave+1), field_name,         &
     &    ave_eflux_sym1_sym2, ave_eflux_asym1_asym2,                   &
     &    ave_eflux_sym1_asym2, ave_eflux_asym1_sym2, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine ene_flux_w_sym_monitor_address
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_ene_fluxes_w_symmetry()
      num_ene_fluxes_w_symmetry = neflux_w_sym
      return
      end function num_ene_fluxes_w_symmetry
!
! ----------------------------------------------------------------------
!
      subroutine set_ene_flux_w_symmetry_names(field_names)
!
      character(len = kchara), intent(inout)                            &
     &                        :: field_names(neflux_w_sym)
!
!
      write(field_names( 1),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_wsym_x_usym), CHAR(0)
      write(field_names( 2),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_wasym_x_uasym), CHAR(0)
      write(field_names( 3),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_wsym_x_uasym), CHAR(0)
      write(field_names( 4),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_wasym_x_usym), CHAR(0)
!
      write(field_names( 5),'(a,a1)')                                   &
     &                  trim(fhd_urev_Jsym_x_Bsym), CHAR(0)
      write(field_names( 6),'(a,a1)')                                   &
     &                  trim(fhd_urev_Jasym_x_Basym), CHAR(0)
      write(field_names( 7),'(a,a1)')                                   &
     &                  trim(fhd_urev_Jsym_x_Basym), CHAR(0)
      write(field_names( 8),'(a,a1)')                                   &
     &                  trim(fhd_urev_Jasym_x_Bsym), CHAR(0)
!
      write(field_names( 9),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Jsym_x_Bsym), CHAR(0)
      write(field_names(10),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Jasym_x_Basym), CHAR(0)
      write(field_names(11),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Jsym_x_Basym), CHAR(0)
      write(field_names(12),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Jasym_x_Bsym), CHAR(0)
!
      write(field_names(13),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Bsym_nabla_Bsym), CHAR(0)
      write(field_names(14),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Basym_nabla_Basym), CHAR(0)
      write(field_names(15),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Bsym_nabla_Basym), CHAR(0)
      write(field_names(16),'(a,a1)')                                   &
     &                  trim(fhd_u_dot_Basym_nabla_Bsym), CHAR(0)
!
      write(field_names(17),'(a,a1)') trim(fhd_sym_buo_flux), CHAR(0)
      write(field_names(18),'(a,a1)') trim(fhd_asym_buo_flux), CHAR(0)
      write(field_names(19),'(a,a1)')                                   &
     &                  trim(fhd_sym_comp_buo_flux), CHAR(0)
      write(field_names(20),'(a,a1)')                                   &
     &                  trim(fhd_asym_comp_buo_flux), CHAR(0)
!
      write(field_names(21),'(a,a1)')                                   &
     &                  trim(fhd_B_rot_Basym_x_usym), CHAR(0)
      write(field_names(22),'(a,a1)')                                   &
     &                  trim(fhd_B_rot_Basym_x_uasym), CHAR(0)
      write(field_names(23),'(a,a1)')                                   &
     &                  trim(fhd_B_rot_Bsym_x_uasym), CHAR(0)
      write(field_names(24),'(a,a1)')                                   &
     &                  trim(fhd_B_rot_Basym_x_usym), CHAR(0)
!
      write(field_names(25),'(a,a1)')                                   &
     &                  trim(fhd_Bdot_Bsym_nabla_usym), CHAR(0)
      write(field_names(26),'(a,a1)')                                   &
     &                  trim(fhd_Bdot_Basym_nabla_uasym), CHAR(0)
      write(field_names(27),'(a,a1)')                                   &
     &                  trim(fhd_Bdot_Bsym_nabla_uasym), CHAR(0)
      write(field_names(28),'(a,a1)')                                   &
     &                  trim(fhd_Bdot_Basym_nabla_usym), CHAR(0)
!
      write(field_names(29),'(a,a1)')                                   &
     &                  trim(fhd_T_usym_nabla_Tsym), CHAR(0)
      write(field_names(30),'(a,a1)')                                   &
     &                  trim(fhd_T_uasym_nabla_Tasym), CHAR(0)
      write(field_names(31),'(a,a1)')                                   &
     &                  trim(fhd_T_usym_nabla_Tasym), CHAR(0)
      write(field_names(32),'(a,a1)')                                   &
     &                  trim(fhd_T_uasym_nabla_Tsym), CHAR(0)
!
      write(field_names(33),'(a,a1)')                                   &
     &                  trim(fhd_pT_usym_nabla_pTsym), CHAR(0)
      write(field_names(34),'(a,a1)')                                   &
     &                  trim(fhd_pT_uasym_nabla_pTasym), CHAR(0)
      write(field_names(35),'(a,a1)')                                   &
     &                  trim(fhd_pT_usym_nabla_pTasym), CHAR(0)
      write(field_names(36),'(a,a1)')                                   &
     &                  trim(fhd_pT_uasym_nabla_pTsym), CHAR(0)
!
      write(field_names(37),'(a,a1)')                                   &
     &                  trim(fhd_C_usym_nabla_Csym), CHAR(0)
      write(field_names(38),'(a,a1)')                                   &
     &                  trim(fhd_C_uasym_nabla_Casym), CHAR(0)
      write(field_names(39),'(a,a1)')                                   &
     &                  trim(fhd_C_usym_nabla_Casym), CHAR(0)
      write(field_names(40),'(a,a1)')                                   &
     &                  trim(fhd_C_uasym_nabla_Csym), CHAR(0)
!
      write(field_names(41),'(a,a1)')                                   &
     &                  trim(fhd_pC_usym_nabla_pCsym), CHAR(0)
      write(field_names(42),'(a,a1)')                                   &
     &                  trim(fhd_pC_uasym_nabla_pCasym), CHAR(0)
      write(field_names(43),'(a,a1)')                                   &
     &                  trim(fhd_pC_usym_nabla_pCasym), CHAR(0)
      write(field_names(44),'(a,a1)')                                   &
     &                  trim(fhd_pC_uasym_nabla_pCsym), CHAR(0)
!
      end subroutine set_ene_flux_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_energy_flux_w_sym_labels
