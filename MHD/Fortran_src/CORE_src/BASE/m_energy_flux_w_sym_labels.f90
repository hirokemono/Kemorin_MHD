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
!!      integer(kind = kint) function check_ene_flux_by_sym_sym_id      &
!!     &                    (i_field, field_name, base_fld,             &
!!     &                     force_sym1_sym2, eflux_sym1_sym2)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: force_sym1_sym2
!!        type(energy_flux_address), intent(in) :: eflux_sym1_sym2
!!      integer(kind = kint) function check_ene_flux_by_asym_asym_id    &
!!     &                   (i_field, field_name, base_fld,              &
!!     &                    force_asym1_asym2, eflux_asym1_asym2)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: force_asym1_asym2
!!        type(energy_flux_address), intent(in) :: eflux_asym1_asym2
!!      integer(kind = kint) function check_ene_flux_by_sym_asym_id     &
!!     &         (i_field, field_name, base_fld, force_sym1_asym2,      &
!!     &          force_asym1_sym2, eflux_sym1_asym2, eflux_asym1_sym2)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: force_sym1_asym2
!!        type(base_force_address), intent(in) :: force_asym1_sym2
!!        type(energy_flux_address), intent(in) :: eflux_sym1_asym2
!!        type(energy_flux_address), intent(in) :: eflux_asym1_sym2
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
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   inertia
!!                 :        inertia (\omega \times u)
!!   momentum_flux
!!                 :  momentum flux     u_{i} u_{j}
!!   Lorentz_force
!!                 :  Lorentz force     J \times B
!!   magnetic_tension
!!                 :  magnetic tension   (B \nabla) B
!!   maxwell_tensor_sym_sym, maxwell_tensor_asym_asym,
!!   maxwell_tensor_sym_asym
!!                 :  maxwell tensor       B_{i} B_{j}
!!
!!   sym_termal_buoyancy, asym_termal_buoyancy
!!                 :   Thermal buoyancy       - \alpha_{T} g T
!!   sym_composite_buoyancy, asym_composite_buoyancy
!!                 :   compositional buoyancy  - \alpha_{C} g C
!!
!!   usym_x_Bsym, uasym_x_Basym, usym_x_Basym, uasym_x_Bsym
!!                 :     induction                           u \times B
!!   Bsym_nabla_usym, Basym_nabla_uasym,
!!   Bsym_nabla_uasym, Basym_nabla_usym
!!                 :    magneitic streatch         (B \nabla) u
!!   usym_Bsym, uasym_Basym, usym_Basym
!!                 :    induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!
!!   usym_nabla_Tsym, uasym_nabla_Tasym,
!!   usym_nabla_Tasym, uasym_nabla_Tsym
!!                 :    heat advection     (u \cdot \nabla) T
!!   usym_nabla_pTsym, uasym_nabla_pTasym,
!!   usym_nabla_pTasym, uasym_nabla_pTsym
!!                 :  perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   heat_flux_sym_sym, heat_flux_asym_asym,
!!   heat_flux_sym_asym, heat_flux_asym_sym
!!                 :    heat flux                   uT
!!   part_h_flux_sym_sym, part_h_flux_asym_asym
!!   part_h_flux_sym_asym, part_h_flux_asym_sym
!!                 :  perturbation of heat flux   u\Theta
!!
!!   usym_nabla_Csym, uasym_nabla_Casym
!!   usym_nabla_Casym, uasym_nabla_Csym
!!                 :    composition advection     (u \cdot \nabla) C
!!   usym_nabla_pCsym, uasym_nabla_pCasym,
!!   usym_nabla_pCasym, uasym_nabla_pCsym
!!                 :  perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!   composite_flux_sym_sym, composite_flux_asym_asym, 
!!   composite_flux_sym_asym, composite_flux_asym_sym
!!                 :    composition flux                   uC
!!   part_c_flux_sym_sym, part_c_flux_asym_asym,
!!   part_c_flux_sym_asym, part_c_flux_asym_sym
!!                 :  perturbation of composition flux   u(C-C_0)
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
     &     :: fhd_urev_Jasym_x_Bsym =     'rev_u_dot_Jasym_x_Bsym'
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
      integer(kind = kint) function check_ene_flux_by_sym_sym_id        &
     &                    (i_field, field_name, base_fld,               &
     &                     force_sym1_sym2, eflux_sym1_sym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: force_sym1_sym2
      type(energy_flux_address), intent(in) :: eflux_sym1_sym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. eflux_sym1_sym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_m_advect, fhd_wsym_x_usym)
      else if( (i_field .eq. eflux_sym1_sym2%i_nega_ujb)                &
     &    .or. (i_field .eq. eflux_sym1_sym2%i_ujb)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_lorentz, fhd_Jsym_x_Bsym)
      else if( (i_field .eq. eflux_sym1_sym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_m_tension,                     &
     &                 fhd_Bsym_nabla_Bsym)
!
      else if( (i_field .eq. eflux_sym1_sym2%i_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_buoyancy, fhd_sym_buoyancy)
      else if( (i_field .eq. eflux_sym1_sym2%i_c_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_comp_buo, fhd_sym_comp_buo)
!
      else if((i_field .eq. eflux_sym1_sym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_induction,                     &
     &                 fhd_rot_usym_x_Bsym)
      else if((i_field .eq. eflux_sym1_sym2%i_mag_stretch_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_mag_stretch,                   &
     &                 fhd_Bsym_nabla_usym)
!
      else if( (i_field .eq. eflux_sym1_sym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_h_advect, fhd_usym_nabla_Tsym)
      else if( (i_field .eq. eflux_sym1_sym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_ph_advect,                     &
     &                 fhd_usym_nabla_pTsym)
!
      else if( (i_field .eq. eflux_sym1_sym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_c_advect, fhd_usym_nabla_Csym)
      else if( (i_field .eq. eflux_sym1_sym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_pc_advect,                     &
     &                 fhd_usym_nabla_pCsym)
      end if
      check_ene_flux_by_sym_sym_id = iflag
      return
!
      end function check_ene_flux_by_sym_sym_id
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_ene_flux_by_asym_asym_id      &
     &                   (i_field, field_name, base_fld,                &
     &                    force_asym1_asym2, eflux_asym1_asym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: force_asym1_asym2
      type(energy_flux_address), intent(in) :: eflux_asym1_asym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. eflux_asym1_asym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_m_advect, fhd_wasym_x_uasym)
      else if( (i_field .eq. eflux_asym1_asym2%i_nega_ujb)              &
     &    .or. (i_field .eq. eflux_asym1_asym2%i_ujb)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_lorentz, fhd_Jasym_x_Basym)
      else if((i_field .eq. eflux_asym1_asym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_m_tension,                   &
     &                 fhd_Basym_nabla_Basym)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_buoyancy, fhd_asym_buoyancy)
      else if( (i_field .eq. eflux_asym1_asym2%i_c_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_comp_buo, fhd_asym_comp_buo)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_induction,                   &
     &                 fhd_rot_uasym_x_Basym)
      else if( (i_field .eq. eflux_asym1_asym2%i_mag_stretch_flux))     &
     &    then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_mag_stretch,                 &
     &                 fhd_Basym_nabla_uasym)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_h_advect,                    &
     &                 fhd_uasym_nabla_Tasym)
      else if( (i_field .eq. eflux_asym1_asym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_ph_advect,                   &
     &                 fhd_uasym_nabla_pTasym)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_c_advect,                    &
     &                 fhd_uasym_nabla_Casym)
      else if( (i_field .eq. eflux_asym1_asym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_pc_advect,                   &
     &                 fhd_uasym_nabla_pCasym)
      end if
      check_ene_flux_by_asym_asym_id = iflag
      return
!
      end function check_ene_flux_by_asym_asym_id
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_ene_flux_by_sym_asym_id       &
     &         (i_field, field_name, base_fld, force_sym1_asym2,        &
     &          force_asym1_sym2, eflux_sym1_asym2, eflux_asym1_sym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: force_sym1_asym2
      type(base_force_address), intent(in) :: force_asym1_sym2
      type(energy_flux_address), intent(in) :: eflux_sym1_asym2
      type(energy_flux_address), intent(in) :: eflux_asym1_sym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. eflux_sym1_asym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_m_advect, fhd_wsym_x_uasym)
      else if( (i_field .eq. eflux_sym1_asym2%i_nega_ujb)               &
     &    .or. (i_field .eq. eflux_sym1_asym2%i_ujb)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_lorentz, fhd_Jsym_x_Basym)
      else if( (i_field .eq. eflux_sym1_asym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_m_tension,                    &
     &                 fhd_Bsym_nabla_Basym)
!
      else if( (i_field .eq. eflux_sym1_asym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_induction,                    &
     &                 fhd_rot_usym_x_Basym)
      else if( (i_field .eq. eflux_sym1_asym2%i_mag_stretch_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_mag_stretch,                  &
     &                 fhd_Bsym_nabla_uasym)
!
      else if( (i_field .eq. eflux_sym1_asym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_h_advect,                     &
     &                 fhd_usym_nabla_Tasym)
      else if( (i_field .eq. eflux_sym1_asym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_ph_advect,                    &
     &                 fhd_usym_nabla_pTasym)
!
      else if( (i_field .eq. eflux_sym1_asym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_c_advect,                     &
     &                 fhd_usym_nabla_Casym)
      else if( (i_field .eq. eflux_sym1_asym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_pc_advect,                    &
     &                 fhd_usym_nabla_pCasym)
!
!
      else if( (i_field .eq. eflux_asym1_sym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_m_advect, fhd_wasym_x_uaym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_nega_ujb)               &
     &    .or. (i_field .eq. eflux_asym1_sym2%i_ujb) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_lorentz, fhd_Jasym_x_Bsym)
      else if( (i_field .eq. eflux_asym1_sym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_m_tension,                    &
     &                 fhd_Basym_nabla_Bsym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_induction,                    &
     &                 fhd_rot_uasym_x_Bsym)
      else if( (i_field .eq. eflux_asym1_sym2%i_mag_stretch_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_mag_stretch,                  &
     &                 fhd_Basym_nabla_usym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_h_advect,                     &
     &                 fhd_usym_nabla_Tasym)
      else if( (i_field .eq. eflux_asym1_sym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_ph_advect,                    &
     &                 fhd_uasym_nabla_pTsym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_c_advect,                     &
     &                 fhd_uasym_nabla_Csym)
      else if( (i_field .eq. eflux_asym1_sym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_pc_advect,                    &
     &                 fhd_uasym_nabla_pCsym)
      end if
      check_ene_flux_by_sym_asym_id = iflag
      return
!
      end function check_ene_flux_by_sym_asym_id
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
!
      end module m_energy_flux_w_sym_labels
