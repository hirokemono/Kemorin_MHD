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
!!      integer(kind = kint) function num_ene_fluxes_w_symmetry()
!!      subroutine set_ene_flux_w_symmetry_names(n_comps, names, maths)
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
      use m_phys_constants
      use t_field_labels
      use t_energy_flux_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: neflux_w_sym = 44
!
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{sym}) @f$
      type(field_def), parameter :: u_dot_wsym_x_usym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wsym_x_usym',                              &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{symi})$, '   &
     &             //' $ u \cdot (\omega_{sym} \times u_{sym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{asym}) @f$
      type(field_def), parameter :: u_dot_wasym_x_uasym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wasym_x_uasym',                            &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{asymi})$, ' &
     &             //' $ u \cdot (\omega_{asym} \times u_{asym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{symj} \partial_{j} u_{asymi}) @f$
!!         or @f$ u \cdot (\omega_{sym} \times u_{asym}) @f$
      type(field_def), parameter :: u_dot_wsym_x_uasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wsym_x_uasym',                             &
     &         math = '$ u \cdot (u_{symj} \partial_{j} u_{asymi})$, '  &
     &             //' $ u \cdot (\omega_{sym} \times u_{asym})$')
!>        Field label of work of inertia
!!         @f$ u \cdot (u_{asymj} \partial_{j} u_{symi}) @f$
!!         or @f$ u \cdot (\omega_{asym} \times u_{sym}) @f$
      type(field_def), parameter :: u_dot_wasym_x_usym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &         name = 'u_dot_wasym_x_usym',                             &
     &         math = '$ u \cdot (u_{asymj} \partial_{j} u_{symi})$, '  &
     &             //' $ u \cdot (\omega_{asym} \times u_{sym})$')
!
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: rev_u_dot_Jsym_x_Bsym               &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jsym_x_Bsym',                       &
     &            math = '$ -u \cdot (J_{sym} \times B_{sym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: rev_u_dot_Jasym_x_Basym             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jasym_x_Basym',                     &
     &            math = '$ -u \cdot (J_{asym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: rev_u_dot_Jsym_x_Basym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jsym_x_Basym',                      &
     &            math = '$ -u \cdot (J_{sym} \times B_{asym}) $')
!>        Field label of work against Lorentz force
!!         @f$ -u \cdot (J_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: rev_u_dot_Jasym_x_Bsym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'rev_u_dot_Jasym_x_Bsym',                      &
     &            math = '$ -u \cdot (J_{asym} \times B_{sym}) $')
!
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{sym}) @f$
      type(field_def), parameter :: u_dot_Jsym_x_Bsym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jsym_x_Bsym',                           &
     &            math = '$ u \cdot (J_{sym} \times B_{sym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{asym}) @f$
      type(field_def), parameter :: u_dot_Jasym_x_Basym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jasym_x_Basym',                         &
     &            math = '$ u \cdot (J_{asym} \times B_{asym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{sym} \times B_{asym}) @f$
      type(field_def), parameter :: u_dot_Jsym_x_Basym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jsym_x_Basym',                          &
     &            math = '$ u \cdot (J_{sym} \times B_{asym}) $')
!>        Field label of work of Lorentz force
!!         @f$ u \cdot (J_{asym} \times B_{sym}) @f$
      type(field_def), parameter :: u_dot_Jasym_x_Bsym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Jasym_x_Bsym',                          &
     &            math = '$ u \cdot (J_{asym} \times B_{sym}) $')
!
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Bsym_nabla_Bsym               &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Bsym_nabla_Bsym',                       &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Basym_nabla_Basym             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Basym_nabla_Basym',                     &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Bsym_nabla_Basym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Bsym_nabla_Basym',                      &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!>        Field address of work of magnetic tension
!!         @f$ u \cdot (B_{sym} \cdot \nabla) B_{sym} @f$
      type(field_def), parameter :: u_dot_Basym_nabla_Bsym              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'u_dot_Basym_nabla_Bsym',                      &
     &            math = '$ u \cdot (B_{sym} \cdot \nabla) B_{sym} $')
!
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{sym}) @f$
      type(field_def), parameter :: sym_termal_buo_flux                 &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'sym_termal_buo_flux',                         &
     &            math = '$ -u \cdot (\alpha_{T} g_{i} T_{sym}) $')
!>        Field label of buoyancy flux
!!         @f$ -u \cdot (\alpha_{T} g_{i} T_{asym}) @f$
      type(field_def), parameter :: asym_termal_buo_flux                &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'asym_termal_buo_flux',                        &
     &            math = '$ -u \cdot (\alpha_{T} g_{i} T_{asym}) $')
!
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{sym}) @f$
      type(field_def), parameter :: sym_composite_buo_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'sym_composite_buo_flux',                      &
     &            math = '$ -u \cdot (\alpha_{C} g_{i} C_{sym}) $')
!>        Field label of compositional buoyancy flux
!!         @f$ -u \cdot (\alpha_{C} g_{i} C_{asym}) @f$
      type(field_def), parameter :: asym_composite_buo_flux             &
     &    = field_def(n_comp = n_scalar,                                &
     &            name = 'asym_composite_buo_flux',                     &
     &            math = '$ -u \cdot (\alpha_{C} g_{i} C_{asym}) $')
!
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{sym} \times  u_{sym}) @f$
      type(field_def), parameter :: B_rot_Bsym_x_usym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Bsym_x_usym',                     &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{sym} \times  u_{sym}) $')
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{asym} \times  u_{asym}) @f$
      type(field_def), parameter :: B_rot_Basym_x_uasym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Basym_x_uasym',                   &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{asym} \times  u_{asym}) $')
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{sym} \times  u_{asym}) @f$
      type(field_def), parameter :: B_rot_Bsym_x_uasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Bsym_x_uasym',                    &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{sym} \times  u_{asym}) $')
!>        Field label of magnetic energy flux
!!         @f$ B \cdot \nabla \times (B_{asym} \times  u_{sym}) @f$
      type(field_def), parameter :: B_rot_Basym_x_usym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'B_rot_Basym_x_usym',                    &
     &                  math = '$ B \cdot \nabla \times'                &
     &                      // ' (B_{asym} \times  u_{sym}) $')
!
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{sym} \cdot \nabla \right) u_{sym} @f$
      type(field_def), parameter :: B_dot_Bsym_nabla_usym               &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Bsym_nabla_usym',                        &
     &           math = '$ B \cdot \left(B_{sym} \cdot'                 &
     &                // ' \nabla \right) u_{sym} $')
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{asym} \cdot \nabla \right) u_{asym} @f$
      type(field_def), parameter :: B_dot_Basym_nabla_uasym             &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Basym_nabla_uasym',                      &
     &           math = '$ B \cdot \left(B_{asym} \cdot'                &
     &                // ' \nabla \right) u_{asym} $')
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{sym} \cdot \nabla \right) u_{asym} @f$
      type(field_def), parameter :: B_dot_Bsym_nabla_uasym              &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Bsym_nabla_uasym',                       &
     &           math = '$ B \cdot \left(B_{sym} \cdot'                 &
     &                // ' \nabla \right) u_{asym} $')
!>        Field label of energy flux of magnetic stretch term
!!         @f$ B \cdot \left(B_{asym} \cdot \nabla \right) u_{sym} @f$
      type(field_def), parameter :: B_dot_Basym_nabla_usym              &
     &    = field_def(n_comp = n_scalar,                                &
     &           name = 'B_dot_Basym_nabla_usym',                       &
     &           math = '$ B \cdot \left(B_{asym} \cdot'                &
     &                // ' \nabla \right) u_{sym} $')
!
!>        Field label of temperature flux
!!         @f$ T \left(u_{sim} \cdot \nabla \right) T_{sym} @f$
      type(field_def), parameter :: T_usym_nabla_Tsym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_usym_nabla_Tsym',                     &
     &                  math = '$ T \left(u_{sim} \cdot \nabla'         &
     &                       // ' \right) T_{sym}  $')
!>        Field label of temperature flux
!!         @f$ T \left(u_{asym} \cdot \nabla \right) T_{asym} @f$
      type(field_def), parameter :: T_uasym_nabla_Tasym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_uasym_nabla_Tasym',                   &
     &                  math = '$ T \left(u_{asym} \cdot \nabla'        &
     &                       // ' \right) T_{asym}  $')
!>        Field label of temperature flux
!!         @f$ T \left(u_{sym} \cdot \nabla \right) T_{asym} @f$
      type(field_def), parameter :: T_usym_nabla_Tasym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_usym_nabla_Tasym',                    &
     &                  math = '$ T \left(u_{sym} \cdot \nabla'         &
     &                       // ' \right) T_{asym}  $')
!>        Field label of temperature flux
!!         @f$ T \left(u_{asym} \cdot \nabla \right) T_{sym} @f$
      type(field_def), parameter :: T_uasym_nabla_Tsym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'T_uasym_nabla_Tsym',                    &
     &                  math = '$ T \left(u_{asym} \cdot \nabla'        &
     &                       // ' \right) T_{sym}  $')
!
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{sym} \cdot \nabla \right) \Theta_{sym} @f$
      type(field_def), parameter :: pT_usym_nabla_pTsym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_usym_nabla_pTsym',                              &
     &       math = '$ \Theta \left(u_{sym} \cdot \nabla'               &
     &           // ' \right) \Theta_{sym} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{asym} \cdot \nabla \right)
!!             \Theta_{asym} @f$
      type(field_def), parameter :: pT_uasym_nabla_pTasym               &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_uasym_nabla_pTasym',                            &
     &       math = '$ \Theta \left(u_{asym} \cdot \nabla'              &
     &           // ' \right) \Theta_{asym} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{sym} \cdot \nabla \right)
!!             \Theta_{asym} @f$
      type(field_def), parameter :: pT_usym_nabla_pTasym                &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_usym_nabla_pTasym',                             &
     &       math = '$ \Theta \left(u_{sym} \cdot \nabla'               &
     &           // '  \right) \Theta_{asym} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta \left(u_{asym} \cdot \nabla \right)
!!             \Theta_{sym} @f$
      type(field_def), parameter :: pT_uasym_nabla_pTsym                &
     &    = field_def(n_comp = n_scalar,                                &
     &       name = 'pT_uasym_nabla_pTsym',                             &
     &       math = '$ \Theta \left(u_{asym} \cdot \nabla'              &
     &           // '  \right) \Theta_{sym} $')
!
!>        Field label of composition flux
!!         @f$ C \left(u_{sim} \cdot \nabla C_{sym} \right) @f$
      type(field_def), parameter :: C_usym_nabla_Csym                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_usym_nabla_Csym',                     &
     &                  math = '$ C \left(u_{sim} \cdot \nabla'         &
     &                      // '  C_{sim} \right) $')
!>        Field label of composition flux
!!         @f$ C \left(u_{asym} \cdot \nabla C_{asym} \right) @f$
      type(field_def), parameter :: C_uasym_nabla_Casym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_uasym_nabla_Casym',                   &
     &                  math = '$ C \left(u_{asym} \cdot \nabla'        &
     &                      // ' C_{asym} \right) $')
!>        Field label of composition flux
!!         @f$ C \left(u_{sym} \cdot \nabla C_{asym} \right) @f$
      type(field_def), parameter :: C_usym_nabla_Casym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_usym_nabla_Casym',                    &
     &                  math = '$ C \left(u_{sym} \cdot'                &
     &                      // ' \nabla C_{asym} \right) $')
!>        Field label of composition flux
!!         @f$ C \left(u_{asym} \cdot \nabla C_{sym} \right) @f$
      type(field_def), parameter :: C_uasym_nabla_Csym                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'C_uasym_nabla_Csym',                    &
     &                  math = '$ C \left(u_{asym} \cdot'               &
     &                      // ' \nabla C_{sym} \right) $')
!
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{sym} \cdot \partial_{i} \right)
!!             (C_{sym} - C_0) @f$
      type(field_def), parameter :: pC_usym_nabla_pCsym                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_usym_nabla_pCsym',                   &
     &                  math = '$ (C - C_0) \left(u_{sym} \cdot'        &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{sym} - C_0) $')
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{asym} \cdot  \partial_{i} \right)
!!             (C_{asym} - C_0) @f$
      type(field_def), parameter :: pC_uasym_nabla_pCasym               &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_uasym_nabla_pCasym',                 &
     &                  math = '$ (C - C_0) \left(u_{asym} \cdot'       &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{asym} - C_0) $')
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{sym} \cdot \partial_{i} \right)
!!             (C_{asym} - C_0) @f$
      type(field_def), parameter :: pC_usym_nabla_pCasym                &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_usym_nabla_pCasym',                  &
     &                  math = '$ (C - C_0) \left(u_{sym} \cdot'        &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{asym} - C_0) $')
!>        Field label of composition flux
!!         @f$ (C - C_0) \left(u_{asym} \cdot \partial_{i} \right)
!!             (C_{sym} - C_0) @f$
      type(field_def), parameter :: pC_uasym_nabla_pCsym                &
     &    = field_def(n_comp = n_scalar,                                &
     &                  name = 'pC_uasym_nabla_pCsym',                  &
     &                  math = '$ (C - C_0) \left(u_{asym} \cdot'       &
     &                      // ' \partial_{i} \right)'                  &
     &                      // ' (C_{sym} - C_0) $')
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
      if (    (field_name .eq. u_dot_wsym_x_usym%name)                  &
     &   .or. (field_name .eq. u_dot_wasym_x_uasym%name)                &
     &   .or. (field_name .eq. u_dot_wsym_x_uasym%name)                 &
     &   .or. (field_name .eq. u_dot_wasym_x_usym%name)                 &
!
     &   .or. (field_name .eq. rev_u_dot_Jsym_x_Bsym%name)              &
     &   .or. (field_name .eq. rev_u_dot_Jasym_x_Basym%name)            &
     &   .or. (field_name .eq. rev_u_dot_Jsym_x_Basym%name)             &
     &   .or. (field_name .eq. rev_u_dot_Jasym_x_Bsym%name)             &
!
     &   .or. (field_name .eq. u_dot_Jsym_x_Bsym%name)                  &
     &   .or. (field_name .eq. u_dot_Jasym_x_Basym%name)                &
     &   .or. (field_name .eq. u_dot_Jsym_x_Basym%name)                 &
     &   .or. (field_name .eq. u_dot_Jasym_x_Bsym%name)                 &
!
     &   .or. (field_name .eq. u_dot_Bsym_nabla_Bsym%name)              &
     &   .or. (field_name .eq. u_dot_Basym_nabla_Basym%name)            &
     &   .or. (field_name .eq. u_dot_Bsym_nabla_Basym%name)             &
     &   .or. (field_name .eq. u_dot_Basym_nabla_Bsym%name)             &
!
     &   .or. (field_name .eq. sym_termal_buo_flux%name)                &
     &   .or. (field_name .eq. asym_termal_buo_flux%name)               &
!
     &   .or. (field_name .eq. sym_composite_buo_flux%name)             &
     &   .or. (field_name .eq. asym_composite_buo_flux%name)            &
!
     &   .or. (field_name .eq. B_rot_Bsym_x_usym%name)                  &
     &   .or. (field_name .eq. B_rot_Basym_x_uasym%name)                &
     &   .or. (field_name .eq. B_rot_Bsym_x_uasym%name)                 &
     &   .or. (field_name .eq. B_rot_Basym_x_usym%name)                 &
!
     &   .or. (field_name .eq. B_dot_Bsym_nabla_usym%name)              &
     &   .or. (field_name .eq. B_dot_Basym_nabla_uasym%name)            &
     &   .or. (field_name .eq. B_dot_Bsym_nabla_uasym%name)             &
     &   .or. (field_name .eq. B_dot_Basym_nabla_usym%name)             &
!
     &   .or. (field_name .eq. T_usym_nabla_Tsym%name)                  &
     &   .or. (field_name .eq. T_uasym_nabla_Tasym%name)                &
     &   .or. (field_name .eq. T_usym_nabla_Tasym%name)                 &
     &   .or. (field_name .eq. T_uasym_nabla_Tsym%name)                 &
!
     &   .or. (field_name .eq. pT_usym_nabla_pTsym%name)                &
     &   .or. (field_name .eq. pT_uasym_nabla_pTasym%name)              &
     &   .or. (field_name .eq. pT_usym_nabla_pTasym%name)               &
     &   .or. (field_name .eq. pT_uasym_nabla_pTsym%name)               &
!
     &   .or. (field_name .eq. C_usym_nabla_Csym%name)                  &
     &   .or. (field_name .eq. C_uasym_nabla_Casym%name)                &
     &   .or. (field_name .eq. C_usym_nabla_Casym%name)                 &
     &   .or. (field_name .eq. C_uasym_nabla_Csym%name)                 &
!
     &   .or. (field_name .eq. pC_usym_nabla_pCsym%name)                &
     &   .or. (field_name .eq. pC_uasym_nabla_pCasym%name)              &
     &   .or. (field_name .eq. pC_usym_nabla_pCasym%name)               &
     &   .or. (field_name .eq. pC_uasym_nabla_pCsym%name)               &
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
        if      (field_name .eq. u_dot_wsym_x_usym%name) then
          eflux_sym1_sym2%i_m_advect_work =   i_phys
        else if (field_name .eq. u_dot_wasym_x_uasym%name) then
          eflux_asym1_asym2%i_m_advect_work = i_phys
        else if (field_name .eq. u_dot_wsym_x_uasym%name) then
          eflux_sym1_asym2%i_m_advect_work =  i_phys
        else if (field_name .eq. u_dot_wasym_x_usym%name) then
          eflux_asym1_sym2%i_m_advect_work =  i_phys
!
        else if (field_name .eq. rev_u_dot_Jsym_x_Bsym%name) then
          eflux_sym1_sym2%i_nega_ujb =     i_phys
        else if (field_name .eq. rev_u_dot_Jasym_x_Basym%name) then
          eflux_asym1_asym2%i_nega_ujb =   i_phys
        else if (field_name .eq. rev_u_dot_Jsym_x_Basym%name) then
          eflux_sym1_asym2%i_nega_ujb =    i_phys
        else if (field_name .eq. rev_u_dot_Jasym_x_Bsym%name) then
          eflux_asym1_sym2%i_nega_ujb =    i_phys
!
        else if (field_name .eq. u_dot_Jsym_x_Bsym%name) then
          eflux_sym1_sym2%i_ujb =     i_phys
        else if (field_name .eq. u_dot_Jasym_x_Basym%name) then
          eflux_asym1_asym2%i_ujb =   i_phys
        else if (field_name .eq. u_dot_Jsym_x_Basym%name) then
          eflux_sym1_asym2%i_ujb =    i_phys
        else if (field_name .eq. u_dot_Jasym_x_Bsym%name) then
          eflux_asym1_sym2%i_ujb =    i_phys
!
        else if (field_name .eq. u_dot_Bsym_nabla_Bsym%name) then
          eflux_sym1_sym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. u_dot_Basym_nabla_Basym%name) then
          eflux_asym1_asym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. u_dot_Bsym_nabla_Basym%name) then
          eflux_sym1_asym2%i_m_tension_wk =  i_phys
        else if (field_name .eq. u_dot_Basym_nabla_Bsym%name) then
          eflux_asym1_sym2%i_m_tension_wk =  i_phys
!
        else if (field_name .eq. sym_termal_buo_flux%name) then
          eflux_sym1_sym2%i_buo_gen =   i_phys
        else if (field_name .eq. asym_termal_buo_flux%name) then
          eflux_asym1_asym2%i_buo_gen =   i_phys
!
        else if (field_name .eq. sym_composite_buo_flux%name) then
          eflux_sym1_sym2%i_c_buo_gen =   i_phys
        else if (field_name .eq. asym_composite_buo_flux%name) then
          eflux_asym1_asym2%i_c_buo_gen =   i_phys
!
        else if (field_name .eq. B_rot_Bsym_x_usym%name) then
          eflux_sym1_sym2%i_me_gen =   i_phys
        else if (field_name .eq. B_rot_Basym_x_uasym%name) then
          eflux_asym1_asym2%i_me_gen = i_phys
        else if (field_name .eq. B_rot_Bsym_x_uasym%name) then
          eflux_sym1_asym2%i_me_gen =  i_phys
        else if (field_name .eq. B_rot_Basym_x_usym%name) then
          eflux_asym1_sym2%i_me_gen =  i_phys
!
        else if (field_name .eq. B_dot_Bsym_nabla_usym%name) then
          eflux_sym1_sym2%i_mag_stretch_flux =   i_phys
        else if (field_name .eq. B_dot_Basym_nabla_uasym%name) then
          eflux_asym1_asym2%i_mag_stretch_flux = i_phys
        else if (field_name .eq. B_dot_Bsym_nabla_uasym%name) then
          eflux_sym1_asym2%i_mag_stretch_flux =  i_phys
        else if (field_name .eq. B_dot_Basym_nabla_usym%name) then
          eflux_asym1_sym2%i_mag_stretch_flux =  i_phys
!
        else if (field_name .eq. T_usym_nabla_Tsym%name) then
          eflux_sym1_sym2%i_temp_gen =  i_phys
        else if (field_name .eq. T_uasym_nabla_Tasym%name) then
          eflux_asym1_asym2%i_temp_gen =  i_phys
        else if (field_name .eq. T_usym_nabla_Tasym%name) then
          eflux_sym1_asym2%i_temp_gen =  i_phys
        else if (field_name .eq. T_uasym_nabla_Tsym%name) then
          eflux_asym1_sym2%i_temp_gen =  i_phys
!
        else if (field_name .eq. pT_usym_nabla_pTsym%name) then
          eflux_sym1_sym2%i_par_t_gen = i_phys
        else if (field_name .eq. pT_uasym_nabla_pTasym%name) then
          eflux_asym1_asym2%i_par_t_gen = i_phys
        else if (field_name .eq. pT_usym_nabla_pTasym%name) then
          eflux_sym1_asym2%i_par_t_gen = i_phys
        else if (field_name .eq. pT_uasym_nabla_pTsym%name) then
          eflux_asym1_sym2%i_par_t_gen = i_phys
!
        else if (field_name .eq. C_usym_nabla_Csym%name) then
          eflux_sym1_sym2%i_comp_gen =    i_phys
        else if (field_name .eq. C_uasym_nabla_Casym%name) then
          eflux_asym1_asym2%i_comp_gen =  i_phys
        else if (field_name .eq. C_usym_nabla_Casym%name) then
          eflux_sym1_asym2%i_comp_gen =   i_phys
        else if (field_name .eq. C_uasym_nabla_Csym%name) then
          eflux_asym1_sym2%i_comp_gen =   i_phys
!
        else if (field_name .eq. pC_usym_nabla_pCsym%name) then
          eflux_sym1_sym2%i_par_c_gen =   i_phys
        else if (field_name .eq. pC_uasym_nabla_pCasym%name) then
          eflux_asym1_asym2%i_par_c_gen = i_phys
        else if (field_name .eq. pC_usym_nabla_pCasym%name) then
          eflux_sym1_asym2%i_par_c_gen =  i_phys
        else if (field_name .eq. pC_uasym_nabla_pCsym%name) then
          eflux_asym1_sym2%i_par_c_gen =  i_phys
        end if
      end if
!
      end subroutine set_ene_flux_w_sym_addresses
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
      subroutine set_ene_flux_w_symmetry_names(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(neflux_w_sym)
      character(len = kchara), intent(inout) :: names(neflux_w_sym)
      character(len = kchara), intent(inout) :: maths(neflux_w_sym)
!
!
      call set_field_labels(u_dot_wsym_x_usym,                          &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(u_dot_wasym_x_uasym,                        &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(u_dot_wsym_x_uasym,                         &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(u_dot_wasym_x_usym,                         &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(rev_u_dot_Jsym_x_Bsym,                      &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(rev_u_dot_Jasym_x_Basym,                    &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(rev_u_dot_Jsym_x_Basym,                     &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(rev_u_dot_Jasym_x_Bsym,                     &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(u_dot_Jsym_x_Bsym,                          &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(u_dot_Jasym_x_Basym,                        &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(u_dot_Jsym_x_Basym,                         &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(u_dot_Jasym_x_Bsym,                         &
     &    n_comps(12), names(12), maths(12))
!
      call set_field_labels(u_dot_Bsym_nabla_Bsym,                      &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(u_dot_Basym_nabla_Basym,                    &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(u_dot_Bsym_nabla_Basym,                     &
     &    n_comps(15), names(15), maths(15))
      call set_field_labels(u_dot_Basym_nabla_Bsym,                     &
     &    n_comps(16), names(16), maths(16))
!
      call set_field_labels(sym_termal_buo_flux,                        &
     &    n_comps(17), names(17), maths(17))
      call set_field_labels(asym_termal_buo_flux,                       &
     &    n_comps(18), names(18), maths(18))
!
      call set_field_labels(sym_composite_buo_flux,                     &
     &    n_comps(19), names(19), maths(19))
      call set_field_labels(asym_composite_buo_flux,                    &
     &    n_comps(20), names(20), maths(20))
!
      call set_field_labels(B_rot_Bsym_x_usym,                          &
     &    n_comps(21), names(21), maths(21))
      call set_field_labels(B_rot_Basym_x_uasym,                        &
     &    n_comps(22), names(22), maths(22))
      call set_field_labels(B_rot_Bsym_x_uasym,                         &
     &    n_comps(23), names(23), maths(23))
      call set_field_labels(B_rot_Basym_x_usym,                         &
     &    n_comps(24), names(24), maths(24))
!
      call set_field_labels(B_dot_Bsym_nabla_usym,                      &
     &    n_comps(25), names(25), maths(25))
      call set_field_labels(B_dot_Basym_nabla_uasym,                    &
     &    n_comps(26), names(26), maths(26))
      call set_field_labels(B_dot_Bsym_nabla_uasym,                     &
     &    n_comps(27), names(27), maths(27))
      call set_field_labels(B_dot_Basym_nabla_usym,                     &
     &    n_comps(28), names(28), maths(28))
!
      call set_field_labels(T_usym_nabla_Tsym,                          &
     &    n_comps(29), names(29), maths(29))
      call set_field_labels(T_uasym_nabla_Tasym,                        &
     &    n_comps(30), names(30), maths(30))
      call set_field_labels(T_usym_nabla_Tasym,                         &
     &    n_comps(31), names(31), maths(31))
      call set_field_labels(T_uasym_nabla_Tsym,                         &
     &    n_comps(32), names(32), maths(32))
!
      call set_field_labels(pT_usym_nabla_pTsym,                        &
     &    n_comps(33), names(33), maths(33))
      call set_field_labels(pT_uasym_nabla_pTasym,                      &
     &    n_comps(34), names(34), maths(34))
      call set_field_labels(pT_usym_nabla_pTasym,                       &
     &    n_comps(35), names(35), maths(35))
      call set_field_labels(pT_uasym_nabla_pTsym,                       &
     &    n_comps(36), names(36), maths(36))
!
      call set_field_labels(C_usym_nabla_Csym,                          &
     &    n_comps(37), names(37), maths(37))
      call set_field_labels(C_uasym_nabla_Casym,                        &
     &    n_comps(38), names(38), maths(38))
      call set_field_labels(C_usym_nabla_Casym,                         &
     &    n_comps(39), names(39), maths(39))
      call set_field_labels(C_uasym_nabla_Csym,                         &
     &    n_comps(40), names(40), maths(40))
!
      call set_field_labels(pC_usym_nabla_pCsym,                        &
     &    n_comps(41), names(41), maths(41))
      call set_field_labels(pC_uasym_nabla_pCasym,                      &
     &    n_comps(42), names(42), maths(42))
      call set_field_labels(pC_usym_nabla_pCasym,                       &
     &    n_comps(43), names(43), maths(43))
      call set_field_labels(pC_uasym_nabla_pCsym,                       &
     &    n_comps(44), names(44), maths(44))
!
      end subroutine set_ene_flux_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_energy_flux_w_sym_labels
