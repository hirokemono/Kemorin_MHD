!>@file   m_force_w_sym_labels.f90
!!        module m_force_w_sym_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses of basic forces
!!
!!@verbatim
!!      logical function check_forces_w_sym(field_name)
!!      subroutine set_force_w_sym_addresses(i_phys, field_name,        &
!!     &          force_sym1_sym2, force_asym1_asym2,                   &
!!     &          force_sym1_asym2, force_asym1_sym2, flag)
!!        type(base_force_address), intent(inout) :: force_sym1_sym2
!!        type(base_force_address), intent(inout) :: force_asym1_asym2
!!        type(base_force_address), intent(inout) :: force_sym1_asym2
!!        type(base_force_address), intent(inout) :: force_asym1_sym2
!!      integer(kind = kint) function check_force_by_sym_sym_id         &
!!     &         (i_field, field_name, sym_base_fld, force_sym1_sym2)
!!        type(base_field_address), intent(in) :: sym_base_fld
!!        type(base_force_address), intent(in) :: force_sym1_sym2
!!      integer(kind = kint) function check_force_by_asym_asym_id       &
!!     &         (i_field, field_name, asym_base_fld, force_asym1_asym2)
!!        type(base_field_address), intent(in) :: asym_base_fld
!!        type(base_force_address), intent(in) :: force_asym1_asym2
!!      integer(kind = kint) function check_force_by_sym_asym_id        &
!!     &         (i_field, field_name, sym_base_fld, asym_base_fld,     &
!!     &          force_sym1_asym2, force_asym1_sym2)
!!        type(base_field_address), intent(in) :: sym_base_fld
!!        type(base_field_address), intent(in) :: asym_base_fld
!!        type(base_force_address), intent(in) :: force_sym1_asym2
!!        type(base_force_address), intent(in) :: force_asym1_sym2
!!
!!      subroutine force_w_sym_monitor_address                          &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_force_sym1_sym2, rms_force_asym1_asym2,           &
!!     &          rms_force_sym1_asym2, rms_force_asym1_sym2,           &
!!     &          ave_force_sym1_sym2, ave_force_asym1_asym2,           &
!!     &          ave_force_sym1_asym2, ave_force_asym1_sym2, flag)
!!        type(base_force_address), intent(inout) :: rms_force_sym1_sym2
!!        type(base_force_address), intent(inout)::rms_force_asym1_asym2
!!        type(base_force_address), intent(inout) :: rms_force_sym1_asym2
!!        type(base_force_address), intent(inout) :: rms_force_asym1_sym2
!!        type(base_force_address), intent(inout) :: ave_force_sym1_sym2
!!        type(base_force_address), intent(inout):: ave_force_asym1_asym2
!!        type(base_force_address), intent(inout) :: ave_force_sym1_asym2
!!        type(base_force_address), intent(inout) :: ave_force_asym1_sym2
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
      module m_force_w_sym_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use t_base_force_labels
      use m_field_w_symmetry_labels
!
      implicit  none
! 
!>        Field label of advection of momentum
!!         @f$ u_{symj} \partial_{j} u_{symi} @f$
!!         or @f$ \omega_{sym} \times u_{sym} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_wsym_x_usym = 'wsym_x_usym'
!>        Field label of advection of momentum
!!         @f$ u_{asymj} \partial_{j} u_{asymi} @f$
!!         or @f$ \omega_{asym} \times u_{asym} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_wasym_x_uasym = 'wasym_x_uasym'
!>        Field label of advection of momentum
!!         @f$ u_{symj} \partial_{j} u_{asymi} @f$
!!         or @f$ \omega_{sym} \times u_{asym} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_wsym_x_uasym = 'wsym_x_uasym'
!>        Field label of advection of momentum
!!         @f$ u_{asymj} \partial_{j} u_{symi} @f$
!!         or @f$ \omega_{asym} \times u_{sym} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_wasym_x_uaym = 'wasym_x_usym'
!
!>        Field label of momentum flux
!!         @f$ u_{symi} u_{symj} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_m_flux_sym_sym = 'm_flux_sym_sym'
!>        Field label of momentum flux
!!         @f$ u_{asymi} u_{asymj} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_m_flux_asym_asym = 'm_flux_asym_asym'
!>        Field label of momentum flux
!!         @f$ u_{symi} u_{asymj} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_m_flux_sym_asym = 'm_flux_sym_asym'
!
!>        Field label of Lorentz force
!!         @f$ J_{sym} \times B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Jsym_x_Bsym =        'Jsym_x_Bsym'
!>        Field label of Lorentz force
!!         @f$ J_{asym} \times B_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Jasym_x_Basym =      'Jasym_x_Basym'
!>        Field label of Lorentz force
!!         @f$ J_{sym} \times B_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Jsym_x_Basym =       'Jsym_x_Basym'
!>        Field label of Lorentz force
!!         @f$ J_{asym} \times B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Jasym_x_Bsym =       'Jasym_x_Bsym'
!
!>        start address of magnetic tension
!!         @f$ (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Bsym_nabla_Bsym = 'Bsym_nabla_Bsym'
!>        start address of magnetic tension
!!         @f$ (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Basym_nabla_Basym = 'Basym_nabla_Basym'
!>        start address of magnetic tension
!!         @f$ (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Bsym_nabla_Basym = 'Bsym_nabla_Basym'
!>        start address of magnetic tension
!!         @f$ (B_{sym} \cdot \nabla) B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Basym_nabla_Bsym = 'Basym_nabla_Bsym'
!
!>        Field label of Maxwell tensor
!!         @f$ B_{sym} B_{sym} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_maxwell_sym_sym =   'maxwell_tensor_sym_sym'
!>        Field label ofof Maxwell tensor
!!         @f$ B_{asym} B_{asym} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_maxwell_asym_asym = 'maxwell_tensor_asym_asym'
!>        Field label of Maxwell tensor
!!         @f$ B_{sym} B_{asym} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_maxwell_sym_asym =  'maxwell_tensor_sym_asym'
!
!>        Field label of buoyancy
!!         @f$ -\alpha_{T} g_{i} T_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_sym_buoyancy =    'sym_termal_buoyancy'
!>        Field label of buoyancy
!!         @f$ -\alpha_{T} g_{i} T_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_asym_buoyancy =   'asym_termal_buoyancy'
!
!>        Field label of compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_sym_comp_buo =   'sym_composite_buoyancy'
!>        Field label of compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_asym_comp_buo =  'asym_composite_buoyancy'
!!
!>        Field label of induction of vector potential
!!         @f$ u_{sym} \times B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_x_Bsym =     'usym_x_Bsym'
!>        Field label of induction of vector potential
!!         @f$ u_{asym} \times B_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_x_Basym =     'uasym_x_Basym'
!>        Field label of induction of vector potential
!!         @f$ u_{sym} \times B_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_x_Basym =     'usym_x_Basym'
!>        Field label of induction of vector potential
!!         @f$ u_{asym} \times B_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_x_Bsym =     'uasym_x_Bsym'
!
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{sym} \times B_{sym}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_usym_x_Bsym =     'rot_usym_x_Bsym'
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{asym} \times B_{asym}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_uasym_x_Basym =     'rot_uasym_x_Basym'
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{sym} \times B_{asym}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_usym_x_Basym =     'rot_usym_x_Basym'
!>        Field label of magnetic induction
!!         @f$ \nabla \times (u_{asym} \times B_{sym}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_uasym_x_Bsym =     'rot_uasym_x_Bsym'
!
!>        Field label of magnetic stretch term
!!         @f$ \left(B_{sym} \nabla) u_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Bsym_nabla_usym =   'Bsym_nabla_usym'
!>        Field label of magnetic stretch term
!!         @f$ \left(B_{a} \nabla) u_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Basym_nabla_uasym = 'Basym_nabla_uasym'
!>        Field label of magnetic stretch term
!!         @f$ \left(B_{sym} \nabla) u_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Bsym_nabla_uasym =  'Bsym_nabla_uasym'
!>        Field label of magnetic stretch term
!!         @f$ \left(B_{asym} \nabla) u_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Basym_nabla_usym =  'Basym_nabla_usym'
!
!>        Field label of Tensor of magnetic induction
!!         @f$ u_{sym} B_{sym}  - B_{sym} u_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_Bsym =      'usym_Bsym'
!>        Field label of Tensor of magnetic induction
!!         @f$ u_{asym} B_{asym}  - B_{asym} u_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_Basym =     'uasym_Basym'
!>        Field label of Tensor of magnetic induction
!!         @f$ u_{sym} B_{asym}  - B_{asym} u_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_Bsym =      'usym_Basym'
!
!>        Field label of advection of temperature
!!         @f$ u_{sim} \nabla T_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_Tsym =       'usym_nabla_Tsym'
!>        Field label of advection of temperature
!!         @f$ u_{asym} \nabla T_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_Tasym =     'uasym_nabla_Tasym'
!>        Field label of advection of temperature
!!         @f$ u_{sym} \nabla T_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_Tasym =      'usym_nabla_Tasym'
!>        Field label of advection of temperature
!!         @f$ u_{asym} \nabla T_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_Tsym =      'uasym_nabla_Tsym'
!
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{sym} \partial_{i} \Theta_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_pTsym =     'usym_nabla_pTsym'
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{asym} \partial_{i} \Theta_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_pTasym =   'uasym_nabla_pTasym'
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{sym} \partial_{i} \Theta_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_pTasym =    'usym_nabla_pTasym'
!>        Field label of advection of perturbation of temperature
!!         @f$ u_{asym} \partial_{i} \Theta_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_pTsym =    'uasym_nabla_pTsym'
!
!>        Field label of heat flux
!!         @f$ u_{sym} T_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_h_flux_sym_sym =   'heat_flux_sym_sym'
!>        Field label of heat flux
!!         @f$ u_{asym} T_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_h_flux_asym_asym = 'heat_flux_asym_asym'
!>        Field label of heat flux
!!         @f$ u_{sym} T_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_h_flux_sym_asym =  'heat_flux_sym_asym'
!>        Field label of heat flux
!!         @f$ u_{asym} T_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_h_flux_asym_sym =  'heat_flux_asym_sym'
!
!>        Field label of perturbation of heat flux
!!         @f$ u_{sym} \Theta_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ph_flux_sym_sym =   'part_h_flux_sym_sym'
!>        Field label of perturbation of heat flux
!!         @f$ u_{asym} \Theta_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ph_flux_asym_asym = 'part_h_flux_asym_asym'
!>        Field label of perturbation of heat flux
!!         @f$ u_{sym} \Theta_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ph_flux_sym_asym =  'part_h_flux_sym_asym'
!>        Field label of perturbation of heat flux
!!         @f$ u_{asym} \Theta_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ph_flux_asym_sym =  'part_h_flux_asym_sym'
!
!>        Field label of advection of composition
!!         @f$ u_{sim} \nabla C_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_Csym =       'usym_nabla_Csym'
!>        Field label of advection of composition
!!         @f$ u_{asym} \nabla C_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_Casym =     'uasym_nabla_Casym'
!>        Field label of advection of composition
!!         @f$ u_{sym} \nabla C_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_Casym =      'usym_nabla_Casym'
!>        Field label of advection of composition
!!         @f$ u_{asym} \nabla C_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_Csym =      'uasym_nabla_Csym'
!
!>        Field label of advection of perturbation of composition
!!         @f$ u_{sym} \partial_{i} (C_{sym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_pCsym =     'usym_nabla_pCsym'
!>        Field label of advection of perturbation of composition
!!         @f$ u_{asym} \partial_{i} (C_{asym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_pCasym =   'uasym_nabla_pCasym'
!>        Field label of advection of perturbation of composition
!!         @f$ u_{sym} \partial_{i} (C_{asym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_usym_nabla_pCasym =    'usym_nabla_pCasym'
!>        Field label of advection of perturbation of composition
!!         @f$ u_{asym} \partial_{i} (C_{sym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_uasym_nabla_pCsym =    'uasym_nabla_pCsym'
!
!>        Field label of composition flux
!!         @f$ u_{sym} C_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_flux_sym_sym =   'composite_flux_sym_sym'
!>        Field label of composition flux
!!         @f$ u_{asym} C_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_flux_asym_asym = 'composite_flux_asym_asym'
!>        Field label of composition flux
!!         @f$ u_{sym} C_{asym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_flux_sym_asym =  'composite_flux_sym_asym'
!>        Field label of composition flux
!!         @f$ u_{asym} C_{sym} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_flux_asym_sym =  'composite_flux_asym_sym'
!
!>        Field label of perturbation of composition flux
!!         @f$ u_{sym} (C_{sym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_pc_flux_sym_sym =   'part_c_flux_sym_sym'
!>        Field label of perturbation of composition flux
!!         @f$ u_{asym} (C_{asym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_pc_flux_asym_asym = 'part_c_flux_asym_asym'
!>        Field label of perturbation of composition flux
!!         @f$ u_{sym} (C_{asym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_pc_flux_sym_asym =  'part_c_flux_sym_asym'
!>        Field label of perturbation of composition flux
!!         @f$ u_{asym} (C_{sym} - C0) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_pc_flux_asym_sym =  'part_c_flux_asym_sym'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_forces_w_sym(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_forces_w_sym = .FALSE.
      if (    (field_name .eq. fhd_wsym_x_usym)                         &
     &   .or. (field_name .eq. fhd_wasym_x_uasym)                       &
     &   .or. (field_name .eq. fhd_wsym_x_uasym)                        &
     &   .or. (field_name .eq. fhd_wasym_x_uaym)                        &
!
     &   .or. (field_name .eq. fhd_m_flux_sym_sym)                      &
     &   .or. (field_name .eq. fhd_m_flux_asym_asym)                    &
     &   .or. (field_name .eq. fhd_m_flux_sym_asym)                     &
!
     &   .or. (field_name .eq. fhd_Jsym_x_Bsym)                         &
     &   .or. (field_name .eq. fhd_Jasym_x_Basym)                       &
     &   .or. (field_name .eq. fhd_Jsym_x_Basym)                        &
     &   .or. (field_name .eq. fhd_Jasym_x_Bsym)                        &
!
     &   .or. (field_name .eq. fhd_Bsym_nabla_Bsym)                     &
     &   .or. (field_name .eq. fhd_Basym_nabla_Basym)                   &
     &   .or. (field_name .eq. fhd_Bsym_nabla_Basym)                    &
     &   .or. (field_name .eq. fhd_Basym_nabla_Bsym)                    &
!
     &   .or. (field_name .eq. fhd_maxwell_sym_sym)                     &
     &   .or. (field_name .eq. fhd_maxwell_asym_asym)                   &
     &   .or. (field_name .eq. fhd_maxwell_sym_asym)                    &
!
     &   .or. (field_name .eq. fhd_sym_buoyancy)                        &
     &   .or. (field_name .eq. fhd_asym_buoyancy)                       &
     &   .or. (field_name .eq. fhd_sym_comp_buo)                        &
     &   .or. (field_name .eq. fhd_asym_comp_buo)                       &
!
     &   .or. (field_name .eq. fhd_usym_x_Bsym)                         &
     &   .or. (field_name .eq. fhd_uasym_x_Basym)                       &
     &   .or. (field_name .eq. fhd_usym_x_Basym)                        &
     &   .or. (field_name .eq. fhd_uasym_x_Bsym)                        &
!
     &   .or. (field_name .eq. fhd_rot_usym_x_Bsym)                     &
     &   .or. (field_name .eq. fhd_rot_uasym_x_Basym)                   &
     &   .or. (field_name .eq. fhd_rot_usym_x_Basym)                    &
     &   .or. (field_name .eq. fhd_rot_uasym_x_Bsym)                    &
!
     &   .or. (field_name .eq. fhd_Bsym_nabla_usym)                     &
     &   .or. (field_name .eq. fhd_Basym_nabla_uasym)                   &
     &   .or. (field_name .eq. fhd_Bsym_nabla_uasym)                    &
     &   .or. (field_name .eq. fhd_Basym_nabla_usym)                    &
!
     &   .or. (field_name .eq. fhd_usym_Bsym)                           &
     &   .or. (field_name .eq. fhd_uasym_Basym)                         &
     &   .or. (field_name .eq. fhd_uasym_Bsym)                          &
!
     &   .or. (field_name .eq. fhd_usym_nabla_Tsym)                     &
     &   .or. (field_name .eq. fhd_uasym_nabla_Tasym)                   &
     &   .or. (field_name .eq. fhd_usym_nabla_Tasym)                    &
     &   .or. (field_name .eq. fhd_uasym_nabla_Tsym)                    &
!
     &   .or. (field_name .eq. fhd_usym_nabla_pTsym)                    &
     &   .or. (field_name .eq. fhd_uasym_nabla_pTasym)                  &
     &   .or. (field_name .eq. fhd_usym_nabla_pTasym)                   &
     &   .or. (field_name .eq. fhd_uasym_nabla_pTsym)                   &
!
     &   .or. (field_name .eq. fhd_h_flux_sym_sym)                      &
     &   .or. (field_name .eq. fhd_h_flux_asym_asym)                    &
     &   .or. (field_name .eq. fhd_h_flux_sym_asym)                     &
     &   .or. (field_name .eq. fhd_h_flux_asym_sym)                     &
!
     &   .or. (field_name .eq. fhd_ph_flux_sym_sym)                     &
     &   .or. (field_name .eq. fhd_ph_flux_asym_asym)                   &
     &   .or. (field_name .eq. fhd_ph_flux_sym_asym)                    &
     &   .or. (field_name .eq. fhd_ph_flux_asym_sym)                    &
!
     &   .or. (field_name .eq. fhd_usym_nabla_Csym)                     &
     &   .or. (field_name .eq. fhd_uasym_nabla_Casym)                   &
     &   .or. (field_name .eq. fhd_usym_nabla_Casym)                    &
     &   .or. (field_name .eq. fhd_uasym_nabla_Csym)                    &
!
     &   .or. (field_name .eq. fhd_usym_nabla_pCsym)                    &
     &   .or. (field_name .eq. fhd_uasym_nabla_pCasym)                  &
     &   .or. (field_name .eq. fhd_usym_nabla_pCasym)                   &
     &   .or. (field_name .eq. fhd_uasym_nabla_pCsym)                   &
!
     &   .or. (field_name .eq. fhd_c_flux_sym_sym)                      &
     &   .or. (field_name .eq. fhd_c_flux_asym_asym)                    &
     &   .or. (field_name .eq. fhd_c_flux_sym_asym)                     &
     &   .or. (field_name .eq. fhd_c_flux_asym_sym)                     &
!
     &   .or. (field_name .eq. fhd_pc_flux_sym_sym)                     &
     &   .or. (field_name .eq. fhd_pc_flux_asym_asym)                   &
     &   .or. (field_name .eq. fhd_pc_flux_sym_asym)                    &
     &   .or. (field_name .eq. fhd_pc_flux_asym_sym)                    &
     &      )   check_forces_w_sym = .TRUE.
!
      end function check_forces_w_sym
!
! ----------------------------------------------------------------------
!
      subroutine set_force_w_sym_addresses(i_phys, field_name,          &
     &          force_sym1_sym2, force_asym1_asym2,                     &
     &          force_sym1_asym2, force_asym1_sym2, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_sym1_sym2
      type(base_force_address), intent(inout) :: force_asym1_asym2
      type(base_force_address), intent(inout) :: force_sym1_asym2
      type(base_force_address), intent(inout) :: force_asym1_sym2
      logical, intent(inout) :: flag
!
!
      flag = check_forces_w_sym(field_name)
      if(flag) then
        if      (field_name .eq. fhd_wsym_x_usym) then
          force_sym1_sym2%i_m_advect =   i_phys
        else if (field_name .eq. fhd_wasym_x_uasym) then
          force_asym1_asym2%i_m_advect =   i_phys
        else if (field_name .eq. fhd_wsym_x_uasym) then
          force_sym1_asym2%i_m_advect =   i_phys
        else if (field_name .eq. fhd_wasym_x_uaym) then
          force_asym1_sym2%i_m_advect =   i_phys
!
        else if (field_name .eq. fhd_m_flux_sym_sym ) then
          force_sym1_sym2%i_m_flux =     i_phys
        else if (field_name .eq. fhd_m_flux_asym_asym ) then
          force_asym1_asym2%i_m_flux =     i_phys
        else if (field_name .eq. fhd_m_flux_sym_asym ) then
          force_sym1_asym2%i_m_flux =     i_phys
!
        else if (field_name .eq. fhd_Jsym_x_Bsym) then
          force_sym1_sym2%i_lorentz =    i_phys
        else if (field_name .eq. fhd_Jasym_x_Basym) then
          force_asym1_asym2%i_lorentz =    i_phys
        else if (field_name .eq. fhd_Jsym_x_Basym) then
          force_sym1_asym2%i_lorentz =    i_phys
        else if (field_name .eq. fhd_Jasym_x_Bsym) then
          force_asym1_sym2%i_lorentz =    i_phys
!
        else if (field_name .eq. fhd_Bsym_nabla_Bsym) then
          force_sym1_sym2%i_m_tension =  i_phys
        else if (field_name .eq. fhd_Basym_nabla_Basym) then
          force_asym1_asym2%i_m_tension =  i_phys
        else if (field_name .eq. fhd_Bsym_nabla_Basym) then
          force_sym1_asym2%i_m_tension =  i_phys
        else if (field_name .eq. fhd_Basym_nabla_Bsym) then
          force_asym1_sym2%i_m_tension =  i_phys
!
        else if (field_name .eq. fhd_maxwell_sym_sym) then
          force_sym1_sym2%i_maxwell =    i_phys
        else if (field_name .eq. fhd_maxwell_asym_asym) then
          force_asym1_asym2%i_maxwell =    i_phys
        else if (field_name .eq. fhd_maxwell_sym_asym) then
          force_sym1_asym2%i_maxwell =    i_phys
!
        else if (field_name .eq. fhd_sym_buoyancy) then
          force_sym1_sym2%i_buoyancy =   i_phys
        else if (field_name .eq. fhd_asym_buoyancy) then
          force_asym1_asym2%i_buoyancy =   i_phys
!
        else if (field_name .eq. fhd_sym_comp_buo) then
          force_sym1_sym2%i_comp_buo =   i_phys
        else if (field_name .eq. fhd_asym_comp_buo) then
          force_asym1_asym2%i_comp_buo =   i_phys
!
        else if (field_name .eq. fhd_usym_x_Bsym) then
          force_sym1_sym2%i_vp_induct =    i_phys
        else if (field_name .eq. fhd_uasym_x_Basym) then
          force_asym1_asym2%i_vp_induct =  i_phys
        else if (field_name .eq. fhd_usym_x_Basym) then
          force_sym1_asym2%i_vp_induct =   i_phys
        else if (field_name .eq. fhd_uasym_x_Bsym) then
          force_asym1_sym2%i_vp_induct =   i_phys
!
        else if (field_name .eq. fhd_rot_usym_x_Bsym) then
          force_sym1_sym2%i_induction =    i_phys
        else if (field_name .eq. fhd_rot_uasym_x_Basym) then
          force_asym1_asym2%i_induction =  i_phys
        else if (field_name .eq. fhd_rot_usym_x_Basym) then
          force_sym1_asym2%i_induction =   i_phys
        else if (field_name .eq. fhd_rot_uasym_x_Bsym) then
          force_asym1_sym2%i_induction =   i_phys
!
        else if (field_name .eq. fhd_Bsym_nabla_usym) then
          force_sym1_sym2%i_mag_stretch =   i_phys
        else if (field_name .eq. fhd_Basym_nabla_uasym) then
          force_asym1_asym2%i_mag_stretch = i_phys
        else if (field_name .eq. fhd_Bsym_nabla_uasym) then
          force_sym1_asym2%i_mag_stretch =  i_phys
        else if (field_name .eq. fhd_Basym_nabla_usym) then
          force_asym1_sym2%i_mag_stretch =  i_phys
!
        else if (field_name .eq. fhd_usym_Bsym) then
          force_sym1_sym2%i_induct_t =     i_phys
        else if (field_name .eq. fhd_uasym_Basym) then
          force_asym1_asym2%i_induct_t =   i_phys
        else if (field_name .eq. fhd_uasym_Bsym) then
          force_sym1_asym2%i_induct_t =    i_phys
!
        else if (field_name .eq. fhd_usym_nabla_Tsym) then
          force_sym1_sym2%i_h_advect =  i_phys
        else if (field_name .eq. fhd_uasym_nabla_Tasym) then
          force_asym1_asym2%i_h_advect =  i_phys
        else if (field_name .eq. fhd_usym_nabla_Tasym) then
          force_sym1_asym2%i_h_advect =  i_phys
        else if (field_name .eq. fhd_uasym_nabla_Tsym) then
          force_asym1_sym2%i_h_advect =  i_phys
!
        else if (field_name .eq. fhd_usym_nabla_pTsym) then
          force_sym1_sym2%i_ph_advect = i_phys
        else if (field_name .eq. fhd_uasym_nabla_pTasym) then
          force_asym1_asym2%i_ph_advect = i_phys
        else if (field_name .eq. fhd_usym_nabla_pTasym) then
          force_sym1_asym2%i_ph_advect = i_phys
        else if (field_name .eq. fhd_uasym_nabla_pTsym) then
          force_asym1_sym2%i_ph_advect = i_phys
!
        else if (field_name .eq. fhd_h_flux_sym_sym) then
          force_sym1_sym2%i_h_flux =    i_phys
        else if (field_name .eq. fhd_h_flux_asym_asym) then
          force_asym1_asym2%i_h_flux =  i_phys
        else if (field_name .eq. fhd_h_flux_sym_asym) then
          force_sym1_asym2%i_h_flux =   i_phys
        else if (field_name .eq. fhd_h_flux_asym_sym) then
          force_asym1_sym2%i_h_flux =   i_phys
!
        else if (field_name .eq. fhd_ph_flux_sym_sym) then
          force_sym1_sym2%i_ph_flux =   i_phys
        else if (field_name .eq. fhd_ph_flux_asym_asym) then
          force_asym1_asym2%i_ph_flux =   i_phys
        else if (field_name .eq. fhd_ph_flux_sym_asym) then
          force_sym1_asym2%i_ph_flux =   i_phys
        else if (field_name .eq. fhd_ph_flux_asym_sym) then
          force_asym1_sym2%i_ph_flux =   i_phys
!
        else if (field_name .eq. fhd_usym_nabla_Csym) then
          force_sym1_sym2%i_c_advect =    i_phys
        else if (field_name .eq. fhd_uasym_nabla_Casym) then
          force_asym1_asym2%i_c_advect =  i_phys
        else if (field_name .eq. fhd_usym_nabla_Casym) then
          force_sym1_asym2%i_c_advect =   i_phys
        else if (field_name .eq. fhd_uasym_nabla_Csym) then
          force_asym1_sym2%i_c_advect =   i_phys
!
        else if (field_name .eq. fhd_usym_nabla_pCsym) then
          force_sym1_sym2%i_pc_advect =   i_phys
        else if (field_name .eq. fhd_uasym_nabla_pCasym) then
          force_asym1_asym2%i_pc_advect = i_phys
        else if (field_name .eq. fhd_usym_nabla_pCasym) then
          force_sym1_asym2%i_pc_advect =  i_phys
        else if (field_name .eq. fhd_uasym_nabla_pCsym) then
          force_asym1_sym2%i_pc_advect =  i_phys
!
        else if (field_name .eq. fhd_c_flux_sym_sym) then
          force_sym1_sym2%i_c_flux =    i_phys
        else if (field_name .eq. fhd_c_flux_asym_asym) then
          force_asym1_asym2%i_c_flux =  i_phys
        else if (field_name .eq. fhd_c_flux_sym_asym) then
          force_sym1_asym2%i_c_flux =   i_phys
        else if (field_name .eq. fhd_c_flux_asym_sym) then
          force_asym1_sym2%i_c_flux =   i_phys
!
        else if (field_name .eq. fhd_pc_flux_sym_sym) then
          force_sym1_sym2%i_pc_flux =   i_phys
        else if (field_name .eq. fhd_pc_flux_asym_asym) then
          force_asym1_asym2%i_pc_flux = i_phys
        else if (field_name .eq. fhd_pc_flux_sym_asym) then
          force_sym1_asym2%i_pc_flux =  i_phys
        else if (field_name .eq. fhd_pc_flux_asym_sym) then
          force_asym1_sym2%i_pc_flux =  i_phys
        end if
      end if
!
      end subroutine set_force_w_sym_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_force_by_sym_sym_id           &
     &         (i_field, field_name, sym_base_fld, force_sym1_sym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: sym_base_fld
      type(base_force_address), intent(in) :: force_sym1_sym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. force_sym1_sym2%i_m_advect)                &
     &    .or. (i_field .eq. force_sym1_sym2%i_m_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_vort, fhd_sym_vort)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
      else if( (i_field .eq. force_sym1_sym2%i_lorentz) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_current, fhd_sym_current)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_magne, fhd_sym_magne)
      else if( (i_field .eq. force_sym1_sym2%i_maxwell)                 &
     &    .or. (i_field .eq. force_sym1_sym2%i_m_tension) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_magne, fhd_sym_magne)
!
      else if( (i_field .eq. force_sym1_sym2%i_buoyancy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_temp, fhd_sym_temp)
      else if( (i_field .eq. force_sym1_sym2%i_comp_buo)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_light, fhd_sym_light)
!
      else if( (i_field .eq. force_sym1_sym2%i_vp_induct)               &
     &    .or. (i_field .eq. force_sym1_sym2%i_induction)               &
     &    .or. (i_field .eq. force_sym1_sym2%i_mag_stretch)             &
     &    .or. (i_field .eq. force_sym1_sym2%i_induct_t) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_magne, fhd_sym_magne)
!
      else if( (i_field .eq. force_sym1_sym2%i_h_advect)                &
     &    .or. (i_field .eq. force_sym1_sym2%i_h_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_temp, fhd_sym_temp)
      else if( (i_field .eq. force_sym1_sym2%i_ph_advect)               &
     &    .or. (i_field .eq. force_sym1_sym2%i_ph_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_par_temp, fhd_sym_part_temp)
      else if( (i_field .eq. force_sym1_sym2%i_c_advect)                &
     &    .or. (i_field .eq. force_sym1_sym2%i_c_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_light, fhd_sym_light)
      else if( (i_field .eq. force_sym1_sym2%i_pc_advect)               &
     &    .or. (i_field .eq. force_sym1_sym2%i_pc_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_par_light, fhd_sym_part_light)
      end if
      check_force_by_sym_sym_id = iflag
      return
!
      end function check_force_by_sym_sym_id
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_force_by_asym_asym_id         &
     &         (i_field, field_name, asym_base_fld, force_asym1_asym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: asym_base_fld
      type(base_force_address), intent(in) :: force_asym1_asym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. force_asym1_asym2%i_m_advect)              &
     &    .or. (i_field .eq. force_asym1_asym2%i_m_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_vort, fhd_asym_vort)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
      else if( (i_field .eq. force_asym1_asym2%i_lorentz) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_current, fhd_asym_current)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_magne, fhd_asym_magne)
      else if( (i_field .eq. force_asym1_asym2%i_maxwell)               &
     &    .or. (i_field .eq. force_asym1_asym2%i_m_tension) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_magne, fhd_asym_magne)
!
      else if( (i_field .eq. force_asym1_asym2%i_buoyancy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_temp, fhd_asym_temp)
      else if( (i_field .eq. force_asym1_asym2%i_comp_buo)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_light, fhd_asym_light)
!
      else if( (i_field .eq. force_asym1_asym2%i_vp_induct)             &
     &    .or. (i_field .eq. force_asym1_asym2%i_induction)             &
     &    .or. (i_field .eq. force_asym1_asym2%i_mag_stretch)           &
     &    .or. (i_field .eq. force_asym1_asym2%i_induct_t) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_magne, fhd_asym_magne)
      else if( (i_field .eq. force_asym1_asym2%i_h_advect)              &
     &    .or. (i_field .eq. force_asym1_asym2%i_h_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_temp, fhd_asym_temp)
      else if( (i_field .eq. force_asym1_asym2%i_ph_advect)             &
     &    .or. (i_field .eq. force_asym1_asym2%i_ph_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_par_temp, fhd_asym_part_temp)
!
      else if( (i_field .eq. force_asym1_asym2%i_c_advect)              &
     &    .or. (i_field .eq. force_asym1_asym2%i_c_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_light, fhd_asym_light)
      else if( (i_field .eq. force_asym1_asym2%i_pc_advect)             &
     &    .or. (i_field .eq. force_asym1_asym2%i_pc_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_par_light, fhd_asym_part_light)
      end if
      check_force_by_asym_asym_id = iflag
      return
!
      end function check_force_by_asym_asym_id
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_force_by_sym_asym_id          &
     &         (i_field, field_name, sym_base_fld, asym_base_fld,       &
     &          force_sym1_asym2, force_asym1_sym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: sym_base_fld
      type(base_field_address), intent(in) :: asym_base_fld
      type(base_force_address), intent(in) :: force_sym1_asym2
      type(base_force_address), intent(in) :: force_asym1_sym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. force_sym1_asym2%i_m_advect)               &
     &    .or. (i_field .eq. force_sym1_asym2%i_m_flux) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_vort, fhd_sym_vort)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
      else if( (i_field .eq. force_sym1_asym2%i_lorentz)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_current, fhd_sym_current)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_magne, fhd_asym_magne)
!
      else if( (i_field .eq. force_sym1_asym2%i_maxwell)                &
     &    .or. (i_field .eq. force_sym1_asym2%i_m_tension) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_magne, fhd_sym_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_magne, fhd_asym_magne)
!
      else if( (i_field .eq. force_sym1_asym2%i_vp_induct)              &
     &    .or. (i_field .eq. force_sym1_asym2%i_induction)              &
     &    .or. (i_field .eq. force_sym1_asym2%i_mag_stretch)            &
     &    .or. (i_field .eq. force_sym1_asym2%i_induct_t) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_magne, fhd_asym_magne)
!
      else if( (i_field .eq. force_sym1_asym2%i_h_advect)               &
     &    .or. (i_field .eq. force_sym1_asym2%i_h_flux)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_temp, fhd_asym_temp)
      else if( (i_field .eq. force_sym1_asym2%i_ph_advect)              &
     &    .or. (i_field .eq. force_sym1_asym2%i_ph_flux)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_par_temp, fhd_asym_part_temp)
      else if( (i_field .eq. force_sym1_asym2%i_c_advect)               &
     &    .or. (i_field .eq. force_sym1_asym2%i_c_flux)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_light, fhd_asym_light)
      else if( (i_field .eq. force_sym1_asym2%i_pc_advect)              &
     &    .or. (i_field .eq. force_sym1_asym2%i_pc_flux)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_par_light, fhd_asym_part_light)
!
!
      else if( (i_field .eq. force_asym1_sym2%i_m_advect)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_vort, fhd_asym_vort)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_velo, fhd_sym_velo)
!
      else if( (i_field .eq. force_asym1_sym2%i_lorentz)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_current, fhd_asym_current)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_magne, fhd_sym_magne)
!
      else if( (i_field .eq. force_asym1_sym2%i_m_tension)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_magne, fhd_asym_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_magne, fhd_sym_magne)
!
      else if( (i_field .eq. force_asym1_sym2%i_vp_induct)              &
     &    .or. (i_field .eq. force_asym1_sym2%i_induction)              &
     &    .or. (i_field .eq. force_asym1_sym2%i_mag_stretch)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_magne, fhd_sym_magne)
!
      else if( (i_field .eq. force_asym1_sym2%i_h_advect)               &
     &    .or. (i_field .eq. force_asym1_sym2%i_h_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_temp, fhd_asym_temp)
!
      else if( (i_field .eq. force_asym1_sym2%i_ph_advect)              &
     &    .or. (i_field .eq. force_asym1_sym2%i_ph_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_par_temp, fhd_sym_part_temp)
      else if( (i_field .eq. force_asym1_sym2%i_c_advect)               &
     &    .or. (i_field .eq. force_asym1_sym2%i_c_flux)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_light, fhd_sym_light)
!
      else if( (i_field .eq. force_asym1_sym2%i_pc_advect)              &
     &    .or. (i_field .eq. force_asym1_sym2%i_pc_flux)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 asym_base_fld%i_velo, fhd_asym_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 sym_base_fld%i_par_light, fhd_asym_part_light)
      end if
      check_force_by_sym_asym_id = iflag
      return
!
      end function check_force_by_sym_asym_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine force_w_sym_monitor_address                            &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_force_sym1_sym2, rms_force_asym1_asym2,             &
     &          rms_force_sym1_asym2, rms_force_asym1_sym2,             &
     &          ave_force_sym1_sym2, ave_force_asym1_asym2,             &
     &          ave_force_sym1_asym2, ave_force_asym1_sym2, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_force_address), intent(inout) :: rms_force_sym1_sym2
      type(base_force_address), intent(inout) :: rms_force_asym1_asym2
      type(base_force_address), intent(inout) :: rms_force_sym1_asym2
      type(base_force_address), intent(inout) :: rms_force_asym1_sym2
      type(base_force_address), intent(inout) :: ave_force_sym1_sym2
      type(base_force_address), intent(inout) :: ave_force_asym1_asym2
      type(base_force_address), intent(inout) :: ave_force_sym1_asym2
      type(base_force_address), intent(inout) :: ave_force_asym1_sym2
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_force_w_sym_addresses((numrms+1), field_name,            &
     &    rms_force_sym1_sym2, rms_force_asym1_asym2,                   &
     &    rms_force_sym1_asym2, rms_force_asym1_sym2, flag_r)
      call set_force_w_sym_addresses((numave+1), field_name,            &
     &    ave_force_sym1_sym2, ave_force_asym1_asym2,                   &
     &    ave_force_sym1_asym2, ave_force_asym1_sym2, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine force_w_sym_monitor_address
!
! ----------------------------------------------------------------------
!
      end module m_force_w_sym_labels
