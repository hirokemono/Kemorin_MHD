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
!!      integer(kind = kint) function num_forces_w_symmetry()
!!      subroutine set_force_w_symmetry_names(field_names)
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
      integer(kind = kint), parameter, private :: nforce_w_sym = 69
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
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_forces_w_symmetry()
      num_forces_w_symmetry = nforce_w_sym
      return
      end function num_forces_w_symmetry
!
! ----------------------------------------------------------------------
!
      subroutine set_force_w_symmetry_names(field_names)
!
      character(len = kchara), intent(inout)                            &
     &                        :: field_names(nforce_w_sym)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_wsym_x_usym), CHAR(0)
      write(field_names( 2),'(a,a1)') trim(fhd_wasym_x_uasym), CHAR(0)
      write(field_names( 3),'(a,a1)') trim(fhd_wsym_x_uasym), CHAR(0)
      write(field_names( 4),'(a,a1)') trim(fhd_wasym_x_uaym), CHAR(0)
!
      write(field_names( 5),'(a,a1)') trim(fhd_Jsym_x_Bsym), CHAR(0)
      write(field_names( 6),'(a,a1)') trim(fhd_Jasym_x_Basym), CHAR(0)
      write(field_names( 7),'(a,a1)') trim(fhd_Jsym_x_Basym), CHAR(0)
      write(field_names( 8),'(a,a1)') trim(fhd_Jasym_x_Bsym), CHAR(0)
!
      write(field_names( 9),'(a,a1)')                                   &
     &                  trim(fhd_Bsym_nabla_Bsym), CHAR(0)
      write(field_names(10),'(a,a1)')                                   &
     &                  trim(fhd_Basym_nabla_Basym), CHAR(0)
      write(field_names(11),'(a,a1)')                                   &
     &                  trim(fhd_Bsym_nabla_Basym), CHAR(0)
      write(field_names(12),'(a,a1)')                                   &
     &                  trim(fhd_Basym_nabla_Bsym), CHAR(0)
!
      write(field_names(13),'(a,a1)') trim(fhd_sym_buoyancy), CHAR(0)
      write(field_names(14),'(a,a1)') trim(fhd_asym_buoyancy), CHAR(0)
      write(field_names(15),'(a,a1)') trim(fhd_sym_comp_buo), CHAR(0)
      write(field_names(16),'(a,a1)') trim(fhd_asym_comp_buo), CHAR(0)
!
      write(field_names(17),'(a,a1)') trim(fhd_usym_x_Bsym), CHAR(0)
      write(field_names(18),'(a,a1)') trim(fhd_uasym_x_Basym), CHAR(0)
      write(field_names(19),'(a,a1)') trim(fhd_usym_x_Basym), CHAR(0)
      write(field_names(20),'(a,a1)') trim(fhd_uasym_x_Bsym), CHAR(0)
!
      write(field_names(21),'(a,a1)')                                   &
     &                  trim(fhd_rot_usym_x_Bsym), CHAR(0)
      write(field_names(22),'(a,a1)')                                   &
     &                  trim(fhd_rot_uasym_x_Basym), CHAR(0)
      write(field_names(23),'(a,a1)')                                   &
     &                  trim(fhd_rot_usym_x_Basym), CHAR(0)
      write(field_names(24),'(a,a1)')                                   &
     &                  trim(fhd_rot_uasym_x_Bsym), CHAR(0)
!
      write(field_names(25),'(a,a1)')                                   &
     &                  trim(fhd_Bsym_nabla_usym), CHAR(0)
      write(field_names(26),'(a,a1)')                                   &
     &                  trim(fhd_Basym_nabla_uasym), CHAR(0)
      write(field_names(27),'(a,a1)')                                   &
     &                  trim(fhd_Bsym_nabla_uasym), CHAR(0)
      write(field_names(28),'(a,a1)')                                   &
     &                  trim(fhd_Basym_nabla_usym), CHAR(0)
!
      write(field_names(29),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_Tsym), CHAR(0)
      write(field_names(30),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_Tasym), CHAR(0)
      write(field_names(31),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_Tasym), CHAR(0)
      write(field_names(32),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_Tsym), CHAR(0)
!
      write(field_names(33),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_pTsym), CHAR(0)
      write(field_names(34),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_pTasym), CHAR(0)
      write(field_names(35),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_pTasym), CHAR(0)
      write(field_names(36),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_pTsym), CHAR(0)
!
      write(field_names(37),'(a,a1)') trim(fhd_h_flux_sym_sym), CHAR(0)
      write(field_names(38),'(a,a1)')                                   &
     &                  trim(fhd_h_flux_asym_asym), CHAR(0)
      write(field_names(39),'(a,a1)')                                   &
     &                  trim(fhd_h_flux_sym_asym), CHAR(0)
      write(field_names(40),'(a,a1)')                                   &
     &                  trim(fhd_h_flux_asym_sym), CHAR(0)
!
      write(field_names(41),'(a,a1)')                                   &
     &                  trim(fhd_ph_flux_sym_sym), CHAR(0)
      write(field_names(42),'(a,a1)')                                   &
     &                  trim(fhd_ph_flux_asym_asym), CHAR(0)
      write(field_names(43),'(a,a1)')                                   &
     &                  trim(fhd_ph_flux_sym_asym), CHAR(0)
      write(field_names(44),'(a,a1)')                                   &
     &                  trim(fhd_ph_flux_asym_sym), CHAR(0)
!
      write(field_names(45),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_Csym), CHAR(0)
      write(field_names(46),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_Casym), CHAR(0)
      write(field_names(47),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_Casym), CHAR(0)
      write(field_names(48),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_Csym), CHAR(0)
!
      write(field_names(49),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_pCsym), CHAR(0)
      write(field_names(50),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_pCasym), CHAR(0)
      write(field_names(51),'(a,a1)')                                   &
     &                  trim(fhd_usym_nabla_pCasym), CHAR(0)
      write(field_names(52),'(a,a1)')                                   &
     &                  trim(fhd_uasym_nabla_pCsym), CHAR(0)
!
      write(field_names(53),'(a,a1)')                                   &
     &                  trim(fhd_c_flux_sym_sym), CHAR(0)
      write(field_names(54),'(a,a1)')                                   &
     &                  trim(fhd_c_flux_asym_asym), CHAR(0)
      write(field_names(55),'(a,a1)')                                   &
     &                  trim(fhd_c_flux_sym_asym), CHAR(0)
      write(field_names(56),'(a,a1)')                                   &
     &                  trim(fhd_c_flux_asym_sym), CHAR(0)
!
      write(field_names(57),'(a,a1)')                                   &
     &                  trim(fhd_pc_flux_sym_sym), CHAR(0)
      write(field_names(58),'(a,a1)')                                   &
     &                  trim(fhd_pc_flux_asym_asym), CHAR(0)
      write(field_names(59),'(a,a1)')                                   &
     &                  trim(fhd_pc_flux_sym_asym), CHAR(0)
      write(field_names(60),'(a,a1)')                                   &
     &                  trim(fhd_pc_flux_asym_sym), CHAR(0)
!
      write(field_names(61),'(a,a1)')                                   &
     &                  trim(fhd_m_flux_sym_sym), CHAR(0)
      write(field_names(62),'(a,a1)')                                   &
     &                  trim(fhd_m_flux_asym_asym), CHAR(0)
      write(field_names(63),'(a,a1)')                                   &
     &                  trim(fhd_m_flux_sym_asym), CHAR(0)
!
      write(field_names(64),'(a,a1)')                                   &
     &                  trim(fhd_maxwell_sym_sym), CHAR(0)
      write(field_names(65),'(a,a1)')                                   &
     &                  trim(fhd_maxwell_asym_asym), CHAR(0)
      write(field_names(66),'(a,a1)')                                   &
     &                  trim(fhd_maxwell_sym_asym), CHAR(0)
!
      write(field_names(67),'(a,a1)') trim(fhd_usym_Bsym), CHAR(0)
      write(field_names(68),'(a,a1)') trim(fhd_uasym_Basym), CHAR(0)
      write(field_names(69),'(a,a1)') trim(fhd_uasym_Bsym), CHAR(0)
!
      end subroutine set_force_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_force_w_sym_labels
