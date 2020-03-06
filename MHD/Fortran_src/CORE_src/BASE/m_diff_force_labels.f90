!>@file  m_diff_force_labels.f90
!!       module m_diff_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   div_inertia, rot_inertia    [i_m_advect]:
!!                            inertia (\omega \times u)
!!   div_Lorentz_force, rot_Lorentz_force   [i_lorentz]:
!!                            Lorentz force     J \times B
!!   div_Coriolis_force, rot_Coriolis_force   [i_coriolis]:
!!                            Coriolis force     2 \Omega \times u
!!   div_buoyancy, rot_buoyancy   [i_buoyancy]:
!!                            Thermal buoyancy       - \alpha_{T} g T
!!   div_composite_buoyancy, rot_composite_buoyancy   [i_comp_buo]:
!!                            Compositional buoyancy  - \alpha_{C} g C
!!
!!   div_m_flux   [i_m_flux]:  momentum flux  \partial_{j} (u_{i} u_{j})
!!   div_maxwell_t [i_maxwell]:  maxwell tensor
!!                                  \partial_{j} (B_{i} B_{j})
!!   div_induct_t    [i_induct_t]: induction tensor
!!                         \partial_{i} (u_{i} B_{j}  - B_{i} u_{J})
!!
!!   div_heat_flux           [i_h_flux]:    heat flux          uT
!!   div_pert_heat_flux      [i_ph_flux]:  perturbation of heat flux 
!!                                    u\Theta
!!   div_composition_flux     [i_c_flux]:     composition flux         uC
!!   div_pert_composition_flux  [i_pc_flux]:  perturbation of composition flux
!!                                      u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_diff_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
!  rotation of momentum equations
!>        Field label for curl of advection
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_inertia =  'rot_inertia'
!>        Field label for curl of Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_Lorentz =  'rot_Lorentz_force'
!>        Field label for curl of Coriolis term
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_Coriolis = 'rot_Coriolis_force'
!>        Field label for curl of buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_buoyancy = 'rot_buoyancy'
!>        Field label for curl of compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_comp_buo = 'rot_composite_buoyancy'
!
!  divergence of momentum equations
!>        Field label for divergence of advection
      character(len=kchara), parameter                                  &
     &             :: fhd_div_inertia =    'div_inertia'
!>        Field label for divergence of Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_div_Lorentz =    'div_Lorentz_force'
!>        Field label for divergence of Coriolis force
      character(len=kchara), parameter                                  &
     &             :: fhd_div_Coriolis =   'div_Coriolis_force'
!>        Field label for divergence of buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_div_buoyancy =   'div_buoyancy'
!>        Field label for divergence of compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_div_comp_buo =   'div_composite_buoyancy'
!
!>        Field label for divergence of momentum flux
!!         @f$ \partial_{j} \left( u_{i} u_{j} \right) @f$
      character(len=kchara), parameter :: fhd_div_m_flux = 'div_m_flux'
!>        Field label for divergence of Maxwell stress
!!         @f$ \partial_{j} \left( B_{i} B_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_maxwell_t = 'div_maxwell_t'
!>        Field label for divergence of magnetic induction
!!         @f$ \partial_{i} \left(u_{i} B_{j}  - B_{i} u_{J} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_induct_t =  'div_induct_t'
!
!
!>        Field label for divergence of heat flux
!!         @f$ \partial_{i} \left( u_{i} T \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_h_flux =        'div_heat_flux'
!>        Field label for divergence of perturbation of heat flux
!!         @f$ \partial_{i} \left( u_{i} \Theta \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_ph_flux =       'div_pert_heat_flux'
!
!>        Field label for divergence of composition flux
!!         @f$ \partial_{i} \left( u_{i} C \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_c_flux =        'div_composition_flux'
!>        Field label for divergence of perturbation of compopstion flux
!!         @f$ \partial_{i} \left( u_{i} \Theta_C \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_pc_flux =       'div_pert_composition_flux'
!
      end module m_diff_force_labels
