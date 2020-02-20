!>@file  m_diff_filtered_force_labels.f90
!!       module m_diff_filtered_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   div_inertia_by_filtered, rot_inertia_by_filtered    [i_m_advect]:
!!                            inertia (\omega \times u)
!!   div_Lorentz_force_by_filtered, rot_Lorentz_force_by_filtered 
!!        [i_lorentz]:  Lorentz force     J \times B
!!   div_filtered_buoyancy, rot_filtered_buoyancy   [i_buoyancy]:
!!                            Thermal buoyancy       - \alpha_{T} g T
!!   div_filtered_comp_buoyancy, rot_filtered_comp_buoyancy
!!        [i_comp_buo]: Compositional buoyancy  - \alpha_{C} g C
!!
!!   div_m_flux_by_filtered   [i_m_flux]:
!!                        momentum flux  \partial_{j} (u_{i} u_{j})
!!   div_maxwell_t_by_filtered [i_maxwell]:  maxwell tensor
!!                                  \partial_{j} (B_{i} B_{j})
!!   div_induct_t_by_filtered    [i_induct_t]: induction tensor
!!                         \partial_{i} (u_{i} B_{j}  - B_{i} u_{J})
!!
!!   div_h_flux_by_filtered           [i_h_flux]:    heat flux  uT
!!   div_part_h_flux_by_filtered [i_ph_flux]:  perturbation of heat flux 
!!                                    u\Theta
!!   div_c_flux_by_filtered     [i_c_flux]:     composition flux  uC
!!   div_part_c_flux_by_filtered  [i_pc_flux]:
!!          perturbation of composition flux  u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_diff_filtered_force_labels
!
      use m_precision
!
!  divergence of momentum equations
!>        Field label for divergence of advection
      character(len=kchara), parameter :: fhd_div_inertia_by_filter     &
     &                                = 'div_inertia_by_filtered'
!>        Field label for divergence of Lorentz force
      character(len=kchara), parameter :: fhd_div_Lorentz_by_filter     &
     &                                = 'div_Lorentz_force_by_filtered'
!>        Field label for divergence of filtered buoyancy
      character(len=kchara), parameter :: fhd_div_filter_buo            &
     &                                = 'div_filtered_buoyancy'
!>        Field label for divergence of filtered compositional buoyancy
      character(len=kchara), parameter :: fhd_div_filter_comp_buo       &
     &                                = 'div_filtered_comp_buoyancy'
!
!  rotation of momentum equations
!>        Field label for curl of advection
      character(len=kchara), parameter :: fhd_rot_inertia_by_filter     &
     &                                = 'rot_inertia_by_filtered'
!>        Field label for curl of Lorentz force
      character(len=kchara), parameter :: fhd_rot_Lorentz_by_filter     &
     &                                = 'rot_Lorentz_force_by_filtered'
!>        Field label for curl of filtered buoyancy
      character(len=kchara), parameter :: fhd_rot_filter_buo            &
     &                                = 'rot_filtered_buoyancy'
!>        Field label for curl of compositional buoyancy
      character(len=kchara), parameter :: fhd_rot_filter_comp_buo       &
     &                                = 'rot_filtered_comp_buoyancy'
!
!>        Field label for divergence of momentum flux
!!         @f$ \partial_{j} \left( u_{i} u_{j} \right) @f$
      character(len=kchara), parameter :: fhd_div_m_flux_by_filter      &
     &                                = 'div_m_flux_by_filtered'
!>        Field label for divergence of Maxwell stress
!!         @f$ \partial_{j} \left( B_{i} B_{j} \right) @f$
      character(len=kchara), parameter :: fhd_div_maxwell_t_by_filter   &
     &                                = 'div_maxwell_t_by_filtered'
!>        Field label for divergence of magnetic induction
!!         @f$ \partial_{i} \left(u_{i} B_{j}  - B_{i} u_{J} \right) @f$
      character(len=kchara), parameter :: fhd_div_induct_t_by_filter    &
     &                                = 'div_induct_t_by_filtered'
!
!
!>        Field label for divergence of heat flux
!!         @f$ \partial_{i} \left( u_{i} T \right) @f$
      character(len=kchara), parameter :: fhd_div_h_flux_by_filter      &
     &                                = 'div_h_flux_by_filtered'
!>        Field label for divergence of perturbation of heat flux
!!         @f$ \partial_{i} \left( u_{i} \Theta \right) @f$
      character(len=kchara), parameter :: fhd_div_ph_flux_by_filter     &
     &                                = 'div_part_h_flux_by_filtered'
!
!>        Field label for divergence of composition flux
!!         @f$ \partial_{i} \left( u_{i} C \right) @f$
      character(len=kchara), parameter :: fhd_div_c_flux_by_filter      &
     &                                = 'div_c_flux_by_filtered'
!>        Field label for divergence of perturbation of compopstion flux
!!         @f$ \partial_{i} \left( u_{i} \Theta_C \right) @f$
      character(len=kchara), parameter :: fhd_div_pc_flux_by_filter     &
     &                                = 'div_part_c_flux_by_filtered'
!
      end module m_diff_filtered_force_labels
