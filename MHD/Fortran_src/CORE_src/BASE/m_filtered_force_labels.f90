!>@file   m_filtered_force_labels.f90
!!        module m_filtered_force_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!! !!!!!  Filtered force names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   inertia_by_filtered    [i_m_advect]:
!!                    inertia (\omega \times u)
!!   momentum_flux_by_filtered   [i_m_flux]:
!!                    momentum flux     u_{i} u_{j}
!!   Lorentz_force_by_filtered   [i_lorentz]:
!!                    Lorentz force     J \times B
!!   magne_tension_by_filtered  [i_m_tension]:
!!                    magnetic tension   (B \nabla) B
!!   maxwell_tensor_by_filtered    [i_maxwell]:
!!                    maxwell tensor       B_{i} B_{j}
!!
!!   filtered_buoyancy   [i_buoyancy]:   Thermal buoyancy  -\alpha_{T} gT
!!   filtered_comp_buoyancy   [i_comp_buo]:
!!                       compositional buoyancy  -\alpha_{C} gC
!!
!!   vecp_inducti_by_filtered   [i_vp_induct]:
!!                          induction         u \times B
!!   magnetic_induct_by_filtered   [i_induction]:
!!                         magneitic induction \nabla \times (u \times B)
!!   magnetic_stretch_by_filtered    [i_mag_stretch]:
!!                         magneitic streatch         (B \nabla) u
!!   induction_tensor_by_filtered    [i_induct_t]:
!!          induction induction tensor      u_{i} B_{j}  - B_{i} u_{J}
!!   heat_advect_by_filtered         [i_h_advect]:  heat advection
!!                                     (u \cdot \nabla) T
!!   part_h_advect_by_filtered       [i_ph_advect]:
!!                             perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   heat_flux_by_filtered           [i_h_flux]:    heat flux          uT
!!   part_h_flux_by_filtered         [i_ph_flux]:
!!                            perturbation of heat flux    u\Theta
!!
!!   comp_advect_by_filtered [i_c_advect]:    composition advection
!!                                      (u \cdot \nabla) C
!!   comp_advect_by_filtered      [i_pc_advect]:
!!                     perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!   composite_flux_by_filtered     [i_c_flux]:
!!                             composition flux         uC
!!   part_c_flux_by_filtered        [i_pc_flux]:
!!                    perturbation of composition flux   u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_force_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nforce_filter = 19
!
!>        Field label for advection for momentum by filtered field
!!         @f$ u_{j} \partial_{j} u_{i} @f$
      character(len=kchara), parameter                                  &
     &          :: fhd_inertia_by_filter = 'inertia_by_filtered'
!>        Field label for momentum flux by filtered field
!!         @f$ u_{i} u_{j} @f$
      character(len=kchara), parameter                                  &
     &          :: fhd_mom_flux_by_filter = 'momentum_flux_by_filtered'
!>        Field label for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &          :: fhd_Lorentz_by_filter = 'Lorentz_force_by_filtered'
!>        start address for magnetic tension
!!         @f$ B_{j} \partial_{j} B_{i} @f$
      character(len=kchara), parameter :: fhd_mag_tension_by_filter     &
     &                                = 'magne_tension_by_filtered'
!>        Field label for Maxwell tensor
!!         @f$ B_{i} B_{j} @f$
      character(len=kchara), parameter                                  &
     &          :: fhd_maxwell_by_filter = 'maxwell_tensor_by_filtered'
!
!>        Field label for filtered buoyancy
!!         @f$ -\alpha_{C} g_{i} \tilde{T} @f$
      character(len=kchara), parameter                                  &
     &          :: fhd_filter_buo =    'filtered_buoyancy'
!>        Field label for compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C @f$
      character(len=kchara), parameter                                  &
     &          :: fhd_filter_comp_buo = 'filtered_comp_buoyancy'
!!
!>        Field label for induction for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_vp_induct_by_filter = 'vecp_inducti_by_filtered'
!>        Field label for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
      character(len=kchara), parameter :: fhd_mag_induct_by_filter      &
     &                                = 'magnetic_induct_by_filtered'
!>        Field label for magnetic stretch term
!!         @f$ \left(B_{i} \partial_{k} u_{k} \right)@f$
      character(len=kchara), parameter :: fhd_mag_stretch_by_filter     &
     &                                = 'magnetic_stretch_by_filtered'
!>        Field label for Tensor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_induct_t_by_filter = 'induction_tensor_by_filtered'
!
!>        Field label for advection for temperature
!!         @f$ u_{i} \partial_{i} T @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_heat_advect_by_ftr = 'heat_advect_by_filtered'
!>        Field label for advection for perturbation of temperature
!!         @f$ u_{i} \partial_{i} \Theta @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_part_h_advect_by_ftr = 'part_h_advect_by_filtered'
!>        Field label for heat flux
!!         @f$ u_{i} T @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_h_flux_by_filter = 'heat_flux_by_filtered'
!>        Field label for perturbation of heat flux
!!         @f$ u_{i} \Theta @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_ph_flux_by_filter = 'part_h_flux_by_filtered'
!
!>        Field label for advection for composition
!!         @f$ u_{i} \partial_{i} C @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_composit_advect_by_ftr= 'comp_advect_by_filtered'
!>        Field label for advection for perturbation of composition
!!         @f$ u_{i} \partial_{i} \Theta_C @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_part_c_advect_by_ftr = 'part_c_advect_by_filtered'
!>        Field label for compositinoal flux
!!         @f$ u_{i} C @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_c_flux_by_filter = 'composite_flux_by_filtered'
!>        Field label for perturbation of composition flux
!!         @f$ u_{i} \Theta_C @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_pc_flux_by_filter = 'part_c_flux_by_filtered'
!
      end module m_filtered_force_labels
