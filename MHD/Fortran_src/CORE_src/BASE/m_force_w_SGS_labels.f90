!>@file   m_force_w_SGS_labels.f90
!!        module m_force_w_SGS_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!! !!!!!  force include SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   intertia_w_SGS    [i_m_advect]:        inertia (\omega \times u)
!!   momentum_flux_w_SGS   [i_m_flux]:  momentum flux     u_{i} u_{j}
!!   Lorentz_w_SGS   [i_lorentz]:  Lorentz force     J \times B
!!   maxwell_tensor_w_SGS    [i_maxwell]:  maxwell tensor   B_{i} B_{j}
!!
!!   vecp_induction_w_SGS   [i_vp_induct]:     induction    u \times B
!!   induction_w_SGS   [i_induction]:
!!                         magneitic induction \nabla \times (u \times B)
!!   induction_tensor_w_SGS  [i_induct_t]: induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!
!!   heat_flux_w_SGS     [i_h_flux]:    heat flux          uT
!!   comp_flux_w_SGS     [i_c_flux]:    composition flux         uC
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_force_w_SGS_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nforce_base = 23
!
!>        Field label for momentum flux
!!         @f$ u_{i} u_{j}
!!            + (\overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j})@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mom_flux_w_sgs = 'momentum_flux_w_SGS'
!>        Field label for momentum flux
!!         @f$ B_{i} B_{j}
!!            + (\overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j})@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_maxwell_t_w_sgs = 'maxwell_tensor_w_SGS'
!
!>        Field label for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i}
!!           + e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_inertia_w_sgs = 'intertia_w_SGS'
!>        Field label for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k}
!!           + e_{ijk}\left(\overline{B{j}u_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz_w_sgs = 'Lorentz_w_SGS'
!
!>        Field label for inductino for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
!!           + e_{ijk}\left(\overline{u{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vp_induct_w_sgs = 'vecp_induction_w_SGS'
!>        Field label for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
!!           + e_{ijk} \partial_{j}(e_{klm}\left(\overline{u{l}B_{m}}
!!                              - \bar{u}_{l}\bar{B}_{m} \right)) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_induct_w_sgs = 'induction_w_SGS'
!
!>        Field label for heat flux
!!         @f$ u_{i} T + (\overline{u_{i}T} - \bar{u}_{i}\bar{T}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_h_flux_w_sgs =  'heat_flux_w_SGS'
!>        Field label for compositinoal flux
!!         @f$ u_{i} C + (\overline{u_{i}C} - \bar{u}_{i}\bar{C}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_flux_w_sgs =  'comp_flux_w_SGS'
!
!>        Field label for Tensor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_induct_t_w_sgs =   'induction_tensor_w_SGS'
!
      end module m_force_w_SGS_labels
