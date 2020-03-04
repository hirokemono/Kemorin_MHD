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
!!   momentum_flux_w_SGS    [i_SGS_m_flux]
!!   maxwell_tensor_w_SGS   [i_SGS_maxwell]
!!   induction_tensor_w_SGS [i_SGS_induct_t]
!!
!!   heat_flux_w_SGS        [i_SGS_h_flux]
!!   comp_flux_w_SGS        [i_SGS_c_flux]
!!
!!   intertia_w_SGS         [i_SGS_inertia]
!!   Lorentz_w_SGS          [i_SGS_Lorentz]
!!
!!   vecp_induction_w_SGS   [i_SGS_vp_induct]
!!   induction_w_SGS        [i_SGS_induction]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_force_w_SGS_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_base_force_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nforce_w_SGS = 23
!
!>        Field label of momentum flux with SGS term
!!         @f$ u_{i} u_{j}
!!            + (\widetilde{u_{i}u_{j}} - \tilde{u}_{i}\tilde{u}_{j})@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mom_flux_w_sgs = 'momentum_flux_w_SGS'
      type(field_def), parameter :: momentum_flux_w_SGS                 &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'momentum_flux_w_SGS',                     &
     &                math = '$ u_{i}u_{j} + (\widetilde{u_{i}u_{j}}'   &
     &                     // ' - \tilde{u}_{i}\tilde{u}_{j}) $')
!>        Field label of momentum flux with SGS term
!!         @f$ B_{i} B_{j}
!!            + (\widetilde{B_{i}B_{j}} - \tilde{B}_{i}\tilde{B}_{j}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_maxwell_t_w_sgs = 'maxwell_tensor_w_SGS'
      type(field_def), parameter :: maxwell_tensor_w_SGS                &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'maxwell_tensor_w_SGS',                    &
     &                math = '$ B_{i}B_{j} + (\widetilde{B_{i}B_{j}}'   &
     &                     // ' - \tilde{B}_{i}\tilde{B}_{j})$')
!>        Field label of Tensor for magnetic induction with SGS term
!!         @f$ u_{i} B_{j}  - B_{i} u_{J}
!!         @f$ + \widetilde{u_{i}B_{j}} - \tilde{u}_{i}\tilde{B}_{j}
!!             - \widetilde{B_{i}u_{j}} + \tilde{B}_{i}\tilde{u}_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_induct_t_w_sgs =   'induction_tensor_w_SGS'
      type(field_def), parameter :: induction_tensor_w_SGS              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'induction_tensor_w_SGS',                  &
     &                math = '$ u_{i} B_{j}  - B_{i} u_{J}'             &
     &                     // ' + \widetilde{u_{i}B_{j}}'               &
     &                     // ' - \tilde{u}_{i}\tilde{B}_{j}'           &
     &                     // ' - \widetilde{B_{i}u_{j}}'               &
     &                     // ' + \tilde{B}_{i}\tilde{u}_{j} $')
!
!>        Field label of heat flux with SGS term
!!         @f$ u_{i}T + (\widetilde{u_{i}T} - \tilde{u}_{i}\tilde{T}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_h_flux_w_sgs =  'heat_flux_w_SGS'
      type(field_def), parameter :: heat_flux_w_SGS                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'heat_flux_w_SGS',                         &
     &                math = '$ u_{i} T + (\widetilde{u_{i}T}'          &
     &                             // ' - \tilde{u}_{i}\tilde{T}) $')
!>        Field label of compositinoal flux with SGS term
!!         @f$ u_{i} C + (\widetilde{u_{i}C} - \tilde{u}_{i}\tilde{C}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_flux_w_sgs =  'compostion_flux_w_SGS'
      type(field_def), parameter :: compostion_flux_w_SGS               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'compostion_flux_w_SGS',                   &
     &                math = '$ u_{i} C + (\widetilde{u_{i}C}'          &
     &                             // ' - \tilde{u}_{i}\tilde{C}) $')
!
!>        Field label of advection for momentum with SGS term
!!         @f$ e_{ijk} \omega_{j} u_{k}
!!           + e_{ijk} (\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_inertia_w_sgs = 'intertia_w_SGS'
      type(field_def), parameter :: intertia_w_SGS                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'intertia_w_SGS',                          &
     &                math = '$ e_{ijk} \omega_{j} u_{k}'               &
     &                  // ' + e_{ijk} (\widetilde{\omega_{j}u_{k}}'    &
     &                  // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label of Lorentz force with SGS term
!!         @f$ e_{ijk} J_{j} B_{k}
!!           + e_{ijk} (\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz_w_sgs = 'Lorentz_w_SGS'
      type(field_def), parameter :: Lorentz_w_SGS                       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'Lorentz_w_SGS',                           &
     &                math = '$ e_{ijk} J_{j} B_{k}'                    &
     &                    // ' + e_{ijk} (\widetilde{J_{j}B_{k}}'       &
     &                    // ' - \tilde{J}_{j}\tilde{B}_{k})$')
!
!>        Field label of inductino for vector potential with SGS term
!!         @f$ e_{ijk} u_{j} B_{k} @f$
!!           + e_{ijk} (\widetilde{u{j}B_{k}}
!!            - \tilde{u}_{j}\tilde{B}_{k}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vp_induct_w_sgs = 'vecp_induction_w_SGS'
      type(field_def), parameter :: vecp_induction_w_SGS                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vecp_induction_w_SGS',                    &
     &                math = '$ e_{ijk} u_{j}B_{k}'                     &
     &                    // ' + e_{ijk} (\widetilde{u{j}B_{k}}'        &
     &                    // ' - \tilde{u}_{j}\tilde{B}_{k}) $')
!>        Field label of magnetic induction with SGS term
!!         @f$ e_{ijk} \partial_{j} (e_{klm}u_{l}B_{m})  
!!           + e_{ijk} \partial_{j}(e_{klm} (\widetilde{u{l}B_{m}}
!!                              - \tilde{u}_{l}\tilde{B}_{m})) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_induct_w_sgs = 'induction_w_SGS'
      type(field_def), parameter :: induction_w_SGS                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'induction_w_SGS',                         &
     &                math = '$e_{ijk} \partial_{j}(e_{klm}u_{l}B_{m})' &
     &                    // ' + e_{ijk} \partial_{j}(e_{klm}'          &
     &                    // ' (\widetilde{u{l}B_{m}}'                  &
     &                    // ' - \tilde{u}_{l}\tilde{B}_{m})) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      end module m_force_w_SGS_labels
