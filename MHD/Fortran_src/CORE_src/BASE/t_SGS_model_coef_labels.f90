!>@file   t_SGS_model_coef_labels.f90
!!        module t_SGS_model_coef_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms
!!
!!@verbatim
!! !!!!!  SGS model coefficients names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   Csim_SGS_inertia    [i_SGS_inertia]: SGS inertia
!!   Csim_SGS_Lorentz   [i_SGS_Lorentz]:   SGS Lorentz force
!!
!!   Csim_SGS_buoyancy  [i_SGS_buoyancy]:  SGS Thermal buoyancy
!!   Csim_SGS_composit_buo [i_SGS_comp_buo]:  SGS compositional buoyancy
!!
!!   Csim_SGS_vp_induction   [i_SGS_vp_induct]: SGS induction  u \times 
!!
!!   Csim_SGS_heat_flux       [i_SGS_h_flux]:   SGS heat flux
!!   Csim_SGS_composit_flux   [i_SGS_c_flux]:   SGS composition flux
!!
!!
!!   fhd_SGS_simi    []:
!!   fhd_SGS_grad    []:
!!   fhd_SGS_grad_f  []:
!!
!!   SGS_diffuse     []:
!!
!!   temp_4_SGS      []:
!!   temp_4_SGS      []:
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_model_coef_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: num_Csim_SGS = 11
!
!
!>        Field label for model coefficient of SGS heat flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_h_flux = 'Csim_SGS_heat_flux'
!>        Field label for model coefficient of SGS composition flux
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_c_flux = 'Csim_SGS_composit_flux'
!>        Field label for model coefficient of SGS inertia term
!!         @f$ e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_m_flux = 'Csim_SGS_inertia'
!>        Field label for model coefficient of SGS Lorentz force
!!         @f$ e_{ijk}\left(\overline{\J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_Lorentz = 'Csim_SGS_Lorentz'
!>        Field label for model coefficient of SGS induction
!!         @f$ e_{ijk}\left(\overline{\u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_induction = 'Csim_SGS_vp_induction'
!>        Field label for odel coefficient of SGS buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_buoyancy =  'Csim_SGS_buoyancy'
!>        Field label for odel coefficient of SGS compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_comp_buo = 'Csim_SGS_composit_buo'
!
!
!>        Field label for SGS term by scale similarity method
      character(len=kchara), parameter :: fhd_SGS_simi =   'SGS_simi'
!>        Field label for SGS term by nonlinear gradient method
      character(len=kchara), parameter :: fhd_SGS_grad =   'SGS_grad'
!>        Field label for SGS term by nonlinear gradient method
!>        using fileterd field
      character(len=kchara), parameter :: fhd_SGS_grad_f = 'SGS_grad_f'
!>        Field label for SGS term by turbulence diffusivity
      character(len=kchara), parameter                                  &
     &              :: fhd_SGS_diffuse = 'SGS_diffuse'
!
!>        Field label for temperature to obatin commutation error
      character(len=kchara), parameter :: fhd_SGS_temp =   'temp_4_SGS'
!>        Field label for composition variation
!!        to obatin commutation error
      character(len=kchara), parameter :: fhd_SGS_comp =   'comp_4_SGS'
!
!
      end module t_SGS_model_coef_labels
