!>@file   t_diffusion_term_labels.f90
!!        module t_diffusion_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms
!!
!!@verbatim
!! !!!!!  SGS model coefficients names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   viscous_diffusion    []:
!!   Csim_SGS_Lorentz   [i_SGS_Lorentz]:
!!
!!   Csim_SGS_buoyancy  [i_SGS_buoyancy]:
!!   Csim_SGS_composit_buo [i_SGS_comp_buo]:
!!
!!   Csim_SGS_vp_induction   [i_SGS_vp_induct]:
!!
!!   Csim_SGS_heat_flux       [i_SGS_h_flux]:
!!   Csim_SGS_composit_flux   [i_SGS_c_flux]:
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
      module t_diffusion_term_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: num_diffusion = 12
!
!
!>        Field label for viscous diffusion
!!         @f$ \nu \partial_{j}\partial_{j} u_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_viscous = 'viscous_diffusion'
!>        Field label for diffusion of vetor potential
!!         @f$ -J_{i} = \eta \partial_{j}\partial_{j} A_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vecp_diffuse = 'diffuse_vector_p'
!>        Field label for magnetic diffusion
!!         @f$ \nu \partial_{j}\partial_{j} B_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_diffuse = 'magnetic_diffusion'
!>        Field label for thermal diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_thermal_diffusion = 'thermal_diffusion'
!>        Field label for compositional diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_diffuse = 'composition_diffusion'
!
!
!>        Field label for divergence of viscousity
      character(len=kchara), parameter                                  &
     &             :: fhd_div_viscous =    'div_viscousity'
!>        Field label for diffusion of vorticity
!!         @f$ \nu \partial_{j}\partial_{j} \omega_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_w_viscous = 'vorticity_diffusion'
!
!   --------------------------------------------------------------------
!
!>        Field label for viscosity   @f$ \mu @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_viscosity =        'viscosity'
!>        Field label for thermal diffusivity @f$ k @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_T_conductivity =   'thermal_conductivity'
!
!
!>        Field label for kinetic viscosity
!>                               @f$ \nu = \mu / \bar{\rho} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_K_viscosity =      'kinetic_viscosity'
!>        Field label for thermal diffusivity 
!!                               @f$ \kappa_{T} = k / \bar{\rho} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_T_diffusivity =    'thermal_diffusivity'
!>        Field label for chemical diffusivity
      character(len=kchara), parameter                                  &
     &             :: fhd_C_diffusivity =    'chemical_diffusivity'
!>        Field label for magnetic diffusivity
      character(len=kchara), parameter                                  &
     &             :: fhd_B_diffusivity =    'magnetic_diffusivity'
!
!   --------------------------------------------------------------------
!
      end module t_diffusion_term_labels
