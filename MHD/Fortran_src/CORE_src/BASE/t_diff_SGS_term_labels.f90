!>@file   t_diff_SGS_term_labels.f90
!!        module t_diff_SGS_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!! !!!!!  product of fields names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   SGS_div_m_flux_true []:      
!!   SGS_Lorentz_true []:  
!!   SGS_mag_induct_true []:    
!!   SGS_div_h_flux_true []:   
!!   SGS_div_c_flux_true []:    
!!
!!   Reynolds_work_true []: 
!!   SGS_Lorentz_work_true []:      
!!   SGS_m_ene_gen_true []: 
!!   SGS_temp_gen_true []: 
!!   SGS_comp_gen_true []: 
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_diff_SGS_term_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nforce_base = 17
!
!
!  fluxes by resolved field
!>        Field label for energy flux of SGS induction
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen =     'SGS_m_ene_gen'
!>        Field label for temperature generation by SGS heat flux
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen =      'SGS_temp_gen'
!>        Field label for work of SGS Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_work =  'SGS_Lorentz_work'
!>        Field label for work of SGS Reynolds stress
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work =     'Reynolds_work'
!>        Field label for work of SGS buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_buo_flux =      'SGS_buoyancy_flux'
!>        Field label for work of SGS compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_buo_flux = 'SGS_comp_buoyancy_flux'
!
!
      end module t_diff_SGS_term_labels
