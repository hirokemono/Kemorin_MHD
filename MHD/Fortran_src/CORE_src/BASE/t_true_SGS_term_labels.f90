!>@file   t_true_SGS_term_labels.f90
!!        module t_true_SGS_term_labels
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
      module t_true_SGS_term_labels
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
!>        Field label for true divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \overline{u_{i}u_{j}}
!!            - \bar{u}_{i}\bar{u}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_m_flux_true = 'SGS_div_m_flux_true'
!>        Field label for true divergence of SGS Maxwell tensor
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}}
!!             - \bar{B}_{i}\bar{B}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_true =    'SGS_Lorentz_true'
!>        Field label for true divergence
!>            of SGS magnetic induction tensor
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_mag_induct_true = 'SGS_mag_induct_true'
!
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T} 
!!                                - \bar{u}_{i}\bar{T} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_h_flux_true = 'SGS_div_h_flux_true'
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C}
!!                                - \bar{u}_{i}\bar{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_c_flux_true = 'SGS_div_c_flux_true'
!
!>        Field label for work of true SGS Reynolds stress
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work_true =  'Reynolds_work_true'
!>        Field label for work of true SGS Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_wk_true = 'SGS_Lorentz_work_true'
!>        Field label for energy flux of true SGS induction
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen_true =  'SGS_m_ene_gen_true'
!>        Field label for temperature generation
!!                   by true SGS heat flux
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen_true =   'SGS_temp_gen_true'
!>        Field label for composition generation
!!                   by true SGS compostion flux
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_gen_true =   'SGS_comp_gen_true'
!
      end module t_true_SGS_term_labels
