!>@file   t_field_product_labels.f90
!!        module t_field_product_labels
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
!!   geostrophic_balance []:      
!!   truncated_magnetic_field []:  
!!
!!   kinetic_helicity []:    
!!   magnetic_helicity []:   
!!   current_helicity []:    
!!   cross_helicity []:      
!!
!!   square_velocity []: 
!!   square_vorticity []: 
!!   square_magne []: 
!!   square_vector_p []: 
!!   square_current []: 
!!   square_temperature []: 
!!   square_composition []: 
!!
!!   velocity_scale []: 
!!   magnetic_scale []: 
!!   temperature_scale []: 
!!   composition_scale []: 
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_field_product_labels
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
!>        Field label for geostrophic balance
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} + \partial_{i} p @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_geostrophic =  'geostrophic_balance'
!
!>        Field label for filtered vetor potential
!!         @f$ \bar{A}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_truncated_B = 'truncated_magnetic_field'
!
!
!>        Field label for kinetic helicity
!!         @f$ H_{u} = u_{i} \omega_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_kinetic_helicity =  'kinetic_helicity'
!>        Field label for magnetic helicity
!!         @f$ H_{B} = B_{i} A_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_magnetic_helicity = 'magnetic_helicity'
!>        Field label for current helicity
!!         @f$ H_{J} = J_{i} B_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_current_helicity =  'current_helicity'
!>        Field label for cross helicity
!!         @f$ H_{x} = u_{i} B{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_cross_helicity =    'cross_helicity'
!
!  Square of each component of fields
!
!>        Square of velocity @f$ u^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_v = 'square_velocity'
!>        Square of vorticity @f$ \omega^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_w = 'square_vorticity'
!>        Square of magnetic field @f$ B^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_b = 'square_magne'
!>        Square of magnetic vector potential @f$ A^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_a = 'square_vector_p'
!>        Square of current density @f$ J^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_j = 'square_current'
!>        Square of temperature @f$ T^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_t = 'square_temperature'
!>        Square of composition @f$ C^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_c = 'square_composition'
!
!
!>        Field label for velocity length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_velocity_scale =    'velocity_scale'
!>        Field label for magnetic field length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_magnetic_scale =    'magnetic_scale'
!>        Field label for temperature length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_temp_scale =        'temperature_scale'
!>        Field label for composition length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_composition_scale = 'composition_scale'
!
      end module t_field_product_labels
