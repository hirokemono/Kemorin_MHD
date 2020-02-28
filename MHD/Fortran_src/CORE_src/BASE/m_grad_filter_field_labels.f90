!>@file   m_grad_filter_field_labels.f90
!!       module m_grad_filter_field_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!  div_filter_velo         [i_div_v]:  divergence of velocity
!!  div_filter_magne        [i_div_b]:  divergence of magnetic field
!!  div_filter_vecp         [i_div_a]:  divergence of vector potential
!!
!!  grad_filtered_temp            [i_grad_t]:  gradient of temperature
!!  grad_filter_pert_temp         [i_grad_per_t]:
!!                     gradient of perturbation of temperature
!!
!!  grad_filtered_comp            [i_grad_composit]:
!!                     gradient of composition
!!  grad_filter_pert_comp       [i_grad_per_c]:
!!                     gradient of perturbation of composition
!!
!!  grad_filter_density            [i_grad_density]:  gradient of density
!!  grad_filter_pert_density       [i_grad_per_density]:
!!                     gradient of perturbation of density
!!
!!  grad_filter_entropy            [i_grad_entropy]:  gradient of entropy
!!  grad_filter_pert_entropy       [i_grad_per_entropy]:
!!                     gradient of perturbation of entropy
!!
!!   grad_filtered_v_1  [i_grad_vx], grad_filtered_v_2  [i_grad_vy],
!!   grad_filtered_v_3  [i_grad_vz]:   difference of velocity
!!   grad_filtered_w_1  [i_grad_wx], grad_filtered_w_2  [i_grad_wy],
!!   grad_filtered_w_3  [i_grad_wz]:   difference of vorticity
!!   grad_filtered_a_1  [i_grad_ax], grad_filtered_a_2  [i_grad_ay],
!!   grad_filtered_a_3  [i_grad_az]:   difference of vector potential
!!   grad_filtered_b_1  [i_grad_bx], grad_filtered_b_2  [i_grad_by], 
!!   grad_filtered_b_3  [i_grad_bz]:   difference of magnetic field
!!   grad_filtered_j_1  [i_grad_jx], grad_filtered_j_2 [i_grad_jy]
!!   grad_filtered_j_3  [i_grad_jz]:   difference of current density
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_grad_filter_field_labels
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>        Field label for filtered velocity
!!         @f$ \partial_{i} \bar{u}_{i} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_div_filter_v = 'div_filter_velo'
!>        Field label for filtered magnetic field
!!         @f$ \partial_{i} \bar{B}_{i} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_div_filter_b = 'div_filter_magne'
!>        Field label for filtered magnetic vector potential
!!         @f$ \partial_{i} \bar{A}_{i} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_div_filter_a = 'div_filter_vecp'
!
!>        Field label for gradient of @f$ \tilde{T} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_temp =     'grad_filtered_temp'
!>        Field label for gradient of @f$ \Theta @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_per_temp = 'grad_filter_pert_temp'
!
!>        Field label for gradient of @f$ \tilde{C} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_comp = 'grad_filtered_comp'
!>        Field label for gradient of perturbation of composition
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_per_comp = 'grad_filter_pert_comp'
!
!>        Field label for gradient of @f$ \rho @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_density = 'grad_filter_density'
!>        Field label for gradient of perturbation of density
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_per_density = 'grad_filter_pert_density'
!
!>        Field label for gradient of @f$ S @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_entropy = 'grad_filter_entropy'
!>        Field label for gradient of perturbation of entropy
      character(len=kchara), parameter                                  &
     &      :: fhd_grad_filter_per_entropy = 'grad_filter_pert_entropy'
!
      end module m_grad_filter_field_labels
