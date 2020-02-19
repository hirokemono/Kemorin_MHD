!>@file  t_grad_field_labels.f90
!!       module t_grad_field_labels
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
!!  div_velocity             [i_grad_t]:  divergence of velocity
!!  div_magnetic             [i_grad_t]:  divergence of magnetic field
!!  div_vector_p             [i_grad_t]:  divergence of vector potential
!!
!!  grad_temp                   [i_grad_t]:  gradient of temperature
!!  grad_pert_temp              [i_grad_per_t]:
!!                     gradient of perturbation of temperature
!!  fhd_grad_ref_temp           [i_grad_ref_t]:
!!                     gradient of reference temperature
!!
!!  grad_composition            [i_grad_composit]:
!!                     gradient of composition
!!  grad_pert_composition       [i_grad_per_c]:
!!                     gradient of perturbation of composition
!!  grad_reference_composition  [i_grad_ref_c]:
!!                     gradient of reference composition
!!
!!  grad_density            [i_grad_density]:  gradient of density
!!  grad_pert_density       [i_grad_per_density]:
!!                     gradient of perturbation of density
!!  grad_reference_density  [i_grad_ref_density]:
!!                     gradient of reference density
!!
!!  grad_entropy            [i_grad_entropy]:  gradient of entropy
!!  grad_pert_entropy       [i_grad_per_entropy]:
!!                     gradient of perturbation of entropy
!!  grad_reference_entropy  [i_grad_ref_entropy]:
!!                     gradient of reference entropy
!!
!!   grad_v_1  [i_grad_vx], grad_v_2  [i_grad_vy],
!!   grad_v_3  [i_grad_vz]:   difference of velocity
!!   grad_w_1  [i_grad_wx], grad_w_2  [i_grad_wy],
!!   grad_w_3  [i_grad_wz]:   difference of vorticity
!!   grad_a_1  [i_grad_ax], grad_a_2  [i_grad_ay],
!!   grad_a_3  [i_grad_az]:   difference of vector potential
!!   grad_b_1  [i_grad_bx], grad_b_2  [i_grad_by], 
!!   grad_b_3  [i_grad_bz]:   difference of magnetic field
!!   grad_j_1  [i_grad_jx], grad_j_2 [i_grad_jy]
!!   grad_j_3  [i_grad_jz]:   difference of current density
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_grad_field_labels
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type field_def
        integer(kind = kint) ::  n_comp
        character(len=kchara) :: name
        character(len=kchara) :: math
      end type field_def
!
      type(field_def), parameter :: velocity                            &
     &   = field_def(n_comp = 3, name = 'velocity',                     &
     &               math = ('$u$'//char(0)))
!
!>        Divergence of velocity
!!         @f$ \partial_{i} u_{i} @f$
      character(len=kchara), parameter :: fhd_div_v = 'div_velocity'
!>        Divergence of magnetic field
!!         @f$ \partial_{igrad} B_{i} @f$
      character(len=kchara), parameter :: fhd_div_b = 'div_magnetic'
!>        Divergence of magnetic vector potential
!!         @f$ \partial_{i} A_{i} @f$
      character(len=kchara), parameter :: fhd_div_a = 'div_vector_p'
!
!>        Field label for gradient of @f$ T @f$
      character(len=kchara), parameter :: fhd_grad_temp = 'grad_temp'
!>        Field label for gradient of @f$ \Theta @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_per_temp = 'grad_pert_temp'
!>        Field label for gradient of reference temperature
!!         @f$  \partial T_{0} / dz@f$
!>         or @f$  \partial T_{0} / dr@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_ref_temp = 'grad_reference_temp'
!
!>        Field label for gradient of @f$ C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_composit = 'grad_composition'
!>        Field label for gradient of perturbation of composition
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_per_light = 'grad_pert_composition'
!>        Field label for gradient of reference composition
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_ref_light = 'grad_reference_composition'
!
!>        Field label for gradient of @f$ \rho @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_density = 'grad_density'
!>        Field label for gradient of perturbation of density
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_per_density = 'grad_pert_density'
!>        Field label for gradient of reference density
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_ref_density = 'grad_reference_density'
!
!>        Field label for gradient of @f$ S @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_entropy = 'grad_entropy'
!>        Field label for gradient of perturbation of entropy
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_per_entropy = 'grad_pert_entropy'
!>        Field label for gradient of reference density
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_ref_entropy = 'grad_reference_entropy'
!
!
!
!  difference of field
!>        Field label for gradient of @f$ u_{x} @f$
      character(len=kchara), parameter :: fhd_grad_v_1 =  'grad_v_1'
!>        Field label for gradient of @f$ u_{y} @f$
      character(len=kchara), parameter :: fhd_grad_v_2 =  'grad_v_2'
!>        Field label for gradient of @f$ u_{z} @f$
      character(len=kchara), parameter :: fhd_grad_v_3 =  'grad_v_3'
!
!>        Field label for gradient of @f$ \omega_{x} @f$
      character(len=kchara), parameter :: fhd_grad_w_1 =  'grad_w_1'
!>        Field label for gradient of @f$ \omega_{y} @f$
      character(len=kchara), parameter :: fhd_grad_w_2 =  'grad_w_2'
!>        Field label for gradient of @f$ \omega_{z} @f$
      character(len=kchara), parameter :: fhd_grad_w_3 =  'grad_w_3'
!
!>        Field label for gradient of @f$ A_{x} @f$
      character(len=kchara), parameter :: fhd_grad_a_1 =  'grad_a_1'
!>        Field label for gradient of @f$ A_{y} @f$
      character(len=kchara), parameter :: fhd_grad_a_2 =  'grad_a_2'
!>        Field label for gradient of @f$ A_{z} @f$
      character(len=kchara), parameter :: fhd_grad_a_3 =  'grad_a_3'
!
!>        Field label for gradient of @f$ B_{x} @f$
      character(len=kchara), parameter :: fhd_grad_b_1 =  'grad_b_1'
!>        Field label for gradient of @f$ B_{y} @f$
      character(len=kchara), parameter :: fhd_grad_b_2 =  'grad_b_2'
!>        Field label for gradient of @f$ B_{z} @f$
      character(len=kchara), parameter :: fhd_grad_b_3 =  'grad_b_3'
!
!>        Field label for gradient of @f$ J_{x} @f$
      character(len=kchara), parameter :: fhd_grad_j_1 =  'grad_j_1'
!>        Field label for gradient of @f$ J_{y} @f$
      character(len=kchara), parameter :: fhd_grad_j_2 =  'grad_j_2'
!>        Field label for gradient of @f$ J_{z} @f$
      character(len=kchara), parameter :: fhd_grad_j_3 =  'grad_j_3'
!
!
      type gradient_field_address
!>        start address for velocity
!!         @f$ \partial_{i} u_{i} @f$
        integer (kind=kint) :: i_div_v =           izero
!>        start address for magnetic field
!!         @f$ \partial_{i} B_{i} @f$
        integer (kind=kint) :: i_div_b =           izero
!>        start address for magnetic vector potential
!!         @f$ \partial_{i} A_{i} @f$
        integer (kind=kint) :: i_div_a =           izero
!
!>        Field address for gradient of @f$ T @f$
        integer (kind=kint) :: i_grad_t =           izero
!>        Field address for gradient of @f$ \Theta @f$
        integer (kind=kint) :: i_grad_per_t =      izero
!>        Field address for gradient of reference temperature
!!         @f$  \partial T_{0} / dz@f$
!>         or @f$  \partial T_{0} / dr@f$
        integer (kind=kint) :: i_grad_ref_t =      izero
!
!>        Field address for gradient of @f$ C @f$
        integer (kind=kint) :: i_grad_composit =    izero
!>        Field address for gradient of perturbation of composition
        integer (kind=kint) :: i_grad_per_c =       izero
!>        Field address for gradient of reference composition
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
        integer (kind=kint) :: i_grad_ref_c =       izero
!
!>        Field address for gradient of @f$ \rho @f$
        integer (kind=kint) :: i_grad_density =    izero
!>        Field address for gradient of perturbation of density
        integer (kind=kint) :: i_grad_per_density =    izero
!>        Field address for gradient of reference density
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
        integer (kind=kint) :: i_grad_ref_density =    izero
!
!>        Field address for gradient of @f$ S @f$
        integer (kind=kint) :: i_grad_entropy =     izero
!>        Field address for gradient of perturbation of entropy
        integer (kind=kint) :: i_grad_per_entropy = izero
!>        Field address for gradient of reference density
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
        integer (kind=kint) :: i_grad_ref_entropy = izero
!
!  difference of field
!
!>        Field address for gradient of @f$ u_{x} @f$
        integer (kind=kint) :: i_grad_vx = izero
!>        Field address for gradient of @f$ u_{y} @f$
        integer (kind=kint) :: i_grad_vy = izero
!>        Field address for gradient of @f$ u_{z} @f$
        integer (kind=kint) :: i_grad_vz = izero
!
!>        Field address for gradient of @f$ \omega_{x} @f$
        integer (kind=kint) :: i_grad_wx = izero
!>        Field address for gradient of @f$ \omega_{y} @f$
        integer (kind=kint) :: i_grad_wy = izero
!>        Field address for gradient of @f$ \omega_{z} @f$
        integer (kind=kint) :: i_grad_wz = izero
!
!>        Field address for gradient of @f$ A_{x} @f$
        integer (kind=kint) :: i_grad_ax = izero
!>        Field address for gradient of @f$ A_{y} @f$
        integer (kind=kint) :: i_grad_ay = izero
!>        Field address for gradient of @f$ A_{z} @f$
        integer (kind=kint) :: i_grad_az = izero
!
!>        Field address for gradient of @f$ B_{x} @f$
        integer (kind=kint) :: i_grad_bx = izero
!>        Field address for gradient of @f$ B_{y} @f$
        integer (kind=kint) :: i_grad_by = izero
!>        Field address for gradient of @f$ B_{z} @f$
        integer (kind=kint) :: i_grad_bz = izero
!
!>        Field address for gradient of @f$ J_{x} @f$
        integer (kind=kint) :: i_grad_jx = izero
!>        Field address for gradient of @f$ J_{y} @f$
        integer (kind=kint) :: i_grad_jy = izero
!>        Field address for gradient of @f$ J_{z} @f$
        integer (kind=kint) :: i_grad_jz = izero
      end type gradient_field_address
!
!
      end module t_grad_field_labels
