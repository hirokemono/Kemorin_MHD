!>@file   m_grad_filter_field_labels.f90
!!       module m_grad_filter_field_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields of gradient of filtered field
!!
!!@verbatim
!!      logical function check_div_filter_field(field_name)
!!      logical function check_grad_filter_field(field_name)
!!
!!      subroutine set_div_filtered_field_names(array_c2i)
!!      subroutine set_grad_filtered_field_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!! gradient of filtered field !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names  [Address]
!!
!!  div_filtered_velo               [grad_fil_fld%i_div_v]
!!  div_filtered_magne              [grad_fil_fld%i_div_b]
!!  div_filtered_vector_potential   [grad_fil_fld%i_div_a]
!!
!!  grad_filtered_temp            [grad_fil_fld%i_grad_t]
!!  grad_filtered_pert_temp       [grad_fil_fld%i_grad_per_t]
!!
!!  grad_filtered_comp            [grad_fil_fld%i_grad_composit]
!!  grad_filtered_pert_comp       [grad_fil_fld%i_grad_per_c]
!!
!!  grad_filtered_density          [grad_fil_fld%i_grad_density]
!!  grad_filtered_pert_density     [grad_fil_fld%i_grad_per_density]
!!
!!  grad_filtered_entropy          [grad_fil_fld%i_grad_entropy]
!!  grad_filtered_pert_entropy     [grad_fil_fld%i_grad_per_entropy]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_grad_filter_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit none
!
!>        Divergence of velocity
!!         @f$ \partial_{i} \tilde{u}_{i} @f$
      type(field_def), parameter :: div_filtered_velo                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_filtered_velo',                       &
     &                math = '$ \partial_{i} \tilde{u}_{i} $')
!>        Divergence of magnetic field
!!         @f$ \partial_{igrad} \tilde{B}_{i} @f$
      type(field_def), parameter :: div_filtered_magne                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_filtered_magne',                      &
     &                math = '$ \partial_{i} \tilde{B}_{i} $')
!>        Divergence of magnetic vector potential
!!         @f$ \partial_{i} \tilde{A}_{i} @f$
      type(field_def), parameter :: div_filtered_vector_potential       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_filtered_vector_potential',           &
     &                math = '$ \partial_{i} \tilde{A}_{i} $')
!
!>        Field label for gradient of @f$ T @f$
!!         @f$  \partial_{i} \tilde{T} / dz@f$
      type(field_def), parameter :: grad_filtered_temp                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_temp',                      &
     &                math = '$ \partial_{i} \tilde{T} $')
!>        Field label for gradient of @f$ \Theta @f$
!!         @f$  \partial_{i} \tilde{\Theta} / dz@f$
      type(field_def), parameter :: grad_filtered_pert_temp             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_pert_temp',                 &
     &                math = '$ \partial_{i} \tilde{\Theta} $')
!
!>        Field label for gradient of @f$ C @f$
!!         @f$  \partial_{i} \tilde{C} / dz@f$
      type(field_def), parameter :: grad_filtered_comp                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_comp',                      &
     &                math = '$ \partial_{i} \tilde{C} $')
!>        Field label for gradient of perturbation of composition
!!         @f$  \partial_{i} \tilde{\Theta}_{C} / dz@f$
      type(field_def), parameter :: grad_filtered_pert_comp             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_pert_comp',                 &
     &                math = '$ \partial_{i} \tilde{\Theta}_{C} $')
!
!>        Field label for gradient of density
!!         @f$  \partial_{i} \tilde{\rho} / dz@f$
      type(field_def), parameter :: grad_filtered_density               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_density',                   &
     &                math = '$ \partial_{i} \tilde{\rho} $')
!>        Field label for gradient of perturbation of density
!!         @f$  \partial_{i} \tilde{\Theta}_{\rho} / dz@f$
      type(field_def), parameter :: grad_filtered_pert_density          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_pert_density',              &
     &                math = '$ \partial_{i} \tilde{\Theta}_{\rho} $')
!
!>        Field label for gradient of entropy
!!         @f$  \partial_{i} \tilde{S} / dz@f$
      type(field_def), parameter :: grad_filtered_entropy               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_entropy',                   &
     &                math = '$ \partial_{i} \tilde{S} $')
!>        Field label for gradient of perturbation of entropy
!!         @f$  \partial_{i} \tilde{\Theta}_{S} / dz@f$
      type(field_def), parameter :: grad_filtered_pert_entropy          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_pert_entropy',              &
     &                math = '$ \partial_{i} \tilde{\Theta}_{S} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_div_filter_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_filter_field                                            &
     &   =    (field_name .eq. div_filtered_velo%name)                  &
     &   .or. (field_name .eq. div_filtered_magne%name)                 &
     &   .or. (field_name .eq. div_filtered_vector_potential%name)
!
      end function check_div_filter_field
!
! ----------------------------------------------------------------------
!
      logical function check_grad_filter_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_grad_filter_field                                           &
     &   =    (field_name .eq. grad_filtered_temp%name)                 &
     &   .or. (field_name .eq. grad_filtered_pert_temp%name)            &
!
     &   .or. (field_name .eq. grad_filtered_comp%name)                 &
     &   .or. (field_name .eq. grad_filtered_pert_comp%name)            &
!
     &   .or. (field_name .eq. grad_filtered_density%name)              &
     &   .or. (field_name .eq. grad_filtered_pert_density%name)         &
!
     &   .or. (field_name .eq. grad_filtered_entropy%name)              &
     &   .or. (field_name .eq. grad_filtered_pert_entropy%name)
!
      end function check_grad_filter_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_div_filtered_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(div_filtered_velo, array_c2i)
      call set_field_label_to_ctl(div_filtered_magne, array_c2i)
      call set_field_label_to_ctl(div_filtered_vector_potential,        &
     &                            array_c2i)
!
      end subroutine set_div_filtered_field_names
!
! ----------------------------------------------------------------------
!
      subroutine set_grad_filtered_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(grad_filtered_temp,      array_c2i)
      call set_field_label_to_ctl(grad_filtered_pert_temp, array_c2i)
      call set_field_label_to_ctl(grad_filtered_comp,      array_c2i)
      call set_field_label_to_ctl(grad_filtered_pert_comp, array_c2i)
      call set_field_label_to_ctl(grad_filtered_density,   array_c2i)
      call set_field_label_to_ctl(grad_filtered_pert_density,           &
     &                            array_c2i)
      call set_field_label_to_ctl(grad_filtered_entropy, array_c2i)
      call set_field_label_to_ctl(grad_filtered_pert_entropy,           &
     &                            array_c2i)
!
      end subroutine set_grad_filtered_field_names
!
! ----------------------------------------------------------------------
!
      end module m_grad_filter_field_labels
