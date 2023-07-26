!>@file   m_dble_filter_field_labels.f90
!!        module m_dble_filter_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      logical function check_double_filter_vector(field_name)
!!      logical function check_double_filter_scalar(field_name)
!!      logical function check_double_filter_grad(field_name)
!!
!!      subroutine set_double_filter_field_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  Double filterd field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      Field label  [Address]
!!
!!   double_filter_velocity          [dbl_filter_fld%i_velo]
!!   double_filter_vorticity         [dbl_filter_fld%i_vort]
!!
!!   double_filter_magne             [dbl_filter_fld%i_magne]
!!   double_filter_vector_potential  [dbl_filter_fld%i_vecp]
!!   double_filter_current           [dbl_filter_fld%i_current]
!!
!!   double_filter_temp              [dbl_filter_fld%i_temp]
!!   double_filter_composition       [dbl_filter_fld%i_light]
!!   double_filter_density           [dbl_filter_fld%i_density]
!!   double_filter_entropy           [dbl_filter_fld%i_entropy]
!!
!!   double_filter_pert_temp         [dbl_filter_fld%i_per_temp]
!!   double_filter_pert_comp         [dbl_filter_fld%i_per_light]
!!   double_filter_pert_density      [dbl_filter_fld%i_per_density]
!!   double_filter_pert_entropy      [dbl_filter_fld%i_per_entropy]
!!
!!   double_filter_grad_temp [dbl_filter_grad%i_grad_temp]
!!   double_filter_grad_comp [dbl_filter_grad%i_grad_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_dble_filter_field_labels
!
      use m_precision
      use m_constants
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!  double filtered field
!
!>        Field label for filtered velocity by double filtering
!!         @f$ \tilde{\tilde{u}}_{i} @f$
      type(field_def), parameter :: double_filter_velocity              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_filter_velocity',                  &
     &                math = '$ \tilde{\tilde{u}}_{i} $')
!>        Field label for filtered vorticity by double filtering
!!         @f$ \tilde{\tilde{\omega}}_{i} @f$
      type(field_def), parameter :: double_filter_vorticity             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_filter_vorticity',                 &
     &                math = '$ \tilde{\tilde{\omega}}_{i} $')
!>        Field label for filtered magnetic field by double filtering
!!         @f$ \tilde{\tilde{B}}_{i} @f$
      type(field_def), parameter :: double_filter_magne                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_filter_magne',                     &
     &                math = '$ \tilde{\tilde{B}}_{i} $')
!>        Field label for filtered magnetic vector potential
!!        by double filtering
!!         @f$ \tilde{\tilde{A}}_{i} @f$
      type(field_def), parameter :: double_filter_vector_potential      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_filter_vector_potential',          &
     &                math = '$ \tilde{\tilde{A}}_{i} $')
!>        Field label for filtered current density by double filtering
!!         @f$ \tilde{\tilde{J}}_{i} @f$
      type(field_def), parameter :: double_filter_current               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_filter_current',                   &
     &                math = '$ \tilde{\tilde{J}}_{i} $')
!
!>        Field label for filtered temperature by double filtering
!!         @f$ \tilde{\tilde{T}} @f$
      type(field_def), parameter :: double_filter_temp                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_temp',                      &
     &                math = '$ \tilde{\tilde{T}} $')
!>        Field label for filtered perturbation of temperature
!!        by double filtering
!!         @f$ \tilde{\tilde{\Theta}} @f$
      type(field_def), parameter :: double_filter_pert_temp             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_pert_temp',                 &
     &                math = '$ \tilde{\tilde{\Theta}} $')
!
!>        Field label for filtered compostiion by double filtering
!!         @f$ \tilde{\tilde{C}} @f$
      type(field_def), parameter :: double_filter_composition           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_composition',               &
     &                math = '$ \tilde{\tilde{C}} $')
!>        Field label for filtered perturbation of composition
!>        by double filtering
!!         @f$ \tilde{\tilde{\Theta}}_{C} @f$
      type(field_def), parameter :: double_filter_pert_comp             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_pert_comp',                 &
     &                math = '$ \tilde{\tilde{\Theta}}_{C} $')
!
!>        Field label for filtered density by double filtering
!!         @f$ \tilde{\tilde{\rho}} @f$
      type(field_def), parameter :: double_filter_density               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_density',                   &
     &                math = '$ \tilde{\tilde{\rho}} $')
!>        Field label for filtered perturbation of density
!>        by double filtering
!!         @f$ \tilde{\tilde{\Theta}}_{\rho} @f$
      type(field_def), parameter :: double_filter_pert_density          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_pert_density',              &
     &                math = '$ \tilde{\tilde{\Theta}}_{\rho} $')
!
!>        Field label for filtered entropy by double filtering
!!         @f$ \tilde{\tilde{S}} @f$
      type(field_def), parameter :: double_filter_entropy               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_entropy',                   &
     &                math = '$ \tilde{\tilde{S}} $')
!>        Field label for filtered perturbation of entropy
!!        by double filtering
!!         @f$ \tilde{\tilde{\Theta}}_{S} @f$
      type(field_def), parameter :: double_filter_pert_entropy          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'double_filter_pert_entropy',              &
     &                math = '$ \tilde{\tilde{\Theta}}_{S} $')
!
!>        Field label for filtered grad. of temperature
!!        by double filtering
!!         @f$ \partial_{i} \tilde{\tilde{T}} @f$
      type(field_def), parameter :: double_filter_grad_temp             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_filter_grad_temp',                 &
     &                math = '$ \partial_{i} \tilde{\tilde{T}} $')
!>        Field label for filtered grad. of composition
!!        by double filtering
!!         @f$ \partial_{i} \tilde{\tilde{C}} @f$
      type(field_def), parameter :: double_filter_grad_comp             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_filter_grad_comp',                 &
     &                math = '$ \partial_{i} \tilde{\tilde{C}} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_double_filter_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_double_filter_vector                                        &
     &   =    (field_name .eq. double_filter_velocity%name)             &
     &   .or. (field_name .eq. double_filter_vorticity%name)            &
     &   .or. (field_name .eq. double_filter_magne%name)                &
     &   .or. (field_name .eq. double_filter_vector_potential%name)     &
     &   .or. (field_name .eq. double_filter_current%name)
!
      end function check_double_filter_vector
!
! ----------------------------------------------------------------------
!
      logical function check_double_filter_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_double_filter_scalar                                        &
     &   =    (field_name .eq. double_filter_temp%name)                 &
     &   .or. (field_name .eq. double_filter_composition%name)          &
     &   .or. (field_name .eq. double_filter_density%name)              &
     &   .or. (field_name .eq. double_filter_entropy%name)              &
!
     &   .or. (field_name .eq. double_filter_pert_temp%name)            &
     &   .or. (field_name .eq. double_filter_pert_comp%name)            &
     &   .or. (field_name .eq. double_filter_pert_density%name)         &
     &   .or. (field_name .eq. double_filter_pert_entropy%name)
!
      end function check_double_filter_scalar
!
! ----------------------------------------------------------------------
!
      logical function check_double_filter_grad(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_double_filter_grad                                          &
     &   =    (field_name .eq. double_filter_grad_temp%name)            &
     &   .or. (field_name .eq. double_filter_grad_comp%name)
!
      end function check_double_filter_grad
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_double_filter_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(double_filter_velocity,  array_c2i)
      call set_field_label_to_ctl(double_filter_vorticity, array_c2i)
      call set_field_label_to_ctl(double_filter_magne,     array_c2i)
      call set_field_label_to_ctl(double_filter_vector_potential,       &
     &                            array_c2i)
      call set_field_label_to_ctl(double_filter_current,   array_c2i)
      call set_field_label_to_ctl(double_filter_temp,      array_c2i)
      call set_field_label_to_ctl(double_filter_pert_temp, array_c2i)
      call set_field_label_to_ctl(double_filter_composition,            &
     &                            array_c2i)
      call set_field_label_to_ctl(double_filter_pert_comp, array_c2i)
      call set_field_label_to_ctl(double_filter_density,   array_c2i)
      call set_field_label_to_ctl(double_filter_pert_density,           &
     &                            array_c2i)
      call set_field_label_to_ctl(double_filter_entropy,   array_c2i)
      call set_field_label_to_ctl(double_filter_pert_entropy,           &
     &                            array_c2i)
      call set_field_label_to_ctl(double_filter_grad_temp, array_c2i)
      call set_field_label_to_ctl(double_filter_grad_comp, array_c2i)
!
      end subroutine set_double_filter_field_names
!
! ----------------------------------------------------------------------
!
      end module m_dble_filter_field_labels
