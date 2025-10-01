!>@file   m_filtered_field_labels.f90
!!        module m_filtered_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      logical function check_filter_vector(field_name)
!!      logical function check_filter_scalar(field_name)
!!
!!      subroutine set_filter_field_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  Filtered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names (Single filtered, wide filtered, double filtered)
!!
!!   filter_velocity    [filter_fld%i_velo]:     filtered velocity    u
!!   filter_vorticity   [filter_fld%i_vort]: 
!!            filtered vorticity   \omega = \nabra \times v
!!
!!   filter_vector_potential  [filter_fld%i_vecp]: 
!!            filtered vector potential \nabla \times A = B
!!   filter_magne      [filter_fld%i_magne]: filtered magnetic field   B
!!   filter_current    [filter_fld%i_current]: 
!!            filtered current density  J = \nabla \times B
!!
!!   filter_temperature  [filter_fld%i_temp]:  filtered temperature              T
!!   filter_composition  [filter_fld%i_light]:
!!                              filtered Composition anormally   C
!!   filter_density      [filter_fld%i_density]: filtered density  \rho
!!   filter_entropy      [filter_fld%i_entropy]: filtered Entropy  S
!!
!!   filter_pert_temperature   [filter_fld%i_per_temp]: \Theta = T - T_0
!!   filter_pert_composition   [filter_fld%i_per_light]:    C - C_0
!!   filter_pert_density       [filter_fld%i_per_density]: \rho - \rho_0
!!   filter_pert_entropy       [filter_fld%i_per_entropy]:  S - S_0
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!  filtered field
!
!>        Field label for filtered velocity
!!         @f$ \tilde{u}_{i} @f$
      type(field_def), parameter :: filter_velocity                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_velocity',                         &
     &                math = '$ \tilde{u}_{i} $')
!>        Field label for filtered velocity
!!         @f$ \tilde{\omega}_{i} @f$
      type(field_def), parameter :: filter_vorticity                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_vorticity',                        &
     &                math = '$ \tilde{\omega}_{i} $')
!!
!>        Field label for filtered magnetic field
!!         @f$ \tilde{B}_{i} @f$
      type(field_def), parameter :: filter_magne                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_magne',                            &
     &                math = '$ \tilde{B}_{i} $')
!>        Field label for filtered current density
!!         @f$ \tilde{J}_{i} @f$
      type(field_def), parameter :: filter_current                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_current',                          &
     &                math = '$ \tilde{J}_{i} $')
!>        Field label for filtered vetor potential
!!         @f$ \tilde{A}_{i} @f$
      type(field_def), parameter :: filter_vector_potential             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'filter_vector_potential',                 &
     &                math = '$ \tilde{A}_{i} $')
!
!>        Field label for filtered temperature
!!         @f$ \tilde{T} @f$
      type(field_def), parameter :: filter_temperature                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_temperature',                      &
     &                math = '$ \tilde{T} $')
!>        Field label for filtered perturbation of temperature
!!         @f$ \tilde{\Theta} @f$
      type(field_def), parameter :: filter_pert_temperature             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_temperature',                 &
     &                math = '$ \tilde{\Theta} $')
!
!>        Field label for filtered conposition
!!         @f$ \tilde{C} @f$
      type(field_def), parameter :: filter_composition                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_composition',                      &
     &                math = '$ \tilde{C} $')
!>        Field label for filtered conposition
!!         @f$ \tilde{C} - C_{0} @f$
      type(field_def), parameter :: filter_pert_composition             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_composition',                 &
     &                math = '$ \tilde{\Theta}_{C} $')
!
!>        Field label for filtered density
!!         @f$ \tilde{\rho} @f$
      type(field_def), parameter :: filter_density                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_density',                          &
     &                math = '$ \tilde{\rho} $')
!>        Field label for filtered perturbation of density
!!         @f$ \tilde{\rho} - rho_{0} @f$
      type(field_def), parameter :: filter_pert_density                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_density',                     &
     &                math = '$ \tilde{\Theta}_{\rho} $')
!
!>        Field label for filtered entropy
!!         @f$ \tilde{S} @f$
      type(field_def), parameter :: filter_entropy                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_entropy',                          &
     &                math = '$ \tilde{S} $')
!>        Field label for filtered perturbation of entropy
!!         @f$ \tilde{S} - S_{0} @f$
      type(field_def), parameter :: filter_pert_entropy                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filter_pert_entropy',                     &
     &                math = '$ \tilde{\Theta}_{S} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_filter_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_vector = .FALSE.
      if (    (field_name .eq. filter_velocity%name)                    &
     &   .or. (field_name .eq. filter_vorticity%name)                   &
     &   .or. (field_name .eq. filter_magne%name)                       &
     &   .or. (field_name .eq. filter_vector_potential%name)            &
     &   .or. (field_name .eq. filter_current%name)                     &
     &      )   check_filter_vector = .TRUE.
!
      end function check_filter_vector
!
! ----------------------------------------------------------------------
!
      logical function check_filter_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_scalar                                               &
     &   =    (field_name .eq. filter_temperature%name)                 &
     &   .or. (field_name .eq. filter_composition%name)                 &
     &   .or. (field_name .eq. filter_density%name)                     &
     &   .or. (field_name .eq. filter_entropy%name)                     &
!
     &   .or. (field_name .eq. filter_pert_temperature%name)            &
     &   .or. (field_name .eq. filter_pert_composition%name)            &
     &   .or. (field_name .eq. filter_pert_density%name)                &
     &   .or. (field_name .eq. filter_pert_entropy%name)
!
      end function check_filter_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_filter_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(filter_velocity,         array_c2i)
      call set_field_label_to_ctl(filter_vorticity,        array_c2i)
      call set_field_label_to_ctl(filter_magne,            array_c2i)
      call set_field_label_to_ctl(filter_current,          array_c2i)
      call set_field_label_to_ctl(filter_vector_potential, array_c2i)
      call set_field_label_to_ctl(filter_temperature,      array_c2i)
      call set_field_label_to_ctl(filter_pert_temperature, array_c2i)
      call set_field_label_to_ctl(filter_composition,      array_c2i)
      call set_field_label_to_ctl(filter_pert_composition, array_c2i)
      call set_field_label_to_ctl(filter_density,          array_c2i)
      call set_field_label_to_ctl(filter_pert_density,     array_c2i)
      call set_field_label_to_ctl(filter_entropy,          array_c2i)
      call set_field_label_to_ctl(filter_pert_entropy,     array_c2i)
!
      end subroutine set_filter_field_names
!
! ----------------------------------------------------------------------
!
      end module m_filtered_field_labels
