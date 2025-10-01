!>@file   m_wide_filter_field_labels.f90
!!        module m_wide_filter_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      logical function check_wide_filter_vector(field_name)
!!      logical function check_wide_filter_scalar(field_name)
!!      logical function check_wide_filter_grad(field_name)
!!
!!      subroutine set_wide_filter_field_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  Wide fitered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      Field label  [Address]
!!
!!   wide_filter_velocity          [wide_filter_fld%i_velo]
!!   wide_filter_vorticity         [wide_filter_fld%i_vort]
!!
!!   wide_filter_magne             [wide_filter_fld%i_magne]
!!   wide_filter_vector_potential  [wide_filter_fld%i_vecp]
!!   wide_filter_current           [wide_filter_fld%i_current]
!!
!!   wide_filter_temp              [wide_filter_fld%i_temp]
!!   wide_filter_composition       [wide_filter_fld%i_light]
!!   wide_filter_density           [wide_filter_fld%i_density]
!!   wide_filter_entropy           [wide_filter_fld%i_entropy]
!!
!!   wide_filter_pert_temp         [wide_filter_fld%i_per_temp]
!!   wide_filter_pert_comp         [wide_filter_fld%i_per_light]
!!   wide_filter_pert_density      [wide_filter_fld%i_per_density]
!!   wide_filter_pert_entropy      [wide_filter_fld%i_per_entropy]
!!
!!    wide_filter_grad_temp        [wide_filter_grad%i_grad_temp]
!!    wide_filter_grad_composition [wide_filter_grad%i_grad_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_wide_filter_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!>        Field label for filtered velocity by wide filtering
!!         @f$ \overline{u}_{i} @f$
      type(field_def), parameter :: wide_filter_velocity                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_filter_velocity',                    &
     &                math = '$ \overline{u}_{i} $')
!>        Field label for filtered vorticity by wide filtering
!!         @f$ \overline{\omega}_{i} @f$
      type(field_def), parameter :: wide_filter_vorticity               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_filter_vorticity',                   &
     &                math = '$ \overline{\omega}_{i} $')
!>        Field label for filtered magnetic field by wide filtering
!!         @f$ \overline{B}_{i} @f$
      type(field_def), parameter :: wide_filter_magne                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_filter_magne',                       &
     &                math = '$ \overline{B}_{i} $')
!>        Field label for filtered magnetic vector potential
!!        by wide filtering
!!         @f$ \overline{A}_{i} @f$
      type(field_def), parameter :: wide_filter_vector_potential        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_filter_vector_potential',            &
     &                math = '$ \overline{A}_{i} $')
!>        Field label for filtered current density by wide filtering
!!         @f$ \overline{J}_{i} @f$
      type(field_def), parameter :: wide_filter_current                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_filter_current',                     &
     &                math = '$ \overline{J}_{i} $')
!
!>        Field label for filtered temperature by wide filtering
!!         @f$ \overline{T} @f$
      type(field_def), parameter :: wide_filter_temp                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_temp',                        &
     &                math = '$ \overline{T} $')
!>        Field label for filtered perturbation of temperature
!!        by wide filtering
!!         @f$ \overline{\Theta} @f$
      type(field_def), parameter :: wide_filter_pert_temp               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_pert_temp',                   &
     &                math = '$ \overline{\Theta} $')
!
!>        Field label for filtered compostiion by wide filtering
!!         @f$ \overline{C} @f$
      type(field_def), parameter :: wide_filter_composition             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_composition',                 &
     &                math = '$ \overline{C} $')
!>        Field label for filtered perturbation of composition
!>        by wide filtering
!!         @f$ \overline{\Theta}_{C} @f$
      type(field_def), parameter :: wide_filter_pert_comp               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_pert_comp',                   &
     &                math = '$ \overline{\Theta}_{C} $')
!
!>        Field label for filtered density by wide filtering
!!         @f$ \overline{\rho} @f$
      type(field_def), parameter :: wide_filter_density                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_density',                     &
     &                math = '$ \overline{\rho} $')
!>        Field label for filtered perturbation of density
!>        by wide filtering
!!         @f$ \overline{\Theta}_{\rho} @f$
      type(field_def), parameter :: wide_filter_pert_density            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_pert_density',                &
     &                math = '$ \overline{\Theta}_{\rho} $')
!
!>        Field label for filtered entropy by wide filtering
!!         @f$ \overline{S} @f$
      type(field_def), parameter :: wide_filter_entropy                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_entropy',                     &
     &                math = '$ \overline{S} $')
!>        Field label for filtered perturbation of entropy
!!        by wide filtering
!!         @f$ \overline{\Theta}_{S} @f$
      type(field_def), parameter :: wide_filter_pert_entropy            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wide_filter_pert_entropy',                &
     &                math = '$ \overline{\Theta}_{S} $')
!
!>        Field label for filtered grad. of temperature
!!        by wide filtering
!!         @f$ \partial_{i} \overline{T} @f$
      type(field_def), parameter :: wide_filter_grad_temp               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_filter_grad_temp',                   &
     &                math = '$ \partial_{i} \overline{T} $')
!>        Field label for filtered grad. of composition
!!        by wide filtering
!!         @f$ \partial_{i} \overline{C} @f$
      type(field_def), parameter :: wide_filter_grad_composition        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_filter_grad_composition',            &
     &                math = '$ \partial_{i} \overline{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_wide_filter_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_wide_filter_vector                                          &
     &   =    (field_name .eq. wide_filter_velocity%name)               &
     &   .or. (field_name .eq. wide_filter_vorticity%name)              &
     &   .or. (field_name .eq. wide_filter_magne%name)                  &
     &   .or. (field_name .eq. wide_filter_vector_potential%name)       &
     &   .or. (field_name .eq. wide_filter_current%name)
!
      end function check_wide_filter_vector
!
! ----------------------------------------------------------------------
!
      logical function check_wide_filter_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_wide_filter_scalar                                          &
     &   =    (field_name .eq. wide_filter_temp%name)                   &
     &   .or. (field_name .eq. wide_filter_composition%name)            &
     &   .or. (field_name .eq. wide_filter_density%name)                &
     &   .or. (field_name .eq. wide_filter_entropy%name)                &
!
     &   .or. (field_name .eq. wide_filter_pert_temp%name)              &
     &   .or. (field_name .eq. wide_filter_pert_comp%name)              &
     &   .or. (field_name .eq. wide_filter_pert_density%name)           &
     &   .or. (field_name .eq. wide_filter_pert_entropy%name)
!
      end function check_wide_filter_scalar
!
! ----------------------------------------------------------------------
!
      logical function check_wide_filter_grad(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_wide_filter_grad                                            &
     &   =    (field_name .eq. wide_filter_grad_temp%name)              &
     &   .or. (field_name .eq. wide_filter_grad_composition%name)
!
      end function check_wide_filter_grad
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_wide_filter_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(wide_filter_velocity,     array_c2i)
      call set_field_label_to_ctl(wide_filter_vorticity,    array_c2i)
      call set_field_label_to_ctl(wide_filter_magne,        array_c2i)
      call set_field_label_to_ctl(wide_filter_vector_potential,         &
     &                            array_c2i)
      call set_field_label_to_ctl(wide_filter_current,      array_c2i)
      call set_field_label_to_ctl(wide_filter_temp,         array_c2i)
      call set_field_label_to_ctl(wide_filter_pert_temp,    array_c2i)
      call set_field_label_to_ctl(wide_filter_composition,  array_c2i)
      call set_field_label_to_ctl(wide_filter_pert_comp,    array_c2i)
      call set_field_label_to_ctl(wide_filter_density,      array_c2i)
      call set_field_label_to_ctl(wide_filter_pert_density, array_c2i)
      call set_field_label_to_ctl(wide_filter_entropy,      array_c2i)
      call set_field_label_to_ctl(wide_filter_pert_entropy, array_c2i)
      call set_field_label_to_ctl(wide_filter_grad_temp, array_c2i)
      call set_field_label_to_ctl(wide_filter_grad_composition,         &
     &                            array_c2i)
!
      end subroutine set_wide_filter_field_names
!
! ----------------------------------------------------------------------
!
      end module m_wide_filter_field_labels
