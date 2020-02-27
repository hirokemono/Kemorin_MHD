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
!!      subroutine set_wide_fil_vector_addresses                        &
!!     &         (i_phys, field_name, wide_filter_fld, flag)
!!      subroutine set_wide_fil_scaler_addresses                        &
!!     &         (i_phys, field_name, wide_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: wide_filter_fld
!!      subroutine set_wide_fil_grad_addresses                          &
!!     &         (i_phys, field_name, wide_filter_grad, flag)
!!        type(gradient_field_address), intent(inout) :: wide_filter_grad
!!
!!      integer(kind = kint) function num_wide_filter_fields()
!!      subroutine set_wide_filter_field_labels(n_comps, names, maths)
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
!!    wide_filter_grad_temp        [wide_filter_grad%i_grad_t]
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
      use t_base_field_labels
      use t_grad_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_w_filter = 15
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
!
      subroutine set_wide_fil_vector_addresses                          &
     &         (i_phys, field_name, wide_filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: wide_filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_wide_filter_vector(field_name)
      if(flag) then
        if (field_name .eq. wide_filter_velocity%name) then
          wide_filter_fld%i_velo =     i_phys
        else if(field_name .eq. wide_filter_vorticity%name) then
          wide_filter_fld%i_vort =    i_phys
!
        else if(field_name .eq. wide_filter_magne%name) then
          wide_filter_fld%i_magne =   i_phys
        else if(field_name .eq. wide_filter_vector_potential%name)    &
     &   then
          wide_filter_fld%i_vecp =    i_phys
        else if(field_name .eq. wide_filter_current%name) then
          wide_filter_fld%i_current = i_phys
        end if
      end if
!
      end subroutine set_wide_fil_vector_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_fil_scaler_addresses                          &
     &         (i_phys, field_name, wide_filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: wide_filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_wide_filter_scalar(field_name)
      if(flag) then
        if      (field_name .eq. wide_filter_temp%name) then
          wide_filter_fld%i_temp =           i_phys
        else if (field_name .eq. wide_filter_pert_temp%name) then
          wide_filter_fld%i_per_temp =       i_phys
!
        else if (field_name .eq. wide_filter_composition%name) then
          wide_filter_fld%i_light =          i_phys
        else if (field_name .eq. wide_filter_pert_comp%name) then
          wide_filter_fld%i_per_light =      i_phys
!
        else if (field_name .eq. wide_filter_density%name) then
          wide_filter_fld%i_density =        i_phys
        else if (field_name .eq. wide_filter_pert_density%name) then
          wide_filter_fld%i_per_density =    i_phys
!
        else if (field_name .eq. wide_filter_entropy%name) then
          wide_filter_fld%i_entropy =        i_phys
        else if (field_name .eq. wide_filter_pert_entropy%name) then
          wide_filter_fld%i_per_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_wide_fil_scaler_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_fil_grad_addresses                            &
     &         (i_phys, field_name, wide_filter_grad, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(gradient_field_address), intent(inout) :: wide_filter_grad
      logical, intent(inout) :: flag
!
!
      flag = check_wide_filter_grad(field_name)
      if(flag) then
        if(field_name .eq. wide_filter_grad_temp%name) then
          wide_filter_grad%i_grad_t =         i_phys
        else if(field_name .eq. wide_filter_grad_composition%name) then
          wide_filter_grad%i_grad_composit =  i_phys
        end if
      end if
!
      end subroutine set_wide_fil_grad_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_wide_filter_fields()
      num_wide_filter_fields = nfld_w_filter
      return
      end function num_wide_filter_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_filter_field_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(nfld_w_filter)
      character(len = kchara), intent(inout) :: names(nfld_w_filter)
      character(len = kchara), intent(inout) :: maths(nfld_w_filter)
!
!
      call set_field_labels(wide_filter_velocity,                       &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(wide_filter_vorticity,                      &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(wide_filter_magne,                          &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(wide_filter_vector_potential,               &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(wide_filter_current,                        &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(wide_filter_temp,                           &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(wide_filter_pert_temp,                      &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(wide_filter_composition,                    &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(wide_filter_pert_comp,                      &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(wide_filter_density,                        &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(wide_filter_pert_density,                   &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(wide_filter_entropy,                        &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(wide_filter_pert_entropy,                   &
     &    n_comps(13), names(13), maths(13))
!
      call set_field_labels(wide_filter_grad_temp,                      &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(wide_filter_grad_composition,               &
     &    n_comps(15), names(15), maths(15))
!
      end subroutine set_wide_filter_field_labels
!
! ----------------------------------------------------------------------
!
      end module m_wide_filter_field_labels
