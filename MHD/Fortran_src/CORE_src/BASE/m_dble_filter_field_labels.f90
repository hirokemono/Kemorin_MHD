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
!!      subroutine set_dble_fil_vector_addresses                        &
!!     &         (i_phys, field_name, dbl_filter_fld, flag)
!!      subroutine set_dble_fil_scaler_addresses                        &
!!     &         (i_phys, field_name, dbl_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: dbl_filter_fld
!!      subroutine set_dble_fil_grad_addresses                          &
!!     &         (i_phys, field_name, dbl_filter_grad, flag)
!!        type(gradient_field_address), intent(inout) :: dbl_filter_grad
!!
!!      integer(kind = kint) function num_double_filter_fields()
!!      subroutine set_double_filter_field_labels(n_comps, names, maths)
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
!!   double_filter_grad_temp [dbl_filter_grad%i_grad_t]
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
      use t_base_field_labels
      use t_grad_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_d_filter = 15
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
!
      subroutine set_dble_fil_vector_addresses                          &
     &         (i_phys, field_name, dbl_filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: dbl_filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_double_filter_vector(field_name)
      if(flag) then
        if (field_name .eq. double_filter_velocity%name) then
          dbl_filter_fld%i_velo =     i_phys
        else if(field_name .eq. double_filter_vorticity%name) then
          dbl_filter_fld%i_vort =    i_phys
!
        else if(field_name .eq. double_filter_magne%name) then
          dbl_filter_fld%i_magne =   i_phys
        else if(field_name .eq. double_filter_vector_potential%name)    &
     &   then
          dbl_filter_fld%i_vecp =    i_phys
        else if(field_name .eq. double_filter_current%name) then
          dbl_filter_fld%i_current = i_phys
        end if
      end if
!
      end subroutine set_dble_fil_vector_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_dble_fil_scaler_addresses                          &
     &         (i_phys, field_name, dbl_filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: dbl_filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_double_filter_scalar(field_name)
      if(flag) then
        if      (field_name .eq. double_filter_temp%name) then
          dbl_filter_fld%i_temp =           i_phys
        else if (field_name .eq. double_filter_pert_temp%name) then
          dbl_filter_fld%i_per_temp =       i_phys
!
        else if (field_name .eq. double_filter_composition%name) then
          dbl_filter_fld%i_light =          i_phys
        else if (field_name .eq. double_filter_pert_comp%name) then
          dbl_filter_fld%i_per_light =      i_phys
!
        else if (field_name .eq. double_filter_density%name) then
          dbl_filter_fld%i_density =        i_phys
        else if (field_name .eq. double_filter_pert_density%name) then
          dbl_filter_fld%i_per_density =    i_phys
!
        else if (field_name .eq. double_filter_entropy%name) then
          dbl_filter_fld%i_entropy =        i_phys
        else if (field_name .eq. double_filter_pert_entropy%name) then
          dbl_filter_fld%i_per_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_dble_fil_scaler_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_dble_fil_grad_addresses                            &
     &         (i_phys, field_name, dbl_filter_grad, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(gradient_field_address), intent(inout) :: dbl_filter_grad
      logical, intent(inout) :: flag
!
!
      flag = check_double_filter_grad(field_name)
      if(flag) then
        if (field_name .eq. double_filter_grad_temp%name) then
          dbl_filter_grad%i_grad_t =         i_phys
        else if (field_name .eq. double_filter_grad_comp%name) then
          dbl_filter_grad%i_grad_composit =  i_phys
        end if
      end if
!
      end subroutine set_dble_fil_grad_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_double_filter_fields()
      num_double_filter_fields = nfld_d_filter
      return
      end function num_double_filter_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_double_filter_field_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(nfld_d_filter)
      character(len = kchara), intent(inout) :: names(nfld_d_filter)
      character(len = kchara), intent(inout) :: maths(nfld_d_filter)
!
!
      call set_field_labels(double_filter_velocity,                     &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(double_filter_vorticity,                    &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(double_filter_magne,                        &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(double_filter_vector_potential,             &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(double_filter_current,                      &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(double_filter_temp,                         &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(double_filter_pert_temp,                    &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(double_filter_composition,                  &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(double_filter_pert_comp,                    &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(double_filter_density,                      &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(double_filter_pert_density,                 &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(double_filter_entropy,                      &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(double_filter_pert_entropy,                 &
     &    n_comps(13), names(13), maths(13))
!
      call set_field_labels(double_filter_grad_temp,                    &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(double_filter_grad_comp,                    &
     &    n_comps(15), names(15), maths(15))
!
      end subroutine set_double_filter_field_labels
!
! ----------------------------------------------------------------------
!
      end module m_dble_filter_field_labels
