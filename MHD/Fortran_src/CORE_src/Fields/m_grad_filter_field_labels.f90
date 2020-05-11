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
!!      subroutine set_grad_filter_field_addresses                      &
!!     &         (i_phys, field_name, grad_fil_fld, flag)
!!        type(gradient_field_address), intent(inout) :: grad_fil_fld
!!
!!      integer(kind = kint) function num_div_filter_fields()
!!      integer(kind = kint) function num_grad_filter_fields()
!!      subroutine set_div_filter_field_labels(n_comps, names, maths)
!!      subroutine set_grad_filter_field_labels(n_comps, names, maths)
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
      use t_grad_field_labels
!
      implicit none
!
      integer(kind = kint), parameter, private :: ndiv_fil_vect = 3
      integer(kind = kint), parameter, private :: ngrad_fil_scl = 8
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
!
      subroutine set_grad_filter_field_addresses                        &
     &         (i_phys, field_name, grad_fil_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(gradient_field_address), intent(inout) :: grad_fil_fld
      logical, intent(inout) :: flag
!
!
      flag = check_div_filter_field(field_name)                         &
     &      .or. check_grad_filter_field(field_name)
      if(flag) then
        if     (field_name .eq. div_filtered_velo%name) then
          grad_fil_fld%i_div_v =    i_phys
        else if(field_name .eq. div_filtered_magne%name) then
          grad_fil_fld%i_div_b =    i_phys
        else if(field_name .eq. div_filtered_vector_potential%name)     &
     &   then
          grad_fil_fld%i_div_a =    i_phys
!
        else if(field_name .eq. grad_filtered_temp%name) then
          grad_fil_fld%i_grad_temp =  i_phys
        else if(field_name .eq. grad_filtered_pert_temp%name) then
          grad_fil_fld%i_grad_per_t = i_phys
!
        else if(field_name .eq. grad_filtered_comp%name) then
          grad_fil_fld%i_grad_composit = i_phys
        else if (field_name .eq. grad_filtered_pert_comp%name) then
          grad_fil_fld%i_grad_per_c =    i_phys
!
        else if (field_name .eq. grad_filtered_density%name) then
          grad_fil_fld%i_grad_density =     i_phys
        else if (field_name .eq. grad_filtered_pert_density%name) then
          grad_fil_fld%i_grad_per_density = i_phys
!
        else if (field_name .eq. grad_filtered_entropy%name) then
          grad_fil_fld%i_grad_entropy =     i_phys
        else if (field_name .eq. grad_filtered_pert_entropy%name) then
          grad_fil_fld%i_grad_per_entropy = i_phys
        end if
      end if
!
      end subroutine set_grad_filter_field_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_div_filter_fields()
      num_div_filter_fields = ndiv_fil_vect
      return
      end function num_div_filter_fields
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_grad_filter_fields()
      num_grad_filter_fields = ngrad_fil_scl
      return
      end function num_grad_filter_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_div_filter_field_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ndiv_fil_vect)
      character(len = kchara), intent(inout) :: names(ndiv_fil_vect)
      character(len = kchara), intent(inout) :: maths(ndiv_fil_vect)
!
!
      call set_field_labels(div_filtered_velo,                          &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(div_filtered_magne,                         &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(div_filtered_vector_potential,              &
     &    n_comps( 3), names( 3), maths( 3))
!
      end subroutine set_div_filter_field_labels
!
! ----------------------------------------------------------------------
!
      subroutine set_grad_filter_field_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ngrad_fil_scl)
      character(len = kchara), intent(inout) :: names(ngrad_fil_scl)
      character(len = kchara), intent(inout) :: maths(ngrad_fil_scl)
!
!
      call set_field_labels(grad_filtered_temp,                         &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(grad_filtered_pert_temp,                    &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(grad_filtered_comp,                         &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(grad_filtered_pert_comp,                    &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(grad_filtered_density,                      &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(grad_filtered_pert_density,                 &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(grad_filtered_entropy,                      &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(grad_filtered_pert_entropy,                 &
     &    n_comps( 8), names( 8), maths( 8))
!
      end subroutine set_grad_filter_field_labels
!
! ----------------------------------------------------------------------
!
      end module m_grad_filter_field_labels
