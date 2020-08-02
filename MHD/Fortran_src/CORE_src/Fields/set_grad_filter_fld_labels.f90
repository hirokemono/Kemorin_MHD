!>@file   set_grad_filter_fld_labels.f90
!!       module set_grad_filter_fld_labels
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
      module set_grad_filter_fld_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_grad_field_labels
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
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
!
      end module set_grad_filter_fld_labels
