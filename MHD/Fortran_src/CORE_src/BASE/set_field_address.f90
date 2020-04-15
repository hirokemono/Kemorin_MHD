!>@file   set_field_address.f90
!!        module set_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!!
!> @brief Set start address for field data
!!
!!@verbatim
!!      subroutine set_field_addresses                                  &
!!     &         (num_field, field_name, istack_component, iphys)
!!      integer(kind = kint), intent(in) :: num_field
!!      integer(kind = kint), intent(in) :: istack_component(0:num_field)
!!      character(len = kchara), intent(in) :: field_name(num_field)
!!      type(phys_address), intent(inout) :: iphys
!!
!!@endverbatim
!!
!!@n @param num_field                 number of field
!!@n @param num_component(num_field)  number of components of field
!!@n @param field_name(num_field)     list of field names
!!@n @param iphys                     structure of field addresses
!
!
      module set_field_address
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_field_addresses                                    &
     &         (num_field, field_name, istack_component, iphys)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: istack_component(0:num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
!
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i, i_fld
      logical :: flag
!
!
      do i = 1, num_field
        i_fld = istack_component(i-1) + 1
!
        call set_MHD_field_addresses(i_fld, field_name(i), iphys, flag)
        if(flag) cycle
        call set_SGS_MHD_field_addresses                                &
     &     (i_fld, field_name(i), iphys, flag)
        if(flag) cycle
!
!   Old field label... Should be deleted later!!
        if (field_name(i) .eq. buoyancy_work%name) then
          iphys%ene_flux%i_buo_gen = i_fld
          cycle
        end if
        if ( field_name(i) .eq. geostrophic_balance%name ) then
          iphys%prod_fld%i_geostrophic =  i_fld
          cycle
        end if
      end do
!
      end subroutine set_field_addresses
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_MHD_field_addresses                                &
     &         (i_fld, field_name, iphys, flag)
!
      use t_base_field_labels
      use t_base_force_labels
      use t_diff_vector_labels
      use t_field_product_labels
      use t_diffusion_term_labels
      use t_explicit_term_labels

      use m_rot_force_labels
      use m_div_force_labels
!
      integer(kind = kint), intent(in) :: i_fld
      character(len = kchara), intent(in) :: field_name
!
      type(phys_address), intent(inout) :: iphys
      logical, intent(inout) :: flag
!
!
      call set_base_vector_addresses                                    &
     &   (i_fld, field_name, iphys%base, flag)
      if(flag) return
      call set_base_scalar_addresses                                    &
     &   (i_fld, field_name, iphys%base, flag)
      if(flag) return
!
      call set_base_force_addresses                                     &
     &   (i_fld, field_name, iphys%forces, flag)
      if(flag) return
      call set_enegy_fluxes_addresses                                   &
     &   (i_fld, field_name, iphys%ene_flux, flag)
      if(flag) return
      call set_div_force_addresses                                      &
     &   (i_fld, field_name, iphys%div_forces, flag)
      if(flag) return
      call set_base_diffusion_addresses                                 &
     &   (i_fld, field_name, iphys%diffusion, flag)
      if(flag) return
      call set_field_product_addresses                                  &
     &   (i_fld, field_name, iphys%prod_fld, flag)
      if(flag) return
!
      call set_gradient_field_addresses                                 &
     &   (i_fld, field_name, iphys%grad_fld, flag)
      if(flag) return
      call set_diff_vector_addresses                                    &
     &   (i_fld, field_name, iphys%diff_vector, flag)
      if(flag) return
!
      call set_rot_force_addresses                                      &
     &   (i_fld, field_name, iphys%rot_forces, flag)
      if(flag) return
!
      call set_work_field_addresses                                     &
     &   (i_fld, field_name, iphys%exp_work, flag)
      if(flag) return
      call set_check_field_addresses                                    &
     &   (i_fld, field_name, iphys%check_fld1, iphys%check_fld2, flag)
      if(flag) return
!
      end subroutine set_MHD_field_addresses
!
!  --------------------------------------------------------------------
!
      subroutine set_SGS_MHD_field_addresses                            &
     &          (i_fld, field_name, iphys, flag)
!
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels

      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_filtered_ene_flux_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
      use m_diff_filter_vect_labels
      use m_grad_filter_field_labels
      use m_diff_SGS_term_labels
      use m_wide_SGS_term_labels
      use m_true_SGS_term_labels
      use m_force_w_SGS_labels
      use m_wide_filter_field_labels
      use m_dble_filter_field_labels
!
      integer(kind = kint), intent(in) :: i_fld
      character(len = kchara), intent(in) :: field_name
!
      type(phys_address), intent(inout) :: iphys
      logical, intent(inout) :: flag
!
!
      call set_filter_field_addresses                                   &
     &   (i_fld, field_name, iphys%filter_fld, flag)
      if(flag) return
      call set_SGS_term_addresses                                       &
     &   (i_fld, field_name, iphys%SGS_term, flag)
      if(flag) return
      call set_div_SGS_term_addresses                                   &
     &   (i_fld, field_name, iphys%div_SGS, flag)
      if(flag) return
      call set_rot_SGS_term_addresses                                   &
     &   (i_fld, field_name, iphys%rot_SGS, flag)
      if(flag) return
!
      call set_wide_SGS_term_addresses                                  &
     &   (i_fld, field_name, iphys%wide_SGS, flag)
      if(flag) return
      call set_double_SGS_term_addresses                                &
     &   (i_fld, field_name, iphys%dble_SGS, flag)
      if(flag) return
!
      call set_SGS_model_coef_addresses                                 &
     &   (i_fld, field_name, iphys%Csim, flag)
      if(flag) return
!
      call set_SGS_ene_flux_addresses                                   &
     &   (i_fld, field_name, iphys%SGS_ene_flux, flag)
      if(flag) return
!
      call set_force_w_SGS_addresses                                    &
     &   (i_fld, field_name, iphys%frc_w_SGS, flag)
      if(flag) return
!
      call set_true_SGS_term_addresses                                  &
     &   (i_fld, field_name, iphys%true_SGS, flag)
      if(flag) return
      call set_true_div_SGS_term_addresses                              &
     &   (i_fld, field_name, iphys%true_div_SGS, flag)
      if(flag) return
      call set_true_SGS_ene_flux_addresses                              &
     &   (i_fld, field_name, iphys%true_SGS_eflux, flag)
      if(flag) return
!
!
      call set_grad_filter_field_addresses                              &
     &   (i_fld, field_name, iphys%grad_fil_fld, flag)
      if(flag) return
      call set_diff_filter_vect_addresses                               &
     &   (i_fld, field_name, iphys%diff_fil_vect, flag)
      if(flag) return
!
      call set_dynamic_SGS_work_addresses                               &
     &   (i_fld, field_name, iphys%SGS_wk, flag)
      if(flag) return
!
      call set_wide_fil_vector_addresses                                &
     &   (i_fld, field_name, iphys%wide_filter_fld, flag)
      if(flag) return
      call set_wide_fil_scaler_addresses                                &
     &   (i_fld, field_name, iphys%wide_filter_fld, flag)
      if(flag) return
      call set_wide_fil_grad_addresses                                  &
     &   (i_fld, field_name, iphys%wide_filter_grad, flag)
      if(flag) return
!
      call set_dble_fil_vector_addresses                                &
     &   (i_fld, field_name, iphys%dbl_filter_fld, flag)
      if(flag) return
      call set_dble_fil_scaler_addresses                                &
     &   (i_fld, field_name, iphys%dbl_filter_fld, flag)
      if(flag) return
      call set_dble_fil_grad_addresses                                  &
     &   (i_fld, field_name, iphys%dbl_filter_grad, flag)
      if(flag) return
!
      call set_filtered_force_addresses                                 &
     &   (i_fld, field_name, iphys%force_by_filter, flag)
      if(flag) return
      call set_rot_fil_force_addresses                                  &
     &   (i_fld, field_name, iphys%rot_frc_by_filter, flag)
      if(flag) return
      call set_div_fil_force_addresses                                  &
     &   (i_fld, field_name, iphys%div_frc_by_filter, flag)
      if(flag) return
      call set_filter_ene_flux_addresses                                &
     &   (i_fld, field_name, iphys%eflux_by_filter, flag)
      if(flag) return
!
      end subroutine set_SGS_MHD_field_addresses
!
!  --------------------------------------------------------------------
!
      end module set_field_address
