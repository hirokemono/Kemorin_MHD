!>@file   set_SGS_MHD_field_address.f90
!!        module set_SGS_MHD_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!!
!> @brief Set start address for field data
!!
!!@verbatim
!!      logical function check_SGS_vector_fields(phys_name_ctl)
!!      logical function check_SGS_scalar_fields(phys_name_ctl)
!!      logical function check_SGS_sym_tensor_fields(phys_name_ctl)
!!      logical function check_SGS_asym_tensor_fields(phys_name_ctl)
!!        character(len = kchara), intent(in) :: phys_name_ctl
!!
!!      subroutine set_SGS_MHD_field_addresses                          &
!!     &          (i_fld, field_name, iphys, iphys_LES, flag)
!!      integer(kind = kint), intent(in) :: num_field
!!      integer(kind = kint), intent(in) :: istack_component(0:num_field)
!!      character(len = kchara), intent(in) :: field_name(num_field)
!!      type(phys_address), intent(inout) :: iphys
!!      type(SGS_model_addresses), intent(inout) :: iphys_LES
!!@endverbatim
!!
!!@n @param num_field                 number of field
!!@n @param num_component(num_field)  number of components of field
!!@n @param field_name(num_field)     list of field names
!!@n @param iphys                     structure of field addresses
!
      module set_SGS_MHD_field_address
!
      use m_precision
!
      use t_phys_address
      use t_SPH_SGS_structure
      use t_phys_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      logical function check_SGS_vector_fields(phys_name_ctl)
!
      use t_base_field_labels
      use t_diffusion_term_labels
!
      use t_SGS_term_labels
!
      use m_force_w_SGS_labels
      use m_diff_SGS_term_labels
      use m_true_SGS_term_labels
      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_diff_filter_vect_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
      use m_grad_filter_field_labels
      use m_dble_filter_field_labels
      use m_wide_filter_field_labels
      use m_wide_SGS_term_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical :: flag
!
      flag =  check_SGS_vector_terms(phys_name_ctl)                     &
     &   .or. check_div_SGS_flux_tensor(phys_name_ctl)                  &
     &   .or. check_rot_SGS_terms(phys_name_ctl)                        &
     &   .or. check_force_w_SGS(phys_name_ctl)                          &
     &   .or. check_true_SGS_vector_terms(phys_name_ctl)                &
     &   .or. check_true_div_SGS_flux_tensor(phys_name_ctl)             &
     &   .or. check_filtered_force(phys_name_ctl)                       &
     &   .or. check_rot_fil_force(phys_name_ctl)                        &
     &   .or. check_wide_filter_vector(phys_name_ctl)                   &
     &   .or. check_wide_filter_grad(phys_name_ctl)                     &
     &   .or. check_double_filter_grad(phys_name_ctl)                   &
     &   .or. check_double_filter_vector(phys_name_ctl)                 &
     &   .or. check_grad_filter_field(phys_name_ctl)                    &
     &   .or. check_diff_filter_vectors(phys_name_ctl)                  &
     &   .or. check_wide_SGS_vector_terms(phys_name_ctl)                &
     &   .or. check_double_SGS_vector_terms(phys_name_ctl)
!
      check_SGS_vector_fields = flag
!
      end function check_SGS_vector_fields
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function check_SGS_scalar_fields(phys_name_ctl)
!
      use t_base_field_labels
      use t_diffusion_term_labels
!
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
!
      use m_diff_SGS_term_labels
      use m_true_SGS_term_labels
      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_filtered_ene_flux_labels
      use m_div_filtered_force_labels
      use m_grad_filter_field_labels
      use m_wide_filter_field_labels
      use m_dble_filter_field_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical :: flag
!
      flag =  check_div_SGS_flux_vector(phys_name_ctl)                  &
     &   .or. check_SGS_ene_fluxes(phys_name_ctl)                       &
     &   .or. check_SGS_moedel_coefs(phys_name_ctl)                     &
     &   .or. check_true_div_SGS_flux_vector(phys_name_ctl)             &
     &   .or. check_true_SGS_ene_fluxes(phys_name_ctl)                  &
     &   .or. check_filtered_scalar_flux(phys_name_ctl)                 &
     &   .or. check_div_fil_force(phys_name_ctl)                        &
     &   .or. check_filter_enegy_fluxes(phys_name_ctl)                  &
     &   .or. check_wide_filter_scalar(phys_name_ctl)                   &
     &   .or. check_double_filter_scalar(phys_name_ctl)                 &
     &   .or. check_div_filter_field(phys_name_ctl)                     &
     &   .or. check_commute_SGS_work(phys_name_ctl)                     &
     &   .or. check_div_fil_scl_flux(phys_name_ctl)
!
      check_SGS_scalar_fields = flag
!
      end function check_SGS_scalar_fields
!
! -----------------------------------------------------------------------
!
      logical function check_SGS_sym_tensor_fields(phys_name_ctl)
!
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
!
      use m_force_w_SGS_labels
      use m_filtered_force_labels
      use m_div_filtered_force_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical :: flag
!
      flag =  check_SGS_tensor_terms(phys_name_ctl)                     &
     &   .or. check_flux_tensor_w_SGS(phys_name_ctl)                    &
     &   .or. check_filtered_flux_tensor(phys_name_ctl)                 &
     &   .or. check_div_fil_flux_t(phys_name_ctl)                       &
     &   .or. check_dynamic_SGS_work(phys_name_ctl)
!
      check_SGS_sym_tensor_fields = flag
!
      end function check_SGS_sym_tensor_fields
!
! -----------------------------------------------------------------------
!
      logical function check_SGS_asym_tensor_fields(phys_name_ctl)
!
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
!
      use m_force_w_SGS_labels
!
      character(len = kchara), intent(in) :: phys_name_ctl
      logical :: flag
!
      flag =  check_SGS_induction_tensor(phys_name_ctl)                 &
     &   .or. check_induction_tensor_w_SGS(phys_name_ctl)
!
      check_SGS_asym_tensor_fields = flag
!
      end function check_SGS_asym_tensor_fields
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_SGS_MHD_field_addresses                            &
     &          (i_fld, field_name, iphys, iphys_LES, flag)
!
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
      use t_SGS_model_addresses
!
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
      type(SGS_model_addresses), intent(inout) :: iphys_LES
      logical, intent(inout) :: flag
!
!
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
     &   (i_fld, field_name, iphys_LES%wide_SGS, flag)
      if(flag) return
      call set_double_SGS_term_addresses                                &
     &   (i_fld, field_name, iphys_LES%dble_SGS, flag)
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
     &   (i_fld, field_name, iphys_LES%true_SGS_eflux, flag)
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
      end module set_SGS_MHD_field_address
