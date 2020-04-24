!>@file   t_SGS_model_addresses.f90
!!        module t_SGS_model_addresses
!!
!! @author H. Matsui
!! @date  Programmed in Apr. 2020
!!
!!
!> @brief Structure of field addresses for SGS model
!!       These integer points adresses of fields.
!!
!
      module t_SGS_model_addresses
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
      use t_energy_flux_labels
      use t_grad_field_labels
      use t_diff_vector_labels
!
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
!
      implicit  none
!
!>       Structure of start addresses for SGS model
      type SGS_model_addresses
!>        Structure of wide filtered field
!        type(base_field_address) :: wide_filter_fld
!>        Structure of gradient of wide filtered field
!        type(gradient_field_address) :: wide_filter_grad
!
!>        Structure of double filtered field
!        type(base_field_address) :: dbl_filter_fld
!>        Structure of gradient of double filtered field
!        type(gradient_field_address) :: dbl_filter_grad
!
!>        Structure of forces by filtered field
!        type(base_force_address) :: force_by_filter
!>        Structure of rotation of forces by filtered field
!        type(base_force_address) :: rot_frc_by_filter
!>        Structure of divergence of forces by filtered field
!        type(base_force_address) :: div_frc_by_filter
!
!>        Structure of energy fluxes by filtered field
!        type(energy_flux_address) :: eflux_by_filter
!
!>        Structure of gradient of filtered fields
!        type(gradient_field_address) :: grad_fil_fld
!>        Structure of difference of filtered vector
!        type(diff_vector_address) :: diff_fil_vect
!
!>        Structure of SGS terms
!        type(SGS_term_address) :: SGS_term
!>        Structure of divergence of SGS terms
!        type(SGS_term_address) :: div_SGS
!>        Structure of rotation of SGS terms
        type(SGS_term_address) :: rot_SGS
!>        Structure of SGS energy fluxes
!        type(SGS_ene_flux_address) :: SGS_ene_flux
!>        Structure of model coefficients
!        type(SGS_term_address) :: Csim
!>        Structure of work area for dynamic model
!        type(dynamic_SGS_work_address) :: SGS_wk
!
!>        Structure of forces with SGS terms
        type(SGS_term_address) :: frc_w_SGS
!>        Structure of true SGS terms
        type(SGS_term_address) :: true_SGS
!>        Structure of divergence of true SGS terms
        type(SGS_term_address) :: true_div_SGS
!>        Structure of true SGS energy fluxes
        type(SGS_ene_flux_address) :: true_SGS_eflux
!
!>        Structure of wide filtered SGS terms
        type(SGS_term_address) :: wide_SGS
!>        Structure of double filtered SGS terms
        type(SGS_term_address) :: dble_SGS
      end type SGS_model_addresses
!
      end module t_SGS_model_addresses
