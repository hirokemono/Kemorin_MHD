!>@file   t_phys_address.f90
!!        module t_phys_address
!!
!! @author H. Matsui
!! @date ...
!!
!!
!> @brief Structure of field addresses
!!       These integer points adresses of fields.
!!
!
      module t_phys_address
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
      use t_energy_flux_labels
      use t_grad_field_labels
      use t_diffusion_term_labels
      use t_field_product_labels
      use t_explicit_term_labels
      use t_diff_vector_labels
!
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
!
      implicit  none
! 
!>       Structure for start address for fields
      type phys_address
!>        Structure of base fields
        type(base_field_address) :: base
!>        Structure of forces
        type(base_force_address) :: forces
!>        Structure of forces
        type(base_force_address) :: rot_forces
!>        Structure of forces
        type(base_force_address) :: div_forces
!>        Structure of energy fluxes
        type(energy_flux_address) :: ene_flux
!>        Structure of gradient of fields
        type(gradient_field_address) :: grad_fld
!>        Structure of difference of vector
        type(diff_vector_address) :: diff_vector
!
!>        Structure of diffusion terms
        type(diffusion_address) :: diffusion
!>        Structure of diffusivities
        type(diffusivity_adress) :: diffusivity
!
!>        Structure of products of fields
        type(phys_products_address) :: prod_fld
!
!>        Structure of work area
        type(explicit_term_address) :: exp_work
!>        First check work area
        type(explicit_term_address) :: check_fld1
!>        Second check work area
        type(explicit_term_address) :: check_fld2
!
!>        Structure of filtered fields
        type(base_field_address) :: filter_fld
!
!>        Structure of wide filtered field
        type(base_field_address) :: wide_filter_fld
!>        Structure of gradient of wide filtered field
        type(gradient_field_address) :: wide_filter_grad
!
!>        Structure of double filtered field
        type(base_field_address) :: dbl_filter_fld
!>        Structure of gradient of double filtered field
        type(gradient_field_address) :: dbl_filter_grad
!
!>        Structure of forces by filtered field
        type(base_force_address) :: force_by_filter
!>        Structure of rotation of forces by filtered field
        type(base_force_address) :: rot_frc_by_filter
!>        Structure of divergence of forces by filtered field
        type(base_force_address) :: div_frc_by_filter
!
!>        Structure of energy fluxes by filtered field
        type(energy_flux_address) :: eflux_by_filter
!
!>        Structure of gradient of filtered fields
        type(gradient_field_address) :: grad_fil_fld
!>        Structure of difference of filtered vector
        type(diff_vector_address) :: diff_fil_vect
!
!>        Structure of SGS terms
        type(SGS_term_address) :: SGS_term
!>        Structure of divergence of SGS terms
        type(SGS_term_address) :: div_SGS
!>        Structure of rotation of SGS terms
        type(SGS_term_address) :: rot_SGS
!>        Structure of SGS energy fluxes
        type(SGS_ene_flux_address) :: SGS_ene_flux
!>        Structure of model coefficients
        type(SGS_term_address) :: Csim
!>        Structure of work area for dynamic model
        type(dynamic_SGS_work_address) :: SGS_wk
!
!>        Structure of forces with SGS terms
        type(SGS_term_address) :: frc_w_SGS
!>        Structure of true SGS terms
!        type(SGS_term_address) :: true_SGS
      end type phys_address
!
!
      end module t_phys_address
