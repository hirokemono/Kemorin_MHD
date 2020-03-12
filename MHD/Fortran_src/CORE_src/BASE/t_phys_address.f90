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
      use t_diff_vector_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
!
      implicit  none
! 
!>       Structure for start address for fields
      type phys_address
!
!>        Start address for velocity
!!         \f$ u_{i} \f$
        integer (kind=kint) :: i_velo  =           izero
!>        Start address for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
        integer (kind=kint) :: i_vort  =           izero
!
!>        start address for magnetic field
!!         @f$ B_{i} @f$
        integer (kind=kint) :: i_magne =           izero
!>        start address for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
        integer (kind=kint) :: i_vecp =            izero
!>        start address for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
        integer (kind=kint) :: i_current =         izero
!
!>        Start address for filtered velocity
!!         @f$ \bar{u}_{i} @f$
        integer (kind=kint) :: i_filter_velo  =    izero
!>        Start address for filtered velocity
!!         @f$ \bar{\omega}_{i} @f$
        integer (kind=kint) :: i_filter_vort  =    izero
!>        Start address for filtered temperature
!!         @f$ \bar{T} @f$
        integer (kind=kint) :: i_filter_temp  =    izero
!>        Start address for filtered magnetic field
!!         @f$ \bar{B}_{i} @f$
        integer (kind=kint) :: i_filter_magne =    izero
!>        Start address for filtered current density
!!         @f$ \bar{B}_{i} @f$
        integer (kind=kint) :: i_filter_current =  izero
!>        Start address for filtered vetor potential
!!         @f$ \bar{A}_{i} @f$
        integer (kind=kint) :: i_filter_vecp =     izero
!>        Start address for filtered perturbation of temperature
!!         @f$ \bar{\Theta} @f$
        integer (kind=kint) :: i_filter_par_t =    izero
!>        Start address for filtered conposition
!!         @f$ \bar{C} @f$
        integer (kind=kint) :: i_filter_comp =     izero
!
!>        start address for filtered velocity
!!         @f$ \partial_{i} \bar{u}_{i} @f$
        integer (kind=kint) :: i_div_filter_v =    izero
!>        start address for filtered magnetic field
!!         @f$ \partial_{i} \bar{B}_{i} @f$
        integer (kind=kint) :: i_div_filter_b =    izero
!>        start address for filtered magnetic vector potential
!!         @f$ \partial_{i} \bar{A}_{i} @f$
        integer (kind=kint) :: i_div_filter_a =    izero
!
!  arrays for current forces
!
!>        start address for total forces
        integer (kind=kint) :: i_forces =       izero
!>        start address for curl of total forces
        integer (kind=kint) :: i_rot_forces =   izero
!>        start address for divergence of total forces
        integer (kind=kint) :: i_div_forces =   izero
!
!  arrays for previous evolution
!
!>        start address for explicit term for momentum at previous step
        integer (kind=kint) :: i_pre_mom =      izero
!>        start address for explicit term for induction at previous step
        integer (kind=kint) :: i_pre_uxb =      izero
!>        start address for explicit term for heat at previous step
        integer (kind=kint) :: i_pre_heat =     izero
!>        start address for explicit term for composition
!!        at previous step
        integer (kind=kint) :: i_pre_composit = izero
!>        start address for explicit term for pressure at previous step
        integer (kind=kint) :: i_pre_press =    izero
!
!  arrays for evolution check
!
!>        start address for explicit term for momentum for check
        integer (kind=kint) :: i_chk_mom =       izero
!>        start address for explicit term for induction for check
        integer (kind=kint) :: i_chk_uxb =       izero
!>        start address for explicit term for heat for check
        integer (kind=kint) :: i_chk_heat =      izero
!>        start address for explicit term for composition
!!        for check
        integer (kind=kint) :: i_chk_composit =  izero
!>        start address for explicit term for pressure for check
        integer (kind=kint) :: i_chk_press =     izero
!>        start address for explicit term for electric potential
!!        for check
        integer (kind=kint) :: i_chk_potential = izero
!
!>        start address for explicit term for momentum for 2nd check
        integer (kind=kint) :: i_chk_mom_2 =       izero
!>        start address for explicit term for induction for 2nd check
        integer (kind=kint) :: i_chk_uxb_2 =       izero
!>        start address for explicit term for heat for 2nd check
        integer (kind=kint) :: i_chk_heat_2 =      izero
!>        start address for explicit term for composition
!!        for 2nd check
        integer (kind=kint) :: i_chk_composit_2 =  izero
!>        start address for explicit term for pressure for 2nd check
        integer (kind=kint) :: i_chk_press_2 =     izero
!>        start address for explicit term for electric potential
!!        for 2nd check
        integer (kind=kint) :: i_chk_potential_2 = izero
!
!>        start address for rotation of ststem @f$ Omega @f$
!!          i_omega:   poloidal component
!!          i_omega+1: radial derivative of poloidal component
!!          i_omega+2: 2nd radial derivative of poloidal component
        integer (kind=kint) :: i_omega = izero
!
!>        start address for background magnetic field @f$ B_{0} @f$
!!          i_back_B:   poloidal component
!!          i_back_B+1: radial derivative of poloidal component
!!          i_back_B+2: 2nd radial derivative of poloidal component
        integer (kind=kint) :: i_back_B = izero
!
!
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
!>        Structure of diffusion terms
        type(diffusion_address) :: diffusion
!>        Structure of diffusivities
        type(diffusivity_adress) :: diffusivity
!
!>        Structure of energy fluxes by filtered field
        type(energy_flux_address) :: eflux_by_filter
!>        Structure of energy fluxes by filtered field
        type(phys_products_address) :: prod_fld
!
!>        Structure of difference of vector
        type(diff_vector_address) :: diff_vector
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
      end type phys_address
!
!
      end module t_phys_address
