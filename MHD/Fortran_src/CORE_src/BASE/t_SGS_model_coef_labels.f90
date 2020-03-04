!>@file   t_SGS_model_coef_labels.f90
!!        module t_SGS_model_coef_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS model coefficients and work area
!!
!!@verbatim
!!      logical function check_SGS_moedel_coefs(field_name)
!!      logical function check_dynamic_SGS_work(field_name)
!!      logical function check_commute_SGS_work(field_name)
!!
!!      subroutine set_SGS_model_coef_addresses                         &
!!     &         (i_phys, field_name, Csim, flag)
!!        type(SGS_term_address), intent(inout) :: Csim
!!      subroutine set_dynamic_SGS_work_addresses                       &
!!     &         (i_phys, field_name, SGS_wk, flag)
!!        type(dynamic_SGS_work_address), intent(inout) :: SGS_wk
!!
!!      integer(kind = kint) function num_SGS_model_coefs()
!!      integer(kind = kint) function num_dynamic_SGS_work()
!!      subroutine set_SGS_model_coefs_labels(n_comps, names, maths)
!!      subroutine set_dynamic_SGS_work_labels(n_comps, names, maths)
!!
!! !!!!!  SGS model coefficients names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   Csim_SGS_heat_flux       [Csim%i_SGS_h_flux]: SGS heat flux
!!   Csim_SGS_composit_flux   [Csim%i_SGS_c_flux]: SGS composition flux
!!
!!   Csim_SGS_inertia         [Csim%i_SGS_m_flux]: SGS inertia
!!   Csim_SGS_Lorentz         [Csim%i_SGS_Lorentz]: SGS Lorentz force
!!
!!   Csim_SGS_buoyancy        [Csim%i_SGS_buoyancy]:
!!                                            SGS Thermal buoyancy
!!   Csim_SGS_composit_buo    [Csim%i_SGS_comp_buo]:
!!                                            SGS compositional buoyancy
!!
!!   Csim_SGS_induction       [Csim%i_SGS_vp_induct]:
!!                                            SGS induction  u \times 
!!
!! Work area for dynamic model in FEM_MHD
!!
!!   SGS_simi    [i_simi]
!!   SGS_grad    [i_nlg]
!!   SGS_grad_f  [i_wd_nlg]
!!
!!   SGS_diffuse     [i_wk_diffuse]
!!
!!   temp_4_SGS      [i_sgs_temp]
!!   temp_4_SGS      [i_sgs_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_model_coef_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_SGS_term_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: num_SGS_Csim = 7
      integer(kind = kint), parameter, private :: num_SGS_work = 6
!
!
!>        Field label for model coefficient of SGS heat flux
!!         @f$ C^{sim}_{I} @f$
      type(field_def), parameter :: Csim_SGS_heat_flux                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_heat_flux',                      &
     &                math = '$ C^{sim}_{I} $')
!>        Field label for model coefficient of SGS composition flux
!!         @f$ C^{sim}_{Ic} @f$
      type(field_def), parameter :: Csim_SGS_composit_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_composit_flux',                  &
     &                math = '$ C^{sim}_{Ic}} $')
!>        Field label for model coefficient of SGS inertia term
!!         @f$ C^{sim}_{\tau} @f$
      type(field_def), parameter :: Csim_SGS_inertia                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_inertia',                        &
     &                math = '$ C^{sim}_{\tau}} $')
!>        Field label for model coefficient of SGS Lorentz force
!!         @f$ C^{sim}_{M} @f$
      type(field_def), parameter :: Csim_SGS_Lorentz                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_Lorentz',                        &
     &                math = '$ C^{sim}_{M}} $')
!>        Field label for model coefficient of SGS induction
!!         @f$ C^{sim}_{\alphs} @f$
      type(field_def), parameter :: Csim_SGS_induction                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_induction',                      &
     &                math = '$ C^{sim}_{\alpha}} $')
!>        Field label for model coefficient of SGS buoyancy
!!         @f$ C^{sim}_{buo} @f$
      type(field_def), parameter :: Csim_SGS_buoyancy                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_buoyancy',                       &
     &                math = '$  C^{sim}_{buo} $')
!>        Field label for odel coefficient of SGS compositional buoyancy
!!         @f$ C^{sim}_{buo_C} @f$
      type(field_def), parameter :: Csim_SGS_composit_buo               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_composit_buo',                   &
     &                math = '$  C^{sim}_{buo_C} $')
!
!
!>        Field label for SGS term by scale similarity method
!!         @f$ \mathcal{L} @f$
      type(field_def), parameter :: SGS_simi                            &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_simi',                                &
     &                math = '$  \mathcal{L} $')
!>        Field label for SGS term by nonlinear gradient method
!!         @f$ I_{i} @f$
      type(field_def), parameter :: SGS_grad                            &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_grad',                                &
     &                math = '$ I_{i} $')
!>        Field label for SGS term by nonlinear gradient method
!>        using fileterd field
!!         @f$ I_{i}^{2\Delta} @f$
      type(field_def), parameter :: SGS_grad_f                          &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_grad_f',                              &
     &                math = '$ I_{i}^{2\Delta} $')
!
!>        Field label for SGS term by turbulence diffusivity
!!         @f$ I_{\nu} @f$
      type(field_def), parameter :: SGS_diffuse                         &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_diffuse',                             &
     &                math = '$ I_{\nu} $')
!
!>        Field label for temperature to obatin commutation error
      type(field_def), parameter :: temp_4_SGS                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temp_4_SGS',                              &
     &                math = '$ T $')
!>        Field label for composition variation
!!        to obatin commutation error
      type(field_def), parameter :: comp_4_SGS                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'comp_4_SGS',                              &
     &                math = '$ C $')
!
!
!>       Structure of start address for dynamic model's work area
      type dynamic_SGS_work_address
!>        start address of SGS term by scale similarity method
        integer (kind=kint) :: i_simi =   izero
!>        start address of SGS term by nonlinear gradient method
        integer (kind=kint) :: i_nlg =    izero
!>        start address of SGS term by nonlinear gradient method
!!        using fileterd field
        integer (kind=kint) :: i_wd_nlg = izero
!>        start address of SGS term by turbulence diffusivity
        integer (kind=kint) :: i_wk_diffuse = izero
!
!>        start address of temperature to obatin commutation error
        integer (kind=kint) :: i_sgs_temp =        izero
!>        start address of composition variation
!!        to obatin commutation error
        integer (kind=kint) :: i_sgs_composit =    izero
      end type dynamic_SGS_work_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_moedel_coefs(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_moedel_coefs                                            &
     &   =    (field_name .eq. Csim_SGS_heat_flux%name)                 &
     &   .or. (field_name .eq. Csim_SGS_composit_flux%name)             &
     &   .or. (field_name .eq. Csim_SGS_inertia%name)                   &
     &   .or. (field_name .eq. Csim_SGS_Lorentz%name)                   &
     &   .or. (field_name .eq. Csim_SGS_induction%name)                 &
     &   .or. (field_name .eq. Csim_SGS_buoyancy%name)                  &
     &   .or. (field_name .eq. Csim_SGS_composit_buo%name)
!
      end function check_SGS_moedel_coefs
!
! ----------------------------------------------------------------------
!
      logical function check_dynamic_SGS_work(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_dynamic_SGS_work                                            &
     &   =    (field_name .eq. SGS_simi%name)                           &
     &   .or. (field_name .eq. SGS_grad%name)                           &
     &   .or. (field_name .eq. SGS_grad_f%name)                         &
     &   .or. (field_name .eq. SGS_diffuse%name)
!
      end function check_dynamic_SGS_work
!
! ----------------------------------------------------------------------
!
      logical function check_commute_SGS_work(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_commute_SGS_work                                            &
     &   =    (field_name .eq. temp_4_SGS%name)                         &
     &   .or. (field_name .eq. comp_4_SGS%name)
!
      end function check_commute_SGS_work
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_model_coef_addresses                           &
     &         (i_phys, field_name, Csim, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: Csim
      logical, intent(inout) :: flag
!
!
      flag = check_SGS_moedel_coefs(field_name)
      if(flag) then
        if (field_name .eq. Csim_SGS_heat_flux%name) then
          Csim%i_SGS_h_flux =    i_phys
        else if (field_name .eq. Csim_SGS_composit_flux%name) then
          Csim%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. Csim_SGS_inertia%name ) then
          Csim%i_SGS_m_flux =     i_phys
        else if (field_name .eq. Csim_SGS_Lorentz%name ) then
          Csim%i_SGS_Lorentz =    i_phys
!
        else if (field_name .eq. Csim_SGS_buoyancy%name) then
          Csim%i_SGS_buoyancy =   i_phys
        else if (field_name .eq. Csim_SGS_composit_buo%name) then
          Csim%i_SGS_comp_buo =   i_phys
!
        else if (field_name .eq. Csim_SGS_induction%name) then
          Csim%i_SGS_vp_induct =   i_phys
        end if
      end if
!
      end subroutine set_SGS_model_coef_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_dynamic_SGS_work_addresses                         &
     &         (i_phys, field_name, SGS_wk, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(dynamic_SGS_work_address), intent(inout) :: SGS_wk
      logical, intent(inout) :: flag
!
!
      flag =    check_dynamic_SGS_work(field_name)                      &
     &    .or.  check_commute_SGS_work(field_name)
      if(flag) then
        if (field_name .eq. SGS_simi%name) then
          SGS_wk%i_simi =     i_phys
        else if (field_name .eq. SGS_grad%name) then
          SGS_wk%i_nlg =      i_phys
        else if (field_name .eq. SGS_grad_f%name ) then
          SGS_wk%i_wd_nlg =   i_phys
!
        else if (field_name .eq. SGS_diffuse%name ) then
          SGS_wk%i_wk_diffuse =  i_phys
!
        else if (field_name .eq. temp_4_SGS%name) then
          SGS_wk%i_sgs_temp =     i_phys
        else if (field_name .eq. comp_4_SGS%name) then
          SGS_wk%i_sgs_composit = i_phys
        end if
      end if
!
      end subroutine set_dynamic_SGS_work_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_SGS_model_coefs()
      num_SGS_model_coefs = num_SGS_Csim
      return
      end function num_SGS_model_coefs
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_dynamic_SGS_work()
      num_dynamic_SGS_work = num_SGS_work
      return
      end function num_dynamic_SGS_work
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_model_coefs_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(num_SGS_Csim)
      character(len = kchara), intent(inout) :: names(num_SGS_Csim)
      character(len = kchara), intent(inout) :: maths(num_SGS_Csim)
!
!
      call set_field_labels(Csim_SGS_heat_flux,                         &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(Csim_SGS_composit_flux,                     &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(Csim_SGS_inertia,                           &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(Csim_SGS_Lorentz,                           &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(Csim_SGS_induction,                         &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(Csim_SGS_buoyancy,                          &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(Csim_SGS_composit_buo,                      &
     &    n_comps( 7), names( 7), maths( 7))
!
      end subroutine set_SGS_model_coefs_labels
!
! ----------------------------------------------------------------------
!
      subroutine set_dynamic_SGS_work_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(num_SGS_work)
      character(len = kchara), intent(inout) :: names(num_SGS_work)
      character(len = kchara), intent(inout) :: maths(num_SGS_work)
!
!
      call set_field_labels(SGS_diffuse,                                &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(SGS_simi,                                   &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(SGS_grad,                                   &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(SGS_grad_f,                                 &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(temp_4_SGS,                                 &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(comp_4_SGS,                                 &
     &    n_comps( 6), names( 6), maths( 6))
!
      end subroutine set_dynamic_SGS_work_labels
!
! ----------------------------------------------------------------------
!
      end module t_SGS_model_coef_labels
