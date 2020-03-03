!>@file   t_SGS_model_coef_labels.f90
!!        module t_SGS_model_coef_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms
!!
!!@verbatim
!!      integer(kind = kint) function num_SGS_model_coefs()
!!      integer(kind = kint) function num_dynamic_SGS_work()
!!      subroutine set_SGS_model_coefs_labels(n_comps, names, maths)
!!      subroutine set_dynamic_SGS_work_labels(n_comps, names, maths)
!!
!! !!!!!  SGS model coefficients names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   Csim_SGS_inertia    [i_SGS_inertia]: SGS inertia
!!   Csim_SGS_Lorentz   [i_SGS_Lorentz]:   SGS Lorentz force
!!
!!   Csim_SGS_buoyancy  [i_SGS_buoyancy]:  SGS Thermal buoyancy
!!   Csim_SGS_composit_buo [i_SGS_comp_buo]:  SGS compositional buoyancy
!!
!!   Csim_SGS_vp_induction   [i_SGS_vp_induct]: SGS induction  u \times 
!!
!!   Csim_SGS_heat_flux       [i_SGS_h_flux]:   SGS heat flux
!!   Csim_SGS_composit_flux   [i_SGS_c_flux]:   SGS composition flux
!!
!!
!!   SGS_simi    []:
!!   SGS_grad    []:
!!   SGS_grad_f  []:
!!
!!   SGS_diffuse     []:
!!
!!   temp_4_SGS      []:
!!   temp_4_SGS      []:
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_model_coef_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: num_SGS_Csim = 7
      integer(kind = kint), parameter, private :: num_SGS_work = 6
!
!
!>        Field label for model coefficient of SGS heat flux
!!         @f$ C^{sim}_{I} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_h_flux = 'Csim_SGS_heat_flux'
      type(field_def), parameter :: Csim_SGS_heat_flux                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_heat_flux',                      &
     &                math = '$ C^{sim}_{I} $')
!>        Field label for model coefficient of SGS composition flux
!!         @f$ C^{sim}_{Ic} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_c_flux = 'Csim_SGS_composit_flux'
      type(field_def), parameter :: Csim_SGS_composit_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_composit_flux',                  &
     &                math = '$ C^{sim}_{Ic}} $')
!>        Field label for model coefficient of SGS inertia term
!!         @f$ C^{sim}_{\tau} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_m_flux = 'Csim_SGS_inertia'
      type(field_def), parameter :: Csim_SGS_inertia                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_inertia',                        &
     &                math = '$ C^{sim}_{\tau}} $')
!>        Field label for model coefficient of SGS Lorentz force
!!         @f$ C^{sim}_{M} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_Lorentz = 'Csim_SGS_Lorentz'
      type(field_def), parameter :: Csim_SGS_Lorentz                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_Lorentz',                        &
     &                math = '$ C^{sim}_{M}} $')
!>        Field label for model coefficient of SGS induction
!!         @f$ C^{sim}_{\alphs} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_induction = 'Csim_SGS_vp_induction'
      type(field_def), parameter :: Csim_SGS_vp_induction               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_vp_induction',                   &
     &                math = '$ C^{sim}_{\alpha}} $')
!>        Field label for model coefficient of SGS buoyancy
!!         @f$ C^{sim}_{buo} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_buoyancy =  'Csim_SGS_buoyancy'
      type(field_def), parameter :: Csim_SGS_buoyancy                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_buoyancy',                       &
     &                math = '$  C^{sim}_{buo} $')
!>        Field label for odel coefficient of SGS compositional buoyancy
!!         @f$ C^{sim}_{buo_C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_comp_buo = 'Csim_SGS_composit_buo'
      type(field_def), parameter :: Csim_SGS_composit_buo               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_composit_buo',                   &
     &                math = '$  C^{sim}_{buo_C} $')
!
!
!>        Field label for SGS term by scale similarity method
!!         @f$ \mathcal{L} @f$
      character(len=kchara), parameter :: fhd_SGS_simi =   'SGS_simi'
      type(field_def), parameter :: SGS_simi                            &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_simi',                                &
     &                math = '$  \mathcal{L} $')
!>        Field label for SGS term by nonlinear gradient method
!!         @f$ I_{i} @f$
      character(len=kchara), parameter :: fhd_SGS_grad =   'SGS_grad'
      type(field_def), parameter :: SGS_grad                            &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_grad',                                &
     &                math = '$ I_{i} $')
!>        Field label for SGS term by nonlinear gradient method
!>        using fileterd field
!!         @f$ I_{i}^{2\Delta} @f$
      character(len=kchara), parameter :: fhd_SGS_grad_f = 'SGS_grad_f'
      type(field_def), parameter :: SGS_grad_f                          &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_grad_f',                              &
     &                math = '$ I_{i}^{2\Delta} $')
!
!>        Field label for SGS term by turbulence diffusivity
!!         @f$ I_{\nu} @f$
      character(len=kchara), parameter                                  &
     &              :: fhd_SGS_diffuse = 'SGS_diffuse'
      type(field_def), parameter :: SGS_diffuse                         &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_diffuse',                             &
     &                math = '$ I_{\nu} $')
!
!>        Field label for temperature to obatin commutation error
      character(len=kchara), parameter :: fhd_SGS_temp =   'temp_4_SGS'
      type(field_def), parameter :: temp_4_SGS                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temp_4_SGS',                              &
     &                math = '$ T $')
!>        Field label for composition variation
!!        to obatin commutation error
      character(len=kchara), parameter :: fhd_SGS_comp =   'comp_4_SGS'
      type(field_def), parameter :: comp_4_SGS                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'comp_4_SGS',                              &
     &                math = '$ C $')
!
! ----------------------------------------------------------------------
!
      contains
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
      call set_field_labels(Csim_SGS_vp_induction,                      &
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
