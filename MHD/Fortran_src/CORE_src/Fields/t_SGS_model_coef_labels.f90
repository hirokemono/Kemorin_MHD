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
!!      subroutine set_SGS_model_coef_addresses                         &
!!     &         (i_phys, field_name, Csim, flag)
!!        type(SGS_term_address), intent(inout) :: Csim
!!      subroutine set_dynamic_SGS_work_addresses                       &
!!     &         (i_phys, field_name, SGS_wk, flag)
!!        type(dynamic_SGS_work_address), intent(inout) :: SGS_wk
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
!!
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
      subroutine set_SGS_model_coef_addresses                           &
     &         (i_phys, field_name, Csim, flag)
!
      use m_SGS_model_coef_labels
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
      use m_SGS_model_coef_labels
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
!
      end module t_SGS_model_coef_labels
