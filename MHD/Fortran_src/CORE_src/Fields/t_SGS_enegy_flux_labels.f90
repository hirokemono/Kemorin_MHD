!>@file   t_SGS_enegy_flux_labels.f90
!!        module t_SGS_enegy_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      subroutine set_SGS_ene_flux_addresses                           &
!!     &         (i_phys, field_name, SGS_ene_flux, flag)
!!        type(SGS_ene_flux_address), intent(inout) :: SGS_ene_flux
!! !!!!!  energy flux by SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   Reynolds_work [i_reynolds_wk]:   Reynolds stress
!!   SGS_Lorentz_work [i_SGS_Lor_wk]: work of SGS Lorentz force
!!   SGS_buoyancy_flux [i_SGS_buo_wk]: SGS buoyancy flux
!!   SGS_comp_buoyancy_flux [i_SGS_comp_buo_wk]: 
!!                          SGS compositional buoyancy flux
!!
!!   SGS_mag_induction_flux [i_SGS_me_gen]:
!!                           magnetic energy flux by SGS induction
!!
!!   SGS_temp_flux_gen [i_SGS_temp_gen]:
!!                           temperature change by SGS heat flux
!!   SGS_comp_flux_gen [i_SGS_comp_gen]:
!!                           temperature change by SGS composition flux
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_enegy_flux_labels
!
      use m_precision
      use m_constants
!
      implicit  none
!!
!
!>       Structure of start address for energy flux by SGS terms
      type SGS_ene_flux_address
!>        Field address for work of SGS Reynolds stress
!!         @f$ -u_{i} e_{ijk} (\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k}) @f$
        integer (kind=kint) :: i_reynolds_wk =     izero
!>        Field address for work of SGS Lorentz force
!!         @f$  u_{i} e_{ijk} (\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k}) @f$
        integer (kind=kint) :: i_SGS_Lor_wk =      izero
!>        Field address for work of SGS buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{T} g_{i} I_{Ti} @f$
        integer (kind=kint) :: i_SGS_buo_wk =      izero
!>        Field address for work of SGS compositional buoyancy
!!         @f$ - u_{i} C^{sim} \alpha_{C} g_{i} I_{Ci} @f$
        integer (kind=kint) :: i_SGS_comp_buo_wk = izero
!
!>        Field address for energy flux of SGS induction
!!         @f$ B_{i} e_{ijk} \partial_{j} e_{klm} 
!!            (\widetilde{u_{l}B_{m}} - \tilde{u}_{l}\tilde{B}_{m} ) @f$
        integer (kind=kint) :: i_SGS_me_gen =      izero
!
!>        Field address for temperature generation by SGS heat flux
!!         @f$ T \partial_{i} \left( \widetilde{u_{i}T}
!!            - \tilde{u}_{i}\tilde{T} \right) @f$
        integer (kind=kint) :: i_SGS_temp_gen =    izero
!>        Field address for composition generation
!!           by SGS composition flux
!!         @f$ C \partial_{i} \left( \widetilde{u_{i}C}
!!            - \tilde{u}_{i}\tilde{C} \right) @f$
        integer (kind=kint) :: i_SGS_comp_gen =    izero
      end type SGS_ene_flux_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_ene_flux_addresses                             &
     &         (i_phys, field_name, SGS_ene_flux, flag)
!
      use m_SGS_enegy_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_ene_flux_address), intent(inout) :: SGS_ene_flux
      logical, intent(inout) :: flag
!
!
      flag = check_SGS_ene_fluxes(field_name)
      if(flag) then
        if (field_name .eq. Reynolds_work%name ) then
          SGS_ene_flux%i_reynolds_wk =     i_phys
        else if (field_name .eq. SGS_Lorentz_work%name ) then
          SGS_ene_flux%i_SGS_Lor_wk =      i_phys
        else if (field_name .eq. SGS_buoyancy_flux%name ) then
          SGS_ene_flux%i_SGS_buo_wk =      i_phys
        else if (field_name .eq. SGS_comp_buoyancy_flux%name ) then
          SGS_ene_flux%i_SGS_comp_buo_wk = i_phys
!
        else if (field_name .eq. SGS_mag_induction_flux%name) then
          SGS_ene_flux%i_SGS_me_gen =      i_phys
!
        else if (field_name .eq. SGS_temp_flux_gen%name) then
          SGS_ene_flux%i_SGS_temp_gen =    i_phys
        else if (field_name .eq. SGS_comp_flux_gen%name) then
          SGS_ene_flux%i_SGS_comp_gen =    i_phys
        end if
      end if
!
      end subroutine set_SGS_ene_flux_addresses
!
! ----------------------------------------------------------------------
!
      end module t_SGS_enegy_flux_labels
