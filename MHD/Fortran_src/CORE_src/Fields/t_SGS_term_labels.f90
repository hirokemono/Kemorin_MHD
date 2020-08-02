!>@file   t_SGS_term_labels.f90
!!        module t_SGS_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms
!!
!!@verbatim
!!      subroutine set_SGS_term_addresses                               &
!!     &         (i_phys, field_name, SGS_term, flag)
!!        type(SGS_term_address), intent(inout) :: SGS_term
!!      subroutine set_rot_SGS_term_addresses                           &
!!     &         (i_phys, field_name, rot_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: rot_SGS
!!      subroutine set_div_SGS_term_addresses                           &
!!     &         (i_phys, field_name, div_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: div_SGS
!!
!!      subroutine set_SGS_model_coef_addresses                         &
!!     &         (i_phys, field_name, Csim, flag)
!!        type(SGS_term_address), intent(inout) :: Csim
!!
!! !!!!!  SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   SGS_momentum_flux   [i_SGS_m_flux]:  SGS momentum flux
!!   SGS_maxwell_tensor [i_SGS_maxwell]:  SGS maxwell tensor
!!   SGS_induct_tensor [i_SGS_induct_t]:  SGS induction tensor
!!
!!   SGS_heat_flux       [i_SGS_h_flux]:   SGS heat flux
!!   SGS_composit_flux   [i_SGS_c_flux]:   SGS composition flux
!!
!!   SGS_inertia    [i_SGS_inertia]: SGS inertia
!!   SGS_Lorentz   [i_SGS_Lorentz]:   SGS Lorentz force
!!   SGS_buoyancy  [i_SGS_buoyancy]:  SGS Thermal buoyancy
!!   SGS_composit_buoyancy [i_SGS_comp_buo]:  SGS compositional buoyancy
!!
!!   SGS_vecp_induction   [i_SGS_vp_induct]: SGS induction  u \times 
!!   SGS_induction        [i_SGS_induction]: SGS magneitic induction
!!
!! !!!!! rotation of SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  field name  [Address]
!!
!!   rot_SGS_inertia  [rot_SGS%i_SGS_inertia]: SGS inertia
!!   rot_SGS_Lorentz  [rot_SGS%i_SGS_Lorentz]:   SGS Lorentz force
!!
!!
!! !!!!! divergence of SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  field name  [Address]
!!
!!   div_SGS_m_flux   [div_SGS%i_SGS_m_flux]:  SGS momentum flux
!!   div_SGS_h_flux   [div_SGS%i_SGS_h_flux]:   SGS heat flux
!!   div_SGS_c_flux   [div_SGS%i_SGS_c_flux]:   SGS composition flux
!!
!!   div_SGS_inertia  [div_SGS%i_SGS_inertia]: SGS inertia
!!   div_SGS_Lorentz  [div_SGS%i_SGS_Lorentz]:   SGS Lorentz force
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_term_labels
!
      use m_precision
      use m_constants
      use t_SGS_term_labels
!
      implicit  none
!!
!>       Structure of start address for SGS terms
      type SGS_term_address
!>        start address for SGS momentum flux
!!         @f$ \widetilde{u_{i}u_{j}} - \tilde{u}_{i}\tilde{u}_{j} @f$
        integer (kind=kint) :: i_SGS_m_flux =      izero
!>        start address for SGS Maxwell tensor
!!         @f$ \widetilde{B_{i}B_{j}} - \tilde{B}_{i}\tilde{B}_{j} @f$
        integer (kind=kint) :: i_SGS_maxwell =     izero
!>        start address for SGS magnetic induction tensor
        integer (kind=kint) :: i_SGS_induct_t =    izero
!>        start address for SGS heat flux
!!         @f$ \widetilde{u_{i}T} - \tilde{u}_{i}\tilde{T} @f$
        integer (kind=kint) :: i_SGS_h_flux =      izero
!>        start address for SGS compositional flux
!!         @f$ \widetilde{u_{i}C} - \tilde{u}_{i}\tilde{C} @f$
        integer (kind=kint) :: i_SGS_c_flux =      izero
!
!>        start address for SGS inertia term
!!         @f$ e_{ijk}\left(\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k} \right) @f$
        integer (kind=kint) :: i_SGS_inertia =    izero
!>        start address for divergence of SGS Maxwell tensor
!!         @f$ e_{ijk}\left(\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k} \right) @f$
        integer (kind=kint) :: i_SGS_Lorentz =     izero
!
!>        start address for SGS buoyancy
!!         @f$ -C^{sim} \alpha_{T} g_{i} I_{Ti} @f$
        integer (kind=kint) :: i_SGS_buoyancy =   izero
!>        start address for SGS compositional buoyancy
!!         @f$ -C^{sim} \alpha_{C} g_{i} I_{Ci} @f$
        integer (kind=kint) :: i_SGS_comp_buo =   izero
!
!>        start address for SGS induction for vector potential
!!         @f$ e_{ijk} \left(\widetilde{u_{j}B_{k}}
!!            - \tilde{u}_{j}\tilde{B}_{k} \right) @f$
        integer (kind=kint) :: i_SGS_vp_induct =   izero
!
!>        start address for SGS induction for vector potential
!!         @f$ e_{ijk} \partial_{j} e_{klm} \left(\widetilde{u_{l}B_{m}}
!!            - \tilde{u}_{l}\tilde{B}_{m} \right) @f$
        integer (kind=kint) :: i_SGS_induction =   izero
      end type SGS_term_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_term_addresses                                 &
     &         (i_phys, field_name, SGS_term, flag)
!
      use m_SGS_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: SGS_term
      logical, intent(inout) :: flag
!
!
      flag = check_SGS_vector_terms(field_name)                         &
     &    .or. check_SGS_tensor_terms(field_name)                       &
     &    .or. check_SGS_induction_tensor(field_name)
      if(flag) then
        if (field_name .eq. SGS_momentum_flux%name ) then
          SGS_term%i_SGS_m_flux =     i_phys
        else if (field_name .eq. SGS_maxwell_tensor%name ) then
          SGS_term%i_SGS_maxwell =    i_phys
        else if (field_name .eq. SGS_induct_tensor%name ) then
          SGS_term%i_SGS_induct_t =    i_phys
!
        else if (field_name .eq. SGS_heat_flux%name) then
          SGS_term%i_SGS_h_flux =    i_phys
        else if (field_name .eq. SGS_composit_flux%name) then
          SGS_term%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. SGS_inertia%name) then
          SGS_term%i_SGS_inertia =   i_phys
        else if (field_name .eq. SGS_Lorentz%name) then
          SGS_term%i_SGS_Lorentz =    i_phys
!
        else if (field_name .eq. SGS_buoyancy%name) then
          SGS_term%i_SGS_buoyancy =   i_phys
        else if (field_name .eq. SGS_composit_buoyancy%name) then
          SGS_term%i_SGS_comp_buo =   i_phys
!
        else if (field_name .eq. SGS_vecp_induction%name) then
          SGS_term%i_SGS_vp_induct =   i_phys
        else if (field_name .eq. SGS_induction%name) then
          SGS_term%i_SGS_induction = i_phys
        end if
      end if
!
      end subroutine set_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_SGS_term_addresses                             &
     &         (i_phys, field_name, rot_SGS, flag)
!
      use m_diff_SGS_term_labels
!
s      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: rot_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_rot_SGS_terms(field_name)
      if(flag) then
        if (field_name .eq. rot_SGS_inertia%name) then
          rot_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. rot_SGS_Lorentz%name) then
          rot_SGS%i_SGS_Lorentz =   i_phys
        end if
      end if
!
      end subroutine set_rot_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_div_SGS_term_addresses                             &
     &         (i_phys, field_name, div_SGS, flag)
!
      use m_diff_SGS_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: div_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_div_SGS_flux_vector(field_name)                      &
     &    .or. check_div_SGS_flux_tensor(field_name)
      if(flag) then
        if (field_name .eq. div_SGS_m_flux%name ) then
          div_SGS%i_SGS_m_flux =     i_phys
        else if (field_name .eq. div_SGS_h_flux%name ) then
          div_SGS%i_SGS_h_flux =     i_phys
        else if (field_name .eq. div_SGS_c_flux%name ) then
          div_SGS%i_SGS_c_flux =     i_phys
!
        else if (field_name .eq. div_SGS_inertia%name) then
          div_SGS%i_SGS_inertia =    i_phys
        else if (field_name .eq. div_SGS_Lorentz%name) then
          div_SGS%i_SGS_Lorentz =    i_phys
        end if
      end if
!
      end subroutine set_div_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      end module t_SGS_term_labels
