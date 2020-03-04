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
!!      logical function check_SGS_vector_terms(field_name)
!!      logical function check_SGS_tensor_terms(field_name)
!!      logical function check_SGS_induction_tensor(field_name)
!!      subroutine set_SGS_term_addresses                               &
!!     &         (i_phys, field_name, SGS_term, flag)
!!        type(SGS_term_address), intent(inout) :: SGS_term
!!
!!      integer(kind = kint) function num_SGS_terms()
!!      subroutine set_SGS_term_labels(n_comps, names, maths)
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nterms_SGS = 11
!
!>        Field label for SGS momentum flux
!!         @f$ \widetilde{u_{i}u_{j}} - \tilde{u}_{i}\tilde{u}_{j} @f$
      type(field_def), parameter :: SGS_momentum_flux                   &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_momentum_flux',                       &
     &                math = '$ \widetilde{u_{i}u_{j}}'                 &
     &                     // '  - \tilde{u}_{i}\tilde{u}_{j} $')
!>        Field label for SGS Maxwell tensor
!!         @f$ \widetilde{B_{i}B_{j}} - \tilde{B}_{i}\tilde{B}_{j} @f$
      type(field_def), parameter :: SGS_maxwell_tensor                  &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_maxwell_tensor',                      &
     &                math = '$ \widetilde{B_{i}B_{j}}'                 &
     &                     // '  - \tilde{B}_{i}\tilde{B}_{j} $')
!>        Field label for SGS magnetic induction tensor
!!         @f$ \widetilde{u_{i}B_{j}} - \tilde{u}_{i}\tilde{B}_{j}
!!           - \widetilde{B_{i}u_{j}} + \tilde{B}_{i}\tilde{u}_{j} @f$
      type(field_def), parameter :: SGS_induct_tensor                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_induct_tensor',                       &
     &                math = '$ \widetilde{u_{i}B_{j}}'                 &
     &                     // ' - \tilde{u}_{i}\tilde{B}_{j}'           &
     &                     // ' - \widetilde{B_{i}u_{j}}'               &
     &                     // ' + \tilde{B}_{i}\tilde{u}_{j} $')
!
!>        Field label for SGS heat flux
!!         @f$ \widetilde{u_{i}T} - \tilde{u}_{i}\tilde{T} @f$
      type(field_def), parameter :: SGS_heat_flux                       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_heat_flux',                           &
     &                math = '$ \widetilde{u_{i}T}'                     &
     &                     // ' - \tilde{u}_{i}\tilde{T} $')
!>        Field label for SGS compositional flux
!!         @f$ \widetilde{u_{i}C} - \tilde{u}_{i}\tilde{C} @f$
      type(field_def), parameter :: SGS_composit_flux                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_composit_flux',                       &
     &                math = '$ \widetilde{u_{i}C}'                     &
     &                     // ' - \tilde{u}_{i}\tilde{C} $')
!
!>        Field label for SGS inertia term
!!         @f$ e_{ijk}\left(\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k} \right) @f$
      type(field_def), parameter :: SGS_inertia                         &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_inertia',                             &
     &                math = '$ e_{ijk} (\widetilde{\omega_{j}u_{k}}'   &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for divergence of SGS Maxwell tensor
!!         @f$ e_{ijk}\left(\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k} \right) @f$
      type(field_def), parameter :: SGS_Lorentz                         &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_Lorentz',                             &
     &                math = '$ e_{ijk} (\widetilde{J_{j}B_{k}}'        &
     &                     // ' - \tilde{J}_{j}\tilde{B}_{k}) $')
!
!>        Field label for SGS buoyancy
!!         @f$ -C^{sim} \alpha_{T} g_{i} I_{Ti} @f$
      type(field_def), parameter :: SGS_buoyancy                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_buoyancy',                            &
     &                math = '$ -C^{sim} \alpha_{T} g_{i} I_{Ti} $')
!>        Field label for SGS compositional buoyancy
!!         @f$ -C^{sim} \alpha_{C} g_{i} I_{Ci} @f$
      type(field_def), parameter :: SGS_composit_buoyancy               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_composit_buoyancy',                   &
     &                math = '$ -C^{sim} \alpha_{C} g_{i} I_{Ci} $')
!
!>        Field label for SGS induction for vector potential
!!         @f$ e_{ijk} (\widetilde{u_{j}B_{k}}
!!            - \tilde{u}_{j}\tilde{B}_{k}) @f$
      type(field_def), parameter :: SGS_vecp_induction                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_vecp_induction',                      &
     &                math = '$ e_{ijk} (\widetilde{u_{j}B_{k}}'        &
     &                     // ' - \tilde{u}_{j}\tilde{B}_{k}) $')
!>        Field label for SGS magnetic induction
!!         @f$ e_{ijk} \partual_{j} e_{klm} (\widetilde{u_{l}B_{m}}
!!            - \tilde{u}_{l}\tilde{B}_{m} ) @f$
      type(field_def), parameter :: SGS_induction                       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_induction',                           &
     &                math = '$ e_{ijk} \partual_{j} e_{klm}'           &
     &                     // ' (\widetilde{u_{l}B_{m}}'                &
     &                     // ' - \tilde{u}_{j}\tilde{B}_{k}) $')
!
!
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
!!         @f$ e_{ijk} (\widetilde{u_{j}B_{k}}
!!            - \tilde{u}_{j}\tilde{B}_{k}) @f$
        integer (kind=kint) :: i_SGS_vp_induct =   izero
!
!>        start address for SGS induction for vector potential
!!         @f$ e_{ijk} \partual_{j} e_{klm} (\widetilde{u_{l}B_{m}}
!!            - \tilde{u}_{l}\tilde{B}_{m} ) @f$
        integer (kind=kint) :: i_SGS_induction =   izero
      end type SGS_term_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_vector_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_vector_terms                                            &
     &   =    (field_name .eq. SGS_inertia%name)                        &
     &   .or. (field_name .eq. SGS_Lorentz%name)                        &
     &   .or. (field_name .eq. SGS_buoyancy%name)                       &
     &   .or. (field_name .eq. SGS_composit_buoyancy%name)              &
     &   .or. (field_name .eq. SGS_vecp_induction%name)                 &
     &   .or. (field_name .eq. SGS_heat_flux%name)                      &
     &   .or. (field_name .eq. SGS_composit_flux%name)                  &
     &   .or. (field_name .eq. SGS_induction%name)
!
      end function check_SGS_vector_terms
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_tensor_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_tensor_terms                                            &
     &   =    (field_name .eq. SGS_momentum_flux%name)                  &
     &   .or. (field_name .eq. SGS_maxwell_tensor%name)
!
      end function check_SGS_tensor_terms
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_induction_tensor(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_induction_tensor                                        &
     &   =    (field_name .eq. SGS_induct_tensor%name)
!
      end function check_SGS_induction_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_SGS_term_addresses                                 &
     &         (i_phys, field_name, SGS_term, flag)
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
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_SGS_terms()
      num_SGS_terms = nterms_SGS
      return
      end function num_SGS_terms
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_term_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(nterms_SGS)
      character(len = kchara), intent(inout) :: names(nterms_SGS)
      character(len = kchara), intent(inout) :: maths(nterms_SGS)
!
!
      call set_field_labels(SGS_momentum_flux,                          &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(SGS_maxwell_tensor,                         &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(SGS_induct_tensor,                          &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(SGS_heat_flux,                              &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(SGS_composit_flux,                          &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(SGS_inertia,                                &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(SGS_Lorentz,                                &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(SGS_buoyancy,                               &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(SGS_composit_buoyancy,                      &
     &    n_comps( 9), names( 9), maths( 9))
!
      call set_field_labels(SGS_vecp_induction,                         &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(SGS_induction,                              &
     &    n_comps(11), names(11), maths(11))
!
      end subroutine set_SGS_term_labels
!
! ----------------------------------------------------------------------
!
      end module t_SGS_term_labels
