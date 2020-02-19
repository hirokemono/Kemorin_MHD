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
!!      logical function check_SGS_scalar_terms(field_name)
!!      logical function check_SGS_vector_terms(field_name)
!!      logical function check_SGS_tensor_terms(field_name)
!!      subroutine set_SGS_scalar_term_addresses                        &
!!     &         (i_phys, field_name, SGS_term, flag)
!!      subroutine set_SGS_vector_term_addresses                        &
!!     &         (i_phys, field_name, SGS_term, flag)
!!      subroutine set_SGS_tensor_term_addresses                        &
!!     &         (i_phys, field_name, SGS_term, flag)
!!        type(SGS_term_address), intent(inout) :: SGS_term
!!
!!      subroutine SGG_scalar_monitor_address                           &
!!     &         (field_name, i_field, numrms, numave,                    &
!!     &          rms_SGS_term, ave_SGS_term, flag)
!!      subroutine SGG_vector_monitor_address                           &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_SGS_term, ave_SGS_term, flag)
!!      subroutine SGG_tensor_monitor_address                           &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_SGS_term, ave_SGS_term, flag)
!!        type(SGS_term_address), intent(inout) :: rms_SGS_term
!!        type(SGS_term_address), intent(inout) :: ave_SGS_term
!!
!!      integer(kind = kint) function num_SGS_terms()
!!      subroutine set_SGS_term_names(field_names)
!!
!! !!!!!  SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   SGS_momentum_flux   [i_SGS_m_flux]:  SGS momentum flux
!!   SGS_maxwell_tensor [i_SGS_maxwell]:  SGS maxwell tensor
!!   SGS_induct_tensor [i_SGS_induct_t]:  SGS induction tensor
!!
!!   SGS_inertia    [i_SGS_inertia]: SGS inertia
!!   SGS_Lorentz   [i_SGS_Lorentz]:   SGS Lorentz force
!!
!!   SGS_buoyancy  [i_SGS_buoyancy]:  SGS Thermal buoyancy
!!   SGS_composit_buoyancy [i_SGS_comp_buo]:  SGS compositional buoyancy
!!
!!   SGS_vecp_induction   [i_SGS_vp_induct]: SGS induction  u \times 
!!   SGS_induction        [i_SGS_induction]: SGS magneitic induction
!!
!!   SGS_heat_flux       [i_SGS_h_flux]:   SGS heat flux
!!   SGS_composit_flux   [i_SGS_c_flux]:   SGS composition flux
!!
!!   div_SGS_m_flux   [i_SGS_div_m_flux]: divergence of 
!!                                           SGS momentum flux
!!   div_SGS_h_flux   [i_SGS_div_h_flux]: divergence of 
!!                                           SGS heat flux
!!   div_SGS_c_flux   [i_SGS_div_c_flux]: divergence of 
!!                                           SGS composition flux
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_SGS_term_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nterms_SGS = 14
!
!>        Field label for SGS momentum flux
!!         @f$ \overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_flux =    'SGS_momentum_flux'
!>        Field label for SGS Maxwell tensor
!!         @f$ \overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_maxwell_t = 'SGS_maxwell_tensor'
!>        Field label for SGS magnetic induction tensor
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_induct_t =  'SGS_induct_tensor'
!
!>        Field label for SGS inertia term
!!         @f$ e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_inertia =     'SGS_inertia'
!>        Field label for divergence of SGS Maxwell tensor
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}}
!!              - \bar{B}_{i}\bar{B}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz =     'SGS_Lorentz'
!
!>        Field label for SGS buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_buoyancy =  'SGS_buoyancy'
!>        Field label for SGS compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_buo = 'SGS_composit_buoyancy'
!
!>        Field label for SGS induction for vector potential
!!         @f$ e_{ijk}\left(\overline{u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_vp_induct =       'SGS_vecp_induction'
!>        Field label for SGS magnetic induction
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_induction =       'SGS_induction'
!
!>        Field label for SGS heat flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_h_flux =    'SGS_heat_flux'
!>        Field label for SGS compositional flux
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_c_flux =    'SGS_composit_flux'
!
!>        Field label for divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \overline{u_{i}u_{j}}
!!             - \bar{u}_{i}\bar{u}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_m_flux =  'div_SGS_m_flux'
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T}
!!            - \bar{u}_{i}\bar{T} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_h_flux =    'div_SGS_h_flux'
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C}
!!            - \bar{u}_{i}\bar{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_c_flux =    'div_SGS_c_flux'
!
!>       Structure for start address for SGS terms
      type SGS_term_address
!>        start address for SGS inertia term
        integer (kind=kint) :: i_SGS_inertia =    izero
!>        start address for divergence of SGS Maxwell tensor
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}}
!!             - \bar{B}_{i}\bar{B}_{j} \right) @f$
        integer (kind=kint) :: i_SGS_Lorentz =     izero
!
!>        start address for SGS buoyancy
        integer (kind=kint) :: i_SGS_buoyancy =   izero
!>        start address for SGS compositional buoyancy
        integer (kind=kint) :: i_SGS_comp_buo =   izero
!
!>        start address for SGS induction for vector potential
!!         @f$e_{ijk}\left(\overline{u_{j}B_{k}} 
!!              - \bar{u}_{j}\bar{B}_{k} \right) @f$
        integer (kind=kint) :: i_SGS_vp_induct =   izero
!>        start address for divergence of SGS magnetic induction tensor
        integer (kind=kint) :: i_SGS_induction =   izero
!>        start address for SGS magnetic induction tensor
        integer (kind=kint) :: i_SGS_induct_t =    izero
!
!>        start address for SGS heat flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_SGS_h_flux =      izero
!>        start address for SGS compositional flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_SGS_c_flux =      izero
!>        start address for SGS momentum flux
!!         @f$ \overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j} @f$
        integer (kind=kint) :: i_SGS_m_flux =      izero
!>        start address for SGS Maxwell tensor
!!         @f$ \overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j} @f$
        integer (kind=kint) :: i_SGS_maxwell =     izero
!
!>        start address for divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \overline{u_{i}u_{j}} 
!!                               - \bar{u}_{i}\bar{u}_{j} \right) @f$
         integer (kind=kint) :: i_SGS_div_m_flux=   izero
!>        start address for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T}
!!                               - \bar{u}_{i}\bar{T} \right) @f$
        integer (kind=kint) :: i_SGS_div_h_flux=   izero
!>        start address for divergence of SGS composition flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C}
!!                               - \bar{u}_{i}\bar{C} \right) @f$
        integer (kind=kint) :: i_SGS_div_c_flux=   izero
      end type SGS_term_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_scalar_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_scalar_terms = .FALSE.
      if (    (field_name .eq. fhd_div_SGS_h_flux)                      &
     &   .or. (field_name .eq. fhd_div_SGS_c_flux)                      &
     &      )   check_SGS_scalar_terms = .TRUE.
!
      end function check_SGS_scalar_terms
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_vector_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_vector_terms = .FALSE.
      if (    (field_name .eq. fhd_SGS_inertia)                         &
     &   .or. (field_name .eq. fhd_div_SGS_m_flux)                      &
     &   .or. (field_name .eq. fhd_SGS_Lorentz)                         &
     &   .or. (field_name .eq. fhd_SGS_buoyancy)                        &
     &   .or. (field_name .eq. fhd_SGS_comp_buo)                        &
     &   .or. (field_name .eq. fhd_SGS_vp_induct)                       &
     &   .or. (field_name .eq. fhd_SGS_induction)                       &
     &   .or. (field_name .eq. fhd_SGS_induct_t)                        &
     &   .or. (field_name .eq. fhd_SGS_h_flux)                          &
     &   .or. (field_name .eq. fhd_SGS_c_flux)                          &
     &      )   check_SGS_vector_terms = .TRUE.
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
      check_SGS_tensor_terms = .FALSE.
      if (    (field_name .eq. fhd_SGS_m_flux)                          &
     &   .or. (field_name .eq. fhd_SGS_maxwell_t)                       &
     &      )   check_SGS_tensor_terms = .TRUE.
!
      end function check_SGS_tensor_terms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_SGS_scalar_term_addresses                          &
     &         (i_phys, field_name, SGS_term, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: SGS_term
      logical, intent(inout) :: flag
!
!
      flag = check_SGS_tensor_terms(field_name)
      if(flag) then
        if (field_name .eq. fhd_div_SGS_h_flux ) then
          SGS_term%i_SGS_div_h_flux =     i_phys
        else if (field_name .eq. fhd_div_SGS_c_flux ) then
          SGS_term%i_SGS_div_c_flux =    i_phys
        end if
      end if
!
      end subroutine set_SGS_scalar_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_vector_term_addresses                          &
     &         (i_phys, field_name, SGS_term, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: SGS_term
      logical, intent(inout) :: flag
!
!
      flag = check_SGS_vector_terms(field_name)
      if(flag) then
        if (field_name .eq. fhd_SGS_inertia) then
          SGS_term%i_SGS_inertia =   i_phys
        else if (field_name .eq. fhd_div_SGS_m_flux) then
          SGS_term%i_SGS_div_m_flux =    i_phys
        else if (field_name .eq. fhd_SGS_Lorentz) then
          SGS_term%i_SGS_Lorentz =    i_phys
        else if (field_name .eq. fhd_SGS_maxwell_t ) then
          SGS_term%i_SGS_maxwell =    i_phys
!
        else if (field_name .eq. fhd_SGS_buoyancy) then
          SGS_term%i_SGS_buoyancy =   i_phys
        else if (field_name .eq. fhd_SGS_comp_buo) then
          SGS_term%i_SGS_comp_buo =   i_phys
!
        else if (field_name .eq. fhd_SGS_vp_induct) then
          SGS_term%i_SGS_vp_induct =   i_phys
        else if (field_name .eq. fhd_SGS_induction) then
          SGS_term%i_SGS_induction =   i_phys
        else if (field_name .eq. fhd_SGS_induct_t ) then
          SGS_term%i_SGS_induct_t =    i_phys
!
        else if (field_name .eq. fhd_SGS_h_flux) then
          SGS_term%i_SGS_h_flux =    i_phys
        else if (field_name .eq. fhd_SGS_c_flux) then
          SGS_term%i_SGS_c_flux =    i_phys
        end if
      end if
!
      end subroutine set_SGS_vector_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_tensor_term_addresses                          &
     &         (i_phys, field_name, SGS_term, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: SGS_term
      logical, intent(inout) :: flag
!
!
      flag = check_SGS_tensor_terms(field_name)
      if(flag) then
        if (field_name .eq. fhd_SGS_m_flux ) then
          SGS_term%i_SGS_m_flux =     i_phys
        else if (field_name .eq. fhd_SGS_maxwell_t ) then
          SGS_term%i_SGS_maxwell =    i_phys
        end if
      end if
!
      end subroutine set_SGS_tensor_term_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SGG_scalar_monitor_address                             &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_SGS_term, ave_SGS_term, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(SGS_term_address), intent(inout) :: rms_SGS_term
      type(SGS_term_address), intent(inout) :: ave_SGS_term
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_SGS_scalar_term_addresses                                &
     &   ((numrms+1), field_name, rms_SGS_term, flag_r)
      call set_SGS_scalar_term_addresses                                &
     &   ((numave+1), field_name, ave_SGS_term, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine SGG_scalar_monitor_address
!
! ----------------------------------------------------------------------
!
      subroutine SGG_vector_monitor_address                             &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_SGS_term, ave_SGS_term, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(SGS_term_address), intent(inout) :: rms_SGS_term
      type(SGS_term_address), intent(inout) :: ave_SGS_term
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_SGS_vector_term_addresses                                &
     &   ((numrms+1), field_name, rms_SGS_term, flag_r)
      call set_SGS_vector_term_addresses                                &
     &   ((numave+1), field_name, ave_SGS_term, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine SGG_vector_monitor_address
!
! ----------------------------------------------------------------------
!
      subroutine SGG_tensor_monitor_address                             &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_SGS_term, ave_SGS_term, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(SGS_term_address), intent(inout) :: rms_SGS_term
      type(SGS_term_address), intent(inout) :: ave_SGS_term
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_SGS_tensor_term_addresses                                &
     &   ((numrms+1), field_name, rms_SGS_term, flag_r)
      call set_SGS_tensor_term_addresses                                &
     &   ((numave+1), field_name, ave_SGS_term, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine SGG_tensor_monitor_address
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
      subroutine set_SGS_term_names(field_names)
!
      character(len = kchara), intent(inout) :: field_names(nterms_SGS)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_SGS_inertia), CHAR(0)
      write(field_names( 2),'(a,a1)') trim(fhd_SGS_m_flux), CHAR(0)
      write(field_names( 3),'(a,a1)') trim(fhd_SGS_Lorentz), CHAR(0)
      write(field_names( 4),'(a,a1)') trim(fhd_SGS_maxwell_t), CHAR(0)
      write(field_names( 5),'(a,a1)') trim(fhd_SGS_buoyancy), CHAR(0)
      write(field_names( 6),'(a,a1)') trim(fhd_SGS_comp_buo), CHAR(0)
!
      write(field_names( 7),'(a,a1)') trim(fhd_SGS_vp_induct), CHAR(0)
      write(field_names( 8),'(a,a1)') trim(fhd_SGS_induct_t), CHAR(0)
!
      write(field_names( 9),'(a,a1)') trim(fhd_SGS_h_flux), CHAR(0)
      write(field_names(10),'(a,a1)') trim(fhd_SGS_c_flux), CHAR(0)
!
      write(field_names(11),'(a,a1)') trim(fhd_SGS_induction), CHAR(0)
!
      write(field_names(12),'(a,a1)') trim(fhd_div_SGS_m_flux), CHAR(0)
      write(field_names(12),'(a,a1)') trim(fhd_div_SGS_h_flux), CHAR(0)
      write(field_names(12),'(a,a1)') trim(fhd_div_SGS_c_flux), CHAR(0)
!
      end subroutine set_SGS_term_names
!
! ----------------------------------------------------------------------
!
      end module t_SGS_term_labels
