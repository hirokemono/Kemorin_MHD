!>@file   m_wide_SGS_term_labels.f90
!!        module m_wide_SGS_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms using wide filters
!!
!!@verbatim
!!      logical function check_wide_SGS_vector_terms(field_name)
!!      logical function check_double_SGS_vector_terms(field_name)
!!      subroutine set_wide_SGS_term_addresses                          &
!!     &         (i_phys, field_name, wide_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: wide_SGS
!!      subroutine set_double_SGS_term_addresses                        &
!!     &         (i_phys, field_name, dble_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: dble_SGS
!!
!!      integer(kind = kint) function num_wide_SGS_terms()
!!      subroutine set_wide_SGS_term_labels(n_comps, names, maths)
!!
!! !!!!!  SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!     SGS terms using wider filter
!!   wide_SGS_inertia           [wide_SGS%i_SGS_inertia]
!!   wide_SGS_Lorentz           [wide_SGS%i_SGS_Lorentz]
!!
!!   wide_SGS_vp_induction      [wide_SGS%i_SGS_induction]
!!
!!   wide_SGS_heat_flux         [wide_SGS%i_SGS_h_flux]
!!   wide_SGS_composit_flux     [wide_SGS%i_SGS_c_flux]
!!
!!     SGS terms using doulbe filter
!!   double_SGS_inertia         [dble_SGS%i_SGS_inertia]
!!   double_SGS_Lorentz         [dble_SGS%i_SGS_Lorentz]
!!
!!   double_SGS_vp_induction    [dble_SGS%i_SGS_induction]
!!
!!   double_SGS_heat_flux       [dble_SGS%i_SGS_h_flux]
!!   double_SGS_composit_flux   [dble_SGS%i_SGS_c_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_wide_SGS_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_SGS_term_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nterms_wide_SGS = 10
!
!      SGS terms by wider filter
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      type(field_def), parameter :: wide_SGS_heat_flux                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_heat_flux',                      &
     &                math = '$ \overline{u_{i}T}'                      &
     &                     // ' - \bar{u}_{i}\bar{T} $')
!>        Field label for SGS composition flux with wider filter
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      type(field_def), parameter :: wide_SGS_composit_flux              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_composit_flux',                  &
     &                math = '$ \overline{u_{i}C}'                      &
     &                     // ' - \bar{u}_{i}\bar{C} $')
!
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      type(field_def), parameter :: wide_SGS_inertia                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_inertia',                        &
     &                math = '$ e_{ijk} (\overline{\omega_{j}u_{k}}'    &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk}\left(\overline{\J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      type(field_def), parameter :: wide_SGS_Lorentz                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_Lorentz',                        &
     &                math = '$ e_{ijk} (\overline{J_{j}B_{k}}'         &
     &                     // ' - \bar{J}_{j}\bar{B}_{k}) $')
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk}\left(\overline{\u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      type(field_def), parameter :: wide_SGS_vp_induction               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_vp_induction',                   &
     &                math = '$ e_{ijk} (\overline{u_{j}B_{k}}'         &
     &                     // ' - \bar{u}_{j}\bar{B}_{k}) $')
!
!      SGS terms by double filtering
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \widetilde{\widetilde{u_{i}T}}
!!            - \tilde{\tilde{u}}_{i}\tilde{\tilde{T}} @f$
      type(field_def), parameter :: double_SGS_heat_flux                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_heat_flux',                    &
     &       math = '$ \widetilde{\widetilde{u_{i}T}}'                  &
     &           // ' - \tilde{\tilde{u}}_{i}\tilde{\tilde{T}} $')
!>        Field label for SGS composition flux with wider filter
!!         @f$ \widetilde{\widetilde{u_{i}C}}
!!            - \tilde{\tilde{u}}_{i}\tilde{\tilde{C}} @f$
      type(field_def), parameter :: double_SGS_composit_flux            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_composit_flux',                &
     &       math = '$ \widetilde{\widetilde{u_{i}C}}'                  &
     &           // ' - \tilde{\tilde{u}}_{i}\tilde{\tilde{C}} $')
!
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk}\left(\widetilde{\widetilde{\omega_{j}u_{k}}}
!!          - \tilde{\tilde{\omega}}_{j}\tilde{\tilde{u}}_{k} \right) @f$
      type(field_def), parameter :: double_SGS_inertia                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_inertia',                      &
     &     math = '$ e_{ijk} (\widetilde{\widetilde{\omega_{j}u_{k}}}'  &
     &      // ' - \tilde{\tilde{\omega}}_{j}\tilde{\tilde{u}}_{k}) $')
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk}\left(\widetilde{\widetilde{\J_{j}B_{k}}}
!!            - \tilde{\tilde{J}}_{j}\tilde{\tilde{B}}_{k} \right) @f$
      type(field_def), parameter :: double_SGS_Lorentz                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_Lorentz',                      &
     &       math = '$ e_{ijk} (\widetilde{\widetilde{J_{j}B_{k}}}'     &
     &           // ' - \tilde{\tilde{J}}_{j}\tilde{\tilde{B}}_{k}) $')
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk}\left(\widetilde{\widetilde{\u_{j}B_{k}}}
!!            - \tilde{\tilde{u}}_{j}\tilde{\tilde{B}}_{k} \right) @f$
      type(field_def), parameter :: double_SGS_vp_induction             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_vp_induction',                 &
     &       math = '$ e_{ijk} (\widetilde{\widetilde{u_{j}B_{k}}}'     &
     &           // ' - \tilde{\tilde{u}}_{j}\tilde{\tilde{B}}_{k}) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_wide_SGS_vector_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_wide_SGS_vector_terms                                       &
     &   =    (field_name .eq. wide_SGS_inertia%name)                   &
     &   .or. (field_name .eq. wide_SGS_Lorentz%name)                   &
     &   .or. (field_name .eq. wide_SGS_vp_induction%name)              &
     &   .or. (field_name .eq. wide_SGS_heat_flux%name)                 &
     &   .or. (field_name .eq. wide_SGS_composit_flux%name)
!
      end function check_wide_SGS_vector_terms
!
! ----------------------------------------------------------------------
!
      logical function check_double_SGS_vector_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_double_SGS_vector_terms                                     &
     &   =    (field_name .eq. double_SGS_inertia%name)                 &
     &   .or. (field_name .eq. double_SGS_Lorentz%name)                 &
     &   .or. (field_name .eq. double_SGS_vp_induction%name)            &
     &   .or. (field_name .eq. double_SGS_heat_flux%name)               &
     &   .or. (field_name .eq. double_SGS_composit_flux%name)
!
      end function check_double_SGS_vector_terms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_wide_SGS_term_addresses                            &
     &         (i_phys, field_name, wide_SGS, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: wide_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_wide_SGS_vector_terms(field_name)
      if(flag) then
        if (field_name .eq. wide_SGS_heat_flux%name) then
          wide_SGS%i_SGS_h_flux =    i_phys
        else if (field_name .eq. wide_SGS_composit_flux%name) then
          wide_SGS%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. wide_SGS_inertia%name) then
          wide_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. wide_SGS_Lorentz%name) then
          wide_SGS%i_SGS_Lorentz =    i_phys
!
        else if (field_name .eq. wide_SGS_vp_induction%name) then
          wide_SGS%i_SGS_induction =   i_phys
        end if
      end if
!
      end subroutine set_wide_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_double_SGS_term_addresses                          &
     &         (i_phys, field_name, dble_SGS, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: dble_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_double_SGS_vector_terms(field_name)
      if(flag) then
        if (field_name .eq. double_SGS_heat_flux%name) then
          dble_SGS%i_SGS_h_flux =    i_phys
        else if (field_name .eq. double_SGS_composit_flux%name) then
          dble_SGS%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. double_SGS_inertia%name) then
          dble_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. double_SGS_Lorentz%name) then
          dble_SGS%i_SGS_Lorentz =    i_phys
!
        else if (field_name .eq. double_SGS_vp_induction%name) then
          dble_SGS%i_SGS_induction =   i_phys
        end if
      end if
!
      end subroutine set_double_SGS_term_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_wide_SGS_terms()
      num_wide_SGS_terms = nterms_wide_SGS
      return
      end function num_wide_SGS_terms
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_SGS_term_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(nterms_wide_SGS)
      character(len = kchara), intent(inout) :: names(nterms_wide_SGS)
      character(len = kchara), intent(inout) :: maths(nterms_wide_SGS)
!
!
      call set_field_labels(wide_SGS_heat_flux,                         &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(wide_SGS_composit_flux,                     &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(wide_SGS_inertia,                           &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(wide_SGS_Lorentz,                           &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(wide_SGS_vp_induction,                      &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(double_SGS_heat_flux,                       &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(double_SGS_composit_flux,                   &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(double_SGS_inertia,                         &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(double_SGS_Lorentz,                         &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(double_SGS_vp_induction,                    &
     &    n_comps(10), names(10), maths(10))
!
      end subroutine set_wide_SGS_term_labels
!
! ----------------------------------------------------------------------
!
      end module m_wide_SGS_term_labels
