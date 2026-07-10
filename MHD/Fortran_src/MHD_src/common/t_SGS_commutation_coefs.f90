!>@file   t_SGS_commutation_coefs.f90
!!@brief  module t_SGS_commutation_coefs
!!
!!@author H. Matsui
!!@date    Programmed by H.Matsui in 200?
!!
!>@brief  Structures for commutatin error coefficients
!!
!!@verbatim
!!      subroutine set_sgs_diff_addresses(numele, SGS_param, cmt_param, &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop,                   &
!!     &          diff_coefs, num_diff_field, ntot_diff_comp)
!!        integer(kind = kint), intent(in) :: numele
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!!        integer(kind = kint), intent(inout) :: num_diff_field
!!        integer(kind = kint), intent(inout) :: ntot_diff_comp
!!      subroutine check_sgs_diff_addresses(id_file, wk_diff,           &
!!     &                                    diff_coefs)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(dynamic_model_data), intent(in) :: wk_diff
!!        type(SGS_commutation_coefs), intent(in) :: diff_coefs
!!@endverbatim
!
      module t_SGS_commutation_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_FEM_SGS_model_coefs
!
      implicit  none
!
      type SGS_commutation_coefs
!>       Structure for commutationa error coefficient for velocity
        type(SGS_model_coefficient) :: Cdiff_velo
!>       Structure for commutationa error coefficient for magnetic field
        type(SGS_model_coefficient) :: Cdiff_magne
!>       Structure for commutationa error coefficient for temperature
        type(SGS_model_coefficient) :: Cdiff_temp
!>       Structure for commutationa error coefficient for temperature
        type(SGS_model_coefficient) :: Cdiff_light
!
!>       Structure for commutationa error coefficient for SGS inducion
        type(SGS_model_coefficient) :: Cdiff_SGS_uxb
!>       Structure for commutationa error coefficient for SGS Lorenz force
        type(SGS_model_coefficient) :: Cdiff_SGS_lor
!>       Structure for commutationa error coefficient for SGS momentum flux
        type(SGS_model_coefficient) :: Cdiff_SGS_mf
!>       Structure for commutationa error coefficient for SGS heat flux
        type(SGS_model_coefficient) :: Cdiff_SGS_hf
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Cdiff_SGS_cf
      end type SGS_commutation_coefs
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_sgs_diff_addresses(numele, SGS_param, cmt_param,   &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          diff_coefs, num_diff_field, ntot_diff_comp)
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_base_field_labels
      use t_layering_ele_list
      use t_material_property
      use t_scalar_property
      use t_SGS_term_labels
!
      use m_base_field_labels
      use m_SGS_term_labels
!
      integer(kind = kint), intent(in) :: numele
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
!
      type(SGS_commutation_coefs), intent(inout) :: diff_coefs
      integer(kind = kint), intent(inout) :: num_diff_field
      integer(kind = kint), intent(inout) :: ntot_diff_comp
!
      integer(kind = kint) :: id, jd
!
!
      id = 0
      jd = 0
      if(      (ht_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none)     &
     &   .and. (SGS_param%SGS_heat%iflag_commute_flux                   &
     &                         .eq. id_SGS_commute_ON)) then
        call SGS_model_coef_address_by_label(SGS_heat_flux, jd, id,     &
     &                                       diff_coefs%Cdiff_SGS_hf)
      end if
      call alloc_SGS_model_coefficient(numele, diff_coefs%Cdiff_SGS_hf)
!
      if(      (fl_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none) &
     &   .and. (SGS_param%SGS_momentum%iflag_commute_flux               &
     &                             .eq. id_SGS_commute_ON)) then
        call SGS_model_coef_address_by_label(SGS_momentum_flux, jd, id, &
     &                                       diff_coefs%Cdiff_SGS_mf)
      end if
      call alloc_SGS_model_coefficient(numele, diff_coefs%Cdiff_SGS_mf)
!
      if(      (fl_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none)           &
     &   .and. (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON)) then
        call set_SGS_model_coef_address(SGS_Lorentz%name, n_sym_tensor, &
     &                                jd, id, diff_coefs%Cdiff_SGS_lor)
      end if
      call alloc_SGS_model_coefficient(numele,                          &
     &                                 diff_coefs%Cdiff_SGS_lor)
!
      if(      (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)         &
     &   .and. (SGS_param%iflag_SGS_uxb .ne. id_SGS_none)               &
     &   .and. (cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON)) then
        call SGS_model_coef_address_by_label(SGS_induction, jd, id,     &
     &                                       diff_coefs%Cdiff_SGS_uxb)
       end if
      call alloc_SGS_model_coefficient(numele,                          &
     &                                 diff_coefs%Cdiff_SGS_uxb)
!
      if(      (cp_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none)    &
     &   .and. (SGS_param%SGS_light%iflag_commute_flux                  &
     &                          .eq. id_SGS_commute_ON)) then
        call SGS_model_coef_address_by_label(SGS_composit_flux, jd, id, &
     &                                       diff_coefs%Cdiff_SGS_cf)
       end if
      call alloc_SGS_model_coefficient(numele, diff_coefs%Cdiff_SGS_cf)
!
!
      if(      (ht_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%iflag_SGS .ne. id_SGS_none)                   &
     &   .and. (SGS_param%SGS_heat%iflag_commute_field                  &
     &                          .eq. id_SGS_commute_ON)) then
        call SGS_model_coef_address_by_label(temperature, jd, id,       &
     &                                       diff_coefs%Cdiff_temp)
      end if
      call alloc_SGS_model_coefficient(numele, diff_coefs%Cdiff_temp)
!
      if(      (cp_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%iflag_SGS .ne. id_SGS_none)                   &
     &   .and. (SGS_param%SGS_light%iflag_commute_field                 &
     &                           .eq. id_SGS_commute_ON)) then
        call SGS_model_coef_address_by_label(composition, jd, id,       &
     &                                       diff_coefs%Cdiff_light)
      end if
      call alloc_SGS_model_coefficient(numele, diff_coefs%Cdiff_light)
!
      if(      (fl_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%iflag_SGS .ne. id_SGS_none)                   &
     &   .and. (SGS_param%SGS_momentum%iflag_commute_field              &
     &                              .eq. id_SGS_commute_ON)) then
        call SGS_model_coef_address_by_label(velocity, jd, id,          &
     &                                       diff_coefs%Cdiff_velo)
      end if
      call alloc_SGS_model_coefficient(numele, diff_coefs%Cdiff_velo)
!
      if(     (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)          &
     &   .or. (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)) then
        if(      (SGS_param%iflag_SGS .ne. id_SGS_none)                 &
     &     .and. (cmt_param%iflag_c_magne .eq. id_SGS_commute_ON)) then
          call SGS_model_coef_address_by_label(magnetic_field, jd, id,  &
     &                                         diff_coefs%Cdiff_magne)
        end if
      end if
      call alloc_SGS_model_coefficient(numele, diff_coefs%Cdiff_magne)
      num_diff_field = jd
      ntot_diff_comp = id
!
!
      end subroutine set_sgs_diff_addresses
!
!  ------------------------------------------------------------------
!
      subroutine check_sgs_diff_addresses(id_file, wk_diff,             &
     &                                    diff_coefs)
!
      use t_ele_info_4_dynamic
!
      integer(kind = kint), intent(in) :: id_file
      type(dynamic_model_data), intent(in) :: wk_diff
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
!
!
      write(id_file,'(a,i4)') 'diff_coefs%num_field', wk_diff%num_kinds
      write(id_file,'(a,i4)') 'wk_diff%ntot_comp', wk_diff%ntot_comp
!
      call check_SGS_model_coefficient(id_file,                         &
     &                                 diff_coefs%Cdiff_SGS_mf)
      call check_SGS_model_coefficient(id_file,                         &
     &                                 diff_coefs%Cdiff_SGS_hf)
      call check_SGS_model_coefficient(id_file,                         &
     &                                 diff_coefs%Cdiff_SGS_cf)
      call check_SGS_model_coefficient(id_file,                         &
     &                                 diff_coefs%Cdiff_SGS_lor)
      call check_SGS_model_coefficient(id_file,                         &
     &                                 diff_coefs%Cdiff_SGS_uxb)
      call check_SGS_model_coefficient(id_file, diff_coefs%Cdiff_velo)
      call check_SGS_model_coefficient(id_file, diff_coefs%Cdiff_temp)
      call check_SGS_model_coefficient(id_file, diff_coefs%Cdiff_light)
      call check_SGS_model_coefficient(id_file, diff_coefs%Cdiff_magne)
!
      end subroutine check_sgs_diff_addresses
!
! -------------------------------------------------------------------
!
      end module t_SGS_commutation_coefs
