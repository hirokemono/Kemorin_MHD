!>@file   t_SGS_model_coef_strucures.f90
!!@brief  module t_SGS_model_coef_strucures
!!
!!@author H. Matsui
!!@date    Programmed by H.Matsui in 200?
!!
!>@brief  Structures for model coefficients for FEM_SGS_MHD
!!
!!@verbatim
!!      subroutine SGS_model_coef_address_by_label                      &
!!     &         (term, i_field, i_comp, Csim)
!!      subroutine set_SGS_model_coef_address(term_name, n_comp,        &
!!     &                                      i_field, i_comp, Csim)
!!        type(field_def), intent(in) :: term
!!        character(len = kchara), intent(in) :: term_name
!!        integer(kind = kint), intent(in) :: n_comp
!!        integer(kind = kint), intent(inout) :: i_field, i_comp
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!
!!      subroutine dup_SGS_model_coefficient(org_Csim, new_Csim)
!!        type(SGS_model_coefficient), intent(in) :: org_Csim
!!        type(SGS_model_coefficient), intent(inout) :: new_Csim
!!      subroutine alloc_SGS_model_coefficient(n_ele, Csim)
!!      subroutine alloc_SGS_model_coef_on_nod(n_nod, Csim)
!!      subroutine dealloc_SGS_model_coefficient(Csim)
!!      subroutine dealloc_SGS_model_coef_on_nod(Csim)
!!        integer(kind = kint), intent(in) :: n_ele
!!        integer(kind = kint), intent(in) :: n_nod
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!
!!      subroutine check_sgs_addresses(id_file, wk_sgs, sgs_coefs)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!@endverbatim
!
      module t_SGS_model_coef_strucures
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_FEM_SGS_model_coefs
!
      implicit  none
!
      type SGS_coefficients_type
!>       Structure for commutationa error coefficient for SGS inducion
        type(SGS_model_coefficient) :: Csim_SGS_uxb
!>       Structure for commutationa error coefficient for SGS Lorenz force
        type(SGS_model_coefficient) :: Csim_SGS_lor
!>       Structure for commutationa error coefficient for SGS momentum flux
        type(SGS_model_coefficient) :: Csim_SGS_mf
!>       Structure for commutationa error coefficient for SGS heat flux
        type(SGS_model_coefficient) :: Csim_SGS_hf
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Csim_SGS_cf
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Csim_SGS_tbuo
!>       Structure for commutationa error coefficient for SGS composition flux
        type(SGS_model_coefficient) :: Csim_SGS_cbuo
      end type SGS_coefficients_type
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine check_sgs_addresses(id_file, wk_sgs, sgs_coefs)
!
      use t_ele_info_4_dynamic
!
      integer(kind = kint), intent(in) :: id_file
      type(dynamic_model_data), intent(in) :: wk_sgs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
!
      write(id_file,'(a,i4)') 'num_sgs_kinds', wk_sgs%num_kinds
      write(id_file,'(a,i4)') 'num_sgs_coefs', wk_sgs%ntot_comp
!
      call check_SGS_model_coefficient(id_file,sgs_coefs%Csim_SGS_mf)
      call check_SGS_model_coefficient(id_file,sgs_coefs%Csim_SGS_hf)
      call check_SGS_model_coefficient(id_file,sgs_coefs%Csim_SGS_cf)
      call check_SGS_model_coefficient(id_file,sgs_coefs%Csim_SGS_lor)
      call check_SGS_model_coefficient(id_file,sgs_coefs%Csim_SGS_tbuo)
      call check_SGS_model_coefficient(id_file,sgs_coefs%Csim_SGS_cbuo)
      call check_SGS_model_coefficient(id_file,sgs_coefs%Csim_SGS_uxb)
!
      end subroutine check_sgs_addresses
!
! -------------------------------------------------------------------
!
      subroutine set_sgs_addresses(numnod, numele, SGS_param,           &
     &          fl_prop, cd_prop, ht_prop, cp_prop, sgs_coefs,          &
     &          num_SGS_terms, ntot_SGS_comps)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
      use t_scalar_property
      use t_SGS_term_labels
!
      use m_SGS_term_labels
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      integer(kind = kint), intent(inout) :: num_SGS_terms
      integer(kind = kint), intent(inout) :: ntot_SGS_comps
!
      integer(kind = kint) :: i_cmp, i_fld, num_comp
!
!
      i_cmp = 0
      i_fld = 0
      if(      (ht_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%SGS_heat%iflag_SGS_flux                       &
     &                           .ne. id_SGS_none)) then
        call SGS_model_coef_address_by_label(SGS_heat_flux,             &
     &      i_fld, i_cmp, sgs_coefs%Csim_SGS_hf)
      end if
      call alloc_SGS_model_coefficient(numele, sgs_coefs%Csim_SGS_hf)
!
      if(      (fl_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%SGS_momentum%iflag_SGS_flux                   &
     &                               .ne. id_SGS_none)) then
        call SGS_model_coef_address_by_label(SGS_momentum_flux,         &
     &      i_fld, i_cmp, sgs_coefs%Csim_SGS_mf)
      end if
      call alloc_SGS_model_coefficient(numele, sgs_coefs%Csim_SGS_mf)
!
      if(      (fl_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%iflag_SGS_lorentz .ne. id_SGS_none)) then
        call SGS_model_coef_address_by_label(SGS_maxwell_tensor,        &
     &      i_fld, i_cmp, sgs_coefs%Csim_SGS_lor)
      end if
      call alloc_SGS_model_coefficient(numele, sgs_coefs%Csim_SGS_lor)
!
      if(      (fl_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%iflag_SGS_gravity .ne. id_SGS_none)           &
     &   .and. (fl_prop%flag_thermal_buoyancy)) then
        call set_SGS_model_coef_address(SGS_buoyancy%name,              &
     &      n_sym_tensor, i_fld, i_cmp, sgs_coefs%Csim_SGS_tbuo)
      end if
      call alloc_SGS_model_coefficient(numele, sgs_coefs%Csim_SGS_tbuo)
!
      if(      (fl_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%iflag_SGS_gravity .ne. id_SGS_none)           &
     &   .and. (fl_prop%flag_comp_buoyancy)) then
        call set_SGS_model_coef_address(SGS_composit_buoyancy%name,     &
     &      n_sym_tensor, i_fld, i_cmp, sgs_coefs%Csim_SGS_cbuo)
      end if
      call alloc_SGS_model_coefficient(numele, sgs_coefs%Csim_SGS_cbuo)
!
      if(     (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)          &
     &   .or. (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)) then
        if(SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          call SGS_model_coef_address_by_label(SGS_induction,           &
     &        i_fld, i_cmp, sgs_coefs%Csim_SGS_uxb)
        end if
      end if
      call alloc_SGS_model_coefficient(numele, sgs_coefs%Csim_SGS_uxb)
!
      if(      (cp_prop%iflag_scheme .gt. id_no_evolution)              &
     &   .and. (SGS_param%SGS_light%iflag_SGS_flux                      &
     &                            .ne. id_SGS_none)) then
        call SGS_model_coef_address_by_label(SGS_composit_flux,         &
     &     i_fld, i_cmp, sgs_coefs%Csim_SGS_cf)
      end if
      call alloc_SGS_model_coefficient(numele, sgs_coefs%Csim_SGS_cf)
!
      if(     SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF           &
     &   .or. SGS_param%iflag_SGS.eq.id_SGS_similarity)  then
        call alloc_SGS_model_coef_on_nod(numnod, sgs_coefs%Csim_SGS_hf)
        call alloc_SGS_model_coef_on_nod(numnod, sgs_coefs%Csim_SGS_cf)
        call alloc_SGS_model_coef_on_nod(numnod, sgs_coefs%Csim_SGS_mf)
        call alloc_SGS_model_coef_on_nod(numnod,sgs_coefs%Csim_SGS_lor)
        call alloc_SGS_model_coef_on_nod(numnod,sgs_coefs%Csim_SGS_uxb)
      end if
      ntot_SGS_comps = i_cmp
      num_SGS_terms =  i_fld
!
      end subroutine set_sgs_addresses
!
!  ------------------------------------------------------------------
!
      end module t_SGS_model_coef_strucures
