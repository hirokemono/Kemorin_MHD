!>@file   count_sgs_components.f90
!!@brief  module count_sgs_components
!!
!!@author H. Matsui
!!@date    Programmed by H.Matsui in 2004
!!         Modified in July, 2007
!!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine define_sgs_components                                &
!!     &         (numnod, numele, SGS_param, layer_tbl, MHD_prop,       &
!!     &          wk_sgs, sgs_coefs)
!!
!!      subroutine set_sgs_addresses                                    &
!!     &          (SGS_param, fl_prop, cd_prop, ht_prop, cp_prop,       &
!!     &           wk_sgs, sgs_coefs)
!!      subroutine set_SGS_ele_fld_addresses(cd_prop, SGS_param,        &
!!     &                                     mhd_fem_wk)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!@endverbatim
!
      module count_sgs_components
!
      use m_precision
      use m_machine_parameter
      use t_FEM_SGS_model_coefs
      use t_SGS_model_coef_strucures
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine define_sgs_components                                  &
     &         (numnod, numele, SGS_param, layer_tbl, MHD_prop,         &
     &          wk_sgs, sgs_coefs)
!
      use calypso_mpi
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_physical_property
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
      integer(kind = kint) :: num_SGS_terms, ntot_SGS_comps
!
!
!   set index for model coefficients
      call set_sgs_addresses(numnod, numele, SGS_param,                 &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, sgs_coefs,                &
     &    num_SGS_terms, ntot_SGS_comps)
!
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    num_SGS_terms, ntot_SGS_comps, wk_sgs)
      call copy_sgs_model_coef_name(sgs_coefs, wk_sgs)
      if(iflag_debug .gt. 0) call check_sgs_addresses(6, wk_sgs,        &
     &                                                sgs_coefs)
!
      end subroutine define_sgs_components
!
!  ------------------------------------------------------------------
!
      subroutine copy_sgs_model_coef_name(sgs_coefs, wk_sgs)
!
      use t_ele_info_4_dynamic
!
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(dynamic_model_data), intent(inout) :: wk_sgs
!
!
      if(sgs_coefs%Csim_SGS_hf%iak_Csim .gt. 0) then
        wk_sgs%name(sgs_coefs%Csim_SGS_hf%iak_Csim)                     &
     &                       = sgs_coefs%Csim_SGS_hf%term_name
      end if
!
      if(sgs_coefs%Csim_SGS_mf%iak_Csim .gt. 0) then
        wk_sgs%name(sgs_coefs%Csim_SGS_mf%iak_Csim)                     &
     &                       = sgs_coefs%Csim_SGS_mf%term_name
      end if
!
      if(sgs_coefs%Csim_SGS_lor%iak_Csim .gt. 0) then
        wk_sgs%name(sgs_coefs%Csim_SGS_lor%iak_Csim)                    &
     &                       = sgs_coefs%Csim_SGS_lor%term_name
      end if
!
      if(sgs_coefs%Csim_SGS_tbuo%iak_Csim .gt. 0) then
        wk_sgs%name(sgs_coefs%Csim_SGS_tbuo%iak_Csim)                   &
     &                       = sgs_coefs%Csim_SGS_tbuo%term_name
      end if
!
      if(sgs_coefs%Csim_SGS_cbuo%iak_Csim .gt. 0) then
        wk_sgs%name(sgs_coefs%Csim_SGS_cbuo%iak_Csim)                   &
     &                       = sgs_coefs%Csim_SGS_cbuo%term_name
      end if
!
      if(sgs_coefs%Csim_SGS_uxb%iak_Csim .gt. 0) then
        wk_sgs%name(sgs_coefs%Csim_SGS_uxb%iak_Csim)                    &
     &                       = sgs_coefs%Csim_SGS_uxb%term_name
      end if
!
      if(sgs_coefs%Csim_SGS_cf%iak_Csim .gt. 0) then
        wk_sgs%name(sgs_coefs%Csim_SGS_cf%iak_Csim)                     &
     &                       = sgs_coefs%Csim_SGS_cf%term_name
      end if
!
      end subroutine copy_sgs_model_coef_name
!
!  ------------------------------------------------------------------
!
      subroutine set_SGS_ele_fld_addresses(cd_prop, SGS_param,          &
     &                                     mhd_fem_wk)
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_MHD_finite_element_mat
!
      type(conductive_property), intent(in) :: cd_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: ifil_elediff_v,  ifil_elediff_b
      integer(kind = kint) :: iphys_elediff_v, iphys_elediff_b
      integer(kind = kint) :: i
!
      ifil_elediff_v = 0
      ifil_elediff_b = 0
      iphys_elediff_v = 0
      iphys_elediff_b = 0
!
      i = 1
      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne.   id_SGS_none      &
     &   .or. SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none    &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne.   id_SGS_none     &
     &   .or. SGS_param%iflag_SGS_uxb .ne. id_SGS_none ) then
         iphys_elediff_v = i
         ifil_elediff_v = i + 9
         i = i + 18
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff_b = i
         ifil_elediff_b = i + 9
         i = i + 18
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         iphys_elediff_b = i
         ifil_elediff_b = i + 9
         i = i + 18
        end if
!
      else if (SGS_param%iflag_SGS .ne. id_SGS_none                     &
     &   .and. SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none        &
     &   .or. SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none    &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none       &
     &   .or. SGS_param%iflag_SGS_uxb .ne.    id_SGS_none) then
         iphys_elediff_v = i
         i = i + 9
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         iphys_elediff_b = i
         i = i + 9
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         iphys_elediff_b = i
         i = i + 9
        end if
      end if
!
      mhd_fem_wk%ifil_elediff_v =  ifil_elediff_v
      mhd_fem_wk%ifil_elediff_b =  ifil_elediff_b
      mhd_fem_wk%iphys_elediff_v = iphys_elediff_v
      mhd_fem_wk%iphys_elediff_b = iphys_elediff_b
!
      end subroutine set_SGS_ele_fld_addresses
!
!  ------------------------------------------------------------------
!
      end module count_sgs_components
      
