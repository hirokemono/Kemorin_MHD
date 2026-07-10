!>@file   init_sgs_diff_coefs.f90
!!        module init_sgs_diff_coefs
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief initialize model coefficients for commutation
!!
!!@verbatim
!!      subroutine def_sgs_commute_component(SGS_par, mesh, layer_tbl,  &
!!     &          MHD_prop, sgs_coefs, diff_coefs, FEM_SGS_wk)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!@end verbatim
!
      module init_sgs_diff_coefs
!
      use m_precision
      use m_machine_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_base_field_labels
      use t_SGS_term_labels
      use t_FEM_SGS_model_coefs
      use t_SGS_model_coef_strucures
      use t_SGS_commutation_coefs
!
      implicit none
!
      private :: copy_sgs_diff_coef_name
      private :: define_sgs_diff_coefs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine def_sgs_commute_component(SGS_par, mesh, layer_tbl,    &
     &          MHD_prop, sgs_coefs, diff_coefs, FEM_SGS_wk)
!
      use t_mesh_data
      use t_layering_ele_list
      use t_work_FEM_dynamic_SGS
      use count_sgs_components
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_commutation_coefs), intent(inout) :: diff_coefs
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
!
      call define_sgs_components(mesh%node%numnod, mesh%ele%numele,     &
     &    SGS_par%model_p, layer_tbl, MHD_prop, FEM_SGS_wk%wk_sgs,      &
     &    sgs_coefs)
      call define_sgs_diff_coefs(mesh%ele%numele,                       &
     &    SGS_par%model_p, SGS_par%commute_p, layer_tbl, MHD_prop,      &
     &    FEM_SGS_wk%wk_diff, diff_coefs)
!
      end subroutine def_sgs_commute_component
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine define_sgs_diff_coefs(numele, SGS_param, cmt_param,    &
     &          layer_tbl, MHD_prop, wk_diff, diff_coefs)
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_material_property
      use t_FEM_SGS_model_coefs
!
      integer(kind = kint), intent(in) :: numele
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: num_diff_field, ntot_diff_comp
!
!
      call set_sgs_diff_addresses(numele, SGS_param, cmt_param,         &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, diff_coefs,               &
     &    num_diff_field, ntot_diff_comp)
!
      call alloc_sgs_coefs_layer(layer_tbl%e_grp%num_grp,               &
     &    num_diff_field, ntot_diff_comp, wk_diff)
      call copy_sgs_diff_coef_name(diff_coefs, wk_diff)
!
      if(iflag_debug .gt. 0) then
        call check_sgs_diff_addresses(6, wk_diff, diff_coefs)
      end if
!
      end subroutine define_sgs_diff_coefs
!
!  ------------------------------------------------------------------
!
      subroutine copy_sgs_diff_coef_name(diff_coefs, wk_diff)
!
      use t_ele_info_4_dynamic
      use t_SGS_term_labels
!
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
      type(dynamic_model_data), intent(inout) :: wk_diff
!
!
      if(diff_coefs%Cdiff_SGS_hf%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_SGS_hf%iak_Csim)                  &
     &                             = diff_coefs%Cdiff_SGS_hf%term_name
      end if
!
      if(diff_coefs%Cdiff_SGS_mf%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_SGS_mf%iak_Csim)                  &
     &                             = diff_coefs%Cdiff_SGS_mf%term_name
      end if
!
      if(diff_coefs%Cdiff_SGS_lor%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_SGS_lor%iak_Csim)                 &
     &                             = diff_coefs%Cdiff_SGS_lor%term_name
      end if
!
      if(diff_coefs%Cdiff_SGS_uxb%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_SGS_uxb%iak_Csim)                 &
     &                             = diff_coefs%Cdiff_SGS_uxb%term_name
      end if
!
      if(diff_coefs%Cdiff_SGS_cf%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_SGS_cf%iak_Csim)                  &
     &                              = diff_coefs%Cdiff_SGS_cf%term_name
      end if
!
!
      if(diff_coefs%Cdiff_temp%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_temp%iak_Csim)                    &
     &                              = diff_coefs%Cdiff_temp%term_name
      end if
!
      if(diff_coefs%Cdiff_light%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_light%iak_Csim)                   &
     &                              = diff_coefs%Cdiff_light%term_name
      end if
!
      if(diff_coefs%Cdiff_velo%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_velo%iak_Csim)                    &
     &                              = diff_coefs%Cdiff_velo%term_name
      end if
!
      if(diff_coefs%Cdiff_magne%iak_Csim .gt. 0) then
        wk_diff%name(diff_coefs%Cdiff_magne%iak_Csim)                   &
     &                              = diff_coefs%Cdiff_magne%term_name
      end if
!
      end subroutine copy_sgs_diff_coef_name
!
!  ------------------------------------------------------------------
!
      end module init_sgs_diff_coefs
