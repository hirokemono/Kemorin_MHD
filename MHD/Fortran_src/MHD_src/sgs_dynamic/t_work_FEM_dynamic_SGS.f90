!>@file  t_work_FEM_dynamic_SGS.f90
!!       module t_work_FEM_dynamic_SGS
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for Dynamic SGS model in FEM dynamo
!!
!!@verbatim
!!@endverbatim
      module t_work_FEM_dynamic_SGS
!
      use m_precision
      use m_constants
!
      use t_work_layer_correlate
      use t_work_4_dynamic_model
      use t_ele_info_4_dynamic
      use t_filtering_data
!
      implicit  none
!
!
      type work_FEM_dynamic_SGS
        type(dynamic_correlation_data) :: wk_cor
        type(dynamic_least_suare_data) :: wk_lsq
        type(dynamic_model_data) :: wk_sgs
        type(dynamic_model_data) :: wk_diff
!
        type(filtering_work_type) :: wk_filter
      end type work_FEM_dynamic_SGS
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_FEM_dynamic(layer_tbl, FEM_SGS_wk)
!
      use t_layering_ele_list
!
      type(layering_tbl), intent(in) :: layer_tbl
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
!
      call alloc_work_4_dynamic                                         &
     &     (layer_tbl%e_grp%num_grp, FEM_SGS_wk%wk_lsq)
      call alloc_work_layer_correlate                                   &
     &     (layer_tbl%e_grp%num_grp, inine, FEM_SGS_wk%wk_cor)
!
      end subroutine alloc_work_FEM_dynamic
!
! ----------------------------------------------------------------------
!
      subroutine def_sgs_commute_component(SGS_par, mesh, layer_tbl,    &
     &          MHD_prop, Csims_FEM_MHD, FEM_SGS_wk)
!
      use t_mesh_data
      use t_control_parameter
      use t_SGS_control_parameter
      use t_layering_ele_list
      use t_FEM_SGS_model_coefs
      use count_sgs_components
      use init_sgs_diff_coefs
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(layering_tbl), intent(in) :: layer_tbl
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!
!
      call define_sgs_components                                        &
     &   (mesh%node%numnod, mesh%ele%numele,                            &
     &    SGS_par%model_p, layer_tbl, MHD_prop, Csims_FEM_MHD%ifld_sgs, &
     &    Csims_FEM_MHD%icomp_sgs, FEM_SGS_wk%wk_sgs,                   &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod)
      call define_sgs_diff_coefs(mesh%ele%numele,                       &
     &    SGS_par%model_p, SGS_par%commute_p, layer_tbl, MHD_prop,      &
     &    Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%icomp_diff,            &
     &    FEM_SGS_wk%wk_diff, Csims_FEM_MHD%diff_coefs)
!
      end subroutine def_sgs_commute_component
!
! ----------------------------------------------------------------------
!
      end module t_work_FEM_dynamic_SGS