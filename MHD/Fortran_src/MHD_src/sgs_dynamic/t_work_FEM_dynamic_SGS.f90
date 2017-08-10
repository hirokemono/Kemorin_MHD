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
      end module t_work_FEM_dynamic_SGS