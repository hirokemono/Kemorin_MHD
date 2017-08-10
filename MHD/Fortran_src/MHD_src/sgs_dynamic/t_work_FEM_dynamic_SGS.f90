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
        type(dynamis_least_suare_data) :: wk_lsq
        type(dynamic_model_data) :: wk_sgs
        type(dynamic_model_data) :: wk_diff
!
        type(filtering_work_type) :: wk_filter
      end type work_FEM_dynamic_SGS
!
      end module t_work_FEM_dynamic_SGS