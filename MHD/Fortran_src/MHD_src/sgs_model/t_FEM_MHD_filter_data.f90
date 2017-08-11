!t_FEM_MHD_filter_data.f90
!      module t_FEM_MHD_filter_data
!
!     Written by H. Matsui on Nov., 2008
!
      module t_FEM_MHD_filter_data
!
      use m_precision
      use t_layering_ele_list
      use t_filter_elength
      use t_filtering_data
!
      implicit none
!
!
      type filters_on_FEM
!>        Structure of grouping of elements
        type(layering_tbl) :: layer_tbl
!
!>        Structure of element size for nonlinear gradient model
        type(gradient_model_data_type) :: FEM_elens
!
!>        Weights for filtering
        type(filtering_data_type) :: filtering
!>        Weights for wide filtering
        type(filtering_data_type) :: wide_filtering
      end type filters_on_FEM
!
      end module t_FEM_MHD_filter_data
