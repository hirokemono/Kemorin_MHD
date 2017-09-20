!>@file   t_FEM_SGS_structure.f90
!!@brief  module t_FEM_SGS_structure
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief Structure for SGS model in FEM_MHD
!!
!!@verbatim
!!@endverbatim
!
      module t_FEM_SGS_structure
!
      use m_precision
      use t_SGS_control_parameter
      use t_FEM_SGS_model_coefs
      use t_FEM_MHD_filter_data
!
      implicit none
!
!
!>      Structures of SGS model in FEM dynamo
      type FEM_SGS_structure
!>        Structure of input parameters for SGS model
        type(SGS_paremeters) :: SGS_par
!
!>        Structure of model coefficieints for FEM MHD
        type(SGS_coefficients_data) :: Csims
!
!>        Structure of grouping of elements
        type(filters_on_FEM) :: FEM_filters
      end type FEM_SGS_structure
!
      end module t_FEM_SGS_structure
