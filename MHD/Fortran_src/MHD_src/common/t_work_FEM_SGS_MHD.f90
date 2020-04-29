!>@file  t_work_FEM_SGS_MHD.f90
!!       module t_work_FEM_SGS_MHD
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for Dynamic SGS model in FEM dynamo
!!
!!@verbatim
!!@endverbatim
      module t_work_FEM_SGS_MHD
!
      use m_precision
      use m_constants
!
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
!
      implicit  none
!
!
      type work_FEM_SGS_MHD
!>       Structure for field data on element
        type(phys_data) :: ele_fld
!>       Address of element fields
        type(phys_address) :: iphys_ele
!>       Address of filtered element fields
        type(base_field_address) :: iphys_fil_ele
!
!>       Structure of mass matrices for FEM_MHD
        type(lumped_mass_mat_layerd) :: mk_MHD
!>       Stracture for FEM assembling
        type(finite_element_integration) :: fem_int
!
!>        Stracture for FEM assembling
        type(arrays_finite_element_mat) :: rhs_mat
!>        Work array for FEM assemble in MHD model
        type(work_MHD_fe_mat) :: mhd_fem_wk
!>        Work area for SGS model
        type(work_FEM_dynamic_SGS) :: FEM_SGS_wk
      end type work_FEM_SGS_MHD
!
      end module t_work_FEM_SGS_MHD
