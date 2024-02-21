!t_FEM_SGS_model_coefs.f90
!     module t_FEM_SGS_model_coefs
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!       subroutine allocate_model_coefs(numele)
!       subroutine allocate_nod_model_coefs(numnod)
!
!       subroutine deallocate_model_coefs
!       subroutine deallocate_nod_model_coefs
!
      module t_FEM_SGS_model_coefs
!
      use m_precision
      use t_SGS_model_coefs
      use t_base_field_labels
!
      implicit  none
!
!
!>      Structure of model coefficieints for FEM MHD
      type SGS_coefficients_data
!>        Model coefficeints in elements
        type(SGS_coefficients_type) :: sgs_coefs
!>        Commutation error model coefficeints
        type(SGS_commutation_coefs) :: diff_coefs
      end type SGS_coefficients_data
!
      end module t_FEM_SGS_model_coefs
