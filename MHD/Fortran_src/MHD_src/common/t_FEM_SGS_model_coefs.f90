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
!
      implicit  none
!
!
!>      Structure of model coefficieints for FEM MHD
      type SGS_coefficients_data
!>        Model coefficeints in elements
        type(SGS_coefficients_type) :: sgs_coefs
!
!>        Model coefficeints at nodes
        type(SGS_coefficients_type) :: sgs_coefs_nod
!
!>        Commutation error model coefficeints
        type(SGS_coefficients_type) :: diff_coefs
!
!>        field list of SGS terms
        type(SGS_terms_address) :: ifld_sgs
!
!>        component list of SGS terms
        type(SGS_terms_address) :: icomp_sgs
!
!>        field list of differencials
        type(SGS_terms_address) :: ifld_diff
!
!>        component list of differencials
        type(SGS_terms_address) :: icomp_diff
!
!>        field list of element differencials
        type(SGS_terms_address) :: iphys_elediff
      end type SGS_coefficients_data
!
      end module t_FEM_SGS_model_coefs
