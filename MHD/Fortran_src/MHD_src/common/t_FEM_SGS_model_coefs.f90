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
      use t_SGS_term_labels
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
!>        Field adddress for dynamic SGS model
        type(SGS_term_address) :: iak_sgs_term
!>        Component list of SGS terms
        type(SGS_term_address) :: icomp_sgs_term
!
!>        Address of differenciation of elemental field
        type(base_field_address) :: iphys_elediff_vec
!>        Address of differenciation of filtered elemental field
        type(base_field_address) :: iphys_elediff_fil
      end type SGS_coefficients_data
!
      end module t_FEM_SGS_model_coefs
