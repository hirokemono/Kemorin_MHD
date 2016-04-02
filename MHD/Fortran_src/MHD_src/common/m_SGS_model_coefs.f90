!m_SGS_model_coefs.f90
!     module m_SGS_model_coefs
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
      module m_SGS_model_coefs
!
      use m_precision
      use t_material_property
!
      implicit  none
!
!
      type(MHD_coefficients_type), save :: sgs_coefs
!
      type(MHD_coefficients_type), save :: sgs_coefs_nod
!
      type(MHD_coefficients_type), save :: diff_coefs
!
      end module m_SGS_model_coefs
