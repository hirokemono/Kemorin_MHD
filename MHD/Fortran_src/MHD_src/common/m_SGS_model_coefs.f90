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
      type(SGS_terms_address), save :: ifld_sgs
!ifld_sgs%i_buoyancy
!
      type(SGS_terms_address), save :: icomp_sgs
!icomp_sgs%i_mom_flux
!
      type(SGS_terms_address), save :: ifld_diff
!ifld_diff%i_mom_flux
!
      type(SGS_terms_address), save :: icomp_diff
!icomp_diff%i_comp_flux
!
      type(SGS_terms_address), save :: iphys_elediff
!
!
      end module m_SGS_model_coefs
