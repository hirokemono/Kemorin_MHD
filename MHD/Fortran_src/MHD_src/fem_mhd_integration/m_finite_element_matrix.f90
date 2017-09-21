!
!     module m_finite_element_matrix
!.......................................................................
!
!     Written by H. Matsui and H. Okuda on Jul. 2000
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine deallocate_finite_elem_mt
!
      module m_finite_element_matrix
!
      use m_precision
      use t_work_FEM_integration
      use t_MHD_mass_matricxes
!
      implicit  none
!
!
!>      Stracture for FEM assembling
      type(finite_element_integration), save :: fem_int1
!
!>      Structure of mass matrices for FEM_MHD
      type(lumped_mass_mat_layerd), save :: mk_MHD1
!
      end module m_finite_element_matrix
