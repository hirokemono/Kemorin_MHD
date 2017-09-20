!m_3d_filter_coef_MHD.f90
!      module m_3d_filter_coef_MHD
!
!     Written by H. Matsui on Nov., 2008
!
      module m_3d_filter_coef_MHD
!
      use m_precision
      use t_FEM_SGS_structure
!
      implicit none
!
!
!> Structure of grouping of elements
      type(FEM_SGS_structure), save :: FEM_SGS1
!FEM_SGS1%SGS_par
!
!> Structure of grouping of elements
      type(filters_on_FEM), save :: FEM_filters1
!> Structure of Work area for dynamics model
      type(work_FEM_SGS_MHD), save :: SGS_MHD_wk1
!
      end module m_3d_filter_coef_MHD
