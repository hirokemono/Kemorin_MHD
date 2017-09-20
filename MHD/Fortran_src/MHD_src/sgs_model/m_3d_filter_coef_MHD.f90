!m_3d_filter_coef_MHD.f90
!      module m_3d_filter_coef_MHD
!
!     Written by H. Matsui on Nov., 2008
!
      module m_3d_filter_coef_MHD
!
      use m_precision
      use t_FEM_SGS_structure
      use t_work_FEM_SGS_MHD
!
      implicit none
!
!
!> Structure of grouping of elements
      type(FEM_SGS_structure), save :: FEM_SGS1
!
!> Structure of Work area for dynamics model
      type(work_FEM_SGS_MHD), save :: SGS_MHD_wk1
!
      end module m_3d_filter_coef_MHD
