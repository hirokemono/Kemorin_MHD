!m_3d_filter_coef_MHD.f90
!      module m_3d_filter_coef_MHD
!
!     Written by H. Matsui on Nov., 2008
!
      module m_3d_filter_coef_MHD
!
      use m_precision
      use t_FEM_MHD_filter_data
!
      implicit none
!
!
!> Structure of grouping of elements
      type(filters_on_FEM), save :: FEM_filters1
!
      end module m_3d_filter_coef_MHD
