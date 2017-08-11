!m_3d_filter_coef_MHD.f90
!      module m_3d_filter_coef_MHD
!
!     Written by H. Matsui on Nov., 2008
!
      module m_3d_filter_coef_MHD
!
      use m_precision
      use t_filtering_data
!
      implicit none
!
!
      type(filtering_data_type), save :: filtering1
!
      type(filtering_data_type), save :: wide_filtering
!
      end module m_3d_filter_coef_MHD
