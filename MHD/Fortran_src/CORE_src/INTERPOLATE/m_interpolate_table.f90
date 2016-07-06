!m_interpolate_table.f90
!      module m_interpolate_table
!
!> @brief Structure of interpolation table
!
!      Written by H.Matsui on Dec., 2008
!
!
      module m_interpolate_table
!
      use m_precision
!
      use m_constants
      use t_interpolate_table
!
      implicit none
!
!
      type(interpolate_table), save :: itp1_info
!
      end module m_interpolate_table
