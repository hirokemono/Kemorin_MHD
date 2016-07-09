!> @file  m_interpolate_table_org_IO.f90
!!      module m_interpolate_table_org_IO
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief IO routines for origin data of interpolation table
!
      module m_interpolate_table_org_IO
!
      use m_precision
      use t_interpolate_tbl_org
!
      implicit none
!
!> Structure of interpolation table for source grid
      type(interpolate_table_org), save :: IO_itp_org
!
      end module m_interpolate_table_org_IO
