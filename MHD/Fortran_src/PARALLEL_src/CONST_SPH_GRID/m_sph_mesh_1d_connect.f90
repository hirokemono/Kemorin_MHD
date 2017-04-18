!>@file   m_sph_mesh_1d_connect.F90
!!@brief  module m_sph_mesh_1d_connect
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief  One-dimmentional connectivity list for spherical shell
!!
!
      module m_sph_mesh_1d_connect
!
      use m_precision
      use m_constants
      use t_sph_mesh_1d_connect
      use t_2d_sph_trans_table
!
      implicit none
!
!
      type(sph_trans_2d_table), save :: s2d_tbl
!
      end module m_sph_mesh_1d_connect
