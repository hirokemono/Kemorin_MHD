!>@file   m_sph_mesh_1d_connect.F90
!!@brief  module m_sph_mesh_1d_connect
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief  One-dimmentional connectivity list for spherical shell
!!
!!@verbatim
!!      subroutine allocate_radius_1d_gl(nri_global)
!!      subroutine deallocate_radius_1d_gl
!!
!!      subroutine allocate_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,&
!!     &          nidx_global_rtp, m_folding)
!!      subroutine allocate_iele_sph_mesh
!!      subroutine deallocate_nnod_nele_sph_mesh
!!
!!      subroutine check_iele_4_sph_connects
!!@endverbatim
!
      module m_sph_mesh_1d_connect
!
      use m_precision
      use m_constants
      use t_sph_mesh_1d_connect
!
      implicit none
!
!
      type(comm_table_make_sph), save :: stbl
!
      end module m_sph_mesh_1d_connect
