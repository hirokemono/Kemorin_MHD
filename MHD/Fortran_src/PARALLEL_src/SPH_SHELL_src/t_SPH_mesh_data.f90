!>@file   t_SPH_mesh_data.f90
!!@brief  module t_SPH_mesh_data
!!
!!@author H. Matsui
!!@date Programmed on Sep., 2017
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
!!@endverbatim
!
!
      module t_SPH_mesh_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      use calypso_mpi
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
!
      use t_const_spherical_grid
      use t_sph_local_index
!
      implicit none
!
!> Structure of spherical transform mesh information
      type sph_mesh_data
!>         spherical harmonics indexing data
        type(sph_grids) ::       sph
!>         communication tables for spherical transform
        type(sph_comm_tables) :: comms
!>         grouping data for harmonics indices
        type(sph_group_data) ::  sph_grps
      end type sph_mesh_data
!
      end module t_SPH_mesh_data
