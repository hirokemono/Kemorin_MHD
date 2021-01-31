!>@file   t_sph_grid_maker_in_sim.f90
!!@brief  module t_sph_grid_maker_in_sim
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!@endverbatim
!
      module t_sph_grid_maker_in_sim
!
      use m_precision
!
      use t_spheric_parameter
      use t_const_spherical_grid
!
      implicit none
!
!>      Structure to check and construct spherical shell mesh
      type sph_grid_maker_in_sim
!>        Switch to construct spherical shell grid
        logical :: make_SPH_flag =    .FALSE.
!>        Switch to output constructed spherical shell grid
        logical :: mesh_output_flag = .FALSE.
!
!>        Structure to construct grid
        type(construct_spherical_grid) :: gen_sph
!>        Structure for temporal spherical grid
        type(sph_grids) :: sph_tmp
      end type sph_grid_maker_in_sim
!
! ----------------------------------------------------------------------
!
!      contains
!
! ----------------------------------------------------------------------
!
      end module t_sph_grid_maker_in_sim
