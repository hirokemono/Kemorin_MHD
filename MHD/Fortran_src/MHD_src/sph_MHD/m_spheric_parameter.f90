!>@file   m_spheric_parameter.f90
!!@brief  module m_spheric_parameter
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine allocate_spheric_parameter
!!      subroutine deallocate_spheric_parameter
!!      subroutine deallocate_sph_param_smp
!!
!!      subroutine check_global_spheric_parameter
!!@endverbatim
!!
!!@n @param  my_rank     Running rank ID
!!@n @param   l          Sphrical harmonics degree
!!@n @param   m          Sphrical harmonics order
!!
!
      module m_spheric_parameter
!
      use m_precision
      use m_spheric_constants
      use t_spheric_mesh
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!.
      implicit none
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph1
!
!>  Structure for communication table for spherical transform
      type(sph_comm_tables), save :: comms_sph1
!
!> Structure for grid and comm table for spherical transform
      type(sph_group_data), save :: sph_grps1
!
      end module m_spheric_parameter
