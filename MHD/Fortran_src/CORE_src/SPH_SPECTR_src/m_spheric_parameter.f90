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
      use t_spheric_parameter
!.
      implicit none
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph1
!sph1%sph_rtm
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_shell_parameters), save :: sph_param1
!
!      type(sph_rtp_grid), save :: sph_rtp1
!
!      type(sph_rtm_grid), save :: sph_rtm1
!
      type(sph_rlm_grid), save :: sph_rlm1
!
!      type(sph_rj_grid), save :: sph_rj1
!
      end module m_spheric_parameter
