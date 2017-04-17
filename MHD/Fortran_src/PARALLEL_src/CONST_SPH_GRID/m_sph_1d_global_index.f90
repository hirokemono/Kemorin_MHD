!>@file   m_sph_1d_global_index.f90
!!@brief  module m_sph_1d_global_index
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Global addresses for soherical harmonics indices
!!
!!@verbatim
!!      subroutine allocate_sph_1d_global_stack
!!      subroutine allocate_sph_1d_global_idx
!!
!!      subroutine deallocate_sph_1d_global_stack
!!      subroutine deallocate_sph_1d_global_idx
!!
!!      subroutine check_spheric_global_stack(ip_rank)
!!@endverbatim
!
!
      module m_sph_1d_global_index
!
      use m_precision
      use t_sph_1d_global_index
      use t_control_1D_layering
!
      implicit none
!
!
      type(sph_1d_index_stack), save :: stk_lc1d
      type(sph_1d_global_index), save :: sph_gl1d
!
!
!>      Structure of additional radial group
      type(layering_group_list), save :: added_radial_grp
!
!>      Structure of radial group for SGS model
      type(layering_group_list), save :: r_layer_grp
!>      Structure of meridional group for SGS model
      type(layering_group_list), save :: med_layer_grp
!
      end module m_sph_1d_global_index
