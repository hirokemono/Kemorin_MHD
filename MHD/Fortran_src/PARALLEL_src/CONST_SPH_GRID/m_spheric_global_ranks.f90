!>@file   m_spheric_global_ranks.f90
!!@brief  module m_spheric_global_ranks
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Global subdomain informatikn for spherical shell
!!
!
      module m_spheric_global_ranks
!
      use m_precision
      use t_const_spherical_grid
!
      implicit none
!
!
      type(construct_spherical_grid), save :: gen_sph1
!
      end module m_spheric_global_ranks
