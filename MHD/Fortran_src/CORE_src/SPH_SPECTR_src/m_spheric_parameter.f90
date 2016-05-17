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
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_shell_parameters), save :: sph_param1
!sph_param1%l_truncation
!
      type(sph_rtp_grid), save :: sph_rtp1
!sph_rtp1%nidx_global_rtp
!
      type(sph_rtm_grid), save :: sph_rtm1
!sph_rtm1%ist_rtm_order_zero
!
      type(sph_rlm_grid), save :: sph_rlm1
!sph_rlm1%nidx_global_rlm
!
      type(sph_rj_grid), save :: sph_rj1
!
!>      number of data points for @f$ f(r,\theta,\phi) @f$
!      integer(kind = kint) :: nnod_rtp
!>      number of data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nnod_rtm
!>      number of data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nnod_rlm
!
!>      number of 1d data points for @f$ f(r,\theta,\phi) @f$
!      integer(kind = kint) :: nidx_rtp(3)
!>      number of 1d data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nidx_rtm(3)
!>      number of 1d data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nidx_rlm(2)
!
!
      end module m_spheric_parameter
