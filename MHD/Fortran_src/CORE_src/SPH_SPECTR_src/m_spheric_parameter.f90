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
!sph_rj1%nidx_rj
!
!>      integer flag for FEM mesh type
!!@n    iflag_MESH_same:     same grid point as Gauss-Legendre points
!!@n    iflag_MESH_w_pole:   Gauss-Legendre points with poles
!!@n    iflag_MESH_w_center: Gauss-Legendre points with center and poles
      integer (kind=kint) :: iflag_shell_mode =  iflag_MESH_same
!>      radial grid type flag
!!@n    igrid_Chebyshev =    2 :: Chebyshev collocation points
!!@n    igrid_non_euqidist = 1 :: non-equi-distance
!!@n    igrid_euqidistance = 0 :: equi-distance
      integer (kind=kint) :: iflag_radial_grid = igrid_non_euqidist
!
!>      local spectr index for @f$ l = m = 0 @f$ at center
!!@n    if center does not exist in subdomain, inod_rj_center = 0.
!      integer (kind=kint) :: inod_rj_center =   0
!
!>        Truncation for spherical harmonics
      integer(kind = kint) :: l_truncation
!>        m-folding symmetry for longitudinal direction
      integer(kind = kint) :: m_folding = 1

!    global parameteres for radius
!
!>      global radial ID for ICB
      integer(kind = kint) :: nlayer_ICB
!>      global radial ID for CMB
      integer(kind = kint) :: nlayer_CMB
!
!>  Structure of grid and spectr data for spherical spectr method
        type(sph_shell_parameters) :: sph_param1
!sph_param1%radius_ICB
!
!>      number of data points for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: nnod_rtp
!>      number of data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nnod_rtm
!>      number of data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nnod_rlm
!>      number of data points for @f$ f(r,j) @f$
      integer(kind = kint) :: nnod_rj
!
!>      number of 1d data points for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: nidx_rtp(3)
!>      number of 1d data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nidx_rtm(3)
!>      number of 1d data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nidx_rlm(2)
!>      number of 1d data points for @f$ f(r,j) @f$
      integer(kind = kint) :: nidx_rj(2)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_global_spheric_parameter
!
!
      write(*,*) 'truncation degree:           ', l_truncation
      write(*,*) 'm-folding symmetry:          ', m_folding
      write(*,*) 'number of grid for f(r,t,p): ',                       &
     &            sph_rtp1%nidx_global_rtp(1:3)
!
      end subroutine check_global_spheric_parameter
!
! ----------------------------------------------------------------------
!
      end module m_spheric_parameter
