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
!!
!!      subroutine check_global_spheric_parameter
!!      subroutine check_spheric_parameter(my_rank)
!!      subroutine check_spheric_param_rtp(my_rank)
!!      subroutine check_spheric_param_rtm(my_rank)
!!      subroutine check_spheric_param_rlm(my_rank)
!!      subroutine check_spheric_param_rj(my_rank)
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
!sph_rtp1%ist_rtp
!
      type(sph_rtm_grid), save :: sph_rtm1
!sph_rtm1%ist_rtm
!
      type(sph_rlm_grid), save :: sph_rlm1
!sph_rlm1%ist_rlm
!
      type(sph_rj_grid), save :: sph_rj1
!sph_rj1%istep_rj
!
!>      integer flag for FEM mesh type
!!@n    iflag_MESH_same:     same grid point as Gauss-Legendre points
!!@n    iflag_MESH_w_pole:   Gauss-Legendre points with poles
!!@n    iflag_MESH_w_center: Gauss-Legendre points with center and poles
      integer (kind=kint) :: iflag_shell_mode =  iflag_MESH_same
!>      integer flag for center point in spectr data
!!@n    This flag should have same value for all processes
!!@n    0: No center point
!!@n    1: Center point is there
      integer (kind=kint) :: iflag_rj_center =  0
!>      radial grid type flag
!!@n    igrid_Chebyshev =    2 :: Chebyshev collocation points
!!@n    igrid_non_euqidist = 1 :: non-equi-distance
!!@n    igrid_euqidistance = 0 :: equi-distance
      integer (kind=kint) :: iflag_radial_grid = igrid_non_euqidist
!
!>      local spectr index for @f$ l = m = 0 @f$ at center
!!@n    if center does not exist in subdomain, inod_rj_center = 0.
      integer (kind=kint) :: inod_rj_center =   0
!
!>      Start address for @f$ m = 0 @f$ for @f$ f(r,\theta,m) @f$
      integer (kind=kint) :: ist_rtm_order_zero = 0
!>      Start address for @f$ l=1, m=-1 @f$ for @f$ f(r,\theta,m) @f$
      integer (kind=kint) :: ist_rtm_order_1s =   0
!>      Start address for @f$ l=1, m= 1 @f$ for @f$ f(r,\theta,m) @f$
      integer (kind=kint) :: ist_rtm_order_1c =   0
!
!>        Truncation for spherical harmonics
      integer(kind = kint) :: l_truncation
!>        m-folding symmetry for longitudinal direction
      integer(kind = kint) :: m_folding = 1

!    global parameteres for radius
!
!>      global radial ID for innermost point
      integer(kind = kint) :: nlayer_2_center
!>      global radial ID for ICB
      integer(kind = kint) :: nlayer_ICB
!>      global radial ID for CMB
      integer(kind = kint) :: nlayer_CMB
!>      global radial ID for mid-depth of the outer core
      integer(kind = kint) :: nlayer_mid_OC
!
!>      radius for ICB @f$ r_{i} @f$
      real(kind = kreal) :: r_ICB
!>      radius for CMB @f$ r_{o} @f$
      real(kind = kreal) :: r_CMB
!>      Earth's radius @f$ Re @f$
      real(kind = kreal) :: R_earth(0:2)
!
!   global parameters
!
!>      number of global 1d data points for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: nidx_global_rtp(3)
!>      number of global 1d data points for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: nidx_global_rtm(3)
!>      number of global 1d data points for @f$ f(r,l,m) @f$
      integer(kind = kint) :: nidx_global_rlm(2)
!
!    local parameters
!
!>      1d subdomain ID for @f$ f(r,\theta,\phi) @f$ (start from 0)
      integer(kind = kint) :: sph_rank_rtp(3)
!>      1d subdomain ID for @f$ f(r,\theta,m) @f$ (start from 0)
      integer(kind = kint) :: sph_rank_rtm(3)
!>      1d subdomain ID for @f$ f(r,l,m) @f$ (start from 0)
      integer(kind = kint) :: sph_rank_rlm(2)
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
      subroutine allocate_spheric_parameter
!
!
      sph_rtp1%nnod_rtp = nnod_rtp
      sph_rtm1%nnod_rtm = nnod_rtm
      sph_rlm1%nnod_rlm = nnod_rlm
      sph_rj1%nnod_rj =   nnod_rj
!
      call alloc_type_spheric_param_rtp(sph_rtp1)
      call alloc_type_spheric_param_rtm(sph_rtm1)
      call alloc_type_spheric_param_rlm(sph_rlm1)
      call alloc_type_spheric_param_rj(sph_rj1)
!
      end subroutine allocate_spheric_parameter
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_parameter
!
      call dealloc_type_spheric_param_rtp(sph_rtp1)
      call dealloc_type_spheric_param_rtm(sph_rtm1)
      call dealloc_type_spheric_param_rlm(sph_rlm1)
      call dealloc_spheric_param_rj(sph_rj1)
!
      end subroutine deallocate_spheric_parameter
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_global_spheric_parameter
!
!
      write(*,*) 'truncation degree:           ', l_truncation
      write(*,*) 'm-folding symmetry:          ', m_folding
      write(*,*) 'number of grid for f(r,t,p): ', nidx_global_rtp(1:3)
!
      end subroutine check_global_spheric_parameter
!
! ----------------------------------------------------------------------
!
      subroutine check_spheric_parameter(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      call check_spheric_param_rtp(my_rank)
      call check_spheric_param_rtm(my_rank)
      call check_spheric_param_rlm(my_rank)
      call check_spheric_param_rj(my_rank)
!
      end subroutine check_spheric_parameter
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rtp(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rtp ', sph_rank_rtp(1:3)
      write(my_rank+50,*) 'nidx_rtp ', nidx_rtp(1:3)
      write(my_rank+50,*) 'nnod_rtp ', nnod_rtp
!
      write(my_rank+50,*)  'i, idx_global_rtp(r,t,p)'
      do i = 1, nnod_rtp
        write(my_rank+50,*) i, sph_rtp1%idx_global_rtp(i,1:3)
      end do
!
      end subroutine check_spheric_param_rtp
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rtm(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rtm ', sph_rank_rtm(1:3)
      write(my_rank+50,*) 'nidx_rtm ', nidx_rtm(1:3)
      write(my_rank+50,*) 'nnod_rtm ', nnod_rtm
!
      write(my_rank+50,*) 'i, idx_global_rtm(r,t,p)'
      do i = 1, nnod_rtm
        write(my_rank+50,*) i, sph_rtm1%idx_global_rtm(i,1:3)
      end do
!
      end subroutine check_spheric_param_rtm
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rlm(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rlm ', sph_rank_rlm(1:2)
      write(my_rank+50,*) 'nidx_rlm ', nidx_rlm(1:2)
      write(my_rank+50,*) 'nnod_rlm ', nnod_rlm
!
      write(my_rank+50,*) 'i, idx_global_rlm(r,j)'
      do i = 1, nnod_rlm
        write(my_rank+50,*) i, sph_rlm1%idx_global_rlm(i,1:2)
      end do
!
      end subroutine check_spheric_param_rlm
!
! -----------------------------------------------------------------------
!
      subroutine check_spheric_param_rj(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'sph_rank_rj ',  sph_rj1%irank_sph_rj(1:2)
      write(my_rank+50,*) 'nidx_rj  ',  nidx_rj(1:2)
      write(my_rank+50,*) 'nnod_rj ',  nnod_rj
!
      write(my_rank+50,*) 'i, idx_global_rj(r,j)'
      do i = 1, nnod_rj
        write(my_rank+50,*) i, sph_rj1%idx_global_rj(i,1:2)
      end do
!
      end subroutine check_spheric_param_rj
!
! -----------------------------------------------------------------------
!
      end module m_spheric_parameter
