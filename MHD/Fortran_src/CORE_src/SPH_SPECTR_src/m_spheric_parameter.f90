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
!!      subroutine allocate_spheric_param_rtp
!!      subroutine allocate_spheric_param_rtm
!!      subroutine allocate_spheric_param_rlm
!!
!!      subroutine allocate_sph_1d_index_rtp
!!      subroutine allocate_sph_1d_index_rtm
!!      subroutine allocate_sph_1d_index_rlm
!!      subroutine allocate_sph_1d_index_rj
!!
!!      subroutine deallocate_spheric_param_rtp
!!      subroutine deallocate_spheric_param_rtm
!!      subroutine deallocate_spheric_param_rlm
!!
!!      subroutine deallocate_sph_1d_index_rtp
!!      subroutine deallocate_sph_1d_index_rtm
!!      subroutine deallocate_sph_1d_index_rlm
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
!sph_rtp1%radius_1d_rlm_r
!
      type(sph_rtm_grid), save :: sph_rtm1
!sph_rtm1%radius_1d_rtm_r
!
      type(sph_rlm_grid), save :: sph_rlm1
!sph_rlm1%radius_1d_rlm_r
!
      type(sph_rj_grid), save :: sph_rj1
!sph_rj1%istack_inod_rj_smp
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
!>      1d start address of global data for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: ist_rtp(3)
!>      1d start address of global data for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: ist_rtm(3)
!>      1d start address of global data for @f$ f(r,l,m) @f$
      integer(kind = kint) :: ist_rlm(2)
!>      1d start address of global data for @f$ f(r,j) @f$
!      integer(kind = kint) :: ist_rj(2)
!
!>      1d end address of global data for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: ied_rtp(3)
!>      1d end address of global data for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: ied_rtm(3)
!>      1d end address of global data for @f$ f(r,l,m) @f$
      integer(kind = kint) :: ied_rlm(2)
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
!>      number of increments for @f$ f(r,\theta,\phi) @f$
      integer(kind = kint) :: istep_rtp(3)
!>      number of increments for @f$ f(r,\theta,m) @f$
      integer(kind = kint) :: istep_rtm(3)
!>      number of increments for @f$ f(r,l,m) @f$
      integer(kind = kint) :: istep_rlm(2)
!>      number of increments for @f$ f(r,j) @f$
      integer(kind = kint) :: istep_rj(2)
!
!
!>      global address for each direction @f$ f(r,\theta,\phi) @f$
      integer(kind = kint), allocatable :: idx_global_rtp(:,:)
!>      global address for each direction @f$ f(r,\theta,m) @f$
      integer(kind = kint), allocatable :: idx_global_rtm(:,:)
!>      global address for each direction @f$ f(r,l,m) @f$
      integer(kind = kint), allocatable :: idx_global_rlm(:,:)
!
!
!>      radial global address @f$ f(r,\theta,\phi) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtp_r(:)
!>      meridional global address @f$ f(r,\theta,\phi) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtp_t(:)
!>      zonal global address @f$ f(r,\theta,\phi) @f$
!!@n        idx_gl_1d_rtp_p(m,1): zonal grid point ID
!!@n        idx_gl_1d_rtp_p(m,2): Fourier spectr mode
      integer(kind = kint), allocatable :: idx_gl_1d_rtp_p(:,:)
!
!>      radial global address @f$ f(r,\theta,m) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtm_r(:)
!>      meridional global address @f$ f(r,\theta,m) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rtm_t(:)
!>      zonal global address @f$ f(r,\theta,m) @f$
!!@n        idx_gl_1d_rtm_m(m,1): global ID for Fourier transform
!!@n        idx_gl_1d_rtm_m(m,2): Fourier spectr mode
      integer(kind = kint), allocatable :: idx_gl_1d_rtm_m(:,:)
!
!>      radial global address @f$ f(r,l,m) @f$
      integer(kind = kint), allocatable :: idx_gl_1d_rlm_r(:)
!>      spherical harmonics mode for  @f$ f(r,l,m) @f$
!!@n        idx_gl_1d_rlm_j(j,1): global ID for spherical harmonics
!!@n        idx_gl_1d_rlm_j(j,2): spherical hermonincs degree
!!@n        idx_gl_1d_rlm_j(j,3): spherical hermonincs order
      integer(kind = kint), allocatable :: idx_gl_1d_rlm_j(:,:)
!
!>      1d radius data for @f$ f(r,\theta,\phi) @f$
      real(kind = kreal), allocatable :: radius_1d_rtp_r(:)
!>      1d radius data for @f$ f(r,\theta,m) @f$
!      real(kind = kreal), allocatable :: radius_1d_rtm_r(:)
!>      1d radius data for @f$ f(r,l,m) @f$
!      real(kind = kreal), allocatable :: radius_1d_rlm_r(:)
!
!>      1d @f$1 / r @f$ for @f$ f(r,\theta,\phi) @f$
      real(kind = kreal), allocatable :: a_r_1d_rtp_r(:)
!>      1d @f$1 / r @f$ for @f$ f(r,\theta,m) @f$
!      real(kind = kreal), allocatable :: a_r_1d_rtm_r(:)
!>      1d @f$1 / r @f$ for @f$ f(r,l,m) @f$
!      real(kind = kreal), allocatable :: a_r_1d_rlm_r(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_spheric_parameter
!
      call allocate_spheric_param_rtp
      call allocate_spheric_param_rtm
      call allocate_spheric_param_rlm
!
      sph_rj1%nnod_rj = nnod_rj
      call alloc_type_spheric_param_rj(sph_rj1)
!
      end subroutine allocate_spheric_parameter
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_parameter
!
      call deallocate_spheric_param_rtp
      call deallocate_spheric_param_rtm
      call deallocate_spheric_param_rlm
      call dealloc_spheric_param_rj(sph_rj1)
!
      end subroutine deallocate_spheric_parameter
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_spheric_param_rtp
!
      allocate(idx_global_rtp(nnod_rtp,3))
      if(nnod_rtp .gt. 0) idx_global_rtp = 0
!
      end subroutine allocate_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine allocate_spheric_param_rtm
!
      allocate(idx_global_rtm(nnod_rtm,3))
      if(nnod_rtm .gt. 0) idx_global_rtm = 0
!
      end subroutine allocate_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine allocate_spheric_param_rlm
!
      allocate(idx_global_rlm(nnod_rlm,2))
      if(nnod_rlm .gt. 0) idx_global_rlm = 0
!
      end subroutine allocate_spheric_param_rlm
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_index_rtp
!
      integer(kind = kint) :: num
!
      num = nidx_rtp(1)
      allocate(idx_gl_1d_rtp_r(num))
      allocate(radius_1d_rtp_r(num))
      allocate(a_r_1d_rtp_r(num))
      num = nidx_rtp(2)
      allocate(idx_gl_1d_rtp_t(num))
      num = nidx_rtp(3)
      allocate(idx_gl_1d_rtp_p(num,2))
!
      if(nidx_rtp(3) .gt. 0) idx_gl_1d_rtp_p = 0
      if(nidx_rtp(2) .gt. 0) idx_gl_1d_rtp_t = 0
      if(nidx_rtp(1) .gt. 0) then
        idx_gl_1d_rtp_r = 0
        radius_1d_rtp_r = 0.0d0
        a_r_1d_rtp_r = 0.0d0
      end if
!
      end subroutine allocate_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_index_rtm
!
      integer(kind = kint) :: num
!
      num = nidx_rtm(1)
      allocate(idx_gl_1d_rtm_r(num))
      num = nidx_rtm(2)
      allocate(idx_gl_1d_rtm_t(num))
      num = nidx_rtm(3)
      allocate(idx_gl_1d_rtm_m(num,2))
!
      if(nidx_rtm(3) .gt. 0) idx_gl_1d_rtm_m = 0
      if(nidx_rtm(2) .gt. 0) idx_gl_1d_rtm_t = 0
      if(nidx_rtm(1) .gt. 0) then
        idx_gl_1d_rtm_r = 0
      end if
!
      sph_rtm1%nidx_rtm(1:3) = nidx_rtm(1:3)
      call alloc_type_sph_1d_index_rtm(sph_rtm1)
!
      end subroutine allocate_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_index_rlm
!
      integer(kind = kint) :: num
!
      num = nidx_rlm(1)
      allocate(idx_gl_1d_rlm_r(num))
      num = nidx_rlm(2)
      allocate(idx_gl_1d_rlm_j(num,3))
!
      if(nidx_rlm(2) .gt. 0) idx_gl_1d_rlm_j = 0
      if(nidx_rlm(1) .gt. 0) then
        idx_gl_1d_rlm_r = 0
      end if
!
      sph_rlm1%nidx_rlm(1:2) = nidx_rlm(1:2)
      call alloc_type_sph_1d_index_rlm(sph_rlm1)
!
      end subroutine allocate_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_param_rtp
!
      deallocate(idx_global_rtp)
!
      end subroutine deallocate_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_param_rtm
!
      deallocate(idx_global_rtm)
!
      end subroutine deallocate_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_spheric_param_rlm
!
      deallocate(idx_global_rlm)
!
      end subroutine deallocate_spheric_param_rlm
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_index_rtp
!
      deallocate(radius_1d_rtp_r)
      deallocate(a_r_1d_rtp_r)
      deallocate(idx_gl_1d_rtp_r)
      deallocate(idx_gl_1d_rtp_t)
      deallocate(idx_gl_1d_rtp_p)
!
      end subroutine deallocate_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_index_rtm
!
      deallocate(idx_gl_1d_rtm_r)
      deallocate(idx_gl_1d_rtm_t)
      deallocate(idx_gl_1d_rtm_m)
!
      call dealloc_type_sph_1d_index_rtm(sph_rtm1)
!
      end subroutine deallocate_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_index_rlm
!
      deallocate(idx_gl_1d_rlm_r)
      deallocate(idx_gl_1d_rlm_j)
!
      call dealloc_type_sph_1d_index_rlm(sph_rlm1)
!
      end subroutine deallocate_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
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
        write(my_rank+50,*) i, idx_global_rtp(i,1:3)
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
        write(my_rank+50,*) i, idx_global_rtm(i,1:3)
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
        write(my_rank+50,*) i, idx_global_rlm(i,1:2)
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
