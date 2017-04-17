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
!
      implicit none
!
!
      type(sph_1d_index_stack), save :: stk_lc1d
      type(sph_1d_global_index), save :: sph_gl1d
!
!
!
!>      Global radial address for f(r,t,p)
!      integer(kind = kint), allocatable :: istack_idx_local_rtp_r(:)
!>      Global radial address for f(r,t,p)
!      integer(kind = kint), allocatable :: istack_idx_local_rtp_t(:)
!>      Global zonal grid address for f(r,t,p)
!      integer(kind = kint), allocatable :: istack_idx_local_rtp_p(:)
!>      Global radial address for f(r,t,m)
!      integer(kind = kint), allocatable :: istack_idx_local_rtm_r(:)
!>      Global meridional grid address for f(r,t,m)
!      integer(kind = kint), allocatable :: istack_idx_local_rtm_t(:)
!>      Global zonal mode address for f(r,t,m)
!      integer(kind = kint), allocatable :: istack_idx_local_rtm_m(:)
!>      Global radial address for f(r,l,m)
!      integer(kind = kint), allocatable :: istack_idx_local_rlm_r(:)
!>      Global spherical harmonics mode address for f(r,l,m)
!      integer(kind = kint), allocatable :: istack_idx_local_rlm_j(:)
!>      Global radial address for f(r,j)
!      integer(kind = kint), allocatable :: istack_idx_local_rj_r(:)
!>      Global spherical harmonics mode address for f(r,j)
!      integer(kind = kint), allocatable :: istack_idx_local_rj_j(:)
!
!
!>      number of radial address for f(r,t,p)
      integer(kind = kint) :: num_gl_rtp_r
!>      number of radial address for f(r,t,p)
      integer(kind = kint) :: num_gl_rtp_t
!>      number of zonal grid address for f(r,t,p)
      integer(kind = kint) :: num_gl_rtp_p
!>      Global radial address for f(r,t,p)
!      integer(kind = kint), allocatable :: idx_global_rtp_r(:)
!>      Global radial address for f(r,t,p)
!      integer(kind = kint), allocatable :: idx_global_rtp_t(:)
!>      Global zonal grid address for f(r,t,p)
!      integer(kind = kint), allocatable :: idx_global_rtp_p(:,:)
!
!>      number of radial address for f(r,t,m)
      integer(kind = kint) :: num_gl_rtm_r
!>      number of meridional grid address for f(r,t,m)
      integer(kind = kint) :: num_gl_rtm_t
!>      number of zonal mode address for f(r,t,m)
      integer(kind = kint) :: num_gl_rtm_m
!>      Global radial address for f(r,t,m)
!      integer(kind = kint), allocatable :: idx_global_rtm_r(:)
!>      Global meridional grid address for f(r,t,m)
!      integer(kind = kint), allocatable :: idx_global_rtm_t(:)
!>      Global zonal mode address for f(r,t,m)
!      integer(kind = kint), allocatable :: idx_global_rtm_m(:,:)
!
!>      number of radial address for f(r,l,m)
      integer(kind = kint) :: num_gl_rlm_r
!>      number of spherical harmonics mode address for f(r,l,m)
      integer(kind = kint) :: num_gl_rlm_j
!>      Global radial address for f(r,l,m)
!      integer(kind = kint), allocatable :: idx_global_rlm_r(:)
!>      Global spherical harmonics mode address for f(r,l,m)
!      integer(kind = kint), allocatable :: idx_global_rlm_j(:,:)
!
!>      number of Global radial address for f(r,j)
      integer(kind = kint) :: nun_gl_rj_r
!>      number of spherical harmonics mode address for f(r,j)
      integer(kind = kint) :: num_gl_rj_j
!>      Global radial address for f(r,j)
!      integer(kind = kint), allocatable :: idx_global_rj_r(:)
!>      Global spherical harmonics mode address for f(r,j)
!      integer(kind = kint), allocatable :: idx_global_rj_j(:,:)
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_global_stack
!
!
      call alloc_sph_1d_global_stack(stk_lc1d)
!
      end subroutine allocate_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_global_idx
!
      use m_spheric_global_ranks
!
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = ndomain_rtp(1)
      n2 = ndomain_rtp(2)
      n3 = ndomain_rtp(3)
      num_gl_rtp_r = stk_lc1d%istack_idx_local_rtp_r(n1)
      num_gl_rtp_t = stk_lc1d%istack_idx_local_rtp_t(n2)
      num_gl_rtp_p = stk_lc1d%istack_idx_local_rtp_p(n3)
!
      allocate( sph_gl1d%idx_global_rtp_r(1:num_gl_rtp_r) )
      allocate( sph_gl1d%idx_global_rtp_t(1:num_gl_rtp_t) )
      allocate( sph_gl1d%idx_global_rtp_p(1:num_gl_rtp_p,2) )
!
      sph_gl1d%idx_global_rtp_r = 0
      sph_gl1d%idx_global_rtp_t = 0
      sph_gl1d%idx_global_rtp_p = 0
!
!
      n1 = ndomain_rtm(1)
      n2 = ndomain_rtm(2)
      n3 = ndomain_rtm(3)
      num_gl_rtm_r = stk_lc1d%istack_idx_local_rtm_r(n1)
      num_gl_rtm_t = stk_lc1d%istack_idx_local_rtm_t(n2)
      num_gl_rtm_m = stk_lc1d%istack_idx_local_rtm_m(n3)
!
      allocate( sph_gl1d%idx_global_rtm_r(1:num_gl_rtm_r) )
      allocate( sph_gl1d%idx_global_rtm_t(1:num_gl_rtm_t) )
      allocate( sph_gl1d%idx_global_rtm_m(0:num_gl_rtm_m,2) )
!
      sph_gl1d%idx_global_rtm_r = 0
      sph_gl1d%idx_global_rtm_t = 0
      sph_gl1d%idx_global_rtm_m = 0
!
!
      n1 = ndomain_rlm(1)
      n2 = ndomain_rlm(2)
      num_gl_rlm_r = stk_lc1d%istack_idx_local_rlm_r(n1)
      num_gl_rlm_j = stk_lc1d%istack_idx_local_rlm_j(n2)
!
      allocate( sph_gl1d%idx_global_rlm_r(1:num_gl_rlm_r) )
      allocate( sph_gl1d%idx_global_rlm_j(0:num_gl_rlm_j,3) )
!
      sph_gl1d%idx_global_rlm_r = 0
      sph_gl1d%idx_global_rlm_j = 0
!
!
      n1 = ndomain_rj(1)
      n2 = ndomain_rj(2)
      nun_gl_rj_r = stk_lc1d%istack_idx_local_rj_r(n1)
      num_gl_rj_j = stk_lc1d%istack_idx_local_rj_j(n2)
!
      allocate( sph_gl1d%idx_global_rj_r(1:nun_gl_rj_r) )
      allocate( sph_gl1d%idx_global_rj_j(0:num_gl_rj_j,3) )
!
      sph_gl1d%idx_global_rj_r = 0
      sph_gl1d%idx_global_rj_j = 0
!
      end subroutine allocate_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_global_stack
!
!
      call dealloc_sph_1d_global_stack(stk_lc1d)
!
      end subroutine deallocate_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_global_idx
!
!
      call dealloc_sph_1d_global_idx(sph_gl1d)
!
      end subroutine deallocate_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call chk_spheric_global_stack(my_rank, stk_lc1d)
!
      end subroutine check_spheric_global_stack
!
! -----------------------------------------------------------------------
!
      end module m_sph_1d_global_index
