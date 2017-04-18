!m_sph_global_parameter.f90
!      module m_sph_global_parameter
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine allocate_sph_gl_parameter
!!      subroutine allocate_sph_gl_bc_param
!!
!!      subroutine deallocate_sph_gl_parameter
!!      subroutine deallocate_sph_gl_bc_param
!
!
      module m_sph_global_parameter
!
      use m_precision
      use t_sph_local_parameter
!
      implicit none
!
      type(sph_local_parameters), save :: sph_lcp
      type(sph_local_1d_param), save :: sph_lc1
      type(sph_local_default_BC), save :: sph_dbc
!
!
!
      integer(kind = kint), allocatable :: nidx_local_rtp_IC(:)
      integer(kind = kint), allocatable :: nidx_local_rtm_IC(:)
      integer(kind = kint), allocatable :: nidx_local_rtp_OC(:)
      integer(kind = kint), allocatable :: nidx_local_rtm_OC(:)
      integer(kind = kint), allocatable :: nidx_local_rtp_MT(:)
      integer(kind = kint), allocatable :: nidx_local_rtm_MT(:)
!
      integer(kind = kint), allocatable :: ist_idx_local_rtp_IC(:)
      integer(kind = kint), allocatable :: ist_idx_local_rtm_IC(:)
      integer(kind = kint), allocatable :: ist_idx_local_rtp_OC(:)
      integer(kind = kint), allocatable :: ist_idx_local_rtm_OC(:)
      integer(kind = kint), allocatable :: ist_idx_local_rtp_MT(:)
      integer(kind = kint), allocatable :: ist_idx_local_rtm_MT(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_gl_bc_param
!
      use m_spheric_global_ranks
!
      integer(kind = kint) :: num
!
!
      num = s3d_ranks%ndomain_rtp(1)
      allocate(nidx_local_rtp_IC(num))
      allocate(nidx_local_rtp_OC(num))
      allocate(nidx_local_rtp_MT(num))
      allocate(ist_idx_local_rtp_IC(num))
      allocate(ist_idx_local_rtp_OC(num))
      allocate(ist_idx_local_rtp_MT(num))
!
      num = s3d_ranks%ndomain_rtm(1)
      allocate(nidx_local_rtm_IC(num))
      allocate(nidx_local_rtm_OC(num))
      allocate(nidx_local_rtm_MT(num))
      allocate(ist_idx_local_rtm_IC(num))
      allocate(ist_idx_local_rtm_OC(num))
      allocate(ist_idx_local_rtm_MT(num))
!
      nidx_local_rtp_IC =  0
      nidx_local_rtm_IC =  0
      nidx_local_rtp_OC =  0
      nidx_local_rtm_OC =  0
      nidx_local_rtp_MT =  0
      nidx_local_rtm_MT =  0
!
      ist_idx_local_rtp_IC =  0
      ist_idx_local_rtm_IC =  0
      ist_idx_local_rtp_OC =  0
      ist_idx_local_rtm_OC =  0
      ist_idx_local_rtp_MT =  0
      ist_idx_local_rtm_MT =  0
!
      end subroutine allocate_sph_gl_bc_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_gl_bc_param
!
!
      deallocate(nidx_local_rtp_IC)
      deallocate(nidx_local_rtp_OC)
      deallocate(nidx_local_rtp_MT)
      deallocate(ist_idx_local_rtp_IC)
      deallocate(ist_idx_local_rtp_OC)
      deallocate(ist_idx_local_rtp_MT)
!
      deallocate(nidx_local_rtm_IC)
      deallocate(nidx_local_rtm_OC)
      deallocate(nidx_local_rtm_MT)
      deallocate(ist_idx_local_rtm_IC)
      deallocate(ist_idx_local_rtm_OC)
      deallocate(ist_idx_local_rtm_MT)
!
      end subroutine deallocate_sph_gl_bc_param
!
! -----------------------------------------------------------------------
!
      end module m_sph_global_parameter
