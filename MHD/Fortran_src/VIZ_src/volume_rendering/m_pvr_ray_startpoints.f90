!m_pvr_ray_startpoints.f90
!      module m_pvr_ray_startpoints
!
      module m_pvr_ray_startpoints
!
!        programmed by H.Matsui on Aug., 2011
!
      use m_precision
!
      use m_constants
      use m_control_params_4_pvr
!
      implicit  none
!
!
      integer(kind = kint) :: ntot_pvr_ray = 0
      integer(kind = kint) :: num_pvr_ray
      integer(kind = kint), allocatable :: istack_pvr_ray_sf(:)
      integer(kind = kint), allocatable :: num_pvr_ray_sf(:)
!
      integer(kind= kint), allocatable :: icount_pvr_trace(:)
      integer(kind= kint), allocatable :: id_pixel_start(:)
      integer(kind= kint), allocatable :: isf_pvr_ray_start(:,:)
      real(kind = kreal), allocatable ::  xi_pvr_start(:,:)
      real(kind = kreal), allocatable ::  xx_pvr_ray_start(:,:)
      real(kind = kreal), allocatable ::  xx_pvr_start(:,:)
      real(kind = kreal), allocatable ::  pvr_ray_dir(:,:)
      real(kind = kreal), allocatable ::  rgba_ray(:,:)
!
      real(kind = kreal), parameter :: ray_vec(3) =  (/zero, zero, one/)
!
!      subroutine allocate_num_pvr_ray_start
!      subroutine allocate_item_pvr_ray_start
!      subroutine reallocate_item_pvr_ray_start
!      subroutine deallocate_num_pvr_ray_start
!      subroutine deallocate_item_pvr_ray_start
!      subroutine check_pvr_ray_startpoints(my_rank)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_pvr_ray_start
!
      use m_surf_grp_4_pvr_domain
!
!
      allocate(istack_pvr_ray_sf(0:ntot_pvr_surf_domain))
      allocate(num_pvr_ray_sf(ntot_pvr_surf_domain))
!
      istack_pvr_ray_sf =   0
      num_pvr_ray_sf =      0
!
      end subroutine allocate_num_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_item_pvr_ray_start
!
      use m_control_params_4_pvr
!
!
      allocate(id_pixel_start(ntot_pvr_ray))
      allocate(icount_pvr_trace(ntot_pvr_ray))
      allocate(isf_pvr_ray_start(3,ntot_pvr_ray))
      allocate(xi_pvr_start(2,ntot_pvr_ray))
      allocate(xx_pvr_ray_start(3,ntot_pvr_ray))
      allocate(xx_pvr_start(3,ntot_pvr_ray))
      allocate(pvr_ray_dir(3,ntot_pvr_ray))
      allocate(rgba_ray(4,ntot_pvr_ray))
!
      if(ntot_pvr_ray .gt. 0) then
        id_pixel_start = 0
        icount_pvr_trace = 0
        isf_pvr_ray_start = 0
        xi_pvr_start = 0.0d0
        xx_pvr_ray_start = 0.0d0
        xx_pvr_start = 0.0d0
        pvr_ray_dir =  0.0d0
      end if
!
      end subroutine allocate_item_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine reallocate_item_pvr_ray_start
!
      use m_control_params_4_pvr
!
!
      if(num_pvr_ray .gt. ntot_pvr_ray) then
        ntot_pvr_ray = num_pvr_ray
        call deallocate_item_pvr_ray_start
        call allocate_item_pvr_ray_start
      end if
!
      end subroutine reallocate_item_pvr_ray_start
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_pvr_ray_start
!
      deallocate(istack_pvr_ray_sf, num_pvr_ray_sf)
!
      end subroutine deallocate_num_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_item_pvr_ray_start
!
      deallocate(id_pixel_start, icount_pvr_trace)
      deallocate(isf_pvr_ray_start, xx_pvr_ray_start)
      deallocate(xx_pvr_start, xi_pvr_start)
      deallocate(pvr_ray_dir, rgba_ray)
!
      end subroutine deallocate_item_pvr_ray_start
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_pvr_ray_startpoints(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: inum
!
!
        write(50+my_rank,*) 'num_pvr_ray', num_pvr_ray
        do inum = 1, num_pvr_ray
          write(50+my_rank,*) inum, id_pixel_start(inum),              &
     &      isf_pvr_ray_start(1:3,inum), xx_pvr_ray_start(1:3,inum),   &
     &      icount_pvr_trace(inum)
        end do
!
      end subroutine check_pvr_ray_startpoints
!
!  ---------------------------------------------------------------------
!
      end module m_pvr_ray_startpoints
