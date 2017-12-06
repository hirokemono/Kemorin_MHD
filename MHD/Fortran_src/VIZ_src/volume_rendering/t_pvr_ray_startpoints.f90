!>@file  t_pvr_ray_startpoints.f90
!!       module t_pvr_ray_startpoints
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for start points for ray tracing
!!
!!@verbatim
!!      subroutine allocate_num_pvr_ray_start(num_pvr_surf, pvr_start)
!!      subroutine allocate_item_pvr_ray_start(num_ray, pvr_start)
!!      subroutine allocate_item_pvr_ray_pixels(pvr_start)
!!      subroutine deallocate_pvr_ray_start(pvr_start)
!!      subroutine deallocate_item_pvr_ray_start(pvr_start)
!!
!!      subroutine copy_item_pvr_ray_start(pvr_st_org, pvr_start)
!!
!!      subroutine check_pvr_ray_startpoints(my_rank, pvr_start)
!!@endverbatim
!
      module t_pvr_ray_startpoints
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>  Structure for start points of ray tracing
      type pvr_ray_start_type
!>    Total number of ray tracing
        integer(kind = kint) :: ntot_pvr_ray = 0
!
!>    Number of ray tracing
        integer(kind = kint) :: ntot_tmp_pvr_ray
!>    temporal number of pixels to start ray tracing
        integer(kind = kint), allocatable :: istack_tmp_pvr_ray_st(:)
!>    temporal number of pixels to start ray tracing
        integer(kind = kint), allocatable :: ipix_start_tmp(:,:)
!>    temporal number of pixels to start ray tracing
        integer(kind = kint), allocatable :: iflag_start_tmp(:)
!>    start point of ray traing in surface coordinate
        real(kind = kreal), allocatable ::  xi_start_tmp(:,:)
!
!
!>    Number of ray tracing
        integer(kind = kint) :: num_pvr_ray
!>    stack of number of pixels to start ray tracing
        integer(kind = kint), allocatable :: istack_pvr_ray_sf(:)
!
!>    ray trace counter
        integer(kind= kint), allocatable :: icount_pvr_trace(:)
!>    pixel ID for ray tracing
        integer(kind= kint), allocatable :: id_pixel_start(:)
!>    Start surface ID for ray tracing
        integer(kind= kint), allocatable :: isf_pvr_ray_start(:,:)
!>    start point of ray traing in surface coordinate
        real(kind = kreal), allocatable ::  xi_pvr_start(:,:)
!>    start point of ray traing
        real(kind = kreal), allocatable ::  xx_pvr_ray_start(:,:)
!>    start point for each trace
        real(kind = kreal), allocatable ::  xx_pvr_start(:,:)
!>    Direction og ray tracing
        real(kind = kreal), allocatable ::  pvr_ray_dir(:,:)
!>    Color data for tracing
        real(kind = kreal), allocatable ::  rgba_ray(:,:)
!
!>    pixel check flag for ray tracing
        integer(kind= kint), allocatable :: id_pixel_check(:)
      end type pvr_ray_start_type
!
!>  Direction of Ray in screen coordinate
      real(kind = kreal), parameter                                     &
     &                   :: ray_vec(3) = (/zero, zero, -one/)
!
      private :: deallocate_num_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_pvr_ray_start(num_pvr_surf, pvr_start)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      allocate(pvr_start%istack_pvr_ray_sf(0:num_pvr_surf))
      allocate(pvr_start%istack_tmp_pvr_ray_st(0:num_pvr_surf))
!
      pvr_start%istack_pvr_ray_sf =     0
      pvr_start%istack_tmp_pvr_ray_st = 0
!
      end subroutine allocate_num_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_item_pvr_ray_start(num_ray, pvr_start)
!
      integer(kind = kint), intent(in) :: num_ray
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      pvr_start%num_pvr_ray  = num_ray
      allocate(pvr_start%id_pixel_start(pvr_start%num_pvr_ray)     )
      allocate(pvr_start%icount_pvr_trace(pvr_start%num_pvr_ray)   )
      allocate(pvr_start%isf_pvr_ray_start(3,pvr_start%num_pvr_ray))
      allocate(pvr_start%xi_pvr_start(2,pvr_start%num_pvr_ray)     )
      allocate(pvr_start%xx_pvr_ray_start(3,pvr_start%num_pvr_ray) )
      allocate(pvr_start%xx_pvr_start(3,pvr_start%num_pvr_ray)     )
      allocate(pvr_start%pvr_ray_dir(3,pvr_start%num_pvr_ray)      )
      allocate(pvr_start%id_pixel_check(pvr_start%num_pvr_ray)     )
!
      if(pvr_start%num_pvr_ray .gt. 0) then
        pvr_start%id_pixel_start = 0
        pvr_start%icount_pvr_trace = 0
        pvr_start%isf_pvr_ray_start = 0
        pvr_start%xi_pvr_start = 0.0d0
        pvr_start%xx_pvr_ray_start = 0.0d0
        pvr_start%xx_pvr_start = 0.0d0
        pvr_start%pvr_ray_dir =  0.0d0
        pvr_start%id_pixel_check = 0
      end if
!
      end subroutine allocate_item_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_item_pvr_ray_pixels(pvr_start)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      allocate(pvr_start%rgba_ray(4,pvr_start%num_pvr_ray)         )
!
      end subroutine allocate_item_pvr_ray_pixels
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_tmp_pvr_ray_start(pvr_start)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      allocate(pvr_start%ipix_start_tmp(2,pvr_start%ntot_tmp_pvr_ray))
      allocate(pvr_start%iflag_start_tmp(pvr_start%ntot_tmp_pvr_ray))
      allocate(pvr_start%xi_start_tmp(2,pvr_start%ntot_tmp_pvr_ray))
!
      if(pvr_start%ntot_tmp_pvr_ray .gt. 0) then
        pvr_start%ipix_start_tmp = 0
        pvr_start%iflag_start_tmp = 0
        pvr_start%xi_start_tmp =    0.0d0
      end if
!
      end subroutine allocate_tmp_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_pvr_ray_start(pvr_start)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      call deallocate_item_pvr_ray_pixels(pvr_start)
      call deallocate_item_pvr_ray_start(pvr_start)
      call deallocate_tmp_pvr_ray_start(pvr_start)
      call deallocate_num_pvr_ray_start(pvr_start)
!
      end subroutine deallocate_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_tmp_pvr_ray_start(pvr_start)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      deallocate(pvr_start%ipix_start_tmp)
      deallocate(pvr_start%iflag_start_tmp, pvr_start%xi_start_tmp)
!
      end subroutine deallocate_tmp_pvr_ray_start
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_pvr_ray_start(pvr_start)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      deallocate(pvr_start%istack_pvr_ray_sf)
      deallocate(pvr_start%istack_tmp_pvr_ray_st)
!
      end subroutine deallocate_num_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_item_pvr_ray_start(pvr_start)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      deallocate(pvr_start%id_pixel_start, pvr_start%icount_pvr_trace)
      deallocate(pvr_start%isf_pvr_ray_start)
      deallocate(pvr_start%xx_pvr_ray_start)
      deallocate(pvr_start%xx_pvr_start, pvr_start%xi_pvr_start)
      deallocate(pvr_start%pvr_ray_dir, pvr_start%id_pixel_check)
!
      end subroutine deallocate_item_pvr_ray_start
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_item_pvr_ray_pixels(pvr_start)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      deallocate(pvr_start%rgba_ray)
!
      end subroutine deallocate_item_pvr_ray_pixels
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_item_pvr_ray_start(pvr_st_org, pvr_start)
!
      type(pvr_ray_start_type), intent(in) :: pvr_st_org
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
!$omp parallel workshare
       pvr_start%id_pixel_start(:) = pvr_st_org%id_pixel_start(:)
       pvr_start%icount_pvr_trace(:) = pvr_st_org%icount_pvr_trace(:)
!
       pvr_start%isf_pvr_ray_start(:,:)                                 &
     &     = pvr_st_org%isf_pvr_ray_start(:,:)
       pvr_start%xi_pvr_start(:,:) = pvr_st_org%xi_pvr_start(:,:)
       pvr_start%xx_pvr_ray_start(:,:)                                  &
     &     = pvr_st_org%xx_pvr_ray_start(:,:) 
       pvr_start%xx_pvr_start(:,:) = pvr_st_org%xx_pvr_start(:,:)
       pvr_start%pvr_ray_dir(:,:)  = pvr_st_org%pvr_ray_dir(:,:)
!$omp end parallel workshare
!
      end subroutine copy_item_pvr_ray_start
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_pvr_ray_startpoints(my_rank, pvr_start)
!
      integer(kind = kint), intent(in) :: my_rank
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      integer(kind = kint) :: inum
!
!
        write(50+my_rank,*) 'num_pvr_ray', pvr_start%num_pvr_ray
        do inum = 1, pvr_start%num_pvr_ray
          write(50+my_rank,*) inum, pvr_start%id_pixel_start(inum),    &
     &      pvr_start%isf_pvr_ray_start(1:3,inum),                     &
     &      pvr_start%xx_pvr_ray_start(1:3,inum),                      &
     &      pvr_start%icount_pvr_trace(inum)
        end do
!
      end subroutine check_pvr_ray_startpoints
!
!  ---------------------------------------------------------------------
!
      end module t_pvr_ray_startpoints
