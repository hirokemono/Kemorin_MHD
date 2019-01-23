!t_internal_4_partitioner.f90
!      module t_internal_4_partitioner
!
!      Written by H. Matsui on Aug., 2007
!
!!      subroutine alloc_numbers_4_part(nproc, itl_part)
!!      subroutine alloc_internal_4_part(itl_part)
!!      subroutine alloc_id_4_subdomain(itl_part)
!!        type(internal_4_partitioner), intent(inout) :: itl_part
!!
!!      subroutine dealloc_internal_4_part(itl_part)
!!      subroutine dealloc_num_4_subdomain(itl_part)
!!      subroutine dealloc_id_4_subdomain(itl_part)
!!        type(internal_4_partitioner), intent(inout) :: itl_part
!
      module t_internal_4_partitioner
!
      use m_precision
!
      implicit none
!
!>     internal address for each subdomains
      type internal_4_partitioner
        integer(kind = kint) :: ntot_inter_sub
        integer(kind = kint) :: nmin_inter_sub
        integer(kind = kint) :: nmax_inter_sub
        integer(kind = kint), allocatable :: num_inter_sub(:)
        integer(kind = kint), allocatable :: istack_inter_sub(:)
        integer(kind = kint), allocatable :: id_inter_subdomain(:)
!
        integer(kind = kint) :: ntot_sub
        integer(kind = kint) :: nmin_sub
        integer(kind = kint) :: nmax_sub
        integer(kind = kint), allocatable :: num_4_subdomain(:)
        integer(kind = kint), allocatable :: istack_4_subdomain(:)
        integer(kind = kint), allocatable :: id_4_subdomain(:)
      end type internal_4_partitioner
!
!
      type internals_4_part
!>      internal nodes for each subdomains
        type(internal_4_partitioner) :: itl_nod_part
!>      internal elements for each subdomains
        type(internal_4_partitioner) :: itl_ele_part
!>        internal surfaces for each subdomains
        type(internal_4_partitioner) :: itl_surf_part
!>        internal edges for each subdomains
        type(internal_4_partitioner) :: itl_edge_part
      end type internals_4_part
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_numbers_4_part(nproc, itl_part)
!
      integer(kind = kint), intent(in) :: nproc
      type(internal_4_partitioner), intent(inout) :: itl_part
!
      allocate( itl_part%num_4_subdomain(nproc) )
      allocate( itl_part%num_inter_sub(nproc) )
      allocate( itl_part%istack_4_subdomain(0:nproc) )
      allocate( itl_part%istack_inter_sub(0:nproc) )
      itl_part%num_4_subdomain =    0
      itl_part%num_inter_sub =      0
      itl_part%istack_4_subdomain = 0
      itl_part%istack_inter_sub =   0
      itl_part%nmin_inter_sub =     0
      itl_part%nmax_inter_sub =     0
      itl_part%nmin_sub =           0
      itl_part%nmax_sub =           0
!
      end subroutine alloc_numbers_4_part
!
!   --------------------------------------------------------------------
!
      subroutine alloc_internal_4_part(itl_part)
!
      type(internal_4_partitioner), intent(inout) :: itl_part
!
      allocate( itl_part%id_inter_subdomain(itl_part%ntot_inter_sub) )
      if(itl_part%ntot_inter_sub.gt.0) itl_part%id_inter_subdomain = 0
!
      end subroutine alloc_internal_4_part
!
!   --------------------------------------------------------------------
!
      subroutine alloc_id_4_subdomain(itl_part)
!
      type(internal_4_partitioner), intent(inout) :: itl_part
!
      allocate( itl_part%id_4_subdomain(itl_part%ntot_sub) )
      if(itl_part%ntot_sub.gt.0) itl_part%id_4_subdomain = 0
!
      end subroutine alloc_id_4_subdomain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_internal_4_part(itl_part)
!
      type(internal_4_partitioner), intent(inout) :: itl_part
!
!
      deallocate( itl_part%num_inter_sub )
      deallocate( itl_part%istack_inter_sub )
      deallocate( itl_part%id_inter_subdomain )
!
      end subroutine dealloc_internal_4_part
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_num_4_subdomain(itl_part)
!
      type(internal_4_partitioner), intent(inout) :: itl_part
!
      deallocate( itl_part%num_4_subdomain )
      deallocate( itl_part%istack_4_subdomain )
!
      end subroutine dealloc_num_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_id_4_subdomain(itl_part)
!
      type(internal_4_partitioner), intent(inout) :: itl_part
!
      deallocate( itl_part%id_4_subdomain )
!
      end subroutine dealloc_id_4_subdomain
!
!   --------------------------------------------------------------------
!
      end module t_internal_4_partitioner
