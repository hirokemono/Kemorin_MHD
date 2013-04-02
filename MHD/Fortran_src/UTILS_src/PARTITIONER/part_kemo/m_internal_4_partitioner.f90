!m_internal_4_partitioner.f90
!      module m_internal_4_partitioner
!
      module m_internal_4_partitioner
!
!      Written by H. Matsui on Aug., 2007
!
      use m_precision
!
      implicit none
!
!     internal nodes for each subdomains
!
      integer(kind = kint) :: ntot_intnod_sub
      integer(kind = kint) :: nmax_intnod_sub, nmin_intnod_sub
      integer(kind = kint), allocatable :: num_intnod_sub(:)
      integer(kind = kint), allocatable :: istack_intnod_sub(:)
      integer(kind = kint), allocatable :: inod_intnod_sub(:)
!
      integer(kind = kint) :: ntot_numnod_sub
      integer(kind = kint) :: nmax_numnod_sub, nmin_numnod_sub
      integer(kind = kint), allocatable :: numnod_4_subdomain(:)
      integer(kind = kint), allocatable :: istack_numnod_sub(:)
      integer(kind = kint), allocatable :: inod_4_subdomain(:)
!
!     internal elements for each subdomains
!
      integer(kind = kint) :: ntot_intele_sub
      integer(kind = kint) :: nmax_intele_sub, nmin_intele_sub
      integer(kind = kint), allocatable :: num_intele_sub(:)
      integer(kind = kint), allocatable :: istack_intele_sub(:)
      integer(kind = kint), allocatable :: iele_intele_sub(:)
!
      integer(kind = kint) :: ntot_numele_sub
      integer(kind = kint) :: nmax_numele_sub, nmin_numele_sub
      integer(kind = kint), allocatable :: numele_4_subdomain(:)
      integer(kind = kint), allocatable :: istack_numele_sub(:)
      integer(kind = kint), allocatable :: iele_4_subdomain(:)
!
!     internal surfaces for each subdomains
!
      integer(kind = kint) :: ntot_intsurf_sub
      integer(kind = kint) :: nmax_intsurf_sub, nmin_intsurf_sub
      integer(kind = kint), allocatable :: num_intsurf_sub(:)
      integer(kind = kint), allocatable :: istack_intsurf_sub(:)
      integer(kind = kint), allocatable :: isurf_intsurf_sub(:)
!
      integer(kind = kint) :: ntot_numsurf_sub
      integer(kind = kint) :: nmax_numsurf_sub, nmin_numsurf_sub
      integer(kind = kint), allocatable :: numsurf_4_subdomain(:)
      integer(kind = kint), allocatable :: istack_numsurf_sub(:)
      integer(kind = kint), allocatable :: isurf_4_subdomain(:)
!
!     internal edges for each subdomains
!
      integer(kind = kint) :: ntot_intedge_sub
      integer(kind = kint) :: nmax_intedge_sub, nmin_intedge_sub
      integer(kind = kint), allocatable :: num_intedge_sub(:)
      integer(kind = kint), allocatable :: istack_intedge_sub(:)
      integer(kind = kint), allocatable :: iedge_intedge_sub(:)
!
      integer(kind = kint) :: ntot_numedge_sub
      integer(kind = kint) :: nmax_numedge_sub, nmin_numedge_sub
      integer(kind = kint), allocatable :: numedge_4_subdomain(:)
      integer(kind = kint), allocatable :: istack_numedge_sub(:)
      integer(kind = kint), allocatable :: iedge_4_subdomain(:)
!
!      subroutine allocate_num_internod_4_part(nproc)
!      subroutine allocate_internod_4_part
!      subroutine allocate_inod_4_subdomain
!      subroutine allocate_num_interele_4_part(nproc)
!      subroutine allocate_interele_4_part
!      subroutine allocate_iele_4_subdomain
!      subroutine allocate_num_intersurf_4_part(nproc)
!      subroutine allocate_intersurf_4_part
!      subroutine allocate_isurf_4_subdomain
!      subroutine allocate_num_interedge_4_part(nproc)
!      subroutine allocate_interedge_4_part
!      subroutine allocate_iedge_4_subdomain
!
!      subroutine deallocate_internod_4_part
!      subroutine deallocate_nodes_4_subdomain
!      subroutine deallocate_nnod_4_subdomain
!      subroutine deallocate_inod_4_subdomain
!      subroutine deallocate_interele_4_part
!      subroutine deallocate_iele_4_subdomain
!      subroutine deallocate_intersurf_4_part
!      subroutine deallocate_interedge_4_part
!      subroutine deallocate_iedge_4_subdomain
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_num_internod_4_part(nproc)
!
      integer(kind = kint), intent(in) :: nproc
!
      allocate( numnod_4_subdomain(nproc) )
      allocate( num_intnod_sub(nproc) )
      allocate( istack_numnod_sub(0:nproc) )
      allocate( istack_intnod_sub(0:nproc) )
      numnod_4_subdomain = 0
      num_intnod_sub =     0
      istack_numnod_sub =  0
      istack_intnod_sub =  0
      nmax_intnod_sub =    0
      nmin_intnod_sub =    0
      nmax_numnod_sub =    0
      nmin_numnod_sub =    0
!
      end subroutine allocate_num_internod_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_internod_4_part
!
      allocate( inod_intnod_sub(ntot_intnod_sub) )
      inod_intnod_sub = 0
!
      end subroutine allocate_internod_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_inod_4_subdomain
!
      allocate( inod_4_subdomain(ntot_numnod_sub) )
      inod_4_subdomain = 0
!
      end subroutine allocate_inod_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine allocate_num_interele_4_part(nproc)
!
      integer(kind = kint), intent(in) :: nproc
!
      allocate( num_intele_sub(nproc) )
      allocate( numele_4_subdomain(nproc) )
      allocate( istack_intele_sub(0:nproc) )
      allocate( istack_numele_sub(0:nproc) )
      num_intele_sub =     0
      istack_intele_sub =  0
      nmax_intele_sub =    0
      nmin_intele_sub =    0
      numele_4_subdomain = 0
      istack_numele_sub =  0
      nmax_numele_sub =    0
      nmin_numele_sub =    0
!
      end subroutine allocate_num_interele_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_interele_4_part
!
      allocate( iele_intele_sub(ntot_intele_sub) )
      iele_intele_sub = 0
!
      end subroutine allocate_interele_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_iele_4_subdomain
!
      allocate( iele_4_subdomain(ntot_numele_sub) )
      iele_4_subdomain = 0
!
      end subroutine allocate_iele_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine allocate_num_intersurf_4_part(nproc)
!
      integer(kind = kint), intent(in) :: nproc
!
      allocate( num_intsurf_sub(nproc) )
      allocate( numsurf_4_subdomain(nproc) )
      allocate( istack_intsurf_sub(0:nproc) )
      allocate( istack_numsurf_sub(0:nproc) )
      num_intsurf_sub =     0
      istack_intsurf_sub =  0
      nmax_intsurf_sub =    0
      nmin_intsurf_sub =    0
      numsurf_4_subdomain = 0
      istack_numsurf_sub =  0
      nmax_numsurf_sub =    0
      nmin_numsurf_sub =    0
!
      end subroutine allocate_num_intersurf_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_intersurf_4_part
!
      allocate( isurf_intsurf_sub(ntot_intsurf_sub) )
      isurf_intsurf_sub = 0
!
      end subroutine allocate_intersurf_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_isurf_4_subdomain
!
      allocate( isurf_4_subdomain(ntot_numsurf_sub) )
      isurf_4_subdomain = 0
!
      end subroutine allocate_isurf_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine allocate_num_interedge_4_part(nproc)
!
      integer(kind = kint), intent(in) :: nproc
!
      allocate( num_intedge_sub(nproc) )
      allocate( numedge_4_subdomain(nproc) )
      allocate( istack_intedge_sub(0:nproc) )
      allocate( istack_numedge_sub(0:nproc) )
      num_intedge_sub =     0
      istack_intedge_sub =  0
      nmax_intedge_sub =    0
      nmin_intedge_sub =    0
      numedge_4_subdomain = 0
      istack_numedge_sub =  0
      nmax_numedge_sub =    0
      nmin_numedge_sub =    0
!
      end subroutine allocate_num_interedge_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_interedge_4_part
!
      allocate( iedge_intedge_sub(ntot_intedge_sub) )
      iedge_intedge_sub = 0
!
      end subroutine allocate_interedge_4_part
!
!   --------------------------------------------------------------------
!
      subroutine allocate_iedge_4_subdomain
!
      allocate( iedge_4_subdomain(ntot_numedge_sub) )
      iedge_4_subdomain = 0
!
      end subroutine allocate_iedge_4_subdomain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_internod_4_part
!
      deallocate( num_intnod_sub )
      deallocate( istack_intnod_sub )
      deallocate( inod_intnod_sub )
!
      end subroutine deallocate_internod_4_part
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_nodes_4_subdomain
!
      call deallocate_nnod_4_subdomain
      call deallocate_inod_4_subdomain
!
      end subroutine deallocate_nodes_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_nnod_4_subdomain
!
      deallocate( numnod_4_subdomain )
      deallocate( istack_numnod_sub )
!
      end subroutine deallocate_nnod_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_inod_4_subdomain
!
      deallocate( inod_4_subdomain )
!
      end subroutine deallocate_inod_4_subdomain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_interele_4_part
!
      deallocate( num_intele_sub )
      deallocate( istack_intele_sub )
      deallocate( iele_intele_sub )
!
      end subroutine deallocate_interele_4_part
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_iele_4_subdomain
!
      deallocate( numele_4_subdomain )
      deallocate( istack_numele_sub )
      deallocate( iele_4_subdomain )
!
      end subroutine deallocate_iele_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_intersurf_4_part
!
      deallocate( num_intsurf_sub )
      deallocate( istack_intsurf_sub )
      deallocate( isurf_intsurf_sub )
!
      end subroutine deallocate_intersurf_4_part
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_isurf_4_subdomain
!
      deallocate( numsurf_4_subdomain )
      deallocate( istack_numsurf_sub )
      deallocate( isurf_4_subdomain )
!
      end subroutine deallocate_isurf_4_subdomain
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_interedge_4_part
!
      deallocate( num_intedge_sub )
      deallocate( istack_intedge_sub )
      deallocate( iedge_intedge_sub )
!
      end subroutine deallocate_interedge_4_part
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_iedge_4_subdomain
!
      deallocate( numedge_4_subdomain )
      deallocate( istack_numedge_sub )
      deallocate( iedge_4_subdomain )
!
      end subroutine deallocate_iedge_4_subdomain
!
!   --------------------------------------------------------------------
!
      end module m_internal_4_partitioner
