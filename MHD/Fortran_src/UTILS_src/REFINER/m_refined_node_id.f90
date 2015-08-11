!m_refined_node_id.f90
!      module m_refined_node_id
!
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine allocate_num_refine_node                             &
!!     &         (numnod, numele, numsurf, numedge)
!!      subroutine allocate_item_refine_node
!!
!!      subroutine deallocate_refine_node_id
!!      subroutine deallocate_num_refine_node
!!      subroutine deallocate_refined_local_posi
!!      subroutine deallocate_refined_xyz
!!      subroutine deallocate_refined_sph
!!
!!      subroutine check_refine_items(numnod, numele, numsurf, numedge)
!
      module m_refined_node_id
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), allocatable :: num_nod_refine_nod(:)
      integer(kind = kint), allocatable :: num_nod_refine_ele(:)
      integer(kind = kint), allocatable :: num_nod_refine_surf(:)
      integer(kind = kint), allocatable :: num_nod_refine_edge(:)
!
      integer(kind = kint), allocatable :: istack_nod_refine_nod(:)
      integer(kind = kint), allocatable :: istack_nod_refine_ele(:)
      integer(kind = kint), allocatable :: istack_nod_refine_surf(:)
      integer(kind = kint), allocatable :: istack_nod_refine_edge(:)
!
      integer(kind = kint) :: ntot_nod_refine_nod
      integer(kind = kint) :: ntot_nod_refine_ele
      integer(kind = kint) :: ntot_nod_refine_surf
      integer(kind = kint) :: ntot_nod_refine_edge
!
      integer(kind = kint) :: nmax_nod_refine_nod
      integer(kind = kint) :: nmax_nod_refine_ele
      integer(kind = kint) :: nmax_nod_refine_surf
      integer(kind = kint) :: nmax_nod_refine_edge
      integer(kind = kint) :: nmin_nod_refine_nod
      integer(kind = kint) :: nmin_nod_refine_ele
      integer(kind = kint) :: nmin_nod_refine_surf
      integer(kind = kint) :: nmin_nod_refine_edge
!
      integer(kind = kint), allocatable :: iele_refine_nod(:)
      integer(kind = kint), allocatable :: inod_refine_nod(:)
!
      integer(kind = kint), allocatable :: inod_refine_ele(:)
      integer(kind = kint), allocatable :: inod_refine_surf(:)
      integer(kind = kint), allocatable :: inod_refine_edge(:)
!
      real(kind = kreal), allocatable :: xi_refine_nod(:,:)
      real(kind = kreal), allocatable :: xi_refine_ele(:,:)
      real(kind = kreal), allocatable :: xi_refine_surf(:,:)
      real(kind = kreal), allocatable :: xi_refine_edge(:,:)
!
      real(kind = kreal), allocatable :: x_refine_ele(:,:)
      real(kind = kreal), allocatable :: x_refine_surf(:,:)
      real(kind = kreal), allocatable :: x_refine_edge(:,:)
!
      real(kind = kreal), allocatable :: sph_refine_ele(:,:)
      real(kind = kreal), allocatable :: sph_refine_surf(:,:)
      real(kind = kreal), allocatable :: sph_refine_edge(:,:)
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_refine_node                               &
     &         (numnod, numele, numsurf, numedge)
!
      integer(kind = kint), intent(in) :: numnod,  numele
      integer(kind = kint), intent(in) :: numsurf, numedge
!
!
      allocate( num_nod_refine_nod(numnod)   )
      allocate( num_nod_refine_ele(numele)   )
      allocate( num_nod_refine_surf(numsurf) )
      allocate( num_nod_refine_edge(numedge) )
      allocate( istack_nod_refine_nod(0:numnod)   )
      allocate( istack_nod_refine_ele(0:numele)   )
      allocate( istack_nod_refine_surf(0:numsurf) )
      allocate( istack_nod_refine_edge(0:numedge) )
!
      num_nod_refine_nod =  0
      num_nod_refine_ele =  0
      num_nod_refine_surf = 0
      num_nod_refine_edge = 0
!
      istack_nod_refine_nod =  -1
      istack_nod_refine_ele =  -1
      istack_nod_refine_surf = -1
      istack_nod_refine_edge = -1
!
      ntot_nod_refine_nod =  0
      ntot_nod_refine_ele =  0
      ntot_nod_refine_surf = 0
      ntot_nod_refine_edge = 0
!
      end subroutine allocate_num_refine_node
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_item_refine_node
!
      allocate( iele_refine_nod(ntot_nod_refine_nod)   )
      allocate( inod_refine_nod(ntot_nod_refine_nod)   )
!
      allocate( inod_refine_ele(ntot_nod_refine_ele)   )
      allocate( inod_refine_surf(ntot_nod_refine_surf) )
      allocate( inod_refine_edge(ntot_nod_refine_edge) )
!
      allocate( xi_refine_nod(ntot_nod_refine_nod,3)   )
      allocate( xi_refine_ele(ntot_nod_refine_ele,3)   )
      allocate( xi_refine_surf(ntot_nod_refine_surf,2) )
      allocate( xi_refine_edge(ntot_nod_refine_edge,1) )
!
      allocate( x_refine_ele(ntot_nod_refine_ele,3)   )
      allocate( x_refine_surf(ntot_nod_refine_surf,3) )
      allocate( x_refine_edge(ntot_nod_refine_edge,3) )
!
      allocate( sph_refine_ele(ntot_nod_refine_ele,3)   )
      allocate( sph_refine_surf(ntot_nod_refine_surf,3) )
      allocate( sph_refine_edge(ntot_nod_refine_edge,3) )
!
      iele_refine_nod =  0
      inod_refine_nod =  0
      inod_refine_ele =  0
      inod_refine_surf = 0
      inod_refine_edge = 0
!
      xi_refine_nod =  0.0d0
      xi_refine_ele =  0.0d0
      xi_refine_surf = 0.0d0
      xi_refine_edge = 0.0d0
!
      x_refine_ele =  0.0d0
      x_refine_surf = 0.0d0
      x_refine_edge = 0.0d0
!
      sph_refine_ele =  0.0d0
      sph_refine_surf = 0.0d0
      sph_refine_edge = 0.0d0
!
      end subroutine allocate_item_refine_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_refine_node_id
!
!
      call deallocate_refined_xyz
      call deallocate_refined_local_posi
      call deallocate_refined_sph
!
      call deallocate_num_refine_node
!
      end subroutine deallocate_refine_node_id
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_refine_node
!
!
      deallocate( iele_refine_nod  )
      deallocate( inod_refine_nod  )
      deallocate( inod_refine_ele  )
      deallocate( inod_refine_surf )
      deallocate( inod_refine_edge )
!
      deallocate( num_nod_refine_nod   )
      deallocate( num_nod_refine_ele   )
      deallocate( num_nod_refine_surf  )
      deallocate( num_nod_refine_edge  )
      deallocate( istack_nod_refine_nod   )
      deallocate( istack_nod_refine_ele   )
      deallocate( istack_nod_refine_surf )
      deallocate( istack_nod_refine_edge )
!
      end subroutine deallocate_num_refine_node
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_refined_local_posi
!
      deallocate( xi_refine_nod  )
      deallocate( xi_refine_ele  )
      deallocate( xi_refine_surf )
      deallocate( xi_refine_edge )
!
      end subroutine deallocate_refined_local_posi
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_refined_xyz
!
      deallocate( x_refine_ele  )
      deallocate( x_refine_surf )
      deallocate( x_refine_edge )
!
      end subroutine deallocate_refined_xyz
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_refined_sph
!
      deallocate( sph_refine_ele  )
      deallocate( sph_refine_surf )
      deallocate( sph_refine_edge )
!
      end subroutine deallocate_refined_sph
!
!  ---------------------------------------------------------------------
!
      subroutine check_refine_items(numnod, numele, numsurf, numedge)
!
      integer(kind = kint), intent(in) :: numnod,  numele
      integer(kind = kint), intent(in) :: numsurf, numedge
!
!
      integer(kind = kint) :: i, j, ist, ied
!

      write(50,*) 'ntot_nod_refine_nod',  ntot_nod_refine_nod
      write(50,*) 'ntot_nod_refine_ele',  ntot_nod_refine_ele
      write(50,*) 'ntot_nod_refine_surf', ntot_nod_refine_surf
      write(50,*) 'ntot_nod_refine_edge', ntot_nod_refine_edge
!
      write(50,*) 'node at node', numnod
      write(50,*) inod_refine_nod(1:numnod)
!
      write(50,*) 'node in elements'
      do i = 1, numele
        ist = istack_nod_refine_ele(i-1) + 1
        ied = istack_nod_refine_ele(i)
        write(50,*) i, istack_nod_refine_ele(i),num_nod_refine_ele(i)
        do j = ist, ied
          write(50,*) inod_refine_ele(j), xi_refine_ele(j,1:3)
        end do
      end do
!
      write(50,*) 'node on surface'
      do i = 1, numsurf
        ist = istack_nod_refine_surf(i-1) + 1
        ied = istack_nod_refine_surf(i)
        write(50,*) i, istack_nod_refine_surf(i),num_nod_refine_surf(i)
        do j = ist, ied
          write(50,*) inod_refine_surf(j), xi_refine_surf(j,1:2)
        end do
      end do
!
      write(50,*) 'i, iflag_refine_edge(i),num_nod_refine_edge(i)'
      write(50,*) 'node on edge'
      do i= 1, numedge
        ist = istack_nod_refine_edge(i-1) + 1
        ied = istack_nod_refine_edge(i)
        write(50,*) i, istack_nod_refine_edge(i),num_nod_refine_edge(i)
        do j = ist, ied
          write(50,*) inod_refine_edge(j), xi_refine_edge(j,1)
        end do
      end do
!
      end subroutine check_refine_items
!
!  ---------------------------------------------------------------------
!
      end module m_refined_node_id
