!
!      module m_int_commtative_filter
!
!      Written by Kemorin
!
!      subroutine allocate_int_commute_filter(numnod_z, numele_z)
!      subroutine deallocate_int_commute_filter
!
!      subroutine check_int_commutative_filter(id_rank, numnod_z)
!      subroutine check_neib_nod_2nd(id_rank, numnod_z)
!      subroutine check_neib_ele_2nd(id_rank, numele_z)
!
      module m_int_commtative_filter
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: nside, ndep_filter
!
      real(kind = kreal), allocatable :: c_filter(:,:)
!
      real(kind = kreal), allocatable :: xmom_h_x(:,:)
      real(kind = kreal), allocatable :: xmom_h_y(:,:)
      real(kind = kreal), allocatable :: xmom_ht_x(:)
      real(kind = kreal), allocatable :: xmom_ht_y(:)
      real(kind = kreal), allocatable :: xmom_ht_z(:)
!
      real(kind = kreal), allocatable :: xmom_int_org(:,:,:)
      real(kind = kreal), allocatable :: xmom_int(:,:,:)
!
      real(kind = kreal), allocatable :: xmom_int_t(:,:)
      real(kind = kreal), allocatable :: xmom_int_to(:,:)
!
      real(kind = kreal), allocatable :: xmom_dt(:,:)
      real(kind = kreal), allocatable :: xmom_dot(:,:)
!
      integer(kind = kint), allocatable :: nneib_nod2(:,:)
      integer(kind = kint), allocatable :: ineib_nod2(:,:,:)
!
      integer(kind = kint), allocatable :: nneib_ele2(:,:)
      integer(kind = kint), allocatable :: ineib_ele2(:,:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_int_commute_filter(numnod_z, numele_z)
!
      integer(kind = kint) :: numnod_z, numele_z
!
      nside = (ndep_filter-1)/2
!
      allocate( c_filter(ndep_filter,numnod_z) )
!
      allocate( xmom_h_x(ndep_filter,0:2) )
      allocate( xmom_h_y(ndep_filter,0:2) )
      allocate( xmom_ht_x(0:3) )
      allocate( xmom_ht_y(0:3) )
      allocate( xmom_ht_z(0:2) )
!
      allocate( xmom_int_org(numnod_z,ndep_filter,0:2) )
      allocate( xmom_int(numnod_z,ndep_filter,0:2) )
      allocate( xmom_int_t(numnod_z,0:2) )
      allocate( xmom_int_to(numnod_z,0:2) )
      allocate( xmom_dt(numnod_z,0:2) )
      allocate( xmom_dot(numnod_z,0:2) )
!
      allocate( nneib_nod2(numnod_z,2) )
      allocate( ineib_nod2(numnod_z,ndep_filter,2) )
!
      allocate( nneib_ele2(numele_z,2) )
      allocate( ineib_ele2(numele_z,ndep_filter,2) )
!
      c_filter = 0.0d0
!
      xmom_h_x = 0.0d0
      xmom_h_y = 0.0d0
      xmom_ht_x = 0.0d0
      xmom_ht_y = 0.0d0
      xmom_ht_z = 0.0d0
!
      xmom_int_org = 0.0d0
      xmom_int = 0.0d0
      xmom_int_t = 0.0d0
      xmom_int_to = 0.0d0
      xmom_dt = 0.0d0
      xmom_dot = 0.0d0
!
      nneib_nod2 = nside
      ineib_nod2 = -1
!
      nneib_ele2 = nside
      ineib_ele2 = -1
!
      end subroutine allocate_int_commute_filter
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_int_commute_filter
!
!
      deallocate( c_filter )
!
      deallocate( xmom_h_x )
      deallocate( xmom_h_y )
      deallocate( xmom_ht_x )
      deallocate( xmom_ht_y )
      deallocate( xmom_ht_z )
!
      deallocate( xmom_int_org )
      deallocate( xmom_int )
      deallocate( xmom_int_t )
      deallocate( xmom_int_to )
      deallocate( xmom_dt )
      deallocate( xmom_dot )
!
      deallocate( nneib_nod2 )
      deallocate( ineib_nod2 )
!
      deallocate( nneib_ele2 )
      deallocate( ineib_ele2 )
!
      end subroutine deallocate_int_commute_filter
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_int_commutative_filter(id_rank, numnod_z)
!
!
      integer :: id_rank
      integer(kind = kint) :: numnod_z
      integer(kind = kint) :: i, j, k
!
       do i = 1, numnod_z
        write(id_rank+30,*)  'c_filter (inod) = ',i
        write(id_rank+30,'(1p5e16.8)')                                  &
     &              (c_filter(j,i),j=1,ndep_filter)
       end do
!
      do k = 0, 2
       do i = 1, numnod_z
        write(id_rank+30,*)  'xmom_int_org (order, inod) = ', k, i
        write(id_rank+30,'(1p5e16.8)')                                  &
     &              (xmom_int_org(i,j,k),j=1,ndep_filter)
       end do
      end do
!
      do k = 0, 2
       do i = 1, numnod_z
        write(id_rank+30,*)  'xmom_int (order, inod) = ', k, i
        write(id_rank+30,'(1p5e16.8)')                                  &
     &              (xmom_int(i,j,k),j=1,ndep_filter)
       end do
      end do
!
        write(id_rank+30,*)  'xmom_int_t '
       do i = 1, numnod_z
        write(id_rank+30,'(1p5e16.8)')                                  &
     &              (xmom_int_t(i,k),k=0,2)
       end do
!
      end subroutine check_int_commutative_filter
!
!  ---------------------------------------------------------------------
!
      subroutine check_neib_nod_2nd(id_rank, numnod_z)
!
      use m_commute_filter_z
!
      integer :: id_rank
      integer(kind = kint) :: numnod_z
      integer(kind = kint) :: i, j, k, i1
!
      write(50+id_rank,*) 'nneib_nod2'
      write(50+id_rank,'(10i16)') (nneib_nod2(i,1),i=1,numnod_z)
      write(50+id_rank,'(10i16)') (nneib_nod2(i,2),i=1,numnod_z)
      write(50+id_rank,*) 'direction, inod, ineib_nod2'
      do k = 1, 2
        do i = 1, numnod_z
          write(50+id_rank,'(10i16)') k, i,                             &
     &            (ineib_nod2(i,j,k),j=1,nneib_nod2(i,k))
        end do
      end do
!
      end subroutine check_neib_nod_2nd
!
!-----------------------------------------------------------------------
!
      subroutine check_neib_ele_2nd(id_rank, numele_z)
!
      use m_commute_filter_z
!
      integer :: id_rank
      integer(kind = kint) ::  numele_z
      integer(kind = kint) :: i, j, k, i1
!
      write(50+id_rank,*) 'nneib_ele2'
      write(50+id_rank,'(10i16)') (nneib_ele2(i,1),i=1,numele_z)
      write(50+id_rank,'(10i16)') (nneib_ele2(i,2),i=1,numele_z)
      write(50+id_rank,*) 'direction, iele, ineib_ele2'
      do k = 1, 2
        do i = 1, numele_z
          write(50+id_rank,'(10i16)') k, i,                             &
     &            (ineib_ele2(i,j,k),j=1,nneib_ele2(i,k))
        end do
      end do
!
      end subroutine check_neib_ele_2nd
!
!-----------------------------------------------------------------------
!
      end module m_int_commtative_filter
