!
!      module m_int_commtative_filter
!
!      Written by Kemorin
!
!!      subroutine allocate_int_commute_filter(numnod_z)
!!      subroutine deallocate_int_commute_filter
!!
!!      subroutine check_int_commutative_filter(id_rank, numnod_z)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: numnod_z
!!        integer(kind = kint), intent(in) ::  numele_z
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_int_commute_filter(numnod_z)
!
      integer(kind = kint), intent(in) :: numnod_z
!
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
      end module m_int_commtative_filter
