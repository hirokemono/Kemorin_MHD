!
!      module m_l_filtering_data
!
!      Written by H. Matsui
!
      module m_l_filtering_data
!
      use m_precision
!
      implicit none
!
      integer (kind = kint) :: ndepth_l, num_filter_l
!
      integer (kind = kint) :: ntot_l_filter
      integer (kind = kint) :: num_l_filter(3)
!
      integer (kind = kint) :: nmax_l_filter(3)
      integer (kind = kint) :: nmin_l_filter(3)
!
      integer (kind = kint), allocatable :: inod_l_filter(:,:)
!        target local node ID for filtering
      integer (kind = kint), allocatable :: istack_l_filter(:,:)
!        stack data for filtering
      integer (kind = kint), allocatable :: item_l_filter(:,:)
!        node information for filtering
!
      real (kind = kreal), allocatable :: coef_l_filter(:,:)
!        coefficients for filtering
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_l_filtering_data(numnod)
!
      integer (kind = kint) :: numnod
!
      ntot_l_filter = max( num_l_filter(1), num_l_filter(2) )
      ntot_l_filter = max( ntot_l_filter,   num_l_filter(3) )
!
      allocate( inod_l_filter(numnod,3) )
      allocate( istack_l_filter(0:numnod,3) )
      allocate( item_l_filter(ntot_l_filter,3) )
!
      allocate( coef_l_filter(ntot_l_filter,3) )
!
      inod_l_filter = 0
      istack_l_filter = 0
      item_l_filter = 0
!
      coef_l_filter = 0.0d0
!
      end subroutine allocate_l_filtering_data
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_l_filtering_data
!
      deallocate( inod_l_filter )
      deallocate( istack_l_filter )
      deallocate( item_l_filter   )
      deallocate( coef_l_filter   )
!
      end subroutine deallocate_l_filtering_data
!
!  ---------------------------------------------------------------------
!
      subroutine check_istack_l_filter(numnod, my_rank)
!
      integer (kind = kint), intent(in) :: numnod, my_rank
      integer (kind = kint) :: inod, nd
!
      write(50+my_rank,*) 'nd, inod, istack_l_filter(inod, nd)'
      do inod = 1, numnod
       do nd = 1, 3
         write(50+my_rank,*) nd, inod, istack_l_filter(inod, nd)
       end do
      end do
!
      end subroutine check_istack_l_filter
!
!  ---------------------------------------------------------------------
!
      end module m_l_filtering_data
