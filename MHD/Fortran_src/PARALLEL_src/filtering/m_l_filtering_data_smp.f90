!
!      module m_l_filtering_data_smp
!
!      Written by H. Matsui
!
!      subroutine allocate_l_filtering_smp(numnod)
!      subroutine allocate_l_filtering_tmp(numnod)
!      subroutine deallocate_l_filtering_smp
!      subroutine deallocate_l_filtering_tmp
!
!      subroutine check_num_4_lf_smp(my_rank, inod_smp_stack)
!      subroutine check_istack_l_filter_smp(my_rank, inod_smp_stack)
!
      module m_l_filtering_data_smp
!
      use m_precision
!
      implicit none
!
!
      integer (kind = kint) :: nsize_lf_smp
!
      integer (kind = kint), allocatable :: num_4_lf_smp(:,:,:)
!        start item ID for filtering for summuation
!
      integer (kind = kint), allocatable :: inod_l_filter_smp(:,:)
!        target local node ID for filtering
      integer (kind = kint), allocatable :: istack_l_filter_smp(:,:)
!        stack data for filtering
      integer (kind = kint), allocatable :: item_l_filter_smp(:,:)
!        node information for filtering
!
      real (kind = kreal), allocatable :: c_l_filter_smp(:,:)
!        coefficients for filtering
!
      integer (kind = kint), allocatable :: inod_l_filter_tmp(:,:)
!        target local node ID for filtering
      integer (kind = kint), allocatable :: istack_l_filter_tmp(:,:)
!        stack data for filtering
      integer (kind = kint), allocatable :: item_l_filter_tmp(:,:)
!        node information for filtering
      real (kind = kreal), allocatable :: c_l_filter_tmp(:,:)
!        coefficients for filtering
!
      integer (kind = kint), allocatable :: n2o_cyclic_l(:)
!        node information for filtering
!
!
      real (kind=kreal), allocatable :: ff_lf_smp(:,:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_l_filtering_smp(numnod)
!
      use m_machine_parameter
      use m_l_filtering_data
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate( inod_l_filter_smp(numnod,3) )

      allocate( num_4_lf_smp(nsize_lf_smp,np_smp,3) )
      allocate( istack_l_filter_smp(0:nsize_lf_smp*np_smp,3) )
      allocate( item_l_filter_smp(ntot_l_filter,3) )
!
      allocate( c_l_filter_smp(ntot_l_filter,3) )
      allocate( ff_lf_smp(numnod,6) )
!
      num_4_lf_smp = 0
!
      inod_l_filter_smp = 0
      istack_l_filter_smp = 0
      item_l_filter_smp = 0
!
      c_l_filter_smp = 0.0d0
      ff_lf_smp = 0.0d0
!
      end subroutine allocate_l_filtering_smp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_l_filtering_tmp(numnod)
!
      use m_machine_parameter
      use m_l_filtering_data
!
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate( inod_l_filter_tmp(numnod,3) )
      allocate( istack_l_filter_tmp(0:numnod,3) )
      allocate( item_l_filter_tmp(ntot_l_filter,3) )
!
      allocate( c_l_filter_tmp(ntot_l_filter,3) )
!
      allocate( n2o_cyclic_l(numnod) )
!
      inod_l_filter_tmp = 0
      istack_l_filter_tmp = 0
      item_l_filter_tmp = 0
      n2o_cyclic_l = 0
!
      c_l_filter_tmp = 0.0d0
!
      end subroutine allocate_l_filtering_tmp
!
!  ---------------------------------------------------------------------
!
!
      subroutine deallocate_l_filtering_smp
!
      deallocate( inod_l_filter_smp )
      deallocate( num_4_lf_smp )
      deallocate( istack_l_filter_smp )
      deallocate( item_l_filter_smp )
      deallocate( c_l_filter_smp )
!
      end subroutine deallocate_l_filtering_smp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_l_filtering_tmp
!
      deallocate( inod_l_filter_tmp )
      deallocate( istack_l_filter_tmp )
      deallocate( item_l_filter_tmp )
      deallocate( c_l_filter_tmp )
!
      deallocate( n2o_cyclic_l )
!
      end subroutine deallocate_l_filtering_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine check_num_4_lf_smp(my_rank, inod_smp_stack)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint) :: ist, ied, nd, ip, i
!
      write(50+my_rank,*) 'nd, ip, i, num_4_lf_smp(i,ip,nd)'
      do ip = 1, np_smp
       ist = inod_smp_stack(ip-1) + 1
       ied = inod_smp_stack(ip)
       do nd = 1, 3
        do i = 1, nsize_lf_smp
          write(50+my_rank,*) nd, ip, i, num_4_lf_smp(i,ip,nd)
        end do
       end do
      end do
!
      end subroutine check_num_4_lf_smp
!
!  ---------------------------------------------------------------------
!
      subroutine check_istack_l_filter_smp(my_rank, inod_smp_stack)
!
      use m_machine_parameter
      use m_l_filtering_data
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint) :: ist, ied, nd, ip, isum, ii
!
      write(50+my_rank,*) 'nd, ip, isum, istack_l_filter_smp(i,ip,nd)'
      do ip = 1, np_smp
       ist = inod_smp_stack(ip-1) + 1
       ied = inod_smp_stack(ip)
       do nd = 1, 3
        do isum = 1, nmax_l_filter(nd)
          ii = (ip-1)*nsize_lf_smp + isum
          write(50+my_rank,*) ip, nd, isum, istack_l_filter_smp(ii,nd)
        end do
       end do
      end do
!
!
      end subroutine check_istack_l_filter_smp
!
!  ---------------------------------------------------------------------
!
      end module m_l_filtering_data_smp
