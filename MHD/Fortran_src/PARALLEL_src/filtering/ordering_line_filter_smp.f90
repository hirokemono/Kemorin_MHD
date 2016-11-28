!
!      module ordering_line_filter_smp
!
!     Written by H. Matsui in 2004
!     Modified by H. Matsui on Oct., 2006
!
!!      subroutine ordering_l_filter_smp                                &
!!     &         (inod_smp_stack, fil_l, fil_l_smp)
!!        type(line_filtering_type), intent(inout) :: fil_l
!!        type(line_filtering_type), intent(inout) :: fil_l_smp
!
      module ordering_line_filter_smp
!
      use m_precision
      use m_machine_parameter
      use t_l_filtering_data
!
      implicit none
!
      type(line_filtering_type) :: fil_l_tmp
!
      integer (kind = kint), allocatable :: num_4_lf_smp(:,:,:)
!
!        cyclic node information for filtering
      integer (kind = kint), allocatable :: n2o_cyclic_l(:)
!
      private :: num_4_lf_smp, n2o_cyclic_l, fil_l_tmp
      private :: allocate_l_filtering_tmp, deallocate_l_filtering_tmp
      private :: check_num_4_lf_smp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_l_filtering_tmp(numnod, nsize_lf_smp)
!
      integer(kind = kint), intent(in) :: numnod, nsize_lf_smp
!
!
      allocate( n2o_cyclic_l(numnod) )
      allocate( num_4_lf_smp(nsize_lf_smp,np_smp,3) )
      n2o_cyclic_l = 0
      num_4_lf_smp = 0
!
      end subroutine allocate_l_filtering_tmp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_l_filtering_tmp
!
!
      deallocate(n2o_cyclic_l, num_4_lf_smp)
!
      end subroutine deallocate_l_filtering_tmp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ordering_l_filter_smp                                  &
     &         (inod_smp_stack, fil_l, fil_l_smp)
!
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      type(line_filtering_type), intent(inout) :: fil_l
      type(line_filtering_type), intent(inout) :: fil_l_smp
!
      integer (kind = kint) :: inod, inod0, inod1
      integer (kind = kint) :: ist, ied, nd, ii
      integer (kind = kint) :: isum, idx, jdx, inum, ip, j0, i, j
!
!
      fil_l_smp%nsize_smp = 0
      do nd = 1, 3
        fil_l_smp%nsize_smp                                             &
     &       = max(fil_l_smp%nsize_smp,fil_l%nmax_lf(nd))
      end do
!
      fil_l_tmp%num_lf(1:3) = fil_l%num_lf(1:3)
      call alloc_l_filtering_data(fil_l%nnod_lf, fil_l%ndepth_lf,       &
     &    fil_l%num_filter_l, fil_l_tmp)
      call allocate_l_filtering_tmp                                     &
     &   (fil_l%nnod_lf, fil_l_smp%nsize_smp)
!
      call alloc_l_filtering_smp(fil_l, fil_l_smp)
!
!
!    cyclic ordering
!
      do inod0 = 1, fil_l%nnod_lf
        ip =     mod(inod0-1,np_smp) + 1
        inod1 = (inod0 - ip) / np_smp + 1
        inod  = inod_smp_stack(ip-1) + inod1
        n2o_cyclic_l(inod) = inod0
        do nd = 1, 3
          fil_l_tmp%inod_lf(inod,nd) = fil_l%inod_lf(inod0,nd)
        end do
      end do
!
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!
        do inod = ist, ied
         inod0 = n2o_cyclic_l(inod)
         do nd = 1, 3
           inum = fil_l%istack_lf(inod0,nd)                             &
     &            - fil_l%istack_lf(inod0-1,nd)
           fil_l_tmp%istack_lf(inod,nd)                                 &
     &            = fil_l_tmp%istack_lf(inod-1,nd) + inum
           do j0 = 1, inum
            j = fil_l_tmp%istack_lf(inod-1,nd) + j0
            i = fil_l%istack_lf(inod0-1,nd) + j0
            fil_l_tmp%item_lf(j,nd) = fil_l%item_lf(i,nd)
            fil_l_tmp%coef_l(j,nd) =  fil_l%coef_l(i,nd)
           end do
         end do
        end do
!
      end do
!
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!
        do nd = 1, 3
          do inod = 1, fil_l%nnod_lf
            fil_l_smp%inod_lf(inod,nd) = fil_l_tmp%inod_lf(inod,nd)
            fil_l%inod_lf(inod,nd) =    fil_l_tmp%inod_lf(inod,nd)
            fil_l%istack_lf(inod,nd) =  fil_l_tmp%istack_lf(inod,nd)
          end do
          do i =1, fil_l%num_lf(nd)
            fil_l%item_lf(i,nd) = fil_l_tmp%item_lf(i,nd)
            fil_l%coef_l(i,nd) =  fil_l_tmp%coef_l(i,nd)
          end do
        end do
      end do
!
!     call check_istack_l_filter(fil_l%nnod_lf, my_rank, fil_l)
!
      call dealloc_l_filtering_data(fil_l_tmp)
!
!     count number of node for summuation
!
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do nd = 1, 3
          do inod = ist, ied
            inum = fil_l%istack_lf(inod,nd)                             &
     &            - fil_l%istack_lf(inod-1,nd)
            do i = 1, inum
              num_4_lf_smp(i,ip,nd) = num_4_lf_smp(i,ip,nd) + 1
            end do
          end do
        end do
      end do
!
!      call check_num_4_lf_smp                                          &
!     &   (my_rank, inod_smp_stack, fil_l_smp%nsize_smp)
!
      fil_l_smp%istack_lf(0,1:3) = 0
      do ip = 1, np_smp
        do nd = 1, 3
          do isum = 1, fil_l%nmax_lf(nd)
            ii = (ip-1)*fil_l_smp%nsize_smp + isum
            fil_l_smp%istack_lf(ii,nd) = fil_l_smp%istack_lf(ii-1,nd)   &
     &                                  + num_4_lf_smp(isum,ip,nd)
          end do
          do isum = fil_l%nmax_lf(nd)+1, fil_l_smp%nsize_smp
            ii = (ip-1)*fil_l_smp%nsize_smp + isum
            fil_l_smp%istack_lf(ii,nd) = fil_l_smp%istack_lf(ii-1,nd)
          end do
        end do
      end do
!
!      call check_istack_l_filter_smp                                   &
!     &   (my_rank, inod_smp_stack, fil_l_smp)
!
      do ip = 1, np_smp
        do nd = 1, 3
          do isum = 1, fil_l%nmax_lf(nd)
            do i = 1, num_4_lf_smp(isum,ip,nd)
              inod = inod_smp_stack(ip-1) + i
              ii = (ip-1)*fil_l_smp%nsize_smp + isum
              jdx = fil_l%istack_lf(inod-1,nd) + isum
              idx = fil_l_smp%istack_lf(ii-1,nd) + i
              fil_l_smp%item_lf(idx,nd) = fil_l%item_lf(jdx,nd)
              fil_l_smp%coef_l(idx,nd) =  fil_l%coef_l(jdx,nd)
            end do
          end do
        end do
      end do
!
      call deallocate_l_filtering_tmp
      call dealloc_l_filtering_data(fil_l)
!
      end subroutine ordering_l_filter_smp
!
! ----------------------------------------------------------------------
!
      subroutine check_num_4_lf_smp                                     &
     &         (my_rank, inod_smp_stack, nsize_lf_smp)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nsize_lf_smp
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
      end module ordering_line_filter_smp
