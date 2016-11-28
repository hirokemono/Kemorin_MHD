!
!      module ordering_line_filter_smp
!
!     Written by H. Matsui in 2004
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine ordering_l_filter_smp(numnod, inod_smp_stack)
!
      module ordering_line_filter_smp
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine ordering_l_filter_smp(numnod, inod_smp_stack)
!
      use m_machine_parameter
      use m_l_filtering_data
      use m_l_filtering_data_smp
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer (kind = kint) :: inod, inod0, inod1
      integer (kind = kint) :: ist, ied, nd, ii
      integer (kind = kint) :: isum, idx, jdx, inum, ip, j0, i, j
!
!
      nsize_lf_smp = 0
      do nd = 1, 3
        nsize_lf_smp = max(nsize_lf_smp,fil_l1%nmax_lf(nd))
      end do
!
      call allocate_l_filtering_tmp(numnod)
      call allocate_l_filtering_smp(numnod)
!
!
!    cyclic ordering
!
      do inod0 = 1, numnod
        ip =     mod(inod0-1,np_smp) + 1
        inod1 = (inod0 - ip) / np_smp + 1
        inod  = inod_smp_stack(ip-1) + inod1
        n2o_cyclic_l(inod) = inod0
        do nd = 1, 3
          inod_l_filter_tmp(inod,nd) = fil_l1%inod_lf(inod0,nd)
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
           inum = fil_l1%istack_lf(inod0,nd)                            &
     &            - fil_l1%istack_lf(inod0-1,nd)
           istack_l_filter_tmp(inod,nd)                                 &
     &            = istack_l_filter_tmp(inod-1,nd) + inum
           do j0 = 1, inum
            j = istack_l_filter_tmp(inod-1,nd) + j0
            i = fil_l1%istack_lf(inod0-1,nd) + j0
            item_l_filter_tmp(j,nd) = fil_l1%item_lf(i,nd)
            c_l_filter_tmp(j,nd) =    fil_l1%coef_l(i,nd)
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
          do inod = 1, numnod
            inod_l_filter_smp(inod,nd) = inod_l_filter_tmp(inod,nd)
            fil_l1%inod_lf(inod,nd) =   inod_l_filter_tmp(inod,nd)
            fil_l1%istack_lf(inod,nd) = istack_l_filter_tmp(inod,nd)
          end do
          do i =1, fil_l1%num_lf(nd)
            fil_l1%item_lf(i,nd) = item_l_filter_tmp(i,nd)
            fil_l1%coef_l(i,nd) =  c_l_filter_tmp(i,nd)
          end do
        end do
      end do
!
!     call check_istack_l_filter(numnod, my_rank, fil_l1)
!
      call deallocate_l_filtering_tmp
!
!     count number of node for summuation
!
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do nd = 1, 3
          do inod = ist, ied
            inum = fil_l1%istack_lf(inod,nd)                            &
     &            - fil_l1%istack_lf(inod-1,nd)
            do i = 1, inum
              num_4_lf_smp(i,ip,nd) = num_4_lf_smp(i,ip,nd) + 1
            end do
          end do
        end do
      end do
!
!     call check_num_4_lf_smp(my_rank, inod_smp_stack)
!
      istack_l_filter_smp(0,1) = 0
      istack_l_filter_smp(0,2) = 0
      istack_l_filter_smp(0,3) = 0
      do ip = 1, np_smp
        do nd = 1, 3
          do isum = 1, fil_l1%nmax_lf(nd)
            ii = (ip-1)*nsize_lf_smp + isum
            istack_l_filter_smp(ii,nd) = istack_l_filter_smp(ii-1,nd)   &
     &                                  + num_4_lf_smp(isum,ip,nd)
          end do
          do isum = fil_l1%nmax_lf(nd)+1, nsize_lf_smp
            ii = (ip-1)*nsize_lf_smp + isum
            istack_l_filter_smp(ii,nd) = istack_l_filter_smp(ii-1,nd)
          end do
        end do
      end do
!
!      call check_istack_l_filter_smp(my_rank, inod_smp_stack)
!
      do ip = 1, np_smp
        do nd = 1, 3
          do isum = 1, fil_l1%nmax_lf(nd)
            do i = 1, num_4_lf_smp(isum,ip,nd)
              inod = inod_smp_stack(ip-1) + i
              ii = (ip-1)*nsize_lf_smp + isum
              jdx = fil_l1%istack_lf(inod-1,nd) + isum
              idx = istack_l_filter_smp(ii-1,nd) + i
              item_l_filter_smp(idx,nd) = fil_l1%item_lf(jdx,nd)
              c_l_filter_smp(idx,nd) =    fil_l1%coef_l(jdx,nd)
            end do
          end do
        end do
      end do
!
      call dealloc_l_filtering_data(fil_l1)
!
      end subroutine ordering_l_filter_smp
!
! ----------------------------------------------------------------------
!
      end module ordering_line_filter_smp
