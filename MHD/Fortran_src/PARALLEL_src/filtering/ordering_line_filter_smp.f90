!
!      module ordering_line_filter_smp
!
!     Written by H. Matsui in 2004
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine ordering_l_filter_smp
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
      subroutine ordering_l_filter_smp
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_filter_elength
      use m_l_filtering_data
      use m_l_filtering_data_smp
!
      integer (kind = kint) :: inod, inod0, inod1
      integer (kind = kint) :: ist, ied, nd, ii
      integer (kind = kint) :: isum, ifil, idx, jdx, inum, ip, j0, i, j
!
!
      nsize_lf_smp = 0
      do nd = 1, 3
        nsize_lf_smp = max(nsize_lf_smp,nmax_l_filter(nd))
      end do
!
      call allocate_l_filtering_tmp
      call allocate_l_filtering_smp
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
          inod_l_filter_tmp(inod,nd) = inod_l_filter(inod0,nd)
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
           inum = istack_l_filter(inod0,nd)                             &
     &            - istack_l_filter(inod0-1,nd)
           istack_l_filter_tmp(inod,nd)                                 &
     &            = istack_l_filter_tmp(inod-1,nd) + inum
           do j0 = 1, inum
            j = istack_l_filter_tmp(inod-1,nd) + j0
            i = istack_l_filter(inod0-1,nd) + j0
            item_l_filter_tmp(j,nd) = item_l_filter(i,nd)
            c_l_filter_tmp(j,nd) = coef_l_filter(i,nd)
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
         inod_l_filter(inod,nd) = inod_l_filter_tmp(inod,nd)
         istack_l_filter(inod,nd) = istack_l_filter_tmp(inod,nd)
        end do
        do i =1, num_l_filter(nd)
          item_l_filter(i,nd) = item_l_filter_tmp(i,nd)
          coef_l_filter(i,nd) = c_l_filter_tmp(i,nd)
        end do
       end do
      end do
!
!     call check_istack_l_filter(numnod, my_rank)
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
         inum = istack_l_filter(inod,nd) - istack_l_filter(inod-1,nd)
         do i = 1, inum
          num_4_lf_smp(i,ip,nd) = num_4_lf_smp(i,ip,nd) + 1
         end do
        end do
       end do
      end do
!
!     call check_num_4_lf_smp(my_rank)
!
      istack_l_filter_smp(0,1) = 0
      istack_l_filter_smp(0,2) = 0
      istack_l_filter_smp(0,3) = 0
      do ip = 1, np_smp
       do nd = 1, 3
        do isum = 1, nmax_l_filter(nd)
         ii = (ip-1)*nsize_lf_smp + isum
         istack_l_filter_smp(ii,nd) = istack_l_filter_smp(ii-1,nd)      &
     &                                + num_4_lf_smp(isum,ip,nd)
        end do
        do isum = nmax_l_filter(nd)+1, nsize_lf_smp
         ii = (ip-1)*nsize_lf_smp + isum
         istack_l_filter_smp(ii,nd) = istack_l_filter_smp(ii-1,nd)
        end do
       end do
      end do
!
!      call check_istack_l_filter_smp(my_rank)
!
      do ip = 1, np_smp
       do nd = 1, 3
        do isum = 1, nmax_l_filter(nd)
         do i = 1, num_4_lf_smp(isum,ip,nd)
           inod = inod_smp_stack(ip-1) + i
           ii = (ip-1)*nsize_lf_smp + isum
           jdx = istack_l_filter(inod-1,nd) + isum
           idx = istack_l_filter_smp(ii-1,nd) + i
           item_l_filter_smp(idx,nd) = item_l_filter(jdx,nd)
           c_l_filter_smp(idx,nd) = coef_l_filter(jdx,nd)
         end do
        end do
       end do
      end do
!
      call deallocate_l_filtering_data
!
      end subroutine ordering_l_filter_smp
!
! ----------------------------------------------------------------------
!
      end module ordering_line_filter_smp
