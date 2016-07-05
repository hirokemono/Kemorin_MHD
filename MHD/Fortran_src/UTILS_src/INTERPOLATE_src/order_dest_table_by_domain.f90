!
!     module order_dest_table_by_domain
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_order_dest_table_by_domain                          &
!     &         (internal_node, ierr_missing)
!
      module order_dest_table_by_domain
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_order_dest_table_by_domain                           &
     &         (internal_node, ierr_missing)
!
      use calypso_mpi
      use m_2nd_pallalel_vector
      use m_interpolate_table_dest
      use m_work_const_itp_table
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: ierr_missing
!
      integer(kind = kint) :: j, jp, inod, icou
      integer(kind = kint) :: my_rank_2nd
!
!
      do inod = 1, internal_node
        jp = iflag_org_domain(inod)
        itp1_dest%inod_dest_4_dest(inod) = inod
        numnod_dest(jp) = numnod_dest(jp) + 1
      end do
!
      itp1_dest%num_org_domain = 0
      itp1_dest%istack_nod_tbl_dest(0) = 0
      do j = 1, nprocs_2nd
        my_rank_2nd = mod(my_rank+j,nprocs_2nd)
!
        if (numnod_dest(my_rank_2nd+1) .gt. 0) then
          itp1_dest%num_org_domain = itp1_dest%num_org_domain + 1
          itp1_dest%id_org_domain(itp1_dest%num_org_domain)             &
     &               = my_rank_2nd
          itp1_dest%istack_nod_tbl_dest(itp1_dest%num_org_domain)       &
     &      = itp1_dest%istack_nod_tbl_dest(itp1_dest%num_org_domain-1) &
     &       + numnod_dest(my_rank_2nd+1)
        end if
      end do
      write(*,*) 'num_org_domain', itp1_dest%num_org_domain
!
      do j = 1, itp1_dest%num_org_domain
        jp = itp1_dest%id_org_domain(j) + 1
        icou = itp1_dest%istack_nod_tbl_dest(j-1)
!
        do inod = 1, internal_node
          if (iflag_org_domain(inod) .eq. jp) then
            icou = icou + 1
            call swap_interpolation_table(icou, inod)
          end if
        end do
!
      end do
!
!   set missing nodes at the end
!
      if (ierr_missing .gt. 0) then
        do inod = 1, internal_node
          if (iflag_org_domain(inod) .eq. 0) then
            icou = icou + 1
            call swap_interpolation_table(icou, inod)
          end if
        end do
      end if
!
      call copy_table_2_order
!
      end subroutine s_order_dest_table_by_domain
!
!-----------------------------------------------------------------------
!
      end module order_dest_table_by_domain
