!
!     module order_dest_table_by_domain
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine s_order_dest_table_by_domain                         &
!!     &         (node, iflag_org_domain, ierr_missing,                 &
!!     &          itp_dest, itp_coef_dest, orderd)
!!        type(node_data), intent(inout) :: node
!!        type(interpolate_table_dest), intent(inout) :: itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!!        type(ordered_list), intent(inout) :: orderd
!
      module order_dest_table_by_domain
!
      use m_precision
!
      implicit none
!
!>   number of node to be interpolated in each original domain
      integer(kind = kint), allocatable :: numnod_dest(:)
      private :: numnod_dest
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_order_dest_table_by_domain                           &
     &         (node, iflag_org_domain, ierr_missing,                   &
     &          itp_dest, itp_coef_dest, orderd)
!
      use calypso_mpi
      use t_geometry_data
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_work_const_itp_table
      use m_2nd_pallalel_vector
!
      type(node_data), intent(inout) :: node
      integer(kind = kint), intent(in) :: iflag_org_domain(node%numnod)
      integer(kind = kint), intent(in) :: ierr_missing
!
      type(interpolate_table_dest), intent(inout) :: itp_dest
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      type(ordered_list), intent(inout) :: orderd
!
      integer(kind = kint) :: j, jp, inod, icou
      integer(kind = kint) :: my_rank_2nd
!
!
      allocate( numnod_dest(0:nprocs_2nd) )
      if(nprocs_2nd .gt. 0) numnod_dest = 0
!
      do inod = 1, node%internal_node
        jp = iflag_org_domain(inod)
        itp_dest%inod_dest_4_dest(inod) = inod
        numnod_dest(jp) = numnod_dest(jp) + 1
      end do
!
      itp_dest%num_org_domain = 0
      itp_dest%istack_nod_tbl_dest(0) = 0
      do j = 1, nprocs_2nd
        my_rank_2nd = mod(my_rank+j,nprocs_2nd)
!
        if (numnod_dest(my_rank_2nd+1) .gt. 0) then
          itp_dest%num_org_domain = itp_dest%num_org_domain + 1
          itp_dest%id_org_domain(itp_dest%num_org_domain)               &
     &               = my_rank_2nd
          itp_dest%istack_nod_tbl_dest(itp_dest%num_org_domain)         &
     &      = itp_dest%istack_nod_tbl_dest(itp_dest%num_org_domain-1)   &
     &       + numnod_dest(my_rank_2nd+1)
        end if
      end do
!     write(*,*) 'num_org_domain', itp_dest%num_org_domain
!
      deallocate(numnod_dest)
!
      do j = 1, itp_dest%num_org_domain
        jp = itp_dest%id_org_domain(j) + 1
        icou = itp_dest%istack_nod_tbl_dest(j-1)
!
        do inod = 1, node%internal_node
          if (iflag_org_domain(inod) .eq. jp) then
            icou = icou + 1
            call swap_interpolation_table                               &
     &         (icou, inod, itp_dest, itp_coef_dest, orderd)
          end if
        end do
!
      end do
!
!   set missing nodes at the end
!
      if (ierr_missing .gt. 0) then
        do inod = 1, node%internal_node
          if (iflag_org_domain(inod) .eq. 0) then
            icou = icou + 1
            call swap_interpolation_table                               &
     &         (icou, inod, itp_dest, itp_coef_dest, orderd)
          end if
        end do
      end if
!
      call copy_table_2_order(orderd, itp_dest, itp_coef_dest)
!
      end subroutine s_order_dest_table_by_domain
!
!-----------------------------------------------------------------------
!
      end module order_dest_table_by_domain
