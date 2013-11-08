!
!      module copy_interpolate_dest_IO
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine copy_itp_table_dest_from_IO(my_rank)
!      subroutine copy_itp_table_dest_to_IO
!
      module copy_interpolate_dest_IO
!
      use m_precision
!
      use m_interpolate_table_dest
      use m_interpolate_table_dest_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_table_dest_from_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      num_org_domain = num_org_domain_IO
      iflag_self_itp_recv = 0
!
      if (num_org_domain .gt. 0) then
!
        ntot_table_dest = ntot_table_dest_IO
!
        call allocate_itp_num_dest(num_org_domain)
        call allocate_itp_table_dest
!
        id_org_domain(1:num_org_domain)                                 &
     &      = id_org_domain_IO(1:num_org_domain)
        istack_nod_tbl_dest(0:num_org_domain)                           &
     &      = istack_table_dest_IO(0:num_org_domain)
!
        inod_dest_4_dest(1:ntot_table_dest)                             &
     &        = inod_dest_IO(1:ntot_table_dest)
!
        call deallocate_itp_nod_dst_IO
        call deallocate_itp_num_dst_IO
!
        if ( id_org_domain(num_org_domain) .eq. my_rank) then
          iflag_self_itp_recv = 1
        end if
!
      end if
!
      end subroutine copy_itp_table_dest_from_IO
!
!-----------------------------------------------------------------------
!
      end module copy_interpolate_dest_IO
