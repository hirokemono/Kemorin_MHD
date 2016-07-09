!copy_interpolate_type_IO.f90
!      module copy_interpolate_type_IO
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine copy_interpolate_types_from_IO(my_rank, itp_table)
!      subroutine copy_interpolate_types_to_IO(my_rank, itp_table)
!
!      subroutine copy_itp_table_dest_from_IO(my_rank, tbl_dest)
!      subroutine copy_itp_table_org_from_IO(my_rank, tbl_org)
!      subroutine copy_itp_table_dest_to_IO(tbl_dest)
!      subroutine copy_itp_table_org_to_IO(tbl_org)
!
      module copy_interpolate_type_IO
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_interpolate_types_from_IO(my_rank, itp_table)
!
      use m_interpolate_table_org_IO
      use t_interpolate_table
      use copy_interpolate_types
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table), intent(inout) :: itp_table
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_dest_from_IO'
      call copy_itp_table_dest_from_IO(my_rank, itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_types_org'
      call copy_itp_tbl_types_org                                       &
     &   (my_rank, IO_itp_org, itp_table%tbl_org)
!
      end subroutine copy_interpolate_types_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_interpolate_types_to_IO(my_rank, itp_table)
!
      use m_interpolate_table_org_IO
      use t_interpolate_table
      use copy_interpolate_types
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table), intent(inout) :: itp_table
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_dest_to_IO'
      call copy_itp_table_dest_to_IO(itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_types_org'
      call copy_itp_tbl_types_org                                       &
     &   (my_rank, itp_table%tbl_org, IO_itp_org)
      call dealloc_itp_table_org(itp_table%tbl_org)
      call dealloc_itp_num_org(itp_table%tbl_org)
!
      end subroutine copy_interpolate_types_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_itp_table_dest_from_IO(my_rank, tbl_dest)
!
      use t_interpolate_tbl_dest
      use m_interpolate_table_dest_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
      integer(kind = kint) :: ilast_domain
!
!
      tbl_dest%iflag_self_itp_recv = 0
      call set_num_org_domain(num_org_domain_IO, tbl_dest)
!
      if (tbl_dest%num_org_domain .gt. 0) then
        tbl_dest%ntot_table_dest = ntot_table_dest_IO
!
        call alloc_itp_num_dest(tbl_dest)
        call alloc_itp_table_dest(tbl_dest)
!
        tbl_dest%id_org_domain(1:tbl_dest%num_org_domain)               &
     &      = id_org_domain_IO(1:tbl_dest%num_org_domain)
        tbl_dest%istack_nod_tbl_dest(0:tbl_dest%num_org_domain)         &
     &        = istack_table_dest_IO(0:tbl_dest%num_org_domain)
!
        tbl_dest%inod_dest_4_dest(1:tbl_dest%ntot_table_dest)           &
     &        = inod_dest_IO(1:tbl_dest%ntot_table_dest)
!
        call deallocate_itp_nod_dst_IO
        call deallocate_itp_num_dst_IO
!
        ilast_domain = tbl_dest%num_org_domain
        if (tbl_dest%id_org_domain(ilast_domain) .eq. my_rank) then
          tbl_dest%iflag_self_itp_recv = 1
        end if
      else
        tbl_dest%ntot_table_dest = 0
        call alloc_itp_num_dest(tbl_dest)
        call alloc_itp_table_dest(tbl_dest)
      end if
!
      end subroutine copy_itp_table_dest_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_itp_table_dest_to_IO(tbl_dest)
!
      use t_interpolate_tbl_dest
      use m_interpolate_table_dest_IO
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      num_org_domain_IO = tbl_dest%num_org_domain
!
      if (num_org_domain_IO .gt. 0) then
        ntot_table_dest_IO = tbl_dest%ntot_table_dest
!
        call allocate_itp_num_dst_IO
        call allocate_itp_nod_dst_IO
!
        id_org_domain_IO(1:num_org_domain_IO)                           &
     &      = tbl_dest%id_org_domain(1:num_org_domain_IO)
        istack_table_dest_IO(0:num_org_domain_IO)                       &
     &      = tbl_dest%istack_nod_tbl_dest(0:num_org_domain_IO)
!
!
        inod_dest_IO(1:ntot_table_dest_IO)                              &
     &      = tbl_dest%inod_dest_4_dest(1:ntot_table_dest_IO)
      end if
!
      call dealloc_itp_table_dest(tbl_dest)
      call dealloc_itp_num_dest(tbl_dest)
!
      end subroutine copy_itp_table_dest_to_IO
!
!-----------------------------------------------------------------------
!
      end module copy_interpolate_type_IO
