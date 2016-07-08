!copy_interpolate_type_IO.f90
!      module copy_interpolate_type_IO
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine copy_interpolate_types_from_IO(my_rank, itp_table)
!      subroutine copy_interpolate_types_to_IO(itp_table)
!
!      subroutine copy_itp_table_dest_from_IO(my_rank, tbl_dest)
!      subroutine copy_itp_table_org_from_IO(my_rank, tbl_org)
!      subroutine copy_itp_table_dest_to_IO(tbl_dest)
!      subroutine copy_itp_table_org_to_IO(tbl_org)
!
      module copy_interpolate_type_IO
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
      subroutine copy_interpolate_types_from_IO(my_rank, itp_table)
!
      use m_machine_parameter
      use t_interpolate_table
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table), intent(inout) :: itp_table
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_dest_from_IO'
      call copy_itp_table_dest_from_IO(my_rank, itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_org_from_IO'
      call copy_itp_table_org_from_IO(my_rank, itp_table%tbl_org)
!
      end subroutine copy_interpolate_types_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_interpolate_types_to_IO(itp_table)
!
      use m_machine_parameter
      use t_interpolate_table
!
      type(interpolate_table), intent(inout) :: itp_table
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_dest_to_IO'
      call copy_itp_table_dest_to_IO(itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_org_to_IO'
      call copy_itp_table_org_to_IO(itp_table%tbl_org)
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
!
      subroutine copy_itp_table_org_from_IO(my_rank, tbl_org)
!
      use t_interpolate_tbl_org
      use m_machine_parameter
      use m_interpolate_table_org_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      integer(kind = kint) :: i, ilast_domain
!
!
      tbl_org%iflag_self_itp_send = 0
      call set_num_dest_domain(num_dest_domain_IO, tbl_org)
      call alloc_itp_num_org(np_smp, tbl_org)
!
      if (tbl_org%num_dest_domain .gt. 0) then
        tbl_org%ntot_table_org = IO_itp_org%ntot_table_org
!
        call alloc_itp_table_org(tbl_org)
!
        tbl_org%id_dest_domain(1:tbl_org%num_dest_domain)               &
     &     = id_dest_domain_IO(1:tbl_org%num_dest_domain)
        tbl_org%istack_nod_tbl_org(0:num_dest_domain_IO)                &
     &       = istack_nod_table_org_IO(0:num_dest_domain_IO)
        tbl_org%istack_itp_type_org(0:4) = istack_itp_type_org_IO(0:4)
!
        do i = 1, tbl_org%ntot_table_org
          tbl_org%inod_itp_send(i) =      IO_itp_org%inod_itp_send(i)
          tbl_org%inod_gl_dest_4_org(i)                                 &
     &       = IO_itp_org%inod_gl_dest_4_org(i)
          tbl_org%iele_org_4_org(i) =  IO_itp_org%iele_org_4_org(i)
          tbl_org%itype_inter_org(i) = IO_itp_org%itype_inter_org(i)
!
          tbl_org%coef_inter_org(i,1) = IO_itp_org%coef_inter_org(i,1)
          tbl_org%coef_inter_org(i,2) = IO_itp_org%coef_inter_org(i,2)
          tbl_org%coef_inter_org(i,3) = IO_itp_org%coef_inter_org(i,3)
        end do
!
        call dealloc_itp_table_org(IO_itp_org)
        call deallocate_itp_num_org_IO
!
        ilast_domain = tbl_org%num_dest_domain
        if ( tbl_org%id_dest_domain(ilast_domain) .eq. my_rank) then
          tbl_org%iflag_self_itp_send = 1
        end if
      else
        tbl_org%ntot_table_org = 0
        call alloc_itp_num_org(np_smp, tbl_org)
        call alloc_itp_table_org(tbl_org)
      end if
!
      end subroutine copy_itp_table_org_from_IO
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
      subroutine copy_itp_table_org_to_IO(tbl_org)
!
      use t_interpolate_tbl_org
      use m_interpolate_table_org_IO
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
!
      num_dest_domain_IO = tbl_org%num_dest_domain
!
      if (num_dest_domain_IO .gt. 0) then
!
        IO_itp_org%ntot_table_org = tbl_org%ntot_table_org
!
        call allocate_itp_num_org_IO
        call alloc_itp_table_org(IO_itp_org)
!
        id_dest_domain_IO(1:num_dest_domain_IO)                         &
     &       = tbl_org%id_dest_domain(1:num_dest_domain_IO)
        istack_nod_table_org_IO(0:num_dest_domain_IO)                   &
     &       = tbl_org%istack_nod_tbl_org(0:num_dest_domain_IO)
        istack_itp_type_org_IO(0:4) = tbl_org%istack_itp_type_org(0:4)
!
!
        IO_itp_org%inod_itp_send(1:IO_itp_org%ntot_table_org)           &
     &          = tbl_org%inod_itp_send(1:IO_itp_org%ntot_table_org)
        IO_itp_org%inod_gl_dest_4_org(1:IO_itp_org%ntot_table_org)      &
     &          = tbl_org%inod_gl_dest_4_org(1:IO_itp_org%ntot_table_org)
        IO_itp_org%iele_org_4_org(1:IO_itp_org%ntot_table_org)          &
     &          = tbl_org%iele_org_4_org(1:IO_itp_org%ntot_table_org)
        IO_itp_org%itype_inter_org(1:IO_itp_org%ntot_table_org)         &
     &          = tbl_org%itype_inter_org(1:IO_itp_org%ntot_table_org)
!
        IO_itp_org%coef_inter_org(1:IO_itp_org%ntot_table_org,1)        &
     &          = tbl_org%coef_inter_org(1:IO_itp_org%ntot_table_org,1)
        IO_itp_org%coef_inter_org(1:IO_itp_org%ntot_table_org,2)        &
     &          = tbl_org%coef_inter_org(1:IO_itp_org%ntot_table_org,2)
        IO_itp_org%coef_inter_org(1:IO_itp_org%ntot_table_org,3)        &
     &          = tbl_org%coef_inter_org(1:IO_itp_org%ntot_table_org,3)
!
      end if
!
      call dealloc_itp_table_org(tbl_org)
      call dealloc_itp_num_org(tbl_org)
!
      end subroutine copy_itp_table_org_to_IO
!
!-----------------------------------------------------------------------
!
      end module copy_interpolate_type_IO
