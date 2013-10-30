!copy_interpolate_type_IO.f90
!      module copy_interpolate_type_IO
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine copy_interpolate_types_from_IO(my_rank, itp_table)
!      subroutine copy_interpolate_types_to_IO(itp_table)
!
!      subroutine copy_itp_tbl_type_dest_from_IO(my_rank, tbl_dest)
!      subroutine copy_itp_tbl_type_org_from_IO(my_rank, tbl_org)
!
      module copy_interpolate_type_IO
!
      use m_precision
!
      implicit none
!
      private :: copy_itp_tbl_type_dest_from_IO
      private :: copy_itp_tbl_type_org_from_IO
      private :: copy_itp_tbl_type_dest_to_IO
      private :: copy_itp_tbl_type_org_to_IO
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
      use set_stack_tbl_org_smp_type
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table), intent(inout) :: itp_table
!
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_type_dest_from_IO'
      call copy_itp_tbl_type_dest_from_IO(my_rank, itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_type_org_from_IO'
      call copy_itp_tbl_type_org_from_IO(my_rank, itp_table%tbl_org)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_stack_tbl_org_smp_type'
      call s_set_stack_tbl_org_smp_type(itp_table%tbl_org)
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
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_type_dest_to_IO'
      call copy_itp_tbl_type_dest_to_IO(itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_type_org_to_IO'
      call copy_itp_tbl_type_org_to_IO(itp_table%tbl_org)
!
      end subroutine copy_interpolate_types_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_type_dest_from_IO(my_rank, tbl_dest)
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
      tbl_dest%num_org_domain = num_org_domain_IO
      tbl_dest%iflag_self_itp_recv = 0
!
      if (tbl_dest%num_org_domain .gt. 0) then
!
        tbl_dest%ntot_table_dest = ntot_table_dest_IO
!
        call alloc_type_itp_num_dest(tbl_dest)
        call alloc_type_itp_table_dest(tbl_dest)
!
        tbl_dest%id_org_domain(1:tbl_dest%num_org_domain)               &
     &      = id_org_domain_IO(1:tbl_dest%num_org_domain)
        tbl_dest%istack_nod_tbl_dest(0:tbl_dest%num_org_domain)         &
     &        = istack_table_dest_IO(0:tbl_dest%num_org_domain)
        tbl_dest%istack_nod_tbl_wtype_dest(0:4*tbl_dest%num_org_domain) &
     &        = istack_table_wtype_dest_IO(0:4*tbl_dest%num_org_domain)
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
!
      end if
!
      end subroutine copy_itp_tbl_type_dest_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_type_org_from_IO(my_rank, tbl_org)
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
      tbl_org%num_dest_domain = num_dest_domain_IO
!
      if (tbl_org%num_dest_domain .gt. 0) then
!
        tbl_org%ntot_table_org = ntot_table_org_IO
!
        call alloc_type_itp_num_org(np_smp, tbl_org)
        call alloc_type_itp_table_org(tbl_org)
!
        tbl_org%id_dest_domain(1:tbl_org%num_dest_domain)               &
     &     = id_dest_domain_IO(1:tbl_org%num_dest_domain)
        tbl_org%istack_nod_tbl_org(0:num_dest_domain_IO)                &
     &       = istack_nod_table_org_IO(0:num_dest_domain_IO)
        tbl_org%istack_itp_type_org(0:4) = istack_itp_type_org_IO(0:4)
!
        do i = 1, tbl_org%ntot_table_org
          tbl_org%inod_itp_send(i) =      inod_itp_send_IO(i)
          tbl_org%inod_gl_dest_4_org(i) = inod_gl_dest_4_org_IO(i)
          tbl_org%iele_org_4_org(i) =     iele_org_4_org_IO(i)
          tbl_org%itype_inter_org(i) =    itype_inter_org_IO(i)
!
          tbl_org%coef_inter_org(i,1) =   coef_inter_org_IO(i,1)
          tbl_org%coef_inter_org(i,2) =   coef_inter_org_IO(i,2)
          tbl_org%coef_inter_org(i,3) =   coef_inter_org_IO(i,3)
        end do
!
        call deallocate_itp_table_org_IO
        call deallocate_itp_num_org_IO
!
        ilast_domain = tbl_org%num_dest_domain
        if ( tbl_org%id_dest_domain(ilast_domain) .eq. my_rank) then
          tbl_org%iflag_self_itp_send = 1
        end if
!
      end if
!
      end subroutine copy_itp_tbl_type_org_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_type_dest_to_IO(tbl_dest)
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
!
        ntot_table_dest_IO = tbl_dest%ntot_table_dest
!
        call allocate_itp_num_dst_IO
        call allocate_itp_nod_dst_IO
!
        id_org_domain_IO(1:num_org_domain_IO)                           &
     &      = tbl_dest%id_org_domain(1:num_org_domain_IO)
        istack_table_dest_IO(0:num_org_domain_IO)                       &
     &      = tbl_dest%istack_nod_tbl_dest(0:num_org_domain_IO)
        istack_table_wtype_dest_IO(0:4*num_org_domain_IO)               &
     &      = tbl_dest%istack_nod_tbl_wtype_dest(0:4*num_org_domain_IO)
!
!
        inod_dest_IO(1:ntot_table_dest_IO)                              &
     &      = tbl_dest%inod_dest_4_dest(1:ntot_table_dest_IO)
!
        call dealloc_type_itp_table_dest(tbl_dest)
        call dealloc_type_itp_num_dest(tbl_dest)
!
      end if
!
      end subroutine copy_itp_tbl_type_dest_to_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_type_org_to_IO(tbl_org)
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
        ntot_table_org_IO = tbl_org%ntot_table_org
!
        call allocate_itp_num_org_IO
        call allocate_itp_table_org_IO
!
        id_dest_domain_IO(1:num_dest_domain_IO)                         &
     &       = tbl_org%id_dest_domain(1:num_dest_domain_IO)
        istack_nod_table_org_IO(0:num_dest_domain_IO)                   &
     &       = tbl_org%istack_nod_tbl_org(0:num_dest_domain_IO)
        istack_itp_type_org_IO(0:4) = tbl_org%istack_itp_type_org(0:4)
!
!
        inod_itp_send_IO(1:ntot_table_org_IO)                           &
     &          = tbl_org%inod_itp_send(1:ntot_table_org_IO)
        inod_gl_dest_4_org_IO(1:ntot_table_org_IO)                      &
     &          = tbl_org%inod_gl_dest_4_org(1:ntot_table_org_IO)
        iele_org_4_org_IO(1:ntot_table_org_IO)                          &
     &          = tbl_org%iele_org_4_org(1:ntot_table_org_IO)
        itype_inter_org_IO(1:ntot_table_org_IO)                         &
     &          = tbl_org%itype_inter_org(1:ntot_table_org_IO)
!
        coef_inter_org_IO(1:ntot_table_org_IO,1)                        &
     &          = tbl_org%coef_inter_org(1:ntot_table_org_IO,1)
        coef_inter_org_IO(1:ntot_table_org_IO,2)                        &
     &          = tbl_org%coef_inter_org(1:ntot_table_org_IO,2)
        coef_inter_org_IO(1:ntot_table_org_IO,3)                        &
     &          = tbl_org%coef_inter_org(1:ntot_table_org_IO,3)
!
      end if
!
      call dealloc_type_itp_table_org(tbl_org)
      call dealloc_type_itp_num_org(tbl_org)
!
      end subroutine copy_itp_tbl_type_org_to_IO
!
!-----------------------------------------------------------------------
!
      end module copy_interpolate_type_IO
