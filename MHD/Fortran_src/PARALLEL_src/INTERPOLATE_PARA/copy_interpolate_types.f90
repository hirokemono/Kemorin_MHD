!>@file   copy_interpolate_types.f90
!!@brief  module copy_interpolate_types
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!>@brief  Copy interpolation table between structures
!!
!!@verbatim
!!      subroutine copy_interpolate_between_types(my_rank,              &
!!     &          itp_input, itp_copied)
!!
!!      subroutine copy_itp_tbl_types_dst(my_rank,                      &
!!     &          tbl_dst_in, tbl_dst_cp)
!!      subroutine copy_itp_tbl_types_org(my_rank,                      &
!!     &          tbl_org_in, tbl_org_cp)
!!@endverbatim
!
      module copy_interpolate_types
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
      subroutine copy_interpolate_between_types(my_rank,                &
     &          itp_input, itp_copied)
!
      use m_machine_parameter
      use t_interpolate_table
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table), intent(inout) :: itp_input
      type(interpolate_table), intent(inout) :: itp_copied

!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_types_dst'
      call copy_itp_tbl_types_dst(my_rank, itp_input%tbl_dest,          &
     &    itp_copied%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_types_org'
      call copy_itp_tbl_types_org(my_rank, itp_input%tbl_org,           &
     &    itp_copied%tbl_org)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_stack_tbl_org_smp_type'
      call set_stack_tbl_wtype_org_smp(itp_copied%tbl_org)
!
      end subroutine copy_interpolate_between_types
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_types_dst(my_rank,                        &
     &          tbl_dst_in, tbl_dst_cp)
!
      use t_interpolate_tbl_dest
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_dest), intent(inout) :: tbl_dst_in
      type(interpolate_table_dest), intent(inout) :: tbl_dst_cp
!
      integer(kind = kint) :: ilast_domain
!
!
      tbl_dst_cp%iflag_self_itp_recv = 0
      call set_num_org_domain(tbl_dst_in%num_org_domain, tbl_dst_cp)
!
      if (tbl_dst_cp%num_org_domain .gt. 0) then
!
        tbl_dst_cp%ntot_table_dest = tbl_dst_in%ntot_table_dest
!
        call alloc_itp_num_dest(tbl_dst_cp)
        call alloc_itp_table_dest(tbl_dst_cp)
!
        tbl_dst_cp%id_org_domain(1:tbl_dst_cp%num_org_domain)           &
     &   = tbl_dst_in%id_org_domain(1:tbl_dst_cp%num_org_domain)
        tbl_dst_cp%istack_nod_tbl_dest(0:tbl_dst_cp%num_org_domain)     &
     &   = tbl_dst_in%istack_nod_tbl_dest(0:tbl_dst_cp%num_org_domain)
!
        tbl_dst_cp%inod_dest_4_dest(1:tbl_dst_cp%ntot_table_dest)       &
     &   = tbl_dst_in%inod_dest_4_dest(1:tbl_dst_cp%ntot_table_dest)
!
        call dealloc_itp_table_dest(tbl_dst_in)
        call dealloc_itp_num_dest(tbl_dst_in)
!
        ilast_domain = tbl_dst_cp%num_org_domain
        if (tbl_dst_cp%id_org_domain(ilast_domain) .eq. my_rank) then
          tbl_dst_cp%iflag_self_itp_recv = 1
        end if
!
      end if
!
      end subroutine copy_itp_tbl_types_dst
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_types_org(my_rank,                        &
     &          tbl_org_in, tbl_org_cp)
!
      use t_interpolate_tbl_org
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(in) :: tbl_org_in
      type(interpolate_table_org), intent(inout) :: tbl_org_cp
!
      integer(kind = kint) :: i, ilast_domain
!
!
      tbl_org_cp%iflag_self_itp_send = 0
      tbl_org_cp%num_dest_domain = tbl_org_in%num_dest_domain
!
      if (tbl_org_cp%num_dest_domain .gt. 0) then
!
        tbl_org_cp%ntot_table_org = tbl_org_in%ntot_table_org
!
        call alloc_itp_num_org(np_smp, tbl_org_cp)
        call alloc_itp_table_org(tbl_org_cp)
!
        tbl_org_cp%id_dest_domain(1:tbl_org_cp%num_dest_domain)         &
     &   = tbl_org_in%id_dest_domain(1:tbl_org_cp%num_dest_domain)
        tbl_org_cp%istack_nod_tbl_org(0:tbl_org_cp%num_dest_domain)     &
     &   = tbl_org_in%istack_nod_tbl_org(0:tbl_org_cp%num_dest_domain)
        tbl_org_cp%istack_itp_type_org(0:4)                             &
     &   = tbl_org_in%istack_itp_type_org(0:4)
!
        do i = 1, tbl_org_cp%ntot_table_org
          tbl_org_cp%inod_itp_send(i)                                   &
     &       = tbl_org_in%inod_itp_send(i)
          tbl_org_cp%inod_gl_dest_4_org(i)                              &
     &       = tbl_org_in%inod_gl_dest_4_org(i)
          tbl_org_cp%iele_org_4_org(i)                                  &
     &       =     tbl_org_in%iele_org_4_org(i)
          tbl_org_cp%itype_inter_org(i)                                 &
     &       =    tbl_org_in%itype_inter_org(i)
!
          tbl_org_cp%coef_inter_org(i,1)                                &
     &       =   tbl_org_in%coef_inter_org(i,1)
          tbl_org_cp%coef_inter_org(i,2)                                &
     &       =   tbl_org_in%coef_inter_org(i,2)
          tbl_org_cp%coef_inter_org(i,3)                                &
     &       =   tbl_org_in%coef_inter_org(i,3)
        end do
!
        ilast_domain = tbl_org_cp%num_dest_domain
        if ( tbl_org_cp%id_dest_domain(ilast_domain) .eq. my_rank) then
          tbl_org_cp%iflag_self_itp_send = 1
        end if
!
      end if
!
      end subroutine copy_itp_tbl_types_org
!
!-----------------------------------------------------------------------
!
      end module copy_interpolate_types
