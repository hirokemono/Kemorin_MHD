!>@file   copy_interpolate_type_raw.f90
!!@brief  module copy_interpolate_type_raw
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!>@brief  Copy interpolation table to structures
!!
!!@verbatim
!!      subroutine copy_interpolate_types_from_raw(my_rank, itp_table)
!!
!!      subroutine copy_itp_tbl_type_dst_from_raw(my_rank, tbl_dest)
!!      subroutine copy_itp_tbl_type_org_from_raw(my_rank, tbl_org)
!!@endverbatim
!
      module copy_interpolate_type_raw
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
      subroutine copy_interpolate_types_from_raw(my_rank, itp_table)
!
      use m_machine_parameter
      use t_interpolate_table
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table), intent(inout) :: itp_table

!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_type_dst_from_raw'
      call copy_itp_tbl_type_dst_from_raw(my_rank, itp_table%tbl_dest)
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_tbl_type_org_from_raw'
      call copy_itp_tbl_type_org_from_raw(my_rank, itp_table%tbl_org)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_stack_tbl_org_smp_type'
      call set_stack_tbl_org_smp_type(itp_table%tbl_org)
!
      end subroutine copy_interpolate_types_from_raw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_type_dst_from_raw(my_rank, tbl_dest)
!
      use t_interpolate_tbl_dest
      use m_interpolate_table_dest
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
      integer(kind = kint) :: ilast_domain
!
!
      tbl_dest%num_org_domain = num_org_domain
      tbl_dest%iflag_self_itp_recv = 0
!
      if (tbl_dest%num_org_domain .gt. 0) then
!
        tbl_dest%ntot_table_dest = ntot_table_dest
!
        call alloc_type_itp_num_dest(tbl_dest)
        call alloc_type_itp_table_dest(tbl_dest)
!
        tbl_dest%id_org_domain(1:tbl_dest%num_org_domain)               &
     &      = id_org_domain(1:tbl_dest%num_org_domain)
        tbl_dest%istack_nod_tbl_dest(0:tbl_dest%num_org_domain)         &
     &      = istack_nod_tbl_dest(0:tbl_dest%num_org_domain)
        tbl_dest%istack_nod_tbl_wtype_dest(0:4*tbl_dest%num_org_domain) &
     &      = istack_nod_tbl_wtype_dest(0:4*tbl_dest%num_org_domain)
!
        tbl_dest%inod_dest_4_dest(1:tbl_dest%ntot_table_dest)           &
     &      = inod_dest_4_dest(1:tbl_dest%ntot_table_dest)
!
        call deallocate_itp_table_dest
        call deallocate_itp_num_dest
!
        ilast_domain = tbl_dest%num_org_domain
        if (tbl_dest%id_org_domain(ilast_domain) .eq. my_rank) then
          tbl_dest%iflag_self_itp_recv = 1
        end if
!
      end if
!
      end subroutine copy_itp_tbl_type_dst_from_raw
!
!-----------------------------------------------------------------------
!
      subroutine copy_itp_tbl_type_org_from_raw(my_rank, tbl_org)
!
      use t_interpolate_tbl_org
      use m_machine_parameter
      use m_interpolate_table_orgin
!
      integer(kind = kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      integer(kind = kint) :: i, ilast_domain
!
!
      tbl_org%iflag_self_itp_send = 0
      tbl_org%num_dest_domain = num_dest_domain
!
      if (tbl_org%num_dest_domain .gt. 0) then
!
        tbl_org%ntot_table_org = ntot_table_org
!
        call alloc_type_itp_num_org(np_smp, tbl_org)
        call alloc_type_itp_table_org(tbl_org)
!
        tbl_org%id_dest_domain(1:tbl_org%num_dest_domain)               &
     &     = id_dest_domain(1:tbl_org%num_dest_domain)
        tbl_org%istack_nod_tbl_org(0:tbl_org%num_dest_domain)           &
     &     = istack_nod_tbl_org(0:tbl_org%num_dest_domain)
        tbl_org%istack_itp_type_org(0:4) = istack_itp_type_org(0:4)
!
        do i = 1, tbl_org%ntot_table_org
          tbl_org%inod_itp_send(i) =      inod_itp_send(i)
          tbl_org%inod_gl_dest_4_org(i) = inod_gl_dest_4_org(i)
          tbl_org%iele_org_4_org(i) =     iele_org_4_org(i)
          tbl_org%itype_inter_org(i) =    itype_inter_org(i)
!
          tbl_org%coef_inter_org(i,1) =   coef_inter_org(i,1)
          tbl_org%coef_inter_org(i,2) =   coef_inter_org(i,2)
          tbl_org%coef_inter_org(i,3) =   coef_inter_org(i,3)
        end do
!
        call deallocate_itp_table_org
        call deallocate_itp_num_org
!
        ilast_domain = tbl_org%num_dest_domain
        if ( tbl_org%id_dest_domain(ilast_domain) .eq. my_rank) then
          tbl_org%iflag_self_itp_send = 1
        end if
!
      end if
!
      end subroutine copy_itp_tbl_type_org_from_raw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module copy_interpolate_type_raw
