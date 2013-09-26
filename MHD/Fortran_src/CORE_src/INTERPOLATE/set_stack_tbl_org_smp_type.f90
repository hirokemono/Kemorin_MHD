!set_stack_tbl_org_smp_type.f90
!     module set_stack_tbl_org_smp_type
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_set_stack_tbl_org_smp_type
!        type(interpolate_table_org), intent(inout) :: tbl_org
!
      module set_stack_tbl_org_smp_type
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
      subroutine s_set_stack_tbl_org_smp_type(tbl_org)
!
      use m_machine_parameter
      use t_interpolate_tbl_org
      use cal_minmax_and_stacks
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      integer(kind = kint) :: ip, itype, ist, ied
      integer(kind = kint) :: ist_smp, ied_smp, jnum
!
!
      call alloc_type_istack_tbl_wtp_smp(np_smp, tbl_org)
!
      do ip = 1, tbl_org%num_dest_domain
        do itype = 1, 4
          jnum = 4*(ip-1) + itype
!
          ist = tbl_org%istack_nod_tbl_wtype_org(jnum-1) + 1
          ied = tbl_org%istack_nod_tbl_wtype_org(jnum  )
          ist_smp = np_smp * (jnum-1)
          ied_smp = np_smp *  jnum
          call count_number_4_smp( np_smp, ist, ied,                    &
     &        tbl_org%istack_tbl_wtype_org_smp(ist_smp:ied_smp),        &
     &        tbl_org%imax_tbl_wtype_org_smp)
!
        end do
      end do
!
      end subroutine s_set_stack_tbl_org_smp_type
!
!-----------------------------------------------------------------------
!
      end module set_stack_tbl_org_smp_type
