!set_stack_tbl_wtype_org_smp.f90
!     module set_stack_tbl_wtype_org_smp
!
!      subroutine s_set_stack_tbl_wtype_org_smp
!
      module set_stack_tbl_wtype_org_smp
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
      subroutine s_set_stack_tbl_wtype_org_smp
!
      use m_machine_parameter
      use m_interpolate_table_orgin
      use cal_minmax_and_stacks
!
      integer(kind = kint) :: ip, itype, ist, ied, ist_smp, jnum
!
!
      call allocate_istack_tbl_wtype_smp(np_smp)
!
      do ip = 1, num_dest_domain
        do itype = 1, 4
          jnum = 4*(ip-1) + itype
!
          ist = istack_nod_table_wtype_org(jnum-1) + 1
          ied = istack_nod_table_wtype_org(jnum  )
          ist_smp = np_smp * (jnum-1)
          call count_number_4_smp( np_smp, ist, ied,                    &
     &        istack_table_wtype_org_smp(ist_smp),                      &
     &        imax_table_wtype_org_smp)
!
        end do
      end do
!
      end subroutine s_set_stack_tbl_wtype_org_smp
!
!-----------------------------------------------------------------------
!
      end module set_stack_tbl_wtype_org_smp
