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
      integer(kind = kint) :: itype, ist, ied, ist_smp
!
!
      do itype = 1, 4
        ist = istack_itp_type_org(itype-1) + 1
        ied = istack_itp_type_org(itype  )
        ist_smp = np_smp * (itype-1)
        call count_number_4_smp( np_smp, ist, ied,                      &
     &     istack_table_wtype_org_smp(ist_smp),                         &
     &     imax_table_wtype_org_smp)
      end do
!
      end subroutine s_set_stack_tbl_wtype_org_smp
!
!-----------------------------------------------------------------------
!
      end module set_stack_tbl_wtype_org_smp
