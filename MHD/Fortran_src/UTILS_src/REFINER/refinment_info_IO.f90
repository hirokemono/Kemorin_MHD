!
!      module refinment_info_IO
!
!!      subroutine write_refinement_table                               &
!!     &         (iref, refine_info_head, ele, refine_tbl)
!!      subroutine write_merged_refinement_tbl                          &
!!     &         (refine_info_head, ele, ref_itp_wk, refine_tbl)
!!        type(element_data), intent(in) :: ele
!!        type(work_merge_refine_itp), intent(in) :: ref_itp_wk
!!        type(element_refine_table), intent(inout) :: refine_tbl
!!
!!      subroutine read_refinement_table                                &
!!     &         (refine_info_head, ele, refine_tbl)
!!        type(element_data), intent(in) :: ele
!!        type(element_refine_table), intent(inout) :: refine_tbl
!
!
      module refinment_info_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use t_geometry_data
      use t_refined_element_data
      use t_element_refinement_IO
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_work_merge_refine_itp
!
      use set_parallel_file_name
!
      implicit  none
!
      type(interpolate_table_org), save, private :: IO_itp_e_org
      type(interpolate_table_dest), save, private :: IO_itp_e_dest
      type(ele_refine_IO_type), save, private :: IO_e_ref
!
      private :: set_element_refine_flags_2_IO
      private :: set_merged_refine_flags_2_IO
      private :: set_elem_refine_itp_tbl_2_IO
      private :: set_ele_refine_flags_from_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_refinement_table                                 &
     &         (iref, refine_info_head, ele, refine_tbl)
!
      use element_refine_file_IO
!
      integer(kind = kint), intent(in) :: iref
      character(len = kchara), intent(in) :: refine_info_head
      type(element_data), intent(in) :: ele
      type(element_refine_table), intent(in) :: refine_tbl
!
      integer(kind = kint) :: ierr = 0
!
      call set_element_refine_flags_2_IO                                &
     &   (ele%numele, refine_tbl, IO_e_ref)
      call set_elem_refine_itp_tbl_2_IO                                 &
     &   (IO_e_ref, IO_itp_e_org, IO_itp_e_dest)
!
!
      if(refine_tbl%iflag_tmp_tri_refine .eq. 1) then
        IO_e_ref%file_head = add_int_suffix(iref, refine_info_head)
      else
        IO_e_ref%file_head = refine_info_head
      end if
!
      call write_element_refine_file                                    &
     &   (0, izero, IO_itp_e_org, IO_itp_e_dest, IO_e_ref, ierr)
!
      end subroutine write_refinement_table
!
! ----------------------------------------------------------------------
!
      subroutine write_merged_refinement_tbl                            &
     &         (refine_info_head, ele, ref_itp_wk, refine_tbl)
!
      use element_refine_file_IO
!
      character(len = kchara), intent(in) :: refine_info_head
      type(element_data), intent(in) :: ele
      type(work_merge_refine_itp), intent(in) :: ref_itp_wk
      type(element_refine_table), intent(inout) :: refine_tbl
!
      integer(kind = kint) :: ierr = 0
!
!
      call set_merged_refine_flags_2_IO                                 &
     &   (ele%numele, ref_itp_wk%ref_org, ref_itp_wk%elist_1st,         &
     &    refine_tbl, IO_e_ref)
      call set_elem_refine_itp_tbl_2_IO                                 &
     &   (IO_e_ref, IO_itp_e_org, IO_itp_e_dest)
!
      IO_e_ref%file_head = refine_info_head
      call write_element_refine_file                                    &
     &   (0, izero, IO_itp_e_org, IO_itp_e_dest, IO_e_ref, ierr)
!
      end subroutine write_merged_refinement_tbl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_refinement_table                                  &
     &         (refine_info_head, ele, refine_tbl)
!
      use element_refine_file_IO
!
      character(len = kchara), intent(in) :: refine_info_head
      type(element_data), intent(in) :: ele
      type(element_refine_table), intent(inout) :: refine_tbl
!
!
      IO_e_ref%file_head = refine_info_head
      call read_element_refine_file                                     &
     &   (0, izero, IO_itp_e_org, IO_itp_e_dest, IO_e_ref)
!
      call dealloc_itp_num_org(IO_itp_e_org)
      call dealloc_itp_table_org(IO_itp_e_org)
!
      call dealloc_itp_table_dest(IO_itp_e_dest)
      call dealloc_itp_num_dest(IO_itp_e_dest)
!
      call set_ele_refine_flags_from_IO(ele, IO_e_ref, refine_tbl)
!
      end subroutine read_refinement_table
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_element_refine_flags_2_IO                          &
     &         (numele, refine_tbl, e_ref_IO)
!
      integer(kind = kint), intent(in) :: numele
      type(element_refine_table), intent(in) :: refine_tbl
!
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      integer(kind = kint) :: iele_org, ist, iele_neo, icou
!
!
      e_ref_IO%max_refine_level = refine_tbl%max_refine_level
      e_ref_IO%nele_org =         numele
      e_ref_IO%nele_ref =         refine_tbl%istack_ele_refined(numele)
!
      call alloc_element_refine_IO(e_ref_IO)
!
      do iele_org = 1, numele
        ist = refine_tbl%istack_ele_refined(iele_org-1)
        do icou = 1, refine_tbl%num_ele_refined(iele_org)
          iele_neo = icou + refine_tbl%istack_ele_refined(iele_org-1)
          e_ref_IO%iele_gl_new(iele_neo) =  iele_neo
          e_ref_IO%iele_gl_org(iele_neo) =  iele_org
          e_ref_IO%icou_gl_org(iele_neo) =  icou
          e_ref_IO%iflag_refine_ele(iele_neo)                           &
     &        = refine_tbl%iflag_refine_ele(iele_org)
          e_ref_IO%ilevel_refine(iele_neo)                              &
     &        = refine_tbl%ilevel_refine_old(iele_org)                  &
     &         + refine_tbl%ilevel_refine(iele_org)
        end do
      end do
!
      end subroutine set_element_refine_flags_2_IO
!
!------------------------------------------------------------------
!
      subroutine set_merged_refine_flags_2_IO                           &
     &         (numele, ref_org, elist_1st, refine_tbl, e_ref_IO)
!
      use m_refine_flag_parameters
!
      integer(kind = kint), intent(in) :: numele
      type(orginal_refine_level), intent(in) :: ref_org
      type(first_element_list), intent(in) :: elist_1st
!
      type(element_refine_table), intent(inout) :: refine_tbl
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      integer(kind = kint) :: iele_org, ist, icou
      integer(kind = kint) :: iele_1st, iflag_1st, icou_1st
      integer(kind = kint) :: iele_2nd, iflag_2nd, icou_2nd
!
!
      e_ref_IO%max_refine_level = refine_tbl%max_refine_level
      e_ref_IO%nele_org =         numele
      e_ref_IO%nele_ref =         refine_tbl%istack_ele_refined(numele)
!
      call alloc_element_refine_IO(e_ref_IO)
!
      do iele_1st = 1, numele
        iflag_1st = elist_1st%iflag_ref_1st(iele_1st)
        iele_org =  elist_1st%iele_1st(iele_1st,1)
        icou_1st =  elist_1st%iele_1st(iele_1st,2)
!
        ist = refine_tbl%istack_ele_refined(iele_1st-1)
        do icou = 1, refine_tbl%num_ele_refined(iele_1st)
          iele_2nd = icou + refine_tbl%istack_ele_refined(iele_1st-1)
!
          if( iflag_1st .ne. iflag_nothing) then
            iflag_2nd = refine_tbl%iflag_refine_ele(iele_1st)           &
     &                 + iflag_1st*1000
            icou_2nd =  icou + icou_1st * 100
          else
            iflag_2nd = refine_tbl%iflag_refine_ele(iele_1st)
            icou_2nd =  icou
          end if
!
          e_ref_IO%iele_gl_new(iele_2nd) =   iele_2nd
          e_ref_IO%iele_gl_org(iele_2nd) =   iele_org
          e_ref_IO%icou_gl_org(iele_2nd) =   icou_2nd
          e_ref_IO%iflag_refine_ele(iele_2nd) =  iflag_2nd
          e_ref_IO%ilevel_refine(iele_2nd)                              &
     &        =  refine_tbl%ilevel_refine_old(iele_org)                 &
     &         + ref_org%ilevel_refine_org(iele_org)                    &
     &         + refine_tbl%ilevel_refine(iele_1st)
        end do
      end do
!
      call dealloc_old_refine_level(refine_tbl)
!
      end subroutine set_merged_refine_flags_2_IO
!
!------------------------------------------------------------------
!
      subroutine  set_elem_refine_itp_tbl_2_IO                          &
     &          (e_ref_IO, IO_itp_org, IO_itp_dest)
!
      type(ele_refine_IO_type), intent(in) :: e_ref_IO
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      integer(kind = kint) :: iele_neo
!
!
      IO_itp_dest%num_org_domain =   ione
      IO_itp_dest%ntot_table_dest =  e_ref_IO%nele_ref
      call alloc_itp_num_dest(IO_itp_dest)
      call alloc_itp_table_dest(IO_itp_dest)
!
      IO_itp_dest%id_org_domain(1) =     izero
      IO_itp_dest%istack_nod_tbl_dest(0) = izero
      IO_itp_dest%istack_nod_tbl_dest(1) = e_ref_IO%nele_ref
!
      do iele_neo = 1, e_ref_IO%nele_ref
        IO_itp_dest%inod_dest_4_dest(iele_neo)                          &
     &          = e_ref_IO%iele_gl_org(iele_neo)
      end do
!
!
      IO_itp_org%num_dest_domain = ione
      IO_itp_org%ntot_table_org =  e_ref_IO%nele_ref
      call alloc_itp_num_org(np_smp, IO_itp_org)
      call alloc_itp_table_org(IO_itp_org)
!
      IO_itp_org%id_dest_domain(1) =        izero
      IO_itp_org%istack_nod_tbl_org(0) =    izero
      IO_itp_org%istack_nod_tbl_org(1) =    e_ref_IO%nele_ref
      IO_itp_org%istack_itp_type_org(0) =   izero
      IO_itp_org%istack_itp_type_org(1:4) = e_ref_IO%nele_ref
!
      do iele_neo = 1, e_ref_IO%nele_ref
        IO_itp_org%inod_itp_send(iele_neo) = iele_neo
        IO_itp_org%inod_gl_dest_4_org(iele_neo)                         &
     &                = e_ref_IO%iele_gl_new(iele_neo)
        IO_itp_org%iele_org_4_org(iele_neo)                             &
     &                = e_ref_IO%iele_gl_new(iele_neo)
        IO_itp_org%itype_inter_org(iele_neo) = izero
        IO_itp_org%coef_inter_org(iele_neo,1) = zero
        IO_itp_org%coef_inter_org(iele_neo,2) = zero
        IO_itp_org%coef_inter_org(iele_neo,3) = zero
      end do
!
      end subroutine  set_elem_refine_itp_tbl_2_IO
!
!------------------------------------------------------------------
!
      subroutine set_ele_refine_flags_from_IO                           &
     &         (ele, e_ref_IO, refine_tbl)
!
      type(element_data), intent(in) :: ele
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
      type(element_refine_table), intent(inout) :: refine_tbl
!
      integer(kind = kint) :: iele
!
!
      call alloc_old_refine_level(ele, refine_tbl)
!
      refine_tbl%max_refine_level = e_ref_IO%max_refine_level
      do iele = 1, e_ref_IO%nele_ref
        refine_tbl%ilevel_refine_old(iele)                              &
     &        = e_ref_IO%ilevel_refine(iele)
      end do
!
      call dealloc_element_refine_IO(e_ref_IO)
!
      end subroutine set_ele_refine_flags_from_IO
!
!------------------------------------------------------------------
!
      end module refinment_info_IO
