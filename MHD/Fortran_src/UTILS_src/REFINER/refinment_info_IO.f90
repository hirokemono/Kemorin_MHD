!
!      module refinment_info_IO
!
!      subroutine write_refinement_table(numele, iref)
!      subroutine write_merged_refinement_tbl(numele)
!
!      subroutine read_refinement_table(numele)
!
!
      module refinment_info_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_constants
      use m_refined_element_data
      use t_element_refinement_IO
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
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
      subroutine write_refinement_table(numele, iref)
!
      use m_control_param_4_refiner
      use element_refine_file_IO
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: iref
!
      integer(kind = kint) :: ierr = 0
!
      call set_element_refine_flags_2_IO(numele, IO_e_ref)
      call set_elem_refine_itp_tbl_2_IO                                 &
     &   (IO_e_ref, IO_itp_e_org, IO_itp_e_dest)
!
!
      if (iflag_tmp_tri_refine .eq. 1) then
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
      subroutine write_merged_refinement_tbl(numele)
!
      use m_control_param_4_refiner
      use element_refine_file_IO
!
      integer(kind = kint), intent(in) :: numele
!
      integer(kind = kint) :: ierr = 0
!
!
      call set_merged_refine_flags_2_IO(numele, IO_e_ref)
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
      subroutine read_refinement_table(numele)
!
      use m_control_param_4_refiner
      use element_refine_file_IO
!
      integer(kind = kint), intent(in) :: numele
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
      call set_ele_refine_flags_from_IO(numele, IO_e_ref)
!
      end subroutine read_refinement_table
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_element_refine_flags_2_IO(numele, e_ref_IO)
!
      integer(kind = kint), intent(in) :: numele
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      integer(kind = kint) :: iele_org, ist, iele_neo, icou
!
!
      e_ref_IO%max_refine_level = max_refine_level
      e_ref_IO%nele_org =  numele
      e_ref_IO%nele_ref =  istack_ele_refined(numele)
!
      call alloc_element_refine_IO(e_ref_IO)
!
      do iele_org = 1, numele
        ist = istack_ele_refined(iele_org-1)
        do icou = 1, num_ele_refined(iele_org)
          iele_neo = icou + istack_ele_refined(iele_org-1)
          e_ref_IO%iele_gl_new(iele_neo) =  iele_neo
          e_ref_IO%iele_gl_org(iele_neo) =  iele_org
          e_ref_IO%icou_gl_org(iele_neo) =  icou
          e_ref_IO%iflag_refine_ele(iele_neo)                           &
     &                               =  iflag_refine_ele(iele_org)
          e_ref_IO%ilevel_refine(iele_neo)                              &
     &        = ilevel_refine_old(iele_org) + ilevel_refine(iele_org)
        end do
      end do
!
      end subroutine set_element_refine_flags_2_IO
!
!------------------------------------------------------------------
!
      subroutine set_merged_refine_flags_2_IO(numele, e_ref_IO)
!
      use m_refine_flag_parameters
      use m_work_merge_refine_itp
!
      integer(kind = kint), intent(in) :: numele
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      integer(kind = kint) :: iele_org, ist, icou
      integer(kind = kint) :: iele_1st, iflag_1st, icou_1st
      integer(kind = kint) :: iele_2nd, iflag_2nd, icou_2nd
!
!
      e_ref_IO%max_refine_level = max_refine_level
      e_ref_IO%nele_org =  numele
      e_ref_IO%nele_ref =  istack_ele_refined(numele)
!
      call alloc_element_refine_IO(e_ref_IO)
!
      do iele_1st = 1, numele
        iflag_1st = iflag_refine_ele_1st(iele_1st)
        iele_org =  iele_org_1st(iele_1st,1)
        icou_1st =  iele_org_1st(iele_1st,2)
!
        ist = istack_ele_refined(iele_1st-1)
        do icou = 1, num_ele_refined(iele_1st)
          iele_2nd = icou + istack_ele_refined(iele_1st-1)
!
          if( iflag_1st .ne. iflag_nothing) then
            iflag_2nd = iflag_refine_ele(iele_1st) + iflag_1st*1000
            icou_2nd =  icou + icou_1st * 100
          else
            iflag_2nd = iflag_refine_ele(iele_1st)
            icou_2nd =  icou
          end if
!
          e_ref_IO%iele_gl_new(iele_2nd) =   iele_2nd
          e_ref_IO%iele_gl_org(iele_2nd) =   iele_org
          e_ref_IO%icou_gl_org(iele_2nd) =   icou_2nd
          e_ref_IO%iflag_refine_ele(iele_2nd) =  iflag_2nd
          e_ref_IO%ilevel_refine(iele_2nd)                              &
     &        =  ilevel_refine_old(iele_org)                            &
     &         + ilevel_refine_org(iele_org) + ilevel_refine(iele_1st)
        end do
      end do
!
      call deallocate_old_refine_level
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
      subroutine set_ele_refine_flags_from_IO(numele, e_ref_IO)
!
      integer(kind = kint), intent(in) :: numele
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      integer(kind = kint) :: iele
!
!
      call allocate_old_refine_level(numele)
!
      max_refine_level = e_ref_IO%max_refine_level
      do iele = 1, e_ref_IO%nele_ref
        ilevel_refine_old(iele) = e_ref_IO%ilevel_refine(iele)
      end do
!
      call dealloc_element_refine_IO(e_ref_IO)
!
      end subroutine set_ele_refine_flags_from_IO
!
!------------------------------------------------------------------
!
      end module refinment_info_IO
