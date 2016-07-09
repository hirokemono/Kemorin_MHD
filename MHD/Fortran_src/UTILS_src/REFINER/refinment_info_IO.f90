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
      use m_element_refinement_IO
      use t_interpolate_tbl_org
!
      use set_parallel_file_name
!
      implicit  none
!
      type(interpolate_table_org), save, private :: IO_itp_e_org
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
!
      call set_element_refine_flags_2_IO(numele)
      call set_elem_refine_itp_tbl_2_IO(IO_itp_e_org)
!
!
      if (iflag_tmp_tri_refine .eq. 1) then
        call add_int_suffix(iref, refine_info_head, refine_info_fhead)
      else
        refine_info_fhead = refine_info_head
      end if
!
      call write_element_refine_file(izero, izero, IO_itp_e_org)
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
!
      call set_merged_refine_flags_2_IO(numele)
      call set_elem_refine_itp_tbl_2_IO(IO_itp_e_org)
!
      refine_info_fhead = refine_info_head
      call write_element_refine_file(izero, izero, IO_itp_e_org)
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
      refine_info_fhead = refine_info_head
      call read_element_refine_file(izero, izero, IO_itp_e_org)
!
      call dealloc_itp_num_org(IO_itp_e_org)
      call dealloc_itp_table_org(IO_itp_e_org)
!
      call deallocate_itp_num_dst_IO
      call deallocate_itp_nod_dst_IO
!
      call set_ele_refine_flags_from_IO(numele)
!
      end subroutine read_refinement_table
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_element_refine_flags_2_IO(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      integer(kind = kint) :: iele_org, ist, iele_neo, icou
!
!
      max_refine_level_IO = max_refine_level
      nele_org_IO =         numele
      nele_ref_IO =         istack_ele_refined(numele)
!
      call allocate_element_refine_IO
!
      do iele_org = 1, numele
        ist = istack_ele_refined(iele_org-1)
        do icou = 1, num_ele_refined(iele_org)
          iele_neo = icou + istack_ele_refined(iele_org-1)
          iele_global_new_IO(iele_neo) =  iele_neo
          iele_global_org_IO(iele_neo) =  iele_org
          icou_global_org_IO(iele_neo) =  icou
          iflag_refine_ele_IO(iele_neo) =  iflag_refine_ele(iele_org)
          ilevel_refine_IO(iele_neo) =     ilevel_refine_old(iele_org)  &
     &                                   + ilevel_refine(iele_org)
        end do
      end do
!
      end subroutine set_element_refine_flags_2_IO
!
!------------------------------------------------------------------
!
      subroutine set_merged_refine_flags_2_IO(numele)
!
      use m_refine_flag_parameters
      use m_work_merge_refine_itp
!
      integer(kind = kint), intent(in) :: numele
!
      integer(kind = kint) :: iele_org, ist, icou
      integer(kind = kint) :: iele_1st, iflag_1st, icou_1st
      integer(kind = kint) :: iele_2nd, iflag_2nd, icou_2nd
!
!
      max_refine_level_IO = max_refine_level
      nele_org_IO =         numele
      nele_ref_IO =         istack_ele_refined(numele)
!
      call allocate_element_refine_IO
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
          iele_global_new_IO(iele_2nd) =   iele_2nd
          iele_global_org_IO(iele_2nd) =   iele_org
          icou_global_org_IO(iele_2nd) =   icou_2nd
          iflag_refine_ele_IO(iele_2nd) =  iflag_2nd
          ilevel_refine_IO(iele_2nd) =     ilevel_refine_old(iele_org)  &
     &                                   + ilevel_refine_org(iele_org)  &
     &                                   + ilevel_refine(iele_1st)
        end do
      end do
!
      call deallocate_old_refine_level
!
      end subroutine set_merged_refine_flags_2_IO
!
!------------------------------------------------------------------
!
      subroutine  set_elem_refine_itp_tbl_2_IO(IO_itp_org)
!
      use m_interpolate_table_dest_IO
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      integer(kind = kint) :: iele_neo
!
!
      num_org_domain_IO =   ione
      ntot_table_dest_IO =  nele_ref_IO
      call allocate_itp_num_dst_IO
      call allocate_itp_nod_dst_IO
!
      id_org_domain_IO(1) =     izero
      istack_table_dest_IO(0) = izero
      istack_table_dest_IO(1) = nele_ref_IO
!
      do iele_neo = 1, nele_ref_IO
        inod_dest_IO(iele_neo) = iele_global_org_IO(iele_neo)
      end do
!
!
      IO_itp_org%num_dest_domain = ione
      IO_itp_org%ntot_table_org =  nele_ref_IO
      call alloc_itp_num_org(np_smp, IO_itp_org)
      call alloc_itp_table_org(IO_itp_org)
!
      IO_itp_org%id_dest_domain(1) =        izero
      IO_itp_org%istack_nod_tbl_org(0) =    izero
      IO_itp_org%istack_nod_tbl_org(1) =    nele_ref_IO
      IO_itp_org%istack_itp_type_org(0) =   izero
      IO_itp_org%istack_itp_type_org(1:4) = nele_ref_IO
!
      do iele_neo = 1, nele_ref_IO
        IO_itp_org%inod_itp_send(iele_neo) = iele_neo
        IO_itp_org%inod_gl_dest_4_org(iele_neo)                         &
     &                = iele_global_new_IO(iele_neo)
        IO_itp_org%iele_org_4_org(iele_neo)                             &
     &                = iele_global_new_IO(iele_neo)
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
      subroutine set_ele_refine_flags_from_IO(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      integer(kind = kint) :: iele
!
!
      call allocate_old_refine_level(numele)
!
      max_refine_level = max_refine_level_IO
      do iele = 1, nele_ref_IO
        ilevel_refine_old(iele) = ilevel_refine_IO(iele)
      end do
!
      call deallocate_element_refine_IO
!
      end subroutine set_ele_refine_flags_from_IO
!
!------------------------------------------------------------------
!
      end module refinment_info_IO
