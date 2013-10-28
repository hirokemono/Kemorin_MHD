!
!      module refinment_info_IO
!
!      subroutine write_refinement_table
!      subroutine write_merged_refinement_tbl
!
!      subroutine read_refinement_table
!
!
      module refinment_info_IO
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_refined_element_data
      use m_element_refinement_IO
!
      use set_parallel_file_name
!
      implicit  none
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
      subroutine write_refinement_table(iref)
!
      use m_control_param_4_refiner
      use element_refine_file_IO
!
      integer(kind = kint), intent(in) :: iref
!
!
      call set_element_refine_flags_2_IO
      call set_elem_refine_itp_tbl_2_IO
!
!
      if (iflag_tmp_tri_refine .eq. 1) then
        call add_int_suffix(iref, refine_info_head, refine_info_fhead)
      else
        refine_info_fhead = refine_info_head
      end if
!
      call write_element_refine_file(izero, izero)
!
      end subroutine write_refinement_table
!
! ----------------------------------------------------------------------
!
      subroutine write_merged_refinement_tbl
!
      use m_control_param_4_refiner
      use element_refine_file_IO
!
!
      call set_merged_refine_flags_2_IO
      call set_elem_refine_itp_tbl_2_IO
!
      refine_info_fhead = refine_info_head
      call write_element_refine_file(izero, izero)
!
      end subroutine write_merged_refinement_tbl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_refinement_table
!
      use m_control_param_4_refiner
      use element_refine_file_IO
!
!
      refine_info_fhead = refine_info_head
      call read_element_refine_file(izero, izero)
!
      call deallocate_itp_num_org_IO
      call deallocate_itp_table_org_IO
!
      call deallocate_itp_num_dst_IO
      call deallocate_itp_nod_dst_IO
!
      call set_ele_refine_flags_from_IO
!
      end subroutine read_refinement_table
!
! ----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_element_refine_flags_2_IO
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
      subroutine set_merged_refine_flags_2_IO
!
      use m_refine_flag_parameters
      use m_work_merge_refine_itp
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
      subroutine  set_elem_refine_itp_tbl_2_IO
!
      use m_interpolate_table_dest_IO
      use m_interpolate_table_org_IO
!
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
      num_dest_domain_IO = ione
      ntot_table_org_IO =  nele_ref_IO
      call allocate_itp_num_org_IO
      call allocate_itp_table_org_IO
!
      id_dest_domain_IO(1) =     izero
      istack_nod_table_org_IO(0) = izero
      istack_nod_table_org_IO(1) = nele_ref_IO
      istack_table_wtype_org_IO(0) = izero
      istack_table_wtype_org_IO(1:4) = nele_ref_IO
!
      do iele_neo = 1, nele_ref_IO
        inod_itp_send_IO(iele_neo) = iele_neo
        inod_gl_dest_4_org_IO(iele_neo) = iele_global_new_IO(iele_neo)
        iele_org_4_org_IO(iele_neo) =     iele_global_new_IO(iele_neo)
        itype_inter_org_IO(iele_neo) =    izero
        coef_inter_org_IO(iele_neo,1) =    zero
        coef_inter_org_IO(iele_neo,2) =    zero
        coef_inter_org_IO(iele_neo,3) =    zero
      end do
!
      end subroutine  set_elem_refine_itp_tbl_2_IO
!
!------------------------------------------------------------------
!
      subroutine set_ele_refine_flags_from_IO
!
      integer(kind = kint) :: iele
!
!
      call allocate_old_refine_level
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
