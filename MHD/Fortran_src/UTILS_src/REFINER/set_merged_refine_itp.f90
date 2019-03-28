!set_merged_refine_itp.f90
!      module set_merged_refine_itp
!
!!      subroutine set_merged_itp_course_to_fine                        &
!!     &          (nnod_4_ele, nnod_2, nnod_4_ele_2, xx_2,              &
!!     &           ref_itp_wk, itp_org, itp_dest)
!!        type(work_merge_refine_itp), intent(inout) :: ref_itp_wk
!!        type(interpolate_table_org), intent(inout) :: itp_org
!!        type(interpolate_table_dest), intent(inout) :: itp_dest
!!      subroutine set_merged_itp_fine_to_course                        &
!!     &         (nnod_4_ele, ntot_ele_refined, ie_refined,             &
!!     &          node_org_refine, itp_org)
!!        type(node_data), intent(in) :: node_org_refine
!!        type(interpolate_table_org), intent(inout) :: itp_org
!
      module set_merged_refine_itp
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_work_merge_refine_itp
!
      implicit none
!
      private :: copy_merge_itp_table_refine
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_merged_itp_course_to_fine                          &
     &          (nnod_4_ele, nnod_2, nnod_4_ele_2, xx_2,                &
     &           ref_itp_wk, itp_org, itp_dest)
!
      use set_refine_interpolate_tbl
      use merge_refine_itp_table
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: nnod_2, nnod_4_ele_2
      real(kind = kreal), intent(in) :: xx_2(nnod_2,3)
!
      type(work_merge_refine_itp), intent(inout) :: ref_itp_wk
      type(interpolate_table_org), intent(inout) :: itp_org
      type(interpolate_table_dest), intent(inout) :: itp_dest
!
!
      write(*,*) 'set_merged_refine_data_org'
      call set_merged_refine_data_org                                   &
     &   (ref_itp_wk%node_org_refine, ref_itp_wk%ele_org_refine,        &
     &    ref_itp_wk%c2f_2nd, ref_itp_wk%elist_1st,                     &
     &    nnod_4_ele, nnod_2, nnod_4_ele_2, xx_2, ref_itp_wk%c2f_mgd)
      write(*,*) 'set_itp_course_to_fine_dest'
      call set_itp_course_to_fine_dest(nnod_2, itp_dest)
!
      write(*,*) 'copy_merge_itp_table_refine'
      call copy_merge_itp_table_refine(ref_itp_wk%c2f_mgd, itp_org)
!
      call dealloc_itp_table_org(ref_itp_wk%c2f_mgd%tbl_org)
      call dealloc_itp_num_org(ref_itp_wk%c2f_mgd%tbl_org)
!
      write(*,*) 'set_merged_refine_data_org 2nd'
      call set_merged_refine_data_org                                   &
     &   (ref_itp_wk%node_org_refine, ref_itp_wk%ele_org_refine,        &
     &    ref_itp_wk%c2f_2nd, ref_itp_wk%elist_1st,                     &
     &    nnod_4_ele, nnod_2, nnod_4_ele_2, xx_2, ref_itp_wk%c2f_mgd)
!
      end subroutine set_merged_itp_course_to_fine
!
! ----------------------------------------------------------------------
!
      subroutine copy_merge_itp_table_refine(c2f_mgd, itp_org)
!
      type(interpolate_table), intent(in) :: c2f_mgd
!
      type(interpolate_table_org), intent(inout) :: itp_org
!
!
      itp_org%iflag_self_itp_send = c2f_mgd%tbl_org%iflag_self_itp_send
      call set_num_dest_domain                                          &
     &   (c2f_mgd%tbl_org%num_dest_domain, itp_org)
      call alloc_itp_num_org(np_smp, itp_org)
!
      itp_org%ntot_table_org = c2f_mgd%tbl_org%ntot_table_org
!
      call alloc_itp_table_org(itp_org)
!
!
      itp_org%id_dest_domain =     c2f_mgd%tbl_org%id_dest_domain
      itp_org%istack_nod_tbl_org = c2f_mgd%tbl_org%istack_nod_tbl_org
!
      itp_org%istack_itp_type_org(0) = izero
!
      end subroutine copy_merge_itp_table_refine
!
! ----------------------------------------------------------------------
!
      subroutine set_merged_itp_fine_to_course                          &
     &         (nnod_4_ele, ntot_ele_refined, ie_refined,               &
     &          node_org_refine, itp_org)
!
      use copy_local_position_2_ele
!
      integer(kind = kint), intent(in) :: ntot_ele_refined, nnod_4_ele
      integer(kind = kint), intent(in)                                  &
     &             :: ie_refined(ntot_ele_refined,nnod_4_ele)
      type(node_data), intent(in) :: node_org_refine
!
      type(interpolate_table_org), intent(inout) :: itp_org
!
      integer(kind = kint) :: iele, k1, inod
      real(kind = kreal) :: xi_ele(3)
!
!
      itp_org%iflag_self_itp_send = ione
      call set_num_dest_domain(ione, itp_org)
      call alloc_itp_num_org(np_smp, itp_org)
!
      itp_org%id_dest_domain(1) = izero
      itp_org%istack_itp_type_org(0) = izero
      itp_org%istack_itp_type_org(1) = node_org_refine%numnod
      itp_org%istack_itp_type_org(2) = node_org_refine%numnod
      itp_org%istack_itp_type_org(3) = node_org_refine%numnod
      itp_org%istack_itp_type_org(4) = node_org_refine%numnod
      itp_org%istack_nod_tbl_org(0) = izero
      itp_org%istack_nod_tbl_org(1) = node_org_refine%numnod
      itp_org%ntot_table_org =        node_org_refine%numnod
!
      call alloc_itp_table_org(itp_org)
      itp_org%itype_inter_org(1:itp_org%ntot_table_org) = -1
!
      do iele = 1, ntot_ele_refined
!
        do k1 = 1, nnod_4_ele
          inod = ie_refined(iele,k1)
!
          if(inod.le.node_org_refine%numnod) then
            if(itp_org%itype_inter_org(inod).eq.-1) then
              call copy_node_local_posi_2_element(k1, xi_ele)
!
              itp_org%inod_gl_dest_4_org(inod) = inod
              itp_org%iele_org_4_org(inod) =     iele
              itp_org%itype_inter_org(inod) =     k1
              itp_org%coef_inter_org(inod,1:3) = xi_ele(1:3)
            end if
          end if
        end do
      end do
!
      end subroutine set_merged_itp_fine_to_course
!
! ----------------------------------------------------------------------
!
      end module set_merged_refine_itp
