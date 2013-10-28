!set_merged_refine_itp.f90
!      module set_merged_refine_itp
!
!      subroutine set_merged_itp_course_to_fine
!      subroutine set_merged_itp_fine_to_course
!
      module set_merged_refine_itp
!
      use m_precision
!
      use m_constants
      use m_work_merge_refine_itp
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
      subroutine set_merged_itp_course_to_fine
!
      use t_interpolate_tbl_org
      use set_refine_interpolate_tbl
      use merge_refine_itp_table
!
!
      write(*,*) 'set_merged_refine_data_org'
      call set_merged_refine_data_org
      write(*,*) 'set_itp_course_to_fine_dest'
      call set_itp_course_to_fine_dest
!
      write(*,*) 'copy_merge_itp_table_refine'
      call copy_merge_itp_table_refine
!
      call dealloc_type_itp_table_org(c2f_mgd%tbl_org)
      call dealloc_type_itp_num_org(c2f_mgd%tbl_org)
!
      end subroutine set_merged_itp_course_to_fine
!
! ----------------------------------------------------------------------
!
      subroutine copy_merge_itp_table_refine
!
      use m_interpolate_table_orgin
      use merge_refine_itp_table
!
!
      num_dest_domain =     c2f_mgd%tbl_org%num_dest_domain
      iflag_self_itp_send = c2f_mgd%tbl_org%iflag_self_itp_send
!
      ntot_table_org = c2f_mgd%tbl_org%ntot_table_org
!
      call allocate_itp_num_org(num_dest_domain)
      call allocate_itp_table_org
!
!
      id_dest_domain =     c2f_mgd%tbl_org%id_dest_domain
      istack_nod_tbl_org = c2f_mgd%tbl_org%istack_nod_tbl_org
!
      istack_nod_tbl_wtype_org(0) = izero
!
      call set_merged_refine_data_org
!
      end subroutine copy_merge_itp_table_refine
!
! ----------------------------------------------------------------------
!
      subroutine set_merged_itp_fine_to_course
!
      use m_geometry_parameter
      use m_refined_element_data
      use m_interpolate_table_orgin
      use copy_local_position_2_ele
!
!
      integer(kind = kint) :: iele, k1, inod
      real(kind = kreal) :: xi_ele(3)
!
!
      num_dest_domain =     ione
      iflag_self_itp_send = ione
!
      call allocate_itp_num_org(num_dest_domain)
!
      id_dest_domain(1) = izero
      istack_nod_tbl_wtype_org(0) = izero
      istack_nod_tbl_wtype_org(1) = nnod_org
      istack_nod_tbl_wtype_org(2) = nnod_org
      istack_nod_tbl_wtype_org(3) = nnod_org
      istack_nod_tbl_wtype_org(4) = nnod_org
      istack_nod_tbl_org(0) = izero
      istack_nod_tbl_org(1) = nnod_org
      ntot_table_org =        nnod_org
!
      call allocate_itp_table_org
      itype_inter_org(1:ntot_table_org) = -1
!
      do iele = 1, ntot_ele_refined
!
        do k1 = 1, nnod_4_ele
          inod = ie_refined(iele,k1)
!
          if(inod.le.nnod_org) then
            if(itype_inter_org(inod).eq.-1) then
              call copy_node_local_posi_2_element(k1, xi_ele)
!
              inod_gl_dest_4_org(inod) = inod
              iele_org_4_org(inod) =     iele
              itype_inter_org(inod) =     k1
              coef_inter_org(inod,1:3) = xi_ele(1:3)
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
