!t_work_merge_refine_itp.f90
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine dealloc_org_refine_tbl                               &
!!     &         (node_org_refine, ele_org_refine)
!!      subroutine dealloc_mesh_refine_org(ref_org)
!!      subroutine dealloc_1st_refine_info(elist_1st)
!!
!!      subroutine copy_original_mesh_conn_refine(node, ele, refine_tbl,&
!!     &          refine_nod, refine_ele, refine_surf, refine_edge,     &
!!     &          node_org_refine, ele_org_refine, ref_org, elist_1st)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(element_refine_table), intent(in) :: refine_tbl
!!        type(table_4_refine), intent(in) :: refine_nod, refine_ele
!!        type(table_4_refine), intent(in) :: refine_surf, refine_edge
!!        type(orginal_refine_level), intent(inout) :: ref_org
!!        type(first_element_list), intent(inout) :: elist_1st
!
!
      module t_work_merge_refine_itp
!
      use m_precision
!
      use t_geometry_data
      use t_interpolate_table
!
      implicit none
!
      type orginal_refine_level
        real(kind = kreal), allocatable :: xi_org(:,:)
!
        integer(kind = kint), allocatable :: ilevel_refine_org(:)
        integer(kind = kint), allocatable :: iflag_refine_ele_org(:)
        integer(kind = kint), allocatable :: istack_ele_refine_org(:)
      end type orginal_refine_level
!
      type first_element_list
        integer(kind = kint) :: ntot_n
        integer(kind = kint) :: ntot_e
        integer(kind = kint), allocatable :: iflag_ref_1st(:)
        integer(kind = kint), allocatable :: iele_1st(:,:)
      end type first_element_list
!
!
      type work_merge_refine_itp
        type(interpolate_table) :: c2f_1st
        type(interpolate_table) :: c2f_2nd
        type(interpolate_table) :: c2f_mgd
!
        type(node_data) :: node_org_refine
!
        type(element_data) :: ele_org_refine
!
        type(orginal_refine_level) :: ref_org
        type(first_element_list) :: elist_1st
      end type work_merge_refine_itp
!
      private :: alloc_mesh_refine_org
      private :: alloc_1st_refine_info
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_mesh_refine_org                                  &
     &         (node_org_refine, ele_org_refine, ref_org)
!
      type(node_data), intent(in) :: node_org_refine
      type(element_data), intent(in) :: ele_org_refine
      type(orginal_refine_level), intent(inout) :: ref_org
!
!
      allocate(ref_org%xi_org(3,node_org_refine%numnod) )
      ref_org%xi_org = 0.0d0
!
      allocate( ref_org%ilevel_refine_org(ele_org_refine%numele) )
      allocate( ref_org%iflag_refine_ele_org(ele_org_refine%numele) )
      allocate( ref_org%istack_ele_refine_org(0:ele_org_refine%numele) )
      ref_org%istack_ele_refine_org = 0
!
      end subroutine alloc_mesh_refine_org
!
!   --------------------------------------------------------------------
!
      subroutine alloc_1st_refine_info(ntot_ele, elist_1st)
!
      integer(kind = kint), intent(in) :: ntot_ele
      type(first_element_list), intent(inout) :: elist_1st
!
      elist_1st%ntot_e = ntot_ele
      allocate( elist_1st%iflag_ref_1st(elist_1st%ntot_e) )
      allocate( elist_1st%iele_1st(elist_1st%ntot_e,2) )
      elist_1st%iflag_ref_1st = 0
      elist_1st%iele_1st =      0
!
      end subroutine alloc_1st_refine_info
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_org_refine_tbl                                 &
     &         (node_org_refine, ele_org_refine)
!
        type(node_data), intent(inout) :: node_org_refine
        type(element_data), intent(inout) :: ele_org_refine
!
!
      deallocate( node_org_refine%inod_global, node_org_refine%xx )
      deallocate( ele_org_refine%iele_global, ele_org_refine%ie )
!
!
      end subroutine dealloc_org_refine_tbl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_mesh_refine_org(ref_org)
!
      type(orginal_refine_level), intent(inout) :: ref_org
!
!
      deallocate( ref_org%xi_org )
      deallocate( ref_org%istack_ele_refine_org )
      deallocate( ref_org%ilevel_refine_org )
      deallocate( ref_org%iflag_refine_ele_org )
!
      end subroutine dealloc_mesh_refine_org
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_1st_refine_info(elist_1st)
!
      type(first_element_list), intent(inout) :: elist_1st
!
      deallocate(elist_1st%iflag_ref_1st, elist_1st%iele_1st)
!
      end subroutine dealloc_1st_refine_info
!
!   --------------------------------------------------------------------
!
      subroutine copy_original_mesh_conn_refine(node, ele, refine_tbl,  &
     &          refine_nod, refine_ele, refine_surf, refine_edge,       &
     &          node_org_refine, ele_org_refine, ref_org, elist_1st)
!
      use t_refined_node_id
      use t_refined_element_data
      use copy_mesh_structures
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_refine_table), intent(in) :: refine_tbl
      type(table_4_refine), intent(in) :: refine_nod, refine_ele
      type(table_4_refine), intent(in) :: refine_surf, refine_edge
!
      type(node_data), intent(inout) :: node_org_refine
      type(element_data), intent(inout) :: ele_org_refine
      type(orginal_refine_level), intent(inout) :: ref_org
      type(first_element_list), intent(inout) :: elist_1st
!
      integer(kind = kint) :: inum, ist, num, iele, i
!
!
      call copy_node_geometry_types(node, node_org_refine)
      call copy_element_connect_types(ele, ele_org_refine)
      call alloc_sph_node_geometry(node_org_refine)
!
      call alloc_mesh_refine_org                                        &
     &   (node_org_refine, ele_org_refine, ref_org)
!
!
      ref_org%ilevel_refine_org(1:ele_org_refine%numele)                &
     &       = refine_tbl%ilevel_refine(1:ele_org_refine%numele)
      ref_org%iflag_refine_ele_org(1:ele_org_refine%numele)             &
     &       = refine_tbl%iflag_refine_ele(1:ele_org_refine%numele)
      ref_org%istack_ele_refine_org(0:ele_org_refine%numele)            &
     &       = refine_tbl%istack_ele_refined(0:ele_org_refine%numele)
!
!
      elist_1st%ntot_n =    refine_nod%ntot_nod_refine                  &
     &                    + refine_ele%ntot_nod_refine                  &
     &                    + refine_surf%ntot_nod_refine                 &
     &                    + refine_edge%ntot_nod_refine
      call alloc_1st_refine_info                                        &
     &   (ref_org%istack_ele_refine_org(ele_org_refine%numele),         &
     &    elist_1st)
!
      do inum = 1, ele_org_refine%numele
        ist = ref_org%istack_ele_refine_org(inum-1)
        num = ref_org%istack_ele_refine_org(inum) - ist
        do i = 1, num
          iele = i + ist
          elist_1st%iflag_ref_1st(iele)                                 &
     &         = refine_tbl%iflag_refine_ele(inum)
          elist_1st%iele_1st(iele,1) = inum
          elist_1st%iele_1st(iele,2) = i
        end do
      end do
!
      end subroutine copy_original_mesh_conn_refine
!
!   --------------------------------------------------------------------
!
      end module t_work_merge_refine_itp
