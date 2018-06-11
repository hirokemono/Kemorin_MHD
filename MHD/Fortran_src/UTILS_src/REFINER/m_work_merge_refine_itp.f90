!m_work_merge_refine_itp.f90
!     Written by H. Matsui on Oct., 2007
!
!      subroutine allocate_mesh_refine_org
!      subroutine deallocate_mesh_refine_org
!
!      subroutine copy_original_mesh_conn_refine(node, ele)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!
!
      module m_work_merge_refine_itp
!
      use m_precision
!
      use t_geometry_data
      use t_interpolate_table
!
      implicit none
!
      type(interpolate_table) :: c2f_1st
      type(interpolate_table) :: c2f_2nd
      type(interpolate_table) :: c2f_mgd
!
      type(node_data) :: node_org_refine
!
      type(element_data) :: ele_org_refine
!
      real(kind = kreal), allocatable :: xi_org(:,:)
!
      integer(kind = kint), allocatable :: ilevel_refine_org(:)
      integer(kind = kint), allocatable :: iflag_refine_ele_org(:)
      integer(kind = kint), allocatable :: istack_ele_refine_org(:)
!
      integer(kind = kint) :: nnod_1st
      real(kind = kreal), allocatable :: xi_1st(:,:)
!
      integer(kind = kint) :: nele_1st
      integer(kind = kint), allocatable :: iflag_refine_ele_1st(:)
      integer(kind = kint), allocatable :: iele_org_1st(:,:)
!
      real(kind = kreal) :: xi_refine_local_tri(3,64)
!
      private :: set_local_position_full_tri
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_mesh_refine_org
!
!
      allocate( xi_org(3,node_org_refine%numnod) )
      xi_org = 0.0d0
!
      allocate( ilevel_refine_org(ele_org_refine%numele) )
      allocate( iflag_refine_ele_org(ele_org_refine%numele) )
      allocate( istack_ele_refine_org(0:ele_org_refine%numele) )
      istack_ele_refine_org = 0
!
      end subroutine allocate_mesh_refine_org
!
!   --------------------------------------------------------------------
!
      subroutine allocate_1st_refine_info
!
!
      allocate( xi_1st(3,nnod_1st) )
      allocate( iflag_refine_ele_1st(nele_1st) )
      allocate( iele_org_1st(nele_1st,2) )
      xi_1st = 0.0d0
      iflag_refine_ele_1st = 0
      iele_org_1st =         0
!
      end subroutine allocate_1st_refine_info
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_mesh_refine_org
!
!
      deallocate( node_org_refine%inod_global)
      deallocate( node_org_refine%xx, xi_org )
      deallocate( ele_org_refine%iele_global, ele_org_refine%ie )
!
      deallocate( istack_ele_refine_org )
!
      end subroutine deallocate_mesh_refine_org
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_1st_refine_info
!
!
      deallocate( iflag_refine_ele_1st, iele_org_1st)
!
      end subroutine deallocate_1st_refine_info
!
!   --------------------------------------------------------------------
!
      subroutine copy_original_mesh_conn_refine(node, ele)
!
      use m_refined_node_id
      use m_refined_element_data
      use copy_mesh_structures
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint) :: inum, ist, num, iele, i
!
!
      call copy_node_geometry_types(node, node_org_refine)
      call copy_element_connect_types(ele, ele_org_refine)
      call alloc_sph_node_geometry(node_org_refine)
!
      call allocate_mesh_refine_org
!
!
      ilevel_refine_org(1:ele_org_refine%numele)                        &
     &       =   ilevel_refine(1:ele_org_refine%numele)
      iflag_refine_ele_org(1:ele_org_refine%numele)                     &
     &       =  iflag_refine_ele(1:ele_org_refine%numele)
      istack_ele_refine_org(0:ele_org_refine%numele)                    &
     &        = istack_ele_refined(0:ele_org_refine%numele)
      nele_1st = istack_ele_refine_org(ele_org_refine%numele)
!
!
      nnod_1st = ntot_nod_refine_nod + ntot_nod_refine_ele              &
     &           + ntot_nod_refine_surf + ntot_nod_refine_edge
      call allocate_1st_refine_info
!
      do inum = 1, ele_org_refine%numele
        ist = istack_ele_refine_org(inum-1)
        num = istack_ele_refine_org(inum) - ist
        do i = 1, num
          iele = i + ist
          iflag_refine_ele_1st(iele) = iflag_refine_ele(inum)
          iele_org_1st(iele,1) = inum
          iele_org_1st(iele,2) = i
        end do
      end do
!
      call set_local_position_full_tri
!
      end subroutine copy_original_mesh_conn_refine
!
!   --------------------------------------------------------------------
!
      subroutine set_local_position_full_tri
!
      use m_constants
!
      integer(kind = kint) :: i, k
!
!
      do i = 1, 16
        xi_refine_local_tri(1,4*i-3) = -one
        xi_refine_local_tri(1,4*i-2) = -third
        xi_refine_local_tri(1,4*i-1) =  third
        xi_refine_local_tri(1,4*i  ) =  one
      end do
!
      do k = 1, 4
        do i = 1, 4
          xi_refine_local_tri(2,16*k+i-16) = -one
          xi_refine_local_tri(2,16*k+i-12) = -third
          xi_refine_local_tri(2,16*k+i-8 ) =  third
          xi_refine_local_tri(2,16*k+i-4 ) =  one
        end do
      end do
!
      do k = 1, 16
        xi_refine_local_tri(3,k   ) = -one
        xi_refine_local_tri(3,k+16) = -third
        xi_refine_local_tri(3,k+32) =  third
        xi_refine_local_tri(3,k+48) =  one
      end do
!
      end subroutine set_local_position_full_tri
!
!   --------------------------------------------------------------------
!
      end module m_work_merge_refine_itp
