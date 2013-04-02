!m_work_merge_refine_itp.f90
!     Written by H. Matsui on Oct., 2007
!
!      subroutine allocate_mesh_refine_org
!      subroutine deallocate_mesh_refine_org
!
      module m_work_merge_refine_itp
!
      use t_interpolate_table
!
      use m_precision
!
      implicit none
!
      type(interpolate_table) :: c2f_1st
      type(interpolate_table) :: c2f_2nd
      type(interpolate_table) :: c2f_mgd
!
      integer(kind = kint) :: nnod_org, intnod_org
      integer(kind = kint), allocatable :: inod_global_org(:)
      real(kind = kreal), allocatable :: xx_org(:,:)
      real(kind = kreal), allocatable :: xi_org(:,:)
!
      integer(kind = kint) :: nele_org, nnod_4_ele_org, nele_500
      integer(kind = kint), allocatable :: iele_500(:)
      integer(kind = kint), allocatable :: iele_global_org(:)
      integer(kind = kint), allocatable :: ie_org(:,:)
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
      allocate( inod_global_org(nnod_org) )
      allocate( xx_org(nnod_org,3) )
      allocate( xi_org(3,nnod_org) )
      allocate( iele_global_org(nele_org) )
      allocate( ie_org(nele_org,nnod_4_ele_org) )
      inod_global_org = 0
      iele_global_org = 0
      xx_org =  0.0d0
      xi_org = 0.0d0
      ie_org = 0
!
      allocate( ilevel_refine_org(nele_org) )
      allocate( iflag_refine_ele_org(nele_org) )
      allocate( istack_ele_refine_org(0:nele_org) )
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
      deallocate( inod_global_org, xx_org, xi_org )
      deallocate( iele_global_org, ie_org )
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
      subroutine copy_original_mesh_conn_refine
!
      use m_geometry_parameter
      use m_geometry_data
      use m_refined_node_id
      use m_refined_element_data
!
      integer(kind = kint) :: inum, ist, num, iele, i
!
!
      nnod_org =       numnod
      intnod_org =     internal_node
      nele_org =       numele
      nnod_4_ele_org = nnod_4_ele
!
      call allocate_mesh_refine_org
!
      inod_global_org(1:nnod_org) = globalnodid(1:nnod_org)
      xx_org(1:nnod_org,1:3) =      xx(1:nnod_org,1:3)
      iele_global_org(1:nele_org) = globalelmid(1:nele_org)
      ie_org(1:nele_org,1:nnod_4_ele) = ie(1:nele_org,1:nnod_4_ele)
!
      ilevel_refine_org(1:nele_org) =     ilevel_refine(1:nele_org)
      iflag_refine_ele_org(1:nele_org) =  iflag_refine_ele(1:nele_org)
      istack_ele_refine_org(0:nele_org) = istack_ele_refined(0:nele_org)
      nele_1st = istack_ele_refine_org(nele_org)
!
!
      nnod_1st = ntot_nod_refine_nod + ntot_nod_refine_ele              &
     &           + ntot_nod_refine_surf + ntot_nod_refine_edge
      call allocate_1st_refine_info
!
      do inum = 1, nele_org
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
