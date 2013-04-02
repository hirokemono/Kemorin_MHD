!
!      module m_filtering_edge_4_cubmesh
!
      module m_filtering_edge_4_cubmesh
!
!     Written by H. Matsui
!
      use m_precision
!
      implicit none
!
!
      integer (kind = kint) :: iedge_f_total
!
!
      real (kind = kreal), allocatable :: iedge_filter_order0(:,:)
      real (kind = kreal), allocatable :: iedge_filter_order1(:,:,:)
      real (kind = kreal), allocatable :: iedge_filter_order2(:,:,:)
!
      integer(kind = kint), allocatable :: nedge_neib_x(:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_item_x(:,:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_dist_x(:,:,:,:,:)
!
      integer(kind = kint), allocatable :: nedge_neib_y(:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_item_y(:,:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_dist_y(:,:,:,:,:)
!
      integer(kind = kint), allocatable :: nedge_neib_z(:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_item_z(:,:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_dist_z(:,:,:,:,:)
!
      integer(kind = kint), allocatable :: nedge_neib_xy(:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_item_xy(:,:,:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_dist_xy(:,:,:,:,:,:)
!
      integer(kind = kint), allocatable :: nedge_neib(:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_item_3d(:,:,:,:,:,:)
      integer(kind = kint), allocatable :: iedge_f_dist_3d(:,:,:,:,:,:)
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
       subroutine allocate_work_4_filter_edge
!
       use m_size_of_cube
       use m_comm_data_cube_kemo
!
       integer (kind = kint) :: nsize, nsize2, nsize3
!
       nsize = 2*ndepth+1
       nsize2 = nsize*nsize
       nsize3 = nsize2*nsize
!
       allocate( nedge_neib_x (numnod_x,numnod_y,numnod_z,3)    )
       allocate( nedge_neib_y (numnod_x,numnod_y,numnod_z,3)    )
       allocate( nedge_neib_z (numnod_x,numnod_y,numnod_z,3)    )
       allocate( nedge_neib_xy(numnod_x,numnod_y,numnod_z,3)    )
       allocate( nedge_neib   (numnod_x,numnod_y,numnod_z,3)    )
!
       allocate( iedge_f_item_x(nsize,numnod_x,numnod_y,numnod_z,3)  )
       allocate( iedge_f_dist_x(nsize,numnod_x,numnod_y,numnod_z,3)  )
       allocate( iedge_f_item_y(nsize,numnod_x,numnod_y,numnod_z,3)  )
       allocate( iedge_f_dist_y(nsize,numnod_x,numnod_y,numnod_z,3)  )
       allocate( iedge_f_item_z(nsize,numnod_x,numnod_y,numnod_z,3)  )
       allocate( iedge_f_dist_z(nsize,numnod_x,numnod_y,numnod_z,3)  )
!
       allocate( iedge_f_item_xy(nsize2,numnod_x,numnod_y,numnod_z,3,2) )
       allocate( iedge_f_dist_xy(nsize2,numnod_x,numnod_y,numnod_z,3,2) )
!
       allocate( iedge_f_item_3d(nsize3,numnod_x,numnod_y,numnod_z,3,3) )
       allocate( iedge_f_dist_3d(nsize3,numnod_x,numnod_y,numnod_z,3,3) )
!
       nedge_neib =    0
       nedge_neib_x =  0
       nedge_neib_y =  0
       nedge_neib_z =  0
       nedge_neib_xy = 0
!
       iedge_f_item_x =  0
       iedge_f_dist_x =  0
       iedge_f_item_y =  0
       iedge_f_dist_y =  0
       iedge_f_item_z =  0
       iedge_f_dist_z =  0
       iedge_f_item_xy = 0
       iedge_f_dist_xy = 0
       iedge_f_item_3d = 0
       iedge_f_dist_3d = 0
!
       end subroutine allocate_work_4_filter_edge
!
!  ----------------------------------------------------------------------
!  ----------------------------------------------------------------------
!
       subroutine deallocate_work_4_filter_edge
!
       deallocate( nedge_neib_x     )
       deallocate( nedge_neib_y     )
       deallocate( nedge_neib_z     )
       deallocate( nedge_neib_xy    )
       deallocate( nedge_neib       )
!
       deallocate( iedge_f_item_x  )
       deallocate( iedge_f_dist_x  )
       deallocate( iedge_f_item_y  )
       deallocate( iedge_f_dist_y  )
       deallocate( iedge_f_item_z  )
       deallocate( iedge_f_dist_z  )
!
       deallocate( iedge_f_item_xy )
       deallocate( iedge_f_dist_xy )
!
       deallocate( iedge_f_item_3d )
       deallocate( iedge_f_dist_3d )
!
       end subroutine deallocate_work_4_filter_edge
!
!  ----------------------------------------------------------------------
!
      end module m_filtering_edge_4_cubmesh
