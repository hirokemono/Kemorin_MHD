!
!      module m_filtering_edge_4_cubmesh
!
!     Written by H. Matsui
!
      module m_filtering_edge_4_cubmesh
!
      use m_precision
!
      implicit none
!
!
      integer (kind = kint) :: iedge_f_total
!
!
      integer(kind = kint), private ::  nnod_x
      integer(kind = kint), private ::  nnod_y
      integer(kind = kint), private ::  nnod_z
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
       subroutine allocate_work_4_filter_edge(c_size)
!
       use t_size_of_cube
       use m_comm_data_cube_kemo
!
      type(size_of_cube), intent(in) :: c_size
       integer (kind = kint) :: nsize, nsize2, nsize3
!
       nsize = 2 * c_size%ndepth + 1
       nsize2 = nsize*nsize
       nsize3 = nsize2*nsize
!
      nnod_x = c_size%numnod_x
      nnod_y = c_size%numnod_y
      nnod_z = c_size%numnod_z
!
       allocate( nedge_neib_x (nnod_x,nnod_y,nnod_z,3)    )
       allocate( nedge_neib_y (nnod_x,nnod_y,nnod_z,3)    )
       allocate( nedge_neib_z (nnod_x,nnod_y,nnod_z,3)    )
       allocate( nedge_neib_xy(nnod_x,nnod_y,nnod_z,3)    )
       allocate( nedge_neib   (nnod_x,nnod_y,nnod_z,3)    )
!
       allocate( iedge_f_item_x(nsize,nnod_x,nnod_y,nnod_z,3)  )
       allocate( iedge_f_dist_x(nsize,nnod_x,nnod_y,nnod_z,3)  )
       allocate( iedge_f_item_y(nsize,nnod_x,nnod_y,nnod_z,3)  )
       allocate( iedge_f_dist_y(nsize,nnod_x,nnod_y,nnod_z,3)  )
       allocate( iedge_f_item_z(nsize,nnod_x,nnod_y,nnod_z,3)  )
       allocate( iedge_f_dist_z(nsize,nnod_x,nnod_y,nnod_z,3)  )
!
       allocate( iedge_f_item_xy(nsize2,nnod_x,nnod_y,nnod_z,3,2) )
       allocate( iedge_f_dist_xy(nsize2,nnod_x,nnod_y,nnod_z,3,2) )
!
       allocate( iedge_f_item_3d(nsize3,nnod_x,nnod_y,nnod_z,3,3) )
       allocate( iedge_f_dist_3d(nsize3,nnod_x,nnod_y,nnod_z,3,3) )
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
