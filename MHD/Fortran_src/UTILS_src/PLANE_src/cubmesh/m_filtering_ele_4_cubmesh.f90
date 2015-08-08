!
!      module m_filtering_ele_4_cubmesh
!
!     Written by H. Matsui
!
      module m_filtering_ele_4_cubmesh
!
      use m_precision
!
      implicit none
!
!
      real (kind = kreal), allocatable :: iele_filter_order0(:)
      real (kind = kreal), allocatable :: iele_filter_order1(:,:)
      real (kind = kreal), allocatable :: iele_filter_order2(:,:)
!
      integer (kind = kint), allocatable :: nele_neib_x(:,:,:)
      integer (kind = kint), allocatable :: iele_f_item_x(:,:,:,:)
      integer (kind = kint), allocatable :: iele_f_dist_x(:,:,:,:)
!
      integer (kind = kint), allocatable :: nele_neib_y(:,:,:)
      integer (kind = kint), allocatable :: iele_f_item_y(:,:,:,:)
      integer (kind = kint), allocatable :: iele_f_dist_y(:,:,:,:)
!
      integer (kind = kint), allocatable :: nele_neib_z(:,:,:)
      integer (kind = kint), allocatable :: iele_f_item_z(:,:,:,:)
      integer (kind = kint), allocatable :: iele_f_dist_z(:,:,:,:)
!
      integer (kind = kint), allocatable :: nele_neib_xy(:,:,:)
      integer (kind = kint), allocatable :: iele_f_item_xy(:,:,:,:,:)
      integer (kind = kint), allocatable :: iele_f_dist_xy(:,:,:,:,:)
!
      integer (kind = kint), allocatable :: nele_neib(:,:,:)
      integer (kind = kint), allocatable :: iele_f_item_3d(:,:,:,:,:)
      integer (kind = kint), allocatable :: iele_f_dist_3d(:,:,:,:,:)
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
       subroutine allocate_work_4_filter_ele
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
       allocate( nele_neib_x (nx-1,ny-1,nz-1)    )
       allocate( nele_neib_y (nx-1,ny-1,nz-1)    )
       allocate( nele_neib_z (nx-1,ny-1,nz-1)    )
       allocate( nele_neib_xy(nx-1,ny-1,nz-1)    )
       allocate( nele_neib   (nx-1,ny-1,nz-1)    )
!
       allocate( iele_f_item_x(nsize,nx-1,ny-1,nz-1)    )
       allocate( iele_f_dist_x(nsize,nx-1,ny-1,nz-1)    )
       allocate( iele_f_item_y(nsize,nx-1,ny-1,nz-1)    )
       allocate( iele_f_dist_y(nsize,nx-1,ny-1,nz-1)    )
       allocate( iele_f_item_z(nsize,nx-1,ny-1,nz-1)    )
       allocate( iele_f_dist_z(nsize,nx-1,ny-1,nz-1)    )
!
       allocate( iele_f_item_xy(nsize2,nx-1,ny-1,nz-1,2) )
       allocate( iele_f_dist_xy(nsize2,nx-1,ny-1,nz-1,2) )
!
       allocate( iele_f_item_3d(nsize3,nx-1,ny-1,nz-1,3) )
       allocate( iele_f_dist_3d(nsize3,nx-1,ny-1,nz-1,3) )
!
       nele_neib =    0
       nele_neib_x =  0
       nele_neib_y =  0
       nele_neib_z =  0
       nele_neib_xy = 0
!
       iele_f_item_x =  0
       iele_f_dist_x =  0
       iele_f_item_y =  0
       iele_f_dist_y =  0
       iele_f_item_z =  0
       iele_f_dist_z =  0
       iele_f_item_xy = 0
       iele_f_dist_xy = 0
       iele_f_item_3d = 0
       iele_f_dist_3d = 0
!
       end subroutine allocate_work_4_filter_ele
!
!  ----------------------------------------------------------------------
!
       subroutine deallocate_work_4_filter_ele
!
       deallocate( nele_neib_x     )
       deallocate( nele_neib_y     )
       deallocate( nele_neib_z     )
       deallocate( nele_neib_xy    )
       deallocate( nele_neib       )
!
       deallocate( iele_f_item_x  )
       deallocate( iele_f_dist_x  )
       deallocate( iele_f_item_y  )
       deallocate( iele_f_dist_y  )
       deallocate( iele_f_item_z  )
       deallocate( iele_f_dist_z  )
!
       deallocate( iele_f_item_xy )
       deallocate( iele_f_dist_xy )
!
       deallocate( iele_f_item_3d )
       deallocate( iele_f_dist_3d )
!
       end subroutine deallocate_work_4_filter_ele
!
!  ----------------------------------------------------------------------
!
      end module m_filtering_ele_4_cubmesh
