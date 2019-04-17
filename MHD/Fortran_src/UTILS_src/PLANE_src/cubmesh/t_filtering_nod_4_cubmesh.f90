!
!      module t_filtering_nod_4_cubmesh
!
!     Written by H. Matsui
!
!       subroutine alloc_work_4_filter_nod(c_size, c_fil_nod)
!       subroutine reset_work_4_filter_nod(c_fil_nod)
!       subroutine alloc_filters_nod(nf_type, c_fil_nod)
!       subroutine dealloc_work_4_filter_nod(c_fil_nod)
!       subroutine dealloc_filters_nod(c_fil_nod)
!
      module t_filtering_nod_4_cubmesh
!
      use m_precision
!
      implicit none
!
!
      type filtering_nod_4_cubmesh
        integer(kind = kint) ::  ndp1 = 3
        integer(kind = kint) ::  ndp2 = 9
        integer(kind = kint) ::  ndp3 = 27
!
        integer(kind = kint) ::  nnod_x
        integer(kind = kint) ::  nnod_y
        integer(kind = kint) ::  nnod_z
!
        integer(kind = kint), allocatable :: nnod_neib_x(:,:,:)
        integer(kind = kint), allocatable :: inod_f_item_x(:,:,:,:)
        integer(kind = kint), allocatable :: inod_f_dist_x(:,:,:,:)
!
        integer(kind = kint), allocatable :: nnod_neib_y(:,:,:)
        integer(kind = kint), allocatable :: inod_f_item_y(:,:,:,:)
        integer(kind = kint), allocatable :: inod_f_dist_y(:,:,:,:)
!
        integer(kind = kint), allocatable :: nnod_neib_z(:,:,:)
        integer(kind = kint), allocatable :: inod_f_item_z(:,:,:,:)
        integer(kind = kint), allocatable :: inod_f_dist_z(:,:,:,:)
!
        integer(kind = kint), allocatable :: nnod_neib_xy(:,:,:)
        integer(kind = kint), allocatable :: inod_f_item_xy(:,:,:,:,:)
        integer(kind = kint), allocatable :: inod_f_dist_xy(:,:,:,:,:)
!
        integer(kind = kint), allocatable :: nnod_neib(:,:,:)
        integer(kind = kint), allocatable :: inod_f_item_3d(:,:,:,:,:)
        integer(kind = kint), allocatable :: inod_f_dist_3d(:,:,:,:,:)
!
!
        real(kind = kreal), allocatable :: filter_c_x(:,:,:,:,:)
        real(kind = kreal), allocatable :: filter_c_y(:,:,:,:,:)
        real(kind = kreal), allocatable :: filter_c_z(:,:,:,:,:)
!
        real(kind = kreal), allocatable :: filter_c_xy(:,:,:,:,:)
        real(kind = kreal), allocatable :: filter_c_3d(:,:,:,:,:)
      end type filtering_nod_4_cubmesh
!
!
      type filterings_4_cubmesh
        type(filtering_nod_4_cubmesh) :: c_fil_nod
        type(filtering_nod_4_cubmesh) :: c_fil_ele
        type(filtering_nod_4_cubmesh) :: c_fil_edge(3)
      end type filterings_4_cubmesh
!
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
      subroutine alloc_work_4_filter_nod(c_size, c_fil_nod)
!
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
!
      call set_work_size_4_filter_nod(c_size, c_fil_nod)
      call alloc_work_4_plane_filter(c_fil_nod)
      call reset_work_4_filter_nod(c_fil_nod)
!
       end subroutine alloc_work_4_filter_nod
!
!  ----------------------------------------------------------------------
!
      subroutine alloc_work_4_filter_ele(c_size, c_each, c_fil_ele)
!
      use t_size_of_cube
      use m_comm_data_cube_kemo
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_ele
!
!
      call set_work_size_4_filter_ele(c_size, c_each, c_fil_ele)
      call alloc_work_4_plane_filter(c_fil_ele)
      call reset_work_4_filter_nod(c_fil_ele)
!
       end subroutine alloc_work_4_filter_ele
!
!  ----------------------------------------------------------------------
!
      subroutine alloc_work_4_filter_edge(c_size, c_fil_edge)
!
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_edge(3)
!
      integer(kind = kint) :: nd
!
      do nd = 1, 3
        call set_work_size_4_filter_edge(c_size, c_fil_edge(nd))
        call alloc_work_4_plane_filter(c_fil_edge(nd))
        call reset_work_4_filter_nod(c_fil_edge(nd))
      end do
!
       end subroutine alloc_work_4_filter_edge
!
!  ----------------------------------------------------------------------
!
      subroutine dealloc_work_4_filter_edge(c_fil_edge)
!
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_edge(3)
!
      integer(kind = kint) :: nd
!
      do nd = 1, 3
        call dealloc_work_4_filter_nod(c_fil_edge(nd))
      end do
!
       end subroutine dealloc_work_4_filter_edge
!
!  ----------------------------------------------------------------------
!  ----------------------------------------------------------------------
!
      subroutine set_work_size_4_filter_nod(c_size, c_fil_nod)
!
      use t_size_of_cube
      use m_comm_data_cube_kemo
!
      type(size_of_cube), intent(in) :: c_size
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
!
      c_fil_nod%nnod_x = c_size%numnod_x
      c_fil_nod%nnod_y = c_size%numnod_y
      c_fil_nod%nnod_z = c_size%numnod_z
!
      c_fil_nod%ndp1 = c_size%ndep_1
      c_fil_nod%ndp2 = c_fil_nod%ndp1**2
      c_fil_nod%ndp3 = c_fil_nod%ndp2 * c_fil_nod%ndp1
!
       end subroutine set_work_size_4_filter_nod
!
!  ----------------------------------------------------------------------
!
      subroutine set_work_size_4_filter_ele(c_size, c_each, c_fil_ele)
!
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_ele
!
!
      c_fil_ele%nnod_x = c_each%nx - 1
      c_fil_ele%nnod_y = c_each%ny - 1
      c_fil_ele%nnod_z = c_each%nz - 1
!
      c_fil_ele%ndp1 = 2*c_size%ndepth + 1
      c_fil_ele%ndp2 = c_fil_ele%ndp1**2
      c_fil_ele%ndp3 = c_fil_ele%ndp2 * c_fil_ele%ndp1
!
      end subroutine set_work_size_4_filter_ele
!
!  ----------------------------------------------------------------------
!
      subroutine set_work_size_4_filter_edge(c_size, c_fil_edge)
!
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_edge
!
!
      c_fil_edge%nnod_x = c_size%numnod_x
      c_fil_edge%nnod_y = c_size%numnod_y
      c_fil_edge%nnod_z = c_size%numnod_z
!
      c_fil_edge%ndp1 = 2 * c_size%ndepth + 1
      c_fil_edge%ndp2 = c_fil_edge%ndp1**2
      c_fil_edge%ndp3 = c_fil_edge%ndp2 * c_fil_edge%ndp1
!
       end subroutine set_work_size_4_filter_edge
!
!  ----------------------------------------------------------------------
!
      subroutine alloc_work_4_plane_filter(c_fil_nod)
!
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
      integer(kind = kint) ::  ndp1, ndp2, ndp3
      integer(kind = kint) :: nnod_x, nnod_y, nnod_z
!
!
      nnod_x = c_fil_nod%nnod_x
      nnod_y = c_fil_nod%nnod_y
      nnod_z = c_fil_nod%nnod_z
      ndp1 = c_fil_nod%ndp1
      ndp2 = c_fil_nod%ndp2
      ndp3 = c_fil_nod%ndp3
!
      allocate( c_fil_nod%nnod_neib_x (nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%nnod_neib_y (nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%nnod_neib_z (nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%nnod_neib_xy(nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%nnod_neib   (nnod_x,nnod_y,nnod_z)    )
!
      allocate( c_fil_nod%inod_f_item_x(ndp1,nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%inod_f_dist_x(ndp1,nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%inod_f_item_y(ndp1,nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%inod_f_dist_y(ndp1,nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%inod_f_item_z(ndp1,nnod_x,nnod_y,nnod_z)    )
      allocate( c_fil_nod%inod_f_dist_z(ndp1,nnod_x,nnod_y,nnod_z)    )
!
      allocate( c_fil_nod%inod_f_item_xy(ndp2,nnod_x,nnod_y,nnod_z,2) )
      allocate( c_fil_nod%inod_f_dist_xy(ndp2,nnod_x,nnod_y,nnod_z,2) )
!
      allocate( c_fil_nod%inod_f_item_3d(ndp3,nnod_x,nnod_y,nnod_z,3) )
      allocate( c_fil_nod%inod_f_dist_3d(ndp3,nnod_x,nnod_y,nnod_z,3) )
!
       end subroutine alloc_work_4_plane_filter
!
!  ----------------------------------------------------------------------
!
       subroutine reset_work_4_filter_nod(c_fil_nod)
!
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
       c_fil_nod%nnod_neib =    0
       c_fil_nod%nnod_neib_x =  0
       c_fil_nod%nnod_neib_y =  0
       c_fil_nod%nnod_neib_z =  0
       c_fil_nod%nnod_neib_xy = 0
!
       c_fil_nod%inod_f_item_x =  0
       c_fil_nod%inod_f_dist_x =  0
       c_fil_nod%inod_f_item_y =  0
       c_fil_nod%inod_f_dist_y =  0
       c_fil_nod%inod_f_item_z =  0
       c_fil_nod%inod_f_dist_z =  0
       c_fil_nod%inod_f_item_xy = 0
       c_fil_nod%inod_f_dist_xy = 0
       c_fil_nod%inod_f_item_3d = 0
       c_fil_nod%inod_f_dist_3d = 0
!
       end subroutine reset_work_4_filter_nod
!
!  ----------------------------------------------------------------------
!
      subroutine alloc_filters_nod(nf_type, c_fil_nod)
!
      use m_comm_data_cube_kemo
!
      integer(kind = kint), intent(in) :: nf_type
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
      integer(kind = kint) :: nnod_x, nnod_y, nnod_z
      integer(kind = kint) :: ndp1, ndp2, ndp3
!
      nnod_x = c_fil_nod%nnod_x
      nnod_y = c_fil_nod%nnod_y
      nnod_z = c_fil_nod%nnod_z
      ndp1 = c_fil_nod%ndp1
      ndp2 = c_fil_nod%ndp2
      ndp3 = c_fil_nod%ndp3
!
      allocate(c_fil_nod%filter_c_x(ndp1,nnod_x,nnod_y,nnod_z,nf_type))
      allocate(c_fil_nod%filter_c_y(ndp1,nnod_x,nnod_y,nnod_z,nf_type))
      allocate(c_fil_nod%filter_c_z(ndp1,nnod_x,nnod_y,nnod_z,nf_type))
      allocate(c_fil_nod%filter_c_xy(ndp2,nnod_x,nnod_y,nnod_z,nf_type))
      allocate(c_fil_nod%filter_c_3d(ndp3,nnod_x,nnod_y,nnod_z,nf_type))
!
      c_fil_nod%filter_c_x = 0.0d0
      c_fil_nod%filter_c_y = 0.0d0
      c_fil_nod%filter_c_z = 0.0d0
      c_fil_nod%filter_c_xy = 0.0d0
      c_fil_nod%filter_c_3d = 0.0d0
!
      end subroutine alloc_filters_nod
!
!  ----------------------------------------------------------------------
!
       subroutine dealloc_work_4_filter_nod(c_fil_nod)
!
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
       deallocate( c_fil_nod%nnod_neib_x     )
       deallocate( c_fil_nod%nnod_neib_y     )
       deallocate( c_fil_nod%nnod_neib_z     )
       deallocate( c_fil_nod%nnod_neib_xy    )
       deallocate( c_fil_nod%nnod_neib       )
!
       deallocate( c_fil_nod%inod_f_item_x  )
       deallocate( c_fil_nod%inod_f_dist_x  )
       deallocate( c_fil_nod%inod_f_item_y  )
       deallocate( c_fil_nod%inod_f_dist_y  )
       deallocate( c_fil_nod%inod_f_item_z  )
       deallocate( c_fil_nod%inod_f_dist_z  )
!
       deallocate( c_fil_nod%inod_f_item_xy )
       deallocate( c_fil_nod%inod_f_dist_xy )
!
       deallocate( c_fil_nod%inod_f_item_3d )
       deallocate( c_fil_nod%inod_f_dist_3d )
!
       end subroutine dealloc_work_4_filter_nod
!
!  ----------------------------------------------------------------------
!
       subroutine dealloc_filters_nod(c_fil_nod)
!
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
       deallocate (c_fil_nod%filter_c_x)
       deallocate (c_fil_nod%filter_c_y)
       deallocate (c_fil_nod%filter_c_z)
       deallocate (c_fil_nod%filter_c_xy)
       deallocate (c_fil_nod%filter_c_3d)
!
       end subroutine dealloc_filters_nod
!
!  ----------------------------------------------------------------------
!
      end module t_filtering_nod_4_cubmesh
