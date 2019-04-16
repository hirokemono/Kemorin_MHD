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
        integer(kind = kint) :: inod_f_total
!
        integer(kind = kint) ::  ndp1 = 3
        integer(kind = kint) ::  ndp2 = 9
        integer(kind = kint) ::  ndp3 = 27
!
        integer(kind = kint) ::  nnod_x
        integer(kind = kint) ::  nnod_y
        integer(kind = kint) ::  nnod_z
!
        integer(kind = kint), allocatable :: inod_f(:)
        integer(kind = kint), allocatable :: inod_f_stack(:)
        integer(kind = kint), allocatable :: inod_f_item(:)
        integer(kind = kint), allocatable :: inod_f_dist(:,:)
        real (kind = kreal), allocatable :: filter_c(:,:)
!
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
        integer(kind = kint) :: iflag_z_filter
        real(kind = kreal)    :: eps_filter
!
        real(kind = kreal), allocatable :: f_mom_1d(:,:)
        real(kind = kreal), allocatable :: df_mom_1d(:,:)
!
        real(kind = kreal), allocatable :: filter_c_x(:,:,:,:,:)
        real(kind = kreal), allocatable :: filter_c_y(:,:,:,:,:)
        real(kind = kreal), allocatable :: filter_c_z(:,:,:,:,:)
!
        real(kind = kreal), allocatable :: filter_c_xy(:,:,:,:,:)
        real(kind = kreal), allocatable :: filter_c_3d(:,:,:,:,:)
      end type filtering_nod_4_cubmesh
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
      use m_comm_data_cube_kemo
!
      type(size_of_cube), intent(in) :: c_size
      type(filtering_nod_4_cubmesh), intent(inout) :: c_fil_nod
!
      integer(kind = kint) ::  ndp1, ndp2, ndp3
      integer(kind = kint) :: nnod_x, nnod_y, nnod_z
!
!
      c_fil_nod%nnod_x = c_size%numnod_x
      c_fil_nod%nnod_y = c_size%numnod_y
      c_fil_nod%nnod_z = c_size%numnod_z
      nnod_x = c_fil_nod%nnod_x
      nnod_y = c_fil_nod%nnod_y
      nnod_z = c_fil_nod%nnod_z
!
      c_fil_nod%ndp1 = c_size%ndep_1
      c_fil_nod%ndp2 = c_fil_nod%ndp1**2
      c_fil_nod%ndp3 = c_fil_nod%ndp2 * c_fil_nod%ndp1
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
      call s_reset_work_4_filter_nod(c_fil_nod)
!
       end subroutine alloc_work_4_filter_nod
!
!  ----------------------------------------------------------------------
!
       subroutine s_reset_work_4_filter_nod(c_fil_nod)
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
       end subroutine s_reset_work_4_filter_nod
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
