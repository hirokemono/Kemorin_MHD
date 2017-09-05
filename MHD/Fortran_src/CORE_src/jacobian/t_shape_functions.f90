!t_shape_functions.f90
!   module   t_shape_functions
!
!      subroutine alloc_integrate_parameters
!      subroutine set_num_of_int_points
!      subroutine alloc_gauss_point_id_to_4
!      subroutine dealloc_gauss_point_id
!
!>  arrays for shape functions in element coordinate
      module t_shape_functions
!
      use m_precision
!
      implicit  none
!
!
      type volume_shape_function
        integer(kind = kint) :: nnod_4_ele
        integer(kind = kint) :: ntot_int
        integer(kind = kint), allocatable :: l_int(:,:,:)
!
        real(kind=kreal), allocatable :: xi(:)
        real(kind=kreal), allocatable :: ei(:)
        real(kind=kreal), allocatable :: zi(:)
!
        real(kind=kreal), allocatable :: dnxi(:,:)
        real(kind=kreal), allocatable :: dnei(:,:)
        real(kind=kreal), allocatable :: dnzi(:,:)
      end type volume_shape_function
!
      type infty_shape_function
        integer(kind = kint) :: nnod_4_ele
        integer(kind = kint) :: nsurf_4_ele
        integer(kind = kint) :: ntot_int
        real(kind=kreal), allocatable :: dnxi_inf(:,:,:)
        real(kind=kreal), allocatable :: dnei_inf(:,:,:)
        real(kind=kreal), allocatable :: dnzi_inf(:,:,:)
      end type infty_shape_function
!
      type surface_shape_function
        integer(kind = kint) :: nnod_4_surf
        integer(kind = kint) :: ntot_int
        integer (kind=kint), allocatable :: l_int(:,:,:)
!
        real(kind=kreal), allocatable :: xi(:)
        real(kind=kreal), allocatable :: ei(:)
!
        real(kind=kreal), allocatable :: dnxi_sf(:,:)
        real(kind=kreal), allocatable :: dnei_sf(:,:)
      end type surface_shape_function
!
      type edge_shape_function
        integer(kind = kint) :: nnod_4_edge
        integer(kind = kint) :: ntot_int
        integer (kind=kint), allocatable :: l_int(:,:,:)
        real (kind=kreal), allocatable :: xi(:)
!
        real (kind=kreal), allocatable :: dnxi_ed(:,:)
      end type edge_shape_function
!
      private :: alloc_3d_gauss_point_id, dealloc_3d_gauss_point_id
      private :: alloc_2d_gauss_point_id, dealloc_2d_gauss_point_id
      private :: alloc_1d_gauss_point_id, dealloc_1d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_integrate_parameters(sp_3d, sp_2d, sp_1d)
!
      use m_fem_gauss_int_coefs
!
      type(volume_shape_function), intent(inout) :: sp_3d
      type(surface_shape_function), intent(inout) :: sp_2d
      type(edge_shape_function), intent(inout) :: sp_1d
!
!
      call set_num_of_int_points
!
      call alloc_3d_gauss_point_id(maxtot_int_3d, max_int_point, sp_3d)
      call alloc_2d_gauss_point_id(maxtot_int_2d, max_int_point, sp_2d)
      call alloc_1d_gauss_point_id(maxtot_int_1d, max_int_point, sp_1d)
!
      end subroutine alloc_integrate_parameters
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_num_of_int_points
!
      use m_fem_gauss_int_coefs
!
      integer(kind = kint) :: n
!
!
      maxtot_int_3d = 0
      maxtot_int_2d = 0
      maxtot_int_1d = 0
      do n = 1, max_int_point
        maxtot_int_3d = maxtot_int_3d + n*n*n
        maxtot_int_2d = maxtot_int_2d + n*n
        maxtot_int_1d = maxtot_int_1d + n
      end do
!
      end subroutine set_num_of_int_points
!
! ----------------------------------------------------------------------
!
      subroutine alloc_gauss_point_id_to_4(sp_3d, sp_2d, sp_1d)
!
      type(volume_shape_function), intent(inout) :: sp_3d
      type(surface_shape_function), intent(inout) :: sp_2d
      type(edge_shape_function), intent(inout) :: sp_1d
!
      call alloc_3d_gauss_point_id(64, 4, sp_3d)
      call alloc_2d_gauss_point_id(16, 4, sp_2d)
      call alloc_1d_gauss_point_id(4,  4, sp_1d)
!
      end subroutine alloc_gauss_point_id_to_4
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_3d_gauss_point_id                                &
     &         (ntot_int_3d, max_int_point, sp_3d)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: ntot_int_3d
      type(volume_shape_function), intent(inout) :: sp_3d
!
!
      allocate ( sp_3d%l_int(3,ntot_int_3d,max_int_point) )
      allocate ( sp_3d%xi(ntot_int_3d) )
      allocate ( sp_3d%ei(ntot_int_3d) )
      allocate ( sp_3d%zi(ntot_int_3d) )
!
      sp_3d%l_int = 0
      sp_3d%xi = 0.0d0
      sp_3d%ei = 0.0d0
      sp_3d%zi = 0.0d0
!
      end subroutine alloc_3d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine alloc_2d_gauss_point_id                                &
     &         (ntot_int_2d, max_int_point, sp_2d)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: ntot_int_2d
      type(surface_shape_function), intent(inout) :: sp_2d
!
!
      allocate ( sp_2d%l_int(2,ntot_int_2d,max_int_point) )
      allocate ( sp_2d%xi(ntot_int_2d) )
      allocate ( sp_2d%ei(ntot_int_2d) )
      sp_2d%l_int = 0
      sp_2d%xi = 0.0d0
      sp_2d%ei = 0.0d0
!
      end subroutine alloc_2d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine alloc_1d_gauss_point_id                                &
     &         (ntot_int_1d, max_int_point, sp_1d)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: ntot_int_1d
      type(edge_shape_function), intent(inout) :: sp_1d
!
!
      allocate ( sp_1d%l_int(1,ntot_int_1d,max_int_point) )
      allocate ( sp_1d%xi(ntot_int_1d) )
      sp_1d%l_int = 0
      sp_1d%xi = 0.0d0
!
      end subroutine alloc_1d_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_gauss_point_id(sp_3d, sp_2d, sp_1d)
!
      type(volume_shape_function), intent(inout) :: sp_3d
      type(surface_shape_function), intent(inout) :: sp_2d
      type(edge_shape_function), intent(inout) :: sp_1d
!
!
      call dealloc_3d_gauss_point_id(sp_3d)
      call dealloc_2d_gauss_point_id(sp_2d)
      call dealloc_1d_gauss_point_id(sp_1d)
!
      end subroutine dealloc_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_3d_gauss_point_id(sp_3d)
!
      type(volume_shape_function), intent(inout) :: sp_3d
!
!
      deallocate(sp_3d%l_int)
      deallocate (sp_3d%xi, sp_3d%ei, sp_3d%zi)
!
      end subroutine dealloc_3d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_2d_gauss_point_id(sp_2d)
!
      type(surface_shape_function), intent(inout) :: sp_2d
!
!
      deallocate(sp_2d%l_int)
      deallocate (sp_2d%xi, sp_2d%ei)
!
      end subroutine dealloc_2d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_1d_gauss_point_id(sp_1d)
!
      type(edge_shape_function), intent(inout) :: sp_1d
!
!
      deallocate(sp_1d%l_int)
      deallocate (sp_1d%xi)
!
      end subroutine dealloc_1d_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_vol_shape_func(nnod_4_ele, ntot_int, sp_3d)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ntot_int
      type(volume_shape_function), intent(inout) :: sp_3d
!
!
      sp_3d%nnod_4_ele =  nnod_4_ele
      sp_3d%ntot_int =     ntot_int
      allocate(sp_3d%dnxi(sp_3d%nnod_4_ele,sp_3d%ntot_int) )
      allocate(sp_3d%dnei(sp_3d%nnod_4_ele,sp_3d%ntot_int) )
      allocate(sp_3d%dnzi(sp_3d%nnod_4_ele,sp_3d%ntot_int) )
!
       sp_3d%dnxi = 0.0d0
       sp_3d%dnei = 0.0d0
       sp_3d%dnzi = 0.0d0
!
      end subroutine alloc_vol_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine alloc_shape_func_infty                                 &
     &         (nnod_4_ele, nsurf_4_ele, ntot_int, sp_infty)
!
      integer(kind = kint), intent(in) :: nnod_4_ele, nsurf_4_ele
      integer(kind = kint), intent(in) :: ntot_int
      type(infty_shape_function), intent(inout) :: sp_infty
!
!
      sp_infty%nnod_4_ele =  nnod_4_ele
      sp_infty%nsurf_4_ele = nsurf_4_ele
      sp_infty%ntot_int =     ntot_int
      allocate(sp_infty%dnxi_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
      allocate(sp_infty%dnei_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
      allocate(sp_infty%dnzi_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
! 
      sp_infty%dnxi_inf = 0.0d0
      sp_infty%dnei_inf = 0.0d0
      sp_infty%dnzi_inf = 0.0d0
!
      end subroutine alloc_shape_func_infty
!
! ----------------------------------------------------------------------
!
      subroutine alloc_surf_shape_func(nnod_4_sf, ntot_int, sp_2d)
!
      integer(kind = kint), intent(in) :: nnod_4_sf
      integer(kind = kint), intent(in) :: ntot_int
      type(surface_shape_function), intent(inout) :: sp_2d
!
!
      sp_2d%nnod_4_surf =  nnod_4_sf
      sp_2d%ntot_int =     ntot_int
      allocate(sp_2d%dnxi_sf(sp_2d%nnod_4_surf,sp_2d%ntot_int) )
      allocate(sp_2d%dnei_sf(sp_2d%nnod_4_surf,sp_2d%ntot_int) )
!
      sp_2d%dnxi_sf = 0.0d0
      sp_2d%dnei_sf = 0.0d0
!
      end subroutine alloc_surf_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine alloc_edge_shape_func(nnod_4_ed, ntot_int, sp_1d)
!
      integer(kind = kint), intent(in) :: nnod_4_ed
      integer(kind = kint), intent(in) :: ntot_int
      type(edge_shape_function), intent(inout) :: sp_1d
!
!
      sp_1d%nnod_4_edge =  nnod_4_ed
      sp_1d%ntot_int =     ntot_int
      allocate(sp_1d%dnxi_ed(sp_1d%nnod_4_edge,sp_1d%ntot_int) )
!
      sp_1d%dnxi_ed = 0.0d0
!
      end subroutine alloc_edge_shape_func
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_vol_shape_func(sp_3d)
!
      type(volume_shape_function), intent(inout) :: sp_3d
!
!
      deallocate(sp_3d%dnxi)
      deallocate(sp_3d%dnei)
      deallocate(sp_3d%dnzi)
! 
      end subroutine dealloc_vol_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_shape_func_infty(sp_infty)
!
      type(infty_shape_function), intent(inout) :: sp_infty
!
!
      deallocate(sp_infty%dnxi_inf)
      deallocate(sp_infty%dnei_inf)
      deallocate(sp_infty%dnzi_inf)
! 
      end subroutine dealloc_shape_func_infty
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_surf_shape_func(sp_2d)
!
      type(surface_shape_function), intent(inout) :: sp_2d
!
!
      deallocate(sp_2d%dnxi_sf)
      deallocate(sp_2d%dnei_sf)
! 
      end subroutine dealloc_surf_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_edge_shape_func(sp_1d)
!
      type(edge_shape_function), intent(inout) :: sp_1d
!
!
      deallocate(sp_1d%dnxi_ed)
! 
      end subroutine dealloc_edge_shape_func
!
! ----------------------------------------------------------------------
!
      end module   t_shape_functions
