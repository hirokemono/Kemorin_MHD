!t_shape_functions.f90
!   module   t_shape_functions
!
!      subroutine alloc_integrate_parameters
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
!
      type shape_finctions_at_points
        type(volume_shape_function)  :: spf_3d
        type(surface_shape_function)  :: spf_2d
        type(edge_shape_function)  :: spf_1d
      end type shape_finctions_at_points
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_gauss_point_id_to_4(spf_3d, spf_2d, spf_1d)
!
      type(volume_shape_function), intent(inout) :: spf_3d
      type(surface_shape_function), intent(inout) :: spf_2d
      type(edge_shape_function), intent(inout) :: spf_1d
!
      call alloc_3d_gauss_point_id(64, 4, spf_3d)
      call alloc_2d_gauss_point_id(16, 4, spf_2d)
      call alloc_1d_gauss_point_id(4,  4, spf_1d)
!
      end subroutine alloc_gauss_point_id_to_4
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_3d_gauss_point_id                                &
     &         (ntot_int_3d, max_int_point, spf_3d)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: ntot_int_3d
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      allocate ( spf_3d%l_int(3,ntot_int_3d,max_int_point) )
      allocate ( spf_3d%xi(ntot_int_3d) )
      allocate ( spf_3d%ei(ntot_int_3d) )
      allocate ( spf_3d%zi(ntot_int_3d) )
!
      spf_3d%l_int = 0
      spf_3d%xi = 0.0d0
      spf_3d%ei = 0.0d0
      spf_3d%zi = 0.0d0
!
      end subroutine alloc_3d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine alloc_2d_gauss_point_id                                &
     &         (ntot_int_2d, max_int_point, spf_2d)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: ntot_int_2d
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      allocate ( spf_2d%l_int(2,ntot_int_2d,max_int_point) )
      allocate ( spf_2d%xi(ntot_int_2d) )
      allocate ( spf_2d%ei(ntot_int_2d) )
      spf_2d%l_int = 0
      spf_2d%xi = 0.0d0
      spf_2d%ei = 0.0d0
!
      end subroutine alloc_2d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine alloc_1d_gauss_point_id                                &
     &         (ntot_int_1d, max_int_point, spf_1d)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: ntot_int_1d
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      allocate ( spf_1d%l_int(1,ntot_int_1d,max_int_point) )
      allocate ( spf_1d%xi(ntot_int_1d) )
      spf_1d%l_int = 0
      spf_1d%xi = 0.0d0
!
      end subroutine alloc_1d_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_gauss_point_id(spf_3d, spf_2d, spf_1d)
!
      type(volume_shape_function), intent(inout) :: spf_3d
      type(surface_shape_function), intent(inout) :: spf_2d
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      call dealloc_3d_gauss_point_id(spf_3d)
      call dealloc_2d_gauss_point_id(spf_2d)
      call dealloc_1d_gauss_point_id(spf_1d)
!
      end subroutine dealloc_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_3d_gauss_point_id(spf_3d)
!
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      deallocate(spf_3d%l_int)
      deallocate (spf_3d%xi, spf_3d%ei, spf_3d%zi)
!
      end subroutine dealloc_3d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_2d_gauss_point_id(spf_2d)
!
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      deallocate(spf_2d%l_int)
      deallocate (spf_2d%xi, spf_2d%ei)
!
      end subroutine dealloc_2d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_1d_gauss_point_id(spf_1d)
!
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      deallocate(spf_1d%l_int)
      deallocate (spf_1d%xi)
!
      end subroutine dealloc_1d_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_vol_shape_func(nnod_4_ele, ntot_int, spf_3d)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ntot_int
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      spf_3d%nnod_4_ele =  nnod_4_ele
      spf_3d%ntot_int =     ntot_int
      allocate(spf_3d%dnxi(spf_3d%nnod_4_ele,spf_3d%ntot_int) )
      allocate(spf_3d%dnei(spf_3d%nnod_4_ele,spf_3d%ntot_int) )
      allocate(spf_3d%dnzi(spf_3d%nnod_4_ele,spf_3d%ntot_int) )
!
       spf_3d%dnxi = 0.0d0
       spf_3d%dnei = 0.0d0
       spf_3d%dnzi = 0.0d0
!
      end subroutine alloc_vol_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine alloc_shape_func_infty                                 &
     &         (nnod_4_ele, nsurf_4_ele, ntot_int, spf_inf)
!
      integer(kind = kint), intent(in) :: nnod_4_ele, nsurf_4_ele
      integer(kind = kint), intent(in) :: ntot_int
      type(infty_shape_function), intent(inout) :: spf_inf
!
!
      spf_inf%nnod_4_ele =  nnod_4_ele
      spf_inf%nsurf_4_ele = nsurf_4_ele
      spf_inf%ntot_int =     ntot_int
      allocate(spf_inf%dnxi_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
      allocate(spf_inf%dnei_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
      allocate(spf_inf%dnzi_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
! 
      spf_inf%dnxi_inf = 0.0d0
      spf_inf%dnei_inf = 0.0d0
      spf_inf%dnzi_inf = 0.0d0
!
      end subroutine alloc_shape_func_infty
!
! ----------------------------------------------------------------------
!
      subroutine alloc_surf_shape_func(nnod_4_sf, ntot_int, spf_2d)
!
      integer(kind = kint), intent(in) :: nnod_4_sf
      integer(kind = kint), intent(in) :: ntot_int
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      spf_2d%nnod_4_surf =  nnod_4_sf
      spf_2d%ntot_int =     ntot_int
      allocate(spf_2d%dnxi_sf(spf_2d%nnod_4_surf,spf_2d%ntot_int) )
      allocate(spf_2d%dnei_sf(spf_2d%nnod_4_surf,spf_2d%ntot_int) )
!
      spf_2d%dnxi_sf = 0.0d0
      spf_2d%dnei_sf = 0.0d0
!
      end subroutine alloc_surf_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine alloc_edge_shape_func(nnod_4_ed, ntot_int, spf_1d)
!
      integer(kind = kint), intent(in) :: nnod_4_ed
      integer(kind = kint), intent(in) :: ntot_int
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      spf_1d%nnod_4_edge =  nnod_4_ed
      spf_1d%ntot_int =     ntot_int
      allocate(spf_1d%dnxi_ed(spf_1d%nnod_4_edge,spf_1d%ntot_int) )
!
      spf_1d%dnxi_ed = 0.0d0
!
      end subroutine alloc_edge_shape_func
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_vol_shape_func(spf_3d)
!
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      deallocate(spf_3d%dnxi)
      deallocate(spf_3d%dnei)
      deallocate(spf_3d%dnzi)
! 
      end subroutine dealloc_vol_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_shape_func_infty(spf_inf)
!
      type(infty_shape_function), intent(inout) :: spf_inf
!
!
      deallocate(spf_inf%dnxi_inf)
      deallocate(spf_inf%dnei_inf)
      deallocate(spf_inf%dnzi_inf)
! 
      end subroutine dealloc_shape_func_infty
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_surf_shape_func(spf_2d)
!
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      deallocate(spf_2d%dnxi_sf)
      deallocate(spf_2d%dnei_sf)
! 
      end subroutine dealloc_surf_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_edge_shape_func(spf_1d)
!
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      deallocate(spf_1d%dnxi_ed)
! 
      end subroutine dealloc_edge_shape_func
!
! ----------------------------------------------------------------------
!
      end module   t_shape_functions
