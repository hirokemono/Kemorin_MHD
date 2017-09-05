!m_shape_functions.f90
!   module   m_shape_functions
!
!      subroutine allocate_integrate_parameters
!
!      subroutine allocate_gauss_point_id
!      subroutine allocate_gauss_point_id_to_4
!      subroutine allocate_gen_position_to_4
!      subroutine allocate_shape_functions
!
!      subroutine deallocate_gauss_point_id
!      subroutine deallocate_gen_position
!      subroutine deallocate_shape_functions
!
!>  arrays for shape functions in element coordinate
      module   m_shape_functions
!
      use m_precision
      use t_shape_functions
!
      implicit  none
!
!
      integer (kind=kint), allocatable :: l_int(:,:,:)
      integer (kind=kint), allocatable :: l_int2d(:,:,:)
      integer (kind=kint), allocatable :: l_int1d(:,:,:)
! 
!
      real (kind=kreal), allocatable :: xi1(:)
      real (kind=kreal), allocatable :: xi2(:)
      real (kind=kreal), allocatable :: ei2(:)
      real (kind=kreal), allocatable :: xi3(:)
      real (kind=kreal), allocatable :: ei3(:)
      real (kind=kreal), allocatable :: zi3(:)
!
      real (kind=kreal), allocatable :: dnxi_1(:,:)
      real (kind=kreal), allocatable :: dnei_1(:,:)
      real (kind=kreal), allocatable :: dnzi_1(:,:)
! 
      real (kind=kreal), allocatable :: dnxi_20(:,:)
      real (kind=kreal), allocatable :: dnei_20(:,:)
      real (kind=kreal), allocatable :: dnzi_20(:,:)
! 
      real (kind=kreal), allocatable :: dnxi_27(:,:)
      real (kind=kreal), allocatable :: dnei_27(:,:)
      real (kind=kreal), allocatable :: dnzi_27(:,:)
! 
!
      real (kind=kreal), allocatable :: dnxi_sf1(:,:)
      real (kind=kreal), allocatable :: dnei_sf1(:,:)
! 
      real (kind=kreal), allocatable :: dnxi_sf20(:,:)
      real (kind=kreal), allocatable :: dnei_sf20(:,:)
!
      real (kind=kreal), allocatable :: dnxi_sf27(:,:)
      real (kind=kreal), allocatable :: dnei_sf27(:,:)
!
!
      real (kind=kreal), allocatable :: dnxi_ed1(:,:)
! 
      real (kind=kreal), allocatable :: dnxi_ed20(:,:)
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_integrate_parameters
!
!
      call set_num_of_int_points
!
      call allocate_gauss_point_id
      call allocate_gen_position
!
      call allocate_shape_functions
!
      end subroutine allocate_integrate_parameters
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_gauss_point_id
!
      use m_fem_gauss_int_coefs
!
!
      allocate ( l_int(3,maxtot_int_3d,max_int_point) )
      allocate ( l_int2d(2,maxtot_int_2d,max_int_point) )
      allocate ( l_int1d(1,maxtot_int_1d,max_int_point) )
      l_int =   0
      l_int2d = 0
      l_int1d = 0
!
      end subroutine allocate_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine allocate_gauss_point_id_to_4
!
!
      allocate ( l_int(3,64,4) )
      allocate ( l_int2d(2,16,4) )
      allocate ( l_int1d(1,4,4) )
      l_int =   0
      l_int2d = 0
      l_int1d = 0
!
      end subroutine allocate_gauss_point_id_to_4
!
! ----------------------------------------------------------------------
!
      subroutine allocate_gen_position
!
      use m_fem_gauss_int_coefs
!
!
      allocate ( xi1(maxtot_int_1d) )
      allocate ( xi2(maxtot_int_2d) )
      allocate ( ei2(maxtot_int_2d) )
      allocate ( xi3(maxtot_int_3d) )
      allocate ( ei3(maxtot_int_3d) )
      allocate ( zi3(maxtot_int_3d) )
      xi1 = 0.0d0
      xi2 = 0.0d0
      ei2 = 0.0d0
      xi3 = 0.0d0
      ei3 = 0.0d0
      zi3 = 0.0d0
!
      end subroutine allocate_gen_position
!
! ----------------------------------------------------------------------
!
      subroutine allocate_gen_position_to_4
!
      allocate ( xi1(10) )
      allocate ( xi2(30) )
      allocate ( ei2(30) )
      allocate ( xi3(100) )
      allocate ( ei3(100) )
      allocate ( zi3(100) )
      xi1 = 0.0d0
      xi2 = 0.0d0
      ei2 = 0.0d0
      xi3 = 0.0d0
      ei3 = 0.0d0
      zi3 = 0.0d0
!
      end subroutine allocate_gen_position_to_4
!
! ----------------------------------------------------------------------
!
      subroutine allocate_shape_functions
!
      use m_fem_gauss_int_coefs
      use m_geometry_constants
!
!
        allocate ( dnxi_1(num_t_linear,maxtot_int_3d) )
        allocate ( dnei_1(num_t_linear,maxtot_int_3d) )
        allocate ( dnzi_1(num_t_linear,maxtot_int_3d) )
! 
        allocate ( dnxi_20(num_t_quad,maxtot_int_3d) )
        allocate ( dnei_20(num_t_quad,maxtot_int_3d) )
        allocate ( dnzi_20(num_t_quad,maxtot_int_3d) )
! 
        allocate ( dnxi_27(num_t_lag,maxtot_int_3d) )
        allocate ( dnei_27(num_t_lag,maxtot_int_3d) )
        allocate ( dnzi_27(num_t_lag,maxtot_int_3d) )
!
! 
        allocate ( dnxi_sf1(num_linear_sf,maxtot_int_2d) )
        allocate ( dnei_sf1(num_linear_sf,maxtot_int_2d) )
!
        allocate ( dnxi_sf20(num_quad_sf,maxtot_int_2d) )
        allocate ( dnei_sf20(num_quad_sf,maxtot_int_2d) )
!
        allocate ( dnxi_sf27(num_lag_sf,maxtot_int_2d) )
        allocate ( dnei_sf27(num_lag_sf,maxtot_int_2d) )
!
!
        allocate ( dnxi_ed1(num_linear_edge,maxtot_int_1d) )
!
        allocate ( dnxi_ed20(num_quad_edge,maxtot_int_1d) )
!
!
       dnxi_1 = 0.0d0
       dnei_1 = 0.0d0
       dnzi_1 = 0.0d0
!
       dnxi_20 = 0.0d0
       dnei_20 = 0.0d0
       dnzi_20 = 0.0d0
!
       dnxi_27 = 0.0d0
       dnei_27 = 0.0d0
       dnzi_27 = 0.0d0
!
       dnxi_sf1 = 0.0d0
       dnei_sf1 = 0.0d0
!
       dnxi_sf20 = 0.0d0
       dnei_sf20 = 0.0d0
!
       dnxi_sf27 = 0.0d0
       dnei_sf27 = 0.0d0
!
!
       end subroutine allocate_shape_functions
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_gauss_point_id
!
!
      deallocate (l_int, l_int2d, l_int1d)
!
      end subroutine deallocate_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_gen_position
!
!
      deallocate ( xi1 )
      deallocate ( xi2, ei2 )
      deallocate ( xi3, ei3, zi3 )
!
      end subroutine deallocate_gen_position
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_shape_functions
!
!
      deallocate ( dnxi_1, dnei_1, dnzi_1 )
      deallocate ( dnxi_20, dnei_20, dnzi_20 )
      deallocate ( dnxi_27, dnei_27, dnzi_27 )
!
      deallocate ( dnxi_sf1, dnei_sf1 )
      deallocate ( dnxi_sf20, dnei_sf20 )
      deallocate ( dnxi_sf27, dnei_sf27 )
!
      deallocate ( dnxi_ed1 )
      deallocate ( dnxi_ed20 )
!
      end subroutine deallocate_shape_functions
!
! ----------------------------------------------------------------------
!
      end module   m_shape_functions
