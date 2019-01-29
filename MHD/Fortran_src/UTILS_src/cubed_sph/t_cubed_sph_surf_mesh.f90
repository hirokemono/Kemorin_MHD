!t_cubed_sph_surf_mesh.f90
!      module t_cubed_sph_surf_mesh
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine alloc_surface_geometries(c_sphere)
!!      subroutine alloc_coarsing_stack(max_coarse_leve, c_spherel)
!!      subroutine alloc_surface_connect(c_sphere)
!!      subroutine alloc_coarse_surf_connect(c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!
!!      subroutine dealloc_surface_geometries(c_sphere)
!!      subroutine dealloc_coarsing_stack(c_sphere)
!!      subroutine dealloc_surface_connect(c_sphere)
!!      subroutine dealloc_coarse_surf_connect(c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module t_cubed_sph_surf_mesh
!
      use m_precision
!
      implicit none
!
      type cubed_sph_surf_mesh
!   num. of node, element
        integer(kind = kint) :: nele_shell
        integer(kind = kint) :: numnod_sf
        integer(kind = kint) :: numele_sf
        integer(kind = kint) :: numedge_sf
!
        integer(kind = kint) :: numnod_cube
        integer(kind = kint) :: numele_cube
        integer(kind = kint) :: numsurf_cube
        integer(kind = kint) :: numedge_cube
!
        integer(kind = kint) :: numnod_sf20
        integer(kind = kint) :: numele_sf20
        integer(kind = kint) :: numnod_cube20
        integer(kind = kint) :: numele_cube20
        integer(kind = kint) :: numedge_sf20
!
!   position
        real(kind = kreal), allocatable :: xyz_surf(:,:)
        real(kind = kreal), allocatable :: r_surf(:)
        real(kind = kreal), allocatable :: theta_surf(:)
        real(kind = kreal), allocatable :: phi_surf(:)
        real(kind = kreal), allocatable :: s_surf(:)
        real(kind = kreal), allocatable :: ar_surf(:)
        real(kind = kreal), allocatable :: as_surf(:)
!
!   connectivity
!
        integer(kind = kint) :: ntot_ele_sf20
        integer(kind = kint) :: ntot_edge_sf20
        integer(kind = kint), allocatable :: ie_sf20(:,:)
        integer(kind = kint), allocatable :: iedge_sf20(:,:)
        integer(kind = kint), allocatable :: ie_sf_mid(:)
!
        integer(kind = kint) :: numnod_cube_w_coarse
        integer(kind = kint) :: numele_cube_w_coarse
        integer(kind = kint) :: numedge_cube_w_coarse
        integer(kind = kint) :: nsurf_cube_w_coarse
        integer(kind = kint) :: numnod_sf_w_coarse
        integer(kind = kint) :: numele_sf_w_coarse
        integer(kind = kint) :: numedge_sf_w_coarse
        integer(kind = kint) :: nsurf_sf_w_coarse
!
        integer(kind = kint) :: max_level
        integer(kind = kint), allocatable :: inod_stack_cube(:)
        integer(kind = kint), allocatable :: iele_stack_cube(:)
        integer(kind = kint), allocatable :: iedge_stack_cube(:)
        integer(kind = kint), allocatable :: isurf_stack_cube(:)
        integer(kind = kint), allocatable :: inod_stack_sf(:)
        integer(kind = kint), allocatable :: iele_stack_sf(:)
        integer(kind = kint), allocatable :: iedge_stack_sf(:)
!
        integer(kind = kint) :: nmax_merge_sf
        integer(kind = kint), allocatable :: inod_2_org(:)
        integer(kind = kint), allocatable :: inod_2_next(:)
        integer(kind = kint), allocatable :: num_merge_e_sf(:)
        integer(kind = kint), allocatable :: imerge_e_sf(:,:)
!
        integer(kind = kint) :: max_merge_e
        integer(kind = kint), allocatable :: imerge_ele(:)
      end type cubed_sph_surf_mesh
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_surface_geometries(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      allocate (c_sphere%xyz_surf(c_sphere%numnod_sf20,3))
      allocate (c_sphere%r_surf(c_sphere%numnod_sf20))
      allocate (c_sphere%theta_surf(c_sphere%numnod_sf20))
      allocate (c_sphere%phi_surf(c_sphere%numnod_sf20))
!
      allocate (c_sphere%s_surf(c_sphere%numnod_sf20))
      allocate (c_sphere%ar_surf(c_sphere%numnod_sf20))
      allocate (c_sphere%as_surf(c_sphere%numnod_sf20))
!
      c_sphere%xyz_surf = 0.0d0
      c_sphere%r_surf = 0.0d0
      c_sphere%ar_surf = 0.0d0
      c_sphere%s_surf = 0.0d0
      c_sphere%as_surf = 0.0d0
      c_sphere%theta_surf = 0.0d0
      c_sphere%phi_surf = 0.0d0
      c_sphere%xyz_surf = 0.0d0
!
      end subroutine alloc_surface_geometries
!
!   --------------------------------------------------------------------
!
      subroutine alloc_coarsing_stack(max_coarse_level, c_sphere)
!
      integer(kind = kint), intent(in) :: max_coarse_level
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      c_sphere%max_level = max_coarse_level
      allocate(c_sphere%inod_stack_cube(0:c_sphere%max_level))
      allocate(c_sphere%iele_stack_cube(0:c_sphere%max_level))
      allocate(c_sphere%iedge_stack_cube(0:c_sphere%max_level))
      allocate(c_sphere%isurf_stack_cube(0:c_sphere%max_level))
      allocate(c_sphere%inod_stack_sf(0:c_sphere%max_level))
      allocate(c_sphere%iele_stack_sf(0:c_sphere%max_level))
      allocate(c_sphere%iedge_stack_sf(0:c_sphere%max_level))
!
      c_sphere%inod_stack_cube = 0
      c_sphere%iele_stack_cube = 0
      c_sphere%iedge_stack_cube = 0
      c_sphere%isurf_stack_cube = 0
      c_sphere%inod_stack_sf = 0
      c_sphere%iele_stack_sf = 0
      c_sphere%iedge_stack_sf = 0
!
      end subroutine alloc_coarsing_stack
!
!   --------------------------------------------------------------------
!
      subroutine alloc_surface_connect(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      c_sphere%ntot_ele_sf20                                            &
     &      =  c_sphere%numele_sf20 + c_sphere%numele_sf_w_coarse
      c_sphere%ntot_edge_sf20                                           &
     &      = c_sphere%numedge_sf20 + c_sphere%numedge_sf_w_coarse
      allocate( c_sphere%ie_sf20(c_sphere%ntot_ele_sf20,8) )
      allocate( c_sphere%iedge_sf20(c_sphere%ntot_edge_sf20,3) )
      allocate( c_sphere%ie_sf_mid(c_sphere%numele_sf_w_coarse) )
!
      c_sphere%ie_sf20 = 0
      c_sphere%iedge_sf20 = 0
      c_sphere%ie_sf_mid = 0
!
      end subroutine alloc_surface_connect
!
!   --------------------------------------------------------------------
!
      subroutine alloc_coarse_surf_connect(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: nnod, nmax
!
      allocate( c_sphere%inod_2_org(c_sphere%numnod_sf_w_coarse) )
      allocate( c_sphere%inod_2_next(c_sphere%numnod_sf_w_coarse) )
      allocate( c_sphere%num_merge_e_sf(c_sphere%numnod_sf_w_coarse) )
!
      nnod = c_sphere%numnod_sf_w_coarse
      nmax = c_sphere%nmax_merge_sf
      allocate( c_sphere%imerge_e_sf(nnod,nmax) )
!
      allocate( c_sphere%imerge_ele(c_sphere%max_merge_e) )
!
      c_sphere%inod_2_org = 0
      c_sphere%inod_2_next = 0
      c_sphere%num_merge_e_sf = 0
      c_sphere%imerge_e_sf = 0
!
      c_sphere%imerge_ele = 0
!
      end subroutine alloc_coarse_surf_connect
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_surface_geometries(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      deallocate( c_sphere%xyz_surf   )
      deallocate( c_sphere%r_surf     )
      deallocate( c_sphere%theta_surf )
      deallocate( c_sphere%phi_surf   )
!
      deallocate( c_sphere%s_surf  )
      deallocate( c_sphere%ar_surf )
      deallocate( c_sphere%as_surf )
!
      end subroutine dealloc_surface_geometries
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_coarsing_stack(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      deallocate( c_sphere%inod_stack_cube  )
      deallocate( c_sphere%iele_stack_cube  )
      deallocate( c_sphere%iedge_stack_cube )
      deallocate( c_sphere%isurf_stack_cube )
      deallocate( c_sphere%inod_stack_sf    )
      deallocate( c_sphere%iele_stack_sf    )
      deallocate( c_sphere%iedge_stack_sf   )
!
      end subroutine dealloc_coarsing_stack
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_surface_connect(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      deallocate( c_sphere%ie_sf20    )
      deallocate( c_sphere%iedge_sf20 )
      deallocate( c_sphere%ie_sf_mid  )
!
      end subroutine dealloc_surface_connect
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_coarse_surf_connect(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      deallocate( c_sphere%inod_2_org     )
      deallocate( c_sphere%inod_2_next    )
      deallocate( c_sphere%num_merge_e_sf )
      deallocate( c_sphere%imerge_e_sf    )
!
      deallocate( c_sphere%imerge_ele )
!
      end subroutine dealloc_coarse_surf_connect
!
!   --------------------------------------------------------------------
!
      end module t_cubed_sph_surf_mesh
