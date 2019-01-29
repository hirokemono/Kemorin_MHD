!m_cubed_sph_surf_mesh.f90
!      module m_cubed_sph_surf_mesh
!
!      Written by H. Matsui on Apr., 2006
!
      module m_cubed_sph_surf_mesh
!
      use m_precision
      use t_cubed_sph_surf_mesh
!
      implicit none
!
      type(cubed_sph_surf_mesh), save :: c_sphere1
!
!   num. of node, element
!      integer(kind = kint) :: numnod_sf, numele_sf
      integer(kind = kint) ::  numele_sf
      integer(kind = kint) :: numedge_sf
      integer(kind = kint) :: numnod_sf20, numele_sf20
      integer(kind = kint) :: numnod_cube, numele_cube
      integer(kind = kint) :: numsurf_cube, numedge_cube
      integer(kind = kint) :: numnod_cube20, numele_cube20
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
      integer(kind = kint) :: numnod_sf_w_coarse, numele_sf_w_coarse
      integer(kind = kint) :: numedge_sf_w_coarse, nsurf_sf_w_coarse
!
      integer(kind = kint), allocatable :: inod_stack_cube(:)
      integer(kind = kint), allocatable :: iele_stack_cube(:)
      integer(kind = kint), allocatable :: iedge_stack_cube(:)
      integer(kind = kint), allocatable :: isurf_stack_cube(:)
      integer(kind = kint), allocatable :: inod_stack_sf(:)
      integer(kind = kint), allocatable :: iele_stack_sf(:)
      integer(kind = kint), allocatable :: iedge_stack_sf(:)
!
      integer(kind = kint) :: nmax_merge_sf
      integer(kind = kint), allocatable :: inod_2_org(:), inod_2_next(:)
      integer(kind = kint), allocatable :: num_merge_e_sf(:)
      integer(kind = kint), allocatable :: imerge_e_sf(:,:)
!
      integer(kind = kint) :: max_merge_e
      integer(kind = kint), allocatable :: imerge_ele(:)
!
!      subroutine allocate_surface_geometries
!      subroutine allocate_coarsing_stack(max_coarse_level)
!      subroutine allocate_surface_connect
!      subroutine allocate_coarse_surf_connect
!
!      subroutine deallocate_surface_geometries
!      subroutine deallocate_coarsing_stack
!      subroutine deallocate_surface_connect
!      subroutine deallocate_coarse_surf_connect
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_surface_geometries
!
      allocate (xyz_surf(numnod_sf20,3))
      allocate (r_surf(numnod_sf20))
      allocate (theta_surf(numnod_sf20))
      allocate (phi_surf(numnod_sf20))
!
      allocate (s_surf(numnod_sf20))
      allocate (ar_surf(numnod_sf20))
      allocate (as_surf(numnod_sf20))
!
      xyz_surf = 0.0d0
      r_surf = 0.0d0
      ar_surf = 0.0d0
      s_surf = 0.0d0
      as_surf = 0.0d0
      theta_surf = 0.0d0
      phi_surf = 0.0d0
      xyz_surf = 0.0d0
!
      end subroutine allocate_surface_geometries
!
!   --------------------------------------------------------------------
!
      subroutine allocate_coarsing_stack(max_coarse_level)
!
      integer(kind = kint), intent(in) :: max_coarse_level
!
      allocate( inod_stack_cube(0:max_coarse_level) )
      allocate( iele_stack_cube(0:max_coarse_level) )
      allocate( iedge_stack_cube(0:max_coarse_level) )
      allocate( isurf_stack_cube(0:max_coarse_level) )
      allocate( inod_stack_sf(0:max_coarse_level) )
      allocate( iele_stack_sf(0:max_coarse_level) )
      allocate( iedge_stack_sf(0:max_coarse_level) )
!
      inod_stack_cube = 0
      iele_stack_cube = 0
      iedge_stack_cube = 0
      isurf_stack_cube = 0
      inod_stack_sf = 0
      iele_stack_sf = 0
      iedge_stack_sf = 0
!
      end subroutine allocate_coarsing_stack
!
!   --------------------------------------------------------------------
!
      subroutine allocate_surface_connect
!
      ntot_ele_sf20 =  numele_sf20 + numele_sf_w_coarse
      ntot_edge_sf20 = numedge_sf20 + numedge_sf_w_coarse
      allocate( ie_sf20(ntot_ele_sf20,8) )
      allocate( iedge_sf20(ntot_edge_sf20,3) )
      allocate( ie_sf_mid(numele_sf_w_coarse) )
!
      ie_sf20 = 0
      iedge_sf20 = 0
      ie_sf_mid = 0
!
      end subroutine allocate_surface_connect
!
!   --------------------------------------------------------------------
!
      subroutine allocate_coarse_surf_connect
!
      allocate( inod_2_org(numnod_sf_w_coarse) )
      allocate( inod_2_next(numnod_sf_w_coarse) )
      allocate( num_merge_e_sf(numnod_sf_w_coarse) )
      allocate( imerge_e_sf(numnod_sf_w_coarse,nmax_merge_sf) )
!
      allocate( imerge_ele(max_merge_e) )
!
      inod_2_org = 0
      inod_2_next = 0
      num_merge_e_sf = 0
      imerge_e_sf = 0
!
      imerge_ele = 0
!
      end subroutine allocate_coarse_surf_connect
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_surface_geometries
!
      deallocate( xyz_surf   )
      deallocate( r_surf     )
      deallocate( theta_surf )
      deallocate( phi_surf   )
!
      deallocate( s_surf  )
      deallocate( ar_surf )
      deallocate( as_surf )
!
      end subroutine deallocate_surface_geometries
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_coarsing_stack
!
      deallocate( inod_stack_cube  )
      deallocate( iele_stack_cube  )
      deallocate( iedge_stack_cube )
      deallocate( isurf_stack_cube )
      deallocate( inod_stack_sf    )
      deallocate( iele_stack_sf    )
      deallocate( iedge_stack_sf   )
!
      end subroutine deallocate_coarsing_stack
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_surface_connect
!
      deallocate( ie_sf20    )
      deallocate( iedge_sf20 )
      deallocate( ie_sf_mid  )
!
      end subroutine deallocate_surface_connect
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_coarse_surf_connect
!
      deallocate( inod_2_org     )
      deallocate( inod_2_next    )
      deallocate( num_merge_e_sf )
      deallocate( imerge_e_sf    )
!
      deallocate( imerge_ele )
!
      end subroutine deallocate_coarse_surf_connect
!
!   --------------------------------------------------------------------
!
      end module m_cubed_sph_surf_mesh
