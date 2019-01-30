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
      integer(kind = kint), allocatable :: ie_sf20(:,:)
      integer(kind = kint), allocatable :: iedge_sf20(:,:)
      integer(kind = kint), allocatable :: ie_sf_mid(:)
!
!      subroutine allocate_surface_geometries
!      subroutine allocate_surface_connect
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
      allocate (xyz_surf(c_sphere1%numnod_sf20,3))
      allocate (r_surf(c_sphere1%numnod_sf20))
      allocate (theta_surf(c_sphere1%numnod_sf20))
      allocate (phi_surf(c_sphere1%numnod_sf20))
!
      allocate (s_surf(c_sphere1%numnod_sf20))
      allocate (ar_surf(c_sphere1%numnod_sf20))
      allocate (as_surf(c_sphere1%numnod_sf20))
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
      subroutine allocate_surface_connect
!
!
      allocate( ie_sf20(c_sphere1%ntot_ele_sf20,8) )
      allocate( iedge_sf20(c_sphere1%ntot_edge_sf20,3) )
      allocate( ie_sf_mid(c_sphere1%numele_sf_w_coarse) )
!
      ie_sf20 = 0
      iedge_sf20 = 0
      ie_sf_mid = 0
!
      end subroutine allocate_surface_connect
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
      call dealloc_coarsing_stack(c_sphere1)
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
      call dealloc_coarse_surf_connect(c_sphere1)
!
      end subroutine deallocate_coarse_surf_connect
!
!   --------------------------------------------------------------------
!
      end module m_cubed_sph_surf_mesh
