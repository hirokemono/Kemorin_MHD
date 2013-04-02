!
!      module m_surface_group_geometry
!
!> @brief Geometry data for surface group
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_surf_grp_geomtetry
!      subroutine deallocate_surf_grp_geomtetry
!
!      subroutine allocate_normal_4_sf_grp_sph
!      subroutine allocate_normal_4_sf_grp_cyl
!       subroutine deallocate_vector_4_surface
!      subroutine deallocate_normal_4_sf_grp_sph
!      subroutine deallocate_normal_4_sf_grp_cyl
!
      module m_surface_group_geometry
!
      use m_precision
!
      use m_surface_group
!
      implicit none
!
!
      real(kind=kreal),   allocatable :: x_sf_grp(:,:)
!<   position of surface group items
!
      real(kind=kreal),   allocatable :: r_sf_grp(:)
!<   radius of surface group items
      real(kind=kreal),   allocatable :: theta_sf_grp(:)
!<   colatitude of surface group items
      real(kind=kreal),   allocatable :: phi_sf_grp(:)
!<   longitude of surface group items
      real(kind=kreal),   allocatable :: s_sf_grp(:)
!<   cylindrical radius of surface group items
      real(kind=kreal),   allocatable :: ar_sf_grp(:)
!<   1 / r_sf_grp
      real(kind=kreal),   allocatable :: as_sf_grp(:)
!<   1 / s_sf_grp
!
!
      real(kind=kreal),   allocatable :: vnorm_sf_grp(:,:)
!<   normal vector of surface group items
      real(kind=kreal),   allocatable :: area_sf_grp(:)
!<   area of surface group items
      real(kind=kreal),   allocatable :: a_area_sf_grp(:)
!<   1 / area_sf_grp
      real(kind=kreal),   allocatable :: tot_area_sf_grp(:)
!<   total area of each surface group
!
      real(kind=kreal),   allocatable :: vnorm_sf_grp_sph(:,:)
!<   normal vector of surface group items (spherical coordinate)
      real(kind=kreal),   allocatable :: vnorm_sf_grp_cyl(:,:)
!<   normal vector of surface group items (cylindrical coordinate)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_surf_grp_geomtetry
!
      allocate(x_sf_grp(num_surf_bc,3))
      allocate(r_sf_grp(num_surf_bc))
      allocate(theta_sf_grp(num_surf_bc))
      allocate(phi_sf_grp(num_surf_bc))
      allocate(s_sf_grp(num_surf_bc))
      allocate(ar_sf_grp(num_surf_bc))
      allocate(as_sf_grp(num_surf_bc))
!
      x_sf_grp =     0.0d0
!
      r_sf_grp =     0.0d0
      theta_sf_grp = 0.0d0
      phi_sf_grp =   0.0d0
      s_sf_grp =     0.0d0
      ar_sf_grp =    0.0d0
      as_sf_grp =    0.0d0
!
      end subroutine allocate_surf_grp_geomtetry
!
! -----------------------------------------------------------------------
!
       subroutine allocate_vector_4_surface
!
!
       allocate ( vnorm_sf_grp(num_surf_bc,3) )
!
       allocate ( tot_area_sf_grp(num_surf) )
       allocate ( area_sf_grp(num_surf_bc) )
       allocate ( a_area_sf_grp(num_surf_bc) )
!
       vnorm_sf_grp =    0.0d0
       tot_area_sf_grp = 0.0d0
       area_sf_grp =     0.0d0
       a_area_sf_grp =   0.0d0
!
      end subroutine allocate_vector_4_surface
!
!-----------------------------------------------------------------------
!
      subroutine allocate_normal_4_sf_grp_sph
!
      allocate(vnorm_sf_grp_sph(num_surf_bc,3))
      vnorm_sf_grp_sph = 0.0d0
!
      end subroutine allocate_normal_4_sf_grp_sph
!
!-----------------------------------------------------------------------
!
      subroutine allocate_normal_4_sf_grp_cyl
!
      allocate(vnorm_sf_grp_cyl(num_surf_bc,3))
      vnorm_sf_grp_cyl = 0.0d0
!
      end subroutine allocate_normal_4_sf_grp_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_geomtetry
!
      deallocate(x_sf_grp)
      deallocate(r_sf_grp)
      deallocate(theta_sf_grp)
      deallocate(phi_sf_grp)
      deallocate(s_sf_grp)
      deallocate(ar_sf_grp)
      deallocate(as_sf_grp)
!
      end subroutine deallocate_surf_grp_geomtetry
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_vector_4_surface
!
!
       deallocate( vnorm_sf_grp )
       deallocate( tot_area_sf_grp, area_sf_grp, a_area_sf_grp)
!
      end subroutine deallocate_vector_4_surface
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_normal_4_sf_grp_sph
!
      deallocate(vnorm_sf_grp_sph)
!
      end subroutine deallocate_normal_4_sf_grp_sph
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_normal_4_sf_grp_cyl
!
      deallocate(vnorm_sf_grp_cyl)
!
      end subroutine deallocate_normal_4_sf_grp_cyl
!
! -----------------------------------------------------------------------!
      end module m_surface_group_geometry
