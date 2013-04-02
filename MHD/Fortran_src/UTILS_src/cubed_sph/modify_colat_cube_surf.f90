!modify_colat_cube_surf.f90
!     module modify_colat_cube_surf
!
!        programmed by H.Matsui on Dec., 2011
!
!      subroutine allocate_wall_latitude_ratio(num_v)
!      subroutine deallocate_wall_latitude_ratio
!
!      subroutine cal_wall_latitude_ratio(num_h, num_v, edge_lat)
!      subroutine modify_colat_on_cube_sf(nnod, num_h, num_v,           &
!     &          theta_org, theta_new)
!      subroutine modify_colat_on_cube_edge(nedge, num_h, num_v,        &
!     &          theta_org, theta_new)
!
      module modify_colat_cube_surf
!
      use m_precision
!
      implicit  none
!
      real(kind = kreal), allocatable :: lat_ratio(:)
!
      real(kind = kreal), allocatable :: theta_ref(:)
      real(kind = kreal), allocatable :: theta_mod(:)
      real(kind = kreal), allocatable :: dtheta_ref(:)
      real(kind = kreal), allocatable :: dtheta_mod(:)
      real(kind = kreal), allocatable :: lat_ref(:)
      real(kind = kreal), allocatable :: lat_mod(:)
      real(kind = kreal), allocatable :: colat_ratio(:)
!
      private :: colat_ratio
      private :: theta_ref, theta_mod
      private :: lat_ref, lat_mod, lat_ratio
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_wall_latitude_ratio(num_v)
!
      integer(kind = kint), intent(in) :: num_v
!
!
      allocate( theta_ref(num_v+1), theta_mod(num_v+1) )
      allocate( dtheta_ref(num_v+1), dtheta_mod(num_v+1) )
      allocate( colat_ratio(num_v+1) )
      theta_ref = 0.0d0
      theta_mod = 0.0d0
      colat_ratio = 0.0d0
      dtheta_ref =  0.0d0
      dtheta_mod =  0.0d0
!
      end subroutine allocate_wall_latitude_ratio
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_wall_latitude_ratio
!
      deallocate( theta_ref, theta_mod   )
      deallocate( dtheta_ref, dtheta_mod )
!      deallocate( lat_ref, lat_mod, lat_ratio )
      deallocate( colat_ratio )
!
      end subroutine deallocate_wall_latitude_ratio
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine cal_wall_latitude_ratio(num_h, num_v, edge_lat)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: num_h, num_v
      real(kind = kreal), intent(in) :: edge_lat
!
      integer(kind = kint) :: iz, iz_r
      real(kind = kreal) :: dt_h, d2t_v, rz, ratio_tmp
!
!
      dt_h =  two * (90.0d0 - edge_lat) / dble(num_h)
      d2t_v = eight * (edge_lat - half*dt_h*dble(num_v))                &
     &       / dble(num_v*(num_v + itwo))
!
      do iz = 1, num_v + 1
        theta_ref(iz) = 135.0d0 - 90.0 * dble(iz-1) / dble(num_v)
        theta_mod(iz) = (90.0d0 - edge_lat) + dt_h * dble(num_v-iz+1)   &
     &                 + half * d2t_v * dble((num_v-iz+1)*(num_v-iz+2))
        colat_ratio(iz) = theta_mod(iz) / theta_ref(iz)
     end do
!
      do iz = 1, num_v + 1
        if(theta_ref(iz) .le. 90.0d0) exit
        theta_mod(iz) = (90.0d0 + edge_lat) - dt_h * dble(iz-1)         &
     &                 - half * d2t_v * dble(iz*(iz-1))
        colat_ratio(iz) = (180.0d0 - theta_mod(iz))                     &
     &                   / (180.0d0 - theta_ref(iz))
      end do
!
      do iz = 1, num_v
        dtheta_ref(iz) = theta_ref(iz+1) - theta_ref(iz)
        dtheta_mod(iz) = theta_mod(iz+1) - theta_mod(iz)
      end do
      dtheta_ref(num_v+1) = -90.0 / dble(num_h)
      dtheta_mod(num_v+1) = -dt_h
!
!      write(*,*) 'colat_ratio', dt_h, d2t_v
!      do iz = 1, num_v + 1
!        write(*,'(i8,5f12.5)') iz, colat_ratio(iz), theta_mod(iz),     &
!     &           theta_ref(iz), dtheta_ref(iz), dtheta_mod(iz)
!      end do
!
      end subroutine cal_wall_latitude_ratio
!
!   --------------------------------------------------------------------
!
      subroutine modify_colat_on_cube_sf(nnod, num_h, num_v,            &
     &          theta_org, theta_new)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: nnod, num_h, num_v
      real(kind = kreal), intent(in) :: theta_org(nnod)
!
      real(kind = kreal), intent(inout) :: theta_new(nnod)
!
      integer(kind = kint) :: inod, i, iz, ixy
      real(kind = kreal) :: pi, lat_org
!
!
      pi = four * atan(one)
!
      inod = 0
      do i = 1, (num_h+1)**2
        inod = inod + 1
        lat_org = pi - theta_org(inod)
        theta_new(inod) = pi - lat_org * colat_ratio(num_v+1)
      end do
      do iz = 1, num_v-1
        do ixy = 1, 4*num_h
          inod = inod + 1
          if( theta_org(inod) .gt. half*pi) then
            lat_org = pi - theta_org(inod)
            theta_new(inod) = pi - lat_org * colat_ratio(num_v-iz+1)
          else
            theta_new(inod) =theta_org(inod) * colat_ratio(iz+1)
          end if
        end do
      end do
      do i = 1, (num_h+1)**2
        inod = inod + 1
        theta_new(inod) =theta_org(inod) * colat_ratio(num_v+1)
      end do
!
      end subroutine modify_colat_on_cube_sf
!
!   --------------------------------------------------------------------
!
      subroutine modify_colat_on_cube_edge(nnod, nedge, num_h, num_v,   &
     &          theta_org, theta_new)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: nnod, nedge, num_h, num_v
      real(kind = kreal), intent(in) :: theta_org(nedge)
!
      real(kind = kreal), intent(inout) :: theta_new(nedge)
!
      integer(kind = kint) :: inod, i, iz, ixy
      real(kind = kreal) :: pi, lat_org, ratio_mid
!
!
      pi = four * atan(one)
!
      inod = 0
      do i = 1, (2*num_h+2)*num_h
        inod = inod + 1
        lat_org = pi - theta_org(inod)
        theta_new(inod) = pi - lat_org * colat_ratio(num_v+1)
      end do
!
      do iz = 1, num_v
        do ixy = 1, 4*num_h
          inod = inod + 1
          if( theta_org(inod) .gt. half*pi) then
            lat_org = pi - theta_org(inod)
            ratio_mid = half*(colat_ratio(num_v-iz+1)                   &
     &                 + colat_ratio(num_v-iz+2))
            theta_new(inod) = pi - lat_org * ratio_mid
          else
            ratio_mid = half*(colat_ratio(iz+1) + colat_ratio(iz))
            theta_new(inod) = theta_org(inod) * ratio_mid
          end if
        end do
      end do
!
      do iz = 1, num_v-1
        do ixy = 1, 4*num_h
          inod = inod + 1
          if( theta_org(inod) .gt. half*pi) then
            lat_org = pi - theta_org(inod)
            theta_new(inod) = pi - lat_org * colat_ratio(num_v-iz+1)
          else
            theta_new(inod) = theta_org(inod) * colat_ratio(iz+1)
          end if
        end do
      end do
!
      do i = 1, (2*num_h+2)*num_h
        inod = inod + 1
        theta_new(inod) =theta_org(inod) * colat_ratio(num_v+1)
      end do
!
      end subroutine modify_colat_on_cube_edge
!
!   --------------------------------------------------------------------
!
      end module modify_colat_cube_surf
