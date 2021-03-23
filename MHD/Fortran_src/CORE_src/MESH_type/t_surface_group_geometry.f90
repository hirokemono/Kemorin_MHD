!>@file  t_surface_group_geometry.f90
!!       module t_surface_group_geometry
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2008
!
!>   @brief Structure of geometry data for surface group
!!
!!@verbatim
!!      subroutine pick_surface_group_geometry                          &
!!     &         (node, ele, surf, edge, sf_grp, sf_grp_xyz)
!!      subroutine dealloc_surf_grp_geometory(sf_grp_xyz)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data),    intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_group_geometry), intent(inout) :: sf_grp_xyz
!!
!!      subroutine check_center_of_surface_grp                          &
!!     &         (id_check, sf_grp, sf_grp_xyz)
!!      subroutine check_center_of_surface_grp_sph                      &
!!     &          (id_check, sf_grp, sf_grp_xyz)
!!
!!@endverbatim
!
      module t_surface_group_geometry
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
!
!>   Structure of geometry data for surface group
      type surface_group_geometry
!>   position of surface group items
        real(kind=kreal),   allocatable :: x_sf_grp(:,:)
!
!>   radius of surface group items
        real(kind=kreal),   allocatable :: r_sf_grp(:)
!>   colatitude of surface group items
        real(kind=kreal),   allocatable :: theta_sf_grp(:)
!>   longitude of surface group items
        real(kind=kreal),   allocatable :: phi_sf_grp(:)
!>   cylindrical radius of surface group items
        real(kind=kreal),   allocatable :: s_sf_grp(:)
!>   1 / r_sf_grp
        real(kind=kreal),   allocatable :: ar_sf_grp(:)
!>   1 / s_sf_grp
        real(kind=kreal),   allocatable :: as_sf_grp(:)
      end type surface_group_geometry
!
     private :: alloc_surf_grp_geometory
     private :: pick_vector_by_surf_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine pick_surface_group_geometry                            &
     &         (node, ele, surf, edge, sf_grp, sf_grp_xyz)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_group_connects
      use t_surface_group_table
!
      use coordinate_converter
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data),    intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(inout) :: sf_grp_xyz
!
      type(surface_group_table) :: sf_grp_tbl
!
!
      call const_surface_group_table(ele, surf, edge,                   &
     &                               sf_grp, sf_grp_tbl)
!
!    set center of surface
      call alloc_surf_grp_geometory(sf_grp%num_item, sf_grp_xyz)
!
      call pick_vector_by_surf_grp(sf_grp%num_grp, sf_grp%num_item,     &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_tbl%isurf_grp, surf%numsurf, surf%x_surf,              &
     &    sf_grp_xyz%x_sf_grp)
      call dealloc_surf_item_sf_grp(sf_grp_tbl)
!
      call position_2_sph                                               &
     &   (sf_grp%num_item, sf_grp_xyz%x_sf_grp, sf_grp_xyz%r_sf_grp,    &
     &    sf_grp_xyz%theta_sf_grp, sf_grp_xyz%phi_sf_grp,               &
     &    sf_grp_xyz%ar_sf_grp, sf_grp_xyz%s_sf_grp,                    &
     &    sf_grp_xyz%as_sf_grp)
!
       end subroutine pick_surface_group_geometry
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_grp_geometory(sf_grp_xyz)
!
      type(surface_group_geometry), intent(inout) :: sf_grp_xyz
!
!
      deallocate(sf_grp_xyz%x_sf_grp)
      deallocate(sf_grp_xyz%r_sf_grp    , sf_grp_xyz%ar_sf_grp )
      deallocate(sf_grp_xyz%theta_sf_grp, sf_grp_xyz%phi_sf_grp)
      deallocate(sf_grp_xyz%s_sf_grp    , sf_grp_xyz%as_sf_grp )
!
      end subroutine dealloc_surf_grp_geometory
!
! -----------------------------------------------------------------------
!
      subroutine alloc_surf_grp_geometory(num_item, sf_grp_xyz)
!
      integer(kind = kint), intent(in) :: num_item
      type(surface_group_geometry), intent(inout) :: sf_grp_xyz
!
!
      allocate(sf_grp_xyz%x_sf_grp    (num_item,3) )
      allocate(sf_grp_xyz%r_sf_grp    (num_item)   )
      allocate(sf_grp_xyz%theta_sf_grp(num_item)   )
      allocate(sf_grp_xyz%phi_sf_grp  (num_item)   )
      allocate(sf_grp_xyz%s_sf_grp    (num_item)   )
      allocate(sf_grp_xyz%ar_sf_grp   (num_item)   )
      allocate(sf_grp_xyz%as_sf_grp   (num_item)   )
!
      if( num_item .gt. 0) then
        sf_grp_xyz%x_sf_grp =     0.0d0
!
        sf_grp_xyz%r_sf_grp =     0.0d0
        sf_grp_xyz%theta_sf_grp = 0.0d0
        sf_grp_xyz%phi_sf_grp =   0.0d0
        sf_grp_xyz%s_sf_grp =     0.0d0
        sf_grp_xyz%ar_sf_grp =    0.0d0
        sf_grp_xyz%as_sf_grp =    0.0d0
      end if
!
      end subroutine alloc_surf_grp_geometory
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_center_of_surface_grp                            &
     &         (id_check, sf_grp, sf_grp_xyz)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_check
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_xyz
!
      integer(kind = kint) :: i_grp, ist, ied, inum
!
      write(id_check,*) ' inum, center of surface'
      do i_grp = 1, sf_grp%num_grp
        ist = sf_grp%istack_grp(i_grp-1) + 1
        ied = sf_grp%istack_grp(i_grp)
        do inum = ist, ied
          write(id_check,'(i16,1p3e23.12)') inum,                       &
     &              sf_grp_xyz%x_sf_grp(inum,1:3)
        end do
      end do
!
      end subroutine check_center_of_surface_grp
!
!-----------------------------------------------------------------------
!
      subroutine check_center_of_surface_grp_sph                        &
     &          (id_check, sf_grp, sf_grp_xyz)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: id_check
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_xyz
!
      integer(kind = kint) :: i_grp, ist, ied, inum
!
      write(id_check,*) ' inum, center of surface'
      write(id_check,*) '          (r, theta, phi, cyl_r)'
      do i_grp = 1, sf_grp%num_grp
        ist = sf_grp%istack_grp(i_grp-1) + 1
        ied = sf_grp%istack_grp(i_grp)
        do inum = ist, ied
          write(id_check,'(i16,1p4e23.12)') inum,                       &
     &        sf_grp_xyz%r_sf_grp(inum), sf_grp_xyz%theta_sf_grp(inum), &
     &        sf_grp_xyz%phi_sf_grp(inum), sf_grp_xyz%s_sf_grp(inum)
        end do
      end do
!
      end subroutine check_center_of_surface_grp_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_vector_by_surf_grp(num_surf, num_surf_bc,         &
     &          num_surf_smp, isurf_grp_smp_stack, isurf_grp,           &
     &          numsurf, x_surf, x_sf_grp)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
      integer (kind = kint), intent(in) :: isurf_grp(num_surf_bc)
!
      integer(kind = kint), intent(in) :: numsurf
      real(kind=kreal), intent(in) :: x_surf(numsurf,3)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(num_surf_bc,3)
!
      integer (kind = kint) :: i_grp, ip, i, ist, ied, inum, isurf
!
!
      do i_grp = 1, num_surf
!
!$omp parallel do private(i,ist,ied,inum,isurf)
        do ip = 1, np_smp
          i = (i_grp-1)*np_smp + ip
          ist = isurf_grp_smp_stack(i-1) + 1
          ied = isurf_grp_smp_stack(i)
!
!poption parallel
          do inum = ist, ied
            isurf = abs( isurf_grp(inum) )
            x_sf_grp(inum,1) = x_surf(isurf,1)
            x_sf_grp(inum,2) = x_surf(isurf,2)
            x_sf_grp(inum,3) = x_surf(isurf,3)
          end do
        end do
!poption parallel
      end do
!
      end subroutine pick_vector_by_surf_grp
!
!-----------------------------------------------------------------------
!
      end module t_surface_group_geometry
