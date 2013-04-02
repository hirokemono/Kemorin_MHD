!set_surf_grp_vectors_type.f90
!     module set_surf_grp_vectors_type
!
!     Writteg by H.Matsui on Aug., 2006
!
!      subroutine pick_normal_of_surf_group
!      subroutine pick_surface_group_geometry
!
!      subroutine pick_normal_of_surf_grp_type(surf, sf_grp,            &
!     &          sf_grp_data, sf_grp_v)
!      subroutine pick_surf_grp_geometry_type(surf, sf_grp, sf_grp_data,&
!     &          sf_grp_v)
!
      module set_surf_grp_vectors_type
!
      use m_precision
!
      use m_machine_parameter
      use t_group_data
      use t_group_connects
!
      implicit  none
!
      private :: pick_vector_4_surf_grp_type
      private :: pick_scalar_4_surf_grp_type
      private :: pick_vect_surf_grp_w_side_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine pick_normal_of_surf_grp_type(surf, sf_grp,             &
     &          sf_grp_data, sf_grp_v)
!
      use t_surface_data
      use t_surface_group_geometry
!
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_table), intent(in) :: sf_grp_data
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      if (sf_grp%num_grp .ne. 0 ) then
        call pick_vect_surf_grp_w_side_type(sf_grp, sf_grp_data,        &
     &    surf%numsurf, surf%vnorm_surf, sf_grp_v%vnorm_sf_grp)
        call pick_scalar_4_surf_grp_type(sf_grp, sf_grp_data,           &
     &    surf%numsurf, surf%area_surf, sf_grp_v%area_sf_grp)
        call pick_scalar_4_surf_grp_type(sf_grp, sf_grp_data,           &
     &    surf%numsurf, surf%a_area_surf, sf_grp_v%a_area_sf_grp)
      end if
!
      end subroutine pick_normal_of_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine pick_surf_grp_geometry_type(surf, sf_grp, sf_grp_data, &
     &          sf_grp_v)
!
      use t_surface_data
      use t_surface_group_geometry
!
      use coordinate_converter
!
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_table), intent(in) :: sf_grp_data
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
!    set center of surface
!
      call alloc_surf_grp_type_geom(sf_grp, sf_grp_v)
!
      call pick_vector_4_surf_grp_type(sf_grp, sf_grp_data,             &
    &     surf%numsurf, surf%x_surf, sf_grp_v%x_sf_grp)
!
      call position_2_sph(sf_grp%num_item, sf_grp_v%x_sf_grp,           &
     &    sf_grp_v%r_sf_grp, sf_grp_v%theta_sf_grp,                     &
     &    sf_grp_v%phi_sf_grp, sf_grp_v%ar_sf_grp,                      &
     &    sf_grp_v%s_sf_grp, sf_grp_v%as_sf_grp)
!
       end subroutine pick_surf_grp_geometry_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_vector_4_surf_grp_type(sf_grp, sf_grp_data,       &
     &          n_surf, x_surf, x_sf_grp)
!
      use set_surf_grp_vectors
!
      type(surface_group_data), intent(in) ::  sf_grp
      type(surface_group_table), intent(in) :: sf_grp_data
!
      integer (kind = kint), intent(in) :: n_surf
      real(kind=kreal), intent(in) :: x_surf(n_surf,3)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(sf_grp%num_item,3)
!
!
      call pick_vector_by_surf_grp(sf_grp%num_grp, sf_grp%num_item,     &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_data%isurf_grp, n_surf, x_surf, x_sf_grp)
!
      end subroutine pick_vector_4_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine pick_vect_surf_grp_w_side_type(sf_grp, sf_grp_data,    &
     &          n_surf, x_surf, x_sf_grp)
!
      use set_surf_grp_vectors
!
      type(surface_group_data), intent(in) ::  sf_grp
      type(surface_group_table), intent(in) :: sf_grp_data
!
      integer (kind = kint), intent(in) :: n_surf
      real(kind=kreal), intent(in) :: x_surf(n_surf,3)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(sf_grp%num_item,3)
!
!
      call pick_vect_by_surf_grp_w_side(sf_grp%num_grp,                 &
     &    sf_grp%num_item, sf_grp%num_grp_smp, sf_grp%istack_grp_smp,   &
     &    sf_grp_data%isurf_grp, n_surf, x_surf, x_sf_grp)
!
      end subroutine pick_vect_surf_grp_w_side_type
!
!-----------------------------------------------------------------------
!
      subroutine pick_scalar_4_surf_grp_type(sf_grp, sf_grp_data,       &
     &          n_surf, x_surf, x_sf_grp)
!
      use set_surf_grp_vectors
!
      type(surface_group_data), intent(in) ::  sf_grp
      type(surface_group_table), intent(in) :: sf_grp_data
!
      integer (kind = kint), intent(in) :: n_surf
      real(kind=kreal), intent(in) :: x_surf(n_surf)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(sf_grp%num_item)
!
!
      call pick_scalar_by_surf_grp(sf_grp%num_grp, sf_grp%num_item,     &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_data%isurf_grp, n_surf, x_surf, x_sf_grp)
!
      end subroutine pick_scalar_4_surf_grp_type
!
!-----------------------------------------------------------------------
!
      end module set_surf_grp_vectors_type
