!
!     module set_surf_grp_vectors
!
!     Writteg by H.Matsui on Aug., 2006
!
!      subroutine pick_normal_of_surf_group(sf_grp, sf_grp_v)
!      subroutine pick_surface_group_geometry(sf_grp, sf_grp_v)
!
!      subroutine pick_vect_by_surf_grp_w_side(num_surf, num_surf_bc,   &
!     &          num_surf_smp, isurf_grp_smp_stack, isurf_grp,          &
!     &          numsurf, x_surf, x_sf_grp)
!      subroutine pick_scalar_by_surf_grp(num_surf, num_surf_bc,        &
!     &          num_surf_smp, isurf_grp_smp_stack, isurf_grp,          &
!     &          numsurf, x_surf, x_sf_grp)
!
      module set_surf_grp_vectors
!
      use m_precision
!
      use m_machine_parameter
      use t_group_data
!
      implicit  none
!
      private :: pick_scalar_4_surf_grp, pick_vector_4_surf_grp
      private :: pick_vector_4_surf_grp_side
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine pick_normal_of_surf_group(sf_grp, sf_grp_v)
!
      use m_geometry_data
      use m_surface_geometry_data
      use m_surface_group_connect
      use t_surface_group_geometry
!
      use sum_normal_4_surf_group
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      call alloc_vectors_surf_grp_type                                  &
     &   (sf_grp%num_grp, sf_grp%num_item, sf_grp_v)
!
      call pick_vector_4_surf_grp_side(sf_grp, surf1%numsurf,           &
     &    vnorm_surf, sf_grp_v%vnorm_sf_grp)
      call pick_scalar_4_surf_grp(sf_grp, surf1%numsurf,                &
     &    area_surf, sf_grp_v%area_sf_grp)
      call pick_scalar_4_surf_grp(sf_grp, surf1%numsurf,                &
     &    a_area_surf, sf_grp_v%a_area_sf_grp)
!
      end subroutine pick_normal_of_surf_group
!
!-----------------------------------------------------------------------
!
      subroutine pick_surface_group_geometry(sf_grp, sf_grp_v)
!
      use m_geometry_constants
      use m_geometry_data
      use m_surface_geometry_data
      use m_surface_group_connect
      use t_surface_group_geometry
!
      use coordinate_converter
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
!    set center of surface
!
      call alloc_surf_grp_type_geom(sf_grp%num_item, sf_grp_v)
!
      call pick_vector_4_surf_grp                                       &
     &   (sf_grp, surf1%numsurf, surf1%x_surf, sf_grp_v%x_sf_grp)
!
      call position_2_sph                                               &
     &   (sf_grp%num_item, sf_grp_v%x_sf_grp, sf_grp_v%r_sf_grp,        &
     &    sf_grp_v%theta_sf_grp, sf_grp_v%phi_sf_grp,                   &
     &    sf_grp_v%ar_sf_grp, sf_grp_v%s_sf_grp, sf_grp_v%as_sf_grp)
!
       end subroutine pick_surface_group_geometry
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_vector_4_surf_grp                                 &
     &         (sf_grp, numsurf, x_surf, x_sf_grp)
!
      use m_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: numsurf
      real(kind=kreal), intent(in) :: x_surf(numsurf,3)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(sf_grp%num_item,3)
!
!
      call pick_vector_by_surf_grp(sf_grp%num_grp, sf_grp%num_item,     &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_data1%isurf_grp, numsurf, x_surf, x_sf_grp)
!
      end subroutine pick_vector_4_surf_grp
!
!-----------------------------------------------------------------------
!
      subroutine pick_vector_4_surf_grp_side                            &
     &         (sf_grp, numsurf, x_surf, x_sf_grp)
!
      use m_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: numsurf
      real(kind=kreal), intent(in) :: x_surf(numsurf,3)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(sf_grp%num_item,3)
!
!
      call pick_vect_by_surf_grp_w_side                                 &
     &   (sf_grp%num_grp, sf_grp%num_item, sf_grp%num_grp_smp,          &
     &    sf_grp%istack_grp_smp, sf_grp_data1%isurf_grp,                &
     &    numsurf, x_surf, x_sf_grp)
!
      end subroutine pick_vector_4_surf_grp_side
!
!-----------------------------------------------------------------------
!
      subroutine pick_scalar_4_surf_grp                                 &
     &         (sf_grp, numsurf, x_surf, x_sf_grp)
!
      use m_surface_group
      use m_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: numsurf
      real(kind=kreal), intent(in) :: x_surf(numsurf)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(sf_grp%num_item)
!
!
      call pick_scalar_by_surf_grp(sf_grp%num_grp, sf_grp%num_item,     &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_data1%isurf_grp, numsurf, x_surf, x_sf_grp)
!
      end subroutine pick_scalar_4_surf_grp
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
      subroutine pick_vect_by_surf_grp_w_side(num_surf, num_surf_bc,    &
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
      real(kind = kreal) :: side
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
            side =  dble(isurf_grp(inum) / isurf)
            x_sf_grp(inum,1) = x_surf(isurf,1) * side
            x_sf_grp(inum,2) = x_surf(isurf,2) * side
            x_sf_grp(inum,3) = x_surf(isurf,3) * side
          end do
        end do
!poption parallel
      end do
!
      end subroutine pick_vect_by_surf_grp_w_side
!
!-----------------------------------------------------------------------
!
      subroutine pick_scalar_by_surf_grp(num_surf, num_surf_bc,         &
     &          num_surf_smp, isurf_grp_smp_stack, isurf_grp,           &
     &          numsurf, x_surf, x_sf_grp)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
      integer(kind = kint), intent(in) :: isurf_grp(num_surf_bc)
!
      integer(kind = kint), intent(in) :: numsurf
      real(kind=kreal), intent(in) :: x_surf(numsurf)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(num_surf_bc)
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
            x_sf_grp(inum) = x_surf(isurf)
          end do
        end do
!poption parallel
      end do
!
      end subroutine pick_scalar_by_surf_grp
!
!-----------------------------------------------------------------------
!
      end module set_surf_grp_vectors
