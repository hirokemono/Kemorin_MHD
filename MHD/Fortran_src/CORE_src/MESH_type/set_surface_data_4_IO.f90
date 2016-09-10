!set_surface_data_4_IO.f90
!      module set_surface_data_4_IO
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine copy_surf_connect_to_IO(surf, nele)
!!      subroutine copy_surf_geometry_to_IO(surf)
!!      subroutine copy_surf_geometry_to_IO_sph(surf)
!!      subroutine copy_surf_geometry_to_IO_cyl(surf)
!!
!!      subroutine copy_surf_connect_from_IO(surf, nele)
!!        integer(kind = kint), intent(in) :: nele
!!        type(surface_data), intent(inout) :: surf
!
      module set_surface_data_4_IO
!
      use m_precision
!
      use t_surface_data
      use m_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine copy_surf_connect_to_IO(surf, nele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele
      type(surface_data), intent(in) :: surf
!
!
      ele_IO%numele =        surf%numsurf
      ele_IO%nnod_4_ele =    surf%nnod_4_surf
!
      sfed_IO%nsf_4_ele =    nele
      sfed_IO%nsurf_in_ele = nsurf_4_ele
!
      call allocate_ele_connect_type(ele_IO)
!
      call alloc_surface_connect_IO(sfed_IO)
!
      if      (surf%nnod_4_surf .eq. num_linear_sf) then
        ele_IO%elmtyp(1:surf%numsurf) = 221
      else if (surf%nnod_4_surf .eq. num_quad_sf) then
        ele_IO%elmtyp(1:surf%numsurf) = 222
      else if (surf%nnod_4_surf .eq. num_lag_sf) then
        ele_IO%elmtyp(1:surf%numsurf) = 223
      end if
!
!$omp workshare
      ele_IO%nodelm(1:surf%numsurf) = surf%nnod_4_surf
      ele_IO%iele_global(1:surf%numsurf)                                &
     &        = surf%isurf_global(1:surf%numsurf)
!$omp end workshare
!$omp workshare
      ele_IO%ie(1:surf%numsurf,1:surf%nnod_4_surf)                      &
     &        = surf%ie_surf(1:surf%numsurf,1:surf%nnod_4_surf)
!$omp end workshare
!
!$omp workshare
      sfed_IO%isf_for_ele(1:nele,1:nsurf_4_ele)                         &
     &        = surf%isf_4_ele(1:nele,1:nsurf_4_ele)
!$omp end workshare
!
      end subroutine copy_surf_connect_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO(surf)
!
      type(surface_data), intent(inout) :: surf
      integer(kind = kint) :: isurf
!
!
      nod_IO%numnod =        surf%numsurf
      nod_IO%internal_node = surf%internal_surf
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!
!omp parallel do
      do isurf = 1, surf%numsurf
        nod_IO%inod_global(isurf) = surf%isurf_global(isurf)
        nod_IO%xx(isurf,1) =        surf%x_surf(isurf,1)
        nod_IO%xx(isurf,2) =        surf%x_surf(isurf,2)
        nod_IO%xx(isurf,3) =        surf%x_surf(isurf,3)
!
        sfed_IO%ele_scalar(isurf) =     surf%area_surf(isurf)
        sfed_IO%ele_vector(isurf,1) =   surf%vnorm_surf(isurf,1)
        sfed_IO%ele_vector(isurf,2) =   surf%vnorm_surf(isurf,2)
        sfed_IO%ele_vector(isurf,3) =   surf%vnorm_surf(isurf,3)
      end do
!omp end parallel do
!
      end subroutine copy_surf_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO_sph(surf)
!
      type(surface_data), intent(inout) :: surf
      integer(kind = kint) :: isurf
!
!
      nod_IO%numnod =        surf%numsurf
      nod_IO%internal_node = surf%internal_surf
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!omp parallel do
      do isurf = 1, surf%numsurf
        nod_IO%inod_global(isurf) = surf%isurf_global(isurf)
        nod_IO%xx(isurf,1) = surf%r_surf(isurf)
        nod_IO%xx(isurf,2) = surf%theta_surf(isurf)
        nod_IO%xx(isurf,3) = surf%phi_surf(isurf)
!
        sfed_IO%ele_scalar(isurf) =   surf%area_surf(isurf)
        sfed_IO%ele_vector(isurf,1) = surf%vnorm_surf_sph(isurf,1)
        sfed_IO%ele_vector(isurf,2) = surf%vnorm_surf_sph(isurf,2)
        sfed_IO%ele_vector(isurf,3) = surf%vnorm_surf_sph(isurf,3)
      end do
!omp end parallel do
!
      end subroutine copy_surf_geometry_to_IO_sph
!
!------------------------------------------------------------------
!
      subroutine copy_surf_geometry_to_IO_cyl(surf)
!
      type(surface_data), intent(inout) :: surf
      integer(kind = kint) :: isurf
!
!
      nod_IO%numnod =        surf%numsurf
      nod_IO%internal_node = surf%internal_surf
!
      call alloc_node_geometry_base(nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
!omp parallel do
      do isurf = 1, surf%numsurf
        nod_IO%inod_global(isurf) = surf%isurf_global(isurf)
        nod_IO%xx(isurf,1) = surf%s_surf(isurf)
        nod_IO%xx(isurf,2) = surf%phi_surf(isurf)
        nod_IO%xx(isurf,3) = surf%x_surf(isurf,3)
        sfed_IO%ele_scalar(isurf) =   surf%area_surf(isurf)
        sfed_IO%ele_vector(isurf,1) = surf%vnorm_surf_cyl(isurf,1)
        sfed_IO%ele_vector(isurf,2) = surf%vnorm_surf_cyl(isurf,2)
        sfed_IO%ele_vector(isurf,3) = surf%vnorm_surf_cyl(isurf,3)
      end do
!omp end parallel do
!
      end subroutine copy_surf_geometry_to_IO_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_surf_connect_from_IO(surf, nele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele
      type(surface_data), intent(inout) :: surf
!
!
      surf%numsurf = ele_IO%numele
!
      call allocate_surface_connect_type(surf, nele)
!
!$omp workshare
      surf%isurf_global(1:surf%numsurf)                                 &
     &        = ele_IO%iele_global(1:surf%numsurf)
!$omp end workshare
!$omp workshare
      surf%ie_surf(1:surf%numsurf,1:surf%nnod_4_surf)                   &
     &        = ele_IO%ie(1:surf%numsurf,1:surf%nnod_4_surf)
!$omp end workshare
!
!$omp workshare
      surf%isf_4_ele(1:nele,1:nsurf_4_ele)                              &
     &        = sfed_IO%isf_for_ele(1:nele,1:nsurf_4_ele)
!$omp end workshare
!
      call dealloc_surface_connect_IO(sfed_IO)
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine copy_surf_connect_from_IO
!
!------------------------------------------------------------------
!
      end module set_surface_data_4_IO
