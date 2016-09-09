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
      nsf_4_ele_IO =    nele
      nsurf_in_ele_IO = nsurf_4_ele
!
      call allocate_ele_info_dummy
      call allocate_connect_dummy
!
      call allocate_surface_connect_IO
!
      if      (surf%nnod_4_surf .eq. num_linear_sf) then
        i_ele_dummy(1:surf%numsurf) = 221
      else if (surf%nnod_4_surf .eq. num_quad_sf) then
        i_ele_dummy(1:surf%numsurf) = 222
      else if (surf%nnod_4_surf .eq. num_lag_sf) then
        i_ele_dummy(1:surf%numsurf) = 223
      end if
!
!$omp workshare
      ele_IO%nodelm(1:surf%numsurf) = surf%nnod_4_surf
      ele_IO%iele_global(1:surf%numsurf)                                &
     &        = surf%isurf_global(1:surf%numsurf)
!$omp end workshare
!$omp workshare
      ie_dummy(1:surf%numsurf,1:surf%nnod_4_surf)                       &
     &        = surf%ie_surf(1:surf%numsurf,1:surf%nnod_4_surf)
!$omp end workshare
!
!$omp workshare
      isf_4_ele_IO(1:nele,1:nsurf_4_ele)                                &
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
      numnod_dummy =        surf%numsurf
      internal_node_dummy = surf%internal_surf
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!
!omp parallel do
      do isurf = 1, surf%numsurf
        globalnodid_dummy(isurf) = surf%isurf_global(isurf)
        xx_dummy(isurf,1) =        surf%x_surf(isurf,1)
        xx_dummy(isurf,2) =        surf%x_surf(isurf,2)
        xx_dummy(isurf,3) =        surf%x_surf(isurf,3)
!
        ele_scalar_IO(isurf) =     surf%area_surf(isurf)
        ele_vector_IO(isurf,1) =   surf%vnorm_surf(isurf,1)
        ele_vector_IO(isurf,2) =   surf%vnorm_surf(isurf,2)
        ele_vector_IO(isurf,3) =   surf%vnorm_surf(isurf,3)
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
      numnod_dummy =        surf%numsurf
      internal_node_dummy = surf%internal_surf
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!omp parallel do
      do isurf = 1, surf%numsurf
        globalnodid_dummy(isurf) = surf%isurf_global(isurf)
        xx_dummy(isurf,1) = surf%r_surf(isurf)
        xx_dummy(isurf,2) = surf%theta_surf(isurf)
        xx_dummy(isurf,3) = surf%phi_surf(isurf)
!
        ele_scalar_IO(isurf) =   surf%area_surf(isurf)
        ele_vector_IO(isurf,1) = surf%vnorm_surf_sph(isurf,1)
        ele_vector_IO(isurf,2) = surf%vnorm_surf_sph(isurf,2)
        ele_vector_IO(isurf,3) = surf%vnorm_surf_sph(isurf,3)
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
      numnod_dummy =        surf%numsurf
      internal_node_dummy = surf%internal_surf
!
      call allocate_node_data_dummy
      call allocate_ele_vector_IO
      call allocate_ele_scalar_IO
!
!omp parallel do
      do isurf = 1, surf%numsurf
        globalnodid_dummy(isurf) = surf%isurf_global(isurf)
        xx_dummy(isurf,1) = surf%s_surf(isurf)
        xx_dummy(isurf,2) = surf%phi_surf(isurf)
        xx_dummy(isurf,3) = surf%x_surf(isurf,3)
        ele_scalar_IO(isurf) =   surf%area_surf(isurf)
        ele_vector_IO(isurf,1) = surf%vnorm_surf_cyl(isurf,1)
        ele_vector_IO(isurf,2) = surf%vnorm_surf_cyl(isurf,2)
        ele_vector_IO(isurf,3) = surf%vnorm_surf_cyl(isurf,3)
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
     &        = ie_dummy(1:surf%numsurf,1:surf%nnod_4_surf)
!$omp end workshare
!
!$omp workshare
      surf%isf_4_ele(1:nele,1:nsurf_4_ele)                              &
     &        = isf_4_ele_IO(1:nele,1:nsurf_4_ele)
!$omp end workshare
!
      call deallocate_surface_connect_IO
      call deallocate_ele_info_dummy
!
      end subroutine copy_surf_connect_from_IO
!
!------------------------------------------------------------------
!
      end module set_surface_data_4_IO
