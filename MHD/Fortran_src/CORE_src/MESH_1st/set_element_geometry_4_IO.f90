!
!      module set_element_geometry_4_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine copy_ele_geometry_to_IO
!      subroutine copy_ele_sph_geom_to_IO
!      subroutine copy_ele_cyl_geom_to_IO
!      subroutine copy_ele_geometry_from_IO
!      subroutine copy_ele_sph_geom_from_IO
!      subroutine copy_ele_cyl_geom_from_IO
!
      module set_element_geometry_4_IO
!
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
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
      subroutine copy_ele_geometry_to_IO
!
      integer(kind = kint) :: iele
!
!
      numnod_dummy = numele
      internal_node_dummy = internal_ele
!
      call allocate_node_data_dummy
      call allocate_ele_scalar_IO
!
!$omp parallel do
      do iele = 1, numele
        globalnodid_dummy(iele) = globalelmid(iele)
        xx_dummy(iele,1) = x_ele(iele,1)
        xx_dummy(iele,2) = x_ele(iele,2)
        xx_dummy(iele,3) = x_ele(iele,3)
        ele_scalar_IO(iele) = volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_geometry_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_sph_geom_to_IO
!
      integer(kind = kint) :: iele
!
!
      numnod_dummy = numele
      internal_node_dummy = internal_ele
!
      call allocate_node_data_dummy
      call allocate_ele_scalar_IO
!
!$omp parallel do
      do iele = 1, numele
        globalnodid_dummy(iele) = globalelmid(iele)
!
        xx_dummy(iele,1) = r_ele(iele)
        xx_dummy(iele,2) = theta_ele(iele)
        xx_dummy(iele,3) = phi_ele(iele)
        ele_scalar_IO(iele) = volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_sph_geom_to_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_cyl_geom_to_IO
!
      integer(kind = kint) :: iele
!
!
      numnod_dummy = numele
      internal_node_dummy = internal_ele
!
      call allocate_node_data_dummy
      call allocate_ele_scalar_IO
!
!$omp parallel do
      do iele = 1, numele
        globalnodid_dummy(iele) = globalelmid(iele)
!
        xx_dummy(iele,1) = s_ele(iele)
        xx_dummy(iele,2) = phi_ele(iele)
        xx_dummy(iele,3) = x_ele(iele,3)
        ele_scalar_IO(iele) = volume_ele(iele)
      end do
!$omp end parallel do
!
      end subroutine copy_ele_cyl_geom_to_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_ele_geometry_from_IO
!
      integer(kind = kint) :: iele
!
!
!      call allocate_element_geometry
!
!$omp parallel do
      do iele = 1, numele
        x_ele(iele,1) = xx_dummy(iele,1)
        x_ele(iele,2) = xx_dummy(iele,2)
        x_ele(iele,3) = xx_dummy(iele,3)
        volume_ele(iele) = ele_scalar_IO(iele)
      end do
!$omp end parallel do
!
      call deallocate_ele_scalar_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_ele_geometry_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_sph_geom_from_IO
!
      integer(kind = kint) :: iele
!
!
!      call allocate_element_geometry
!
!$omp parallel do
      do iele = 1, numele
        r_ele(iele) = xx_dummy(iele,1)
        theta_ele(iele) = xx_dummy(iele,2)
        phi_ele(iele) = xx_dummy(iele,3)
        volume_ele(iele) = ele_scalar_IO(iele)
      end do
!$omp end parallel do
!
      call deallocate_ele_scalar_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_ele_sph_geom_from_IO
!
!------------------------------------------------------------------
!
      subroutine copy_ele_cyl_geom_from_IO
!
      integer(kind = kint) :: iele
!
!
!      call allocate_element_geometry
!
!$omp parallel do
      do iele = 1, numele
        s_ele(iele) = xx_dummy(iele,1)
        phi_ele(iele) = xx_dummy(iele,2)
        x_ele(iele,3) = xx_dummy(iele,3)
        volume_ele(iele) = ele_scalar_IO(iele)
      end do
!$omp end parallel do
!
      call deallocate_ele_scalar_IO
      call deallocate_node_data_dummy
!
      end subroutine copy_ele_cyl_geom_from_IO
!
!------------------------------------------------------------------
!
      end module set_element_geometry_4_IO
