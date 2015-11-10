!set_magne_boundary.f90
!      module set_magne_boundary
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Nov. 2003
!        modified by H.Matsui on July 2005
!
!      subroutine set_boundary_magne
!      subroutine delete_field_by_fixed_b_bc(i_field)
!
      module set_magne_boundary
!
      use m_precision
      use m_constants
!
      use m_geometry_data
      use m_bc_data_magne
      use m_surf_data_magne
!
      use set_fixed_boundaries
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_b_bc(i_field)
!
      use m_node_phys_data
      use set_velocity_boundary
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call delete_vector_on_bc(nod_bc1_b, i_field, nod_fld1)
!
      end subroutine delete_field_by_fixed_b_bc
!
!  ---------------------------------------------------------------------
!
      end module set_magne_boundary
