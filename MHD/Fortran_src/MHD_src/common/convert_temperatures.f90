!
!      module convert_temperatures
!
!      Written by H. Matsui on Aug. 2005
!
!      subroutine set_2_perturbation_temp
!
      module convert_temperatures
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_2_perturbation_temp
!
      use m_geometry_data
      use m_node_phys_data
      use copy_nodal_fields
!
!
      call subtract_2_nod_scalars(node1, nod_fld1,                      &
     &    iphys%i_temp, iphys%i_ref_t, iphys%i_par_temp)
!
      end subroutine set_2_perturbation_temp
!
! ----------------------------------------------------------------------
!
      end module convert_temperatures
