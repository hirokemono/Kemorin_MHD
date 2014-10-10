!>@file   copy_1st_nodal_4_sph_trans.f90
!!@brief  module copy_1st_nodal_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical transform data to 1st FEM data
!!
!!@verbatim
!!      subroutine copy_1st_scl_from_trans_wpole(ncomp_trans, i_trns,   &
!!     &          i_field)
!!      subroutine copy_1st_vec_from_trans_wpole(ncomp_trans, i_trns,   &
!!     &          i_field)
!!      subroutine copy_1st_tsr_from_trans_wpole(ncomp_trans, i_trns,   &
!!     &         i_field)
!!
!!      subroutine copy_1st_scl_from_sph_trans(i_trns,  i_field)
!!      subroutine copy_1st_vec_from_sph_trans(i_trns, i_field)
!!      subroutine copy_1st_tsr_from_sph_trans(i_trns, i_field)
!!
!!      subroutine copy_1st_scl_to_sph_trans(i_trns, i_field)
!!      subroutine copy_1st_vec_to_sph_trans(i_trns, i_field)
!!      subroutine copy_1st_tsr_to_sph_trans(i_trns, i_field)
!!@endverbatim
!
      module copy_1st_nodal_4_sph_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_scl_from_trans_wpole(ncomp_trans, i_trns,     &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_scalar_from_trans_w_pole(numnod, internal_node,         &
     &    xx, ncomp_trans, i_trns, i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_scl_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_vec_from_trans_wpole(ncomp_trans, i_trns,     &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_vec_from_trans_w_pole(numnod, internal_node,        &
     &    inod_smp_stack, xx, colatitude, longitude,                    &
     &    ncomp_trans, i_trns, i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_vec_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_tsr_from_trans_wpole(ncomp_trans, i_trns,     &
     &         i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_tsr_from_trans_w_pole(numnod, internal_node,        &
     &    inod_smp_stack, xx, radius, s_cylinder,                       &
     &    a_radius, a_s_cylinder, ncomp_trans, i_trns, i_field,         &
     &    num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_tsr_from_trans_wpole
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_1st_scl_from_sph_trans(i_trns,  i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_scalar_from_sph_trans(numnod, i_trns, i_field,          &
     &    num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_scl_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_vec_from_sph_trans(i_trns, i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_vec_from_sph_trans(numnod, inod_smp_stack,          &
     &    colatitude, longitude, i_trns, i_field,                       &
     &    num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_vec_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_tsr_from_sph_trans(i_trns, i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_tsr_from_sph_trans(numnod, inod_smp_stack,          &
     &    xx, radius, s_cylinder, a_radius, a_s_cylinder,               &
     &    i_trns, i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_tsr_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_1st_scl_to_sph_trans(i_trns, i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_scalar_to_sph_trans(numnod, i_trns, i_field,            &
     &    num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_scl_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_vec_to_sph_trans(i_trns, i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_vec_to_sph_trans(numnod, inod_smp_stack,            &
     &    xx, radius, s_cylinder, a_radius, a_s_cylinder,               &
     &    i_trns, i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_vec_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_1st_tsr_to_sph_trans(i_trns, i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_tsr_to_sph_trans(numnod, inod_smp_stack,            &
     &    xx, radius, s_cylinder, a_radius, a_s_cylinder,               &
     &    i_trns, i_field, num_tot_nod_phys, d_nod)
!
      end subroutine copy_1st_tsr_to_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_1st_nodal_4_sph_trans
