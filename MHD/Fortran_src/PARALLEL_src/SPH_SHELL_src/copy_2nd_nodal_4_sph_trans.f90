!copy_2nd_nodal_4_sph_trans.f90
!      module copy_2nd_nodal_4_sph_trans
!
!      Written by H. Matsui on Nov., 2012
!
!      subroutine copy_2nd_scl_from_trans_wpole(nscalar_trans, i_trns,  &
!     &          i_field)
!      subroutine copy_2nd_vec_from_trans_wpole(nvector_trans, i_trns,  &
!     &          i_field)
!      subroutine copy_2nd_tsr_from_trans_wpole(ncomp_trans, i_trns,    &
!     &         i_field)
!
!      subroutine copy_2nd_scl_from_sph_trans(nscalar_trans, i_trns,    &
!     &          i_field)
!      subroutine copy_2nd_vec_from_sph_trans(nvector_trans, i_trns,    &
!     &          i_field)
!      subroutine copy_2nd_tsr_from_sph_trans(ncomp_trans, i_trns,      &
!     &          i_field)
!
!      subroutine copy_2nd_scl_to_sph_trans(nscalar_trans, i_trns,      &
!     &          i_field)
!      subroutine copy_2nd_vec_to_sph_trans(ncomp_trans, i_trns,        &
!     &          i_field)
!      subroutine copy_2nd_tsr_to_sph_trans(ncomp_trans, i_trns,        &
!     &          i_field)
!
      module copy_2nd_nodal_4_sph_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_2nd_scl_from_trans_wpole(nscalar_trans, i_trns,   &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_scalar_from_trans_w_pole(nnod_2nd, internal_nod_2nd,    &
     &    xx_2nd, nscalar_trans, i_trns, i_field,                       &
     &    ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_scl_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_2nd_vec_from_trans_wpole(nvector_trans, i_trns,   &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: nvector_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_vec_from_trans_w_pole(nnod_2nd, internal_nod_2nd,   &
     &    inod_smp_stack_2nd, xx_2nd, theta_2nd, phi_2nd,               &
     &    nvector_trans, i_trns, i_field, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_vec_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_2nd_tsr_from_trans_wpole(ncomp_trans, i_trns,     &
     &         i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_tsr_from_trans_w_pole(nnod_2nd, internal_nod_2nd,   &
     &    inod_smp_stack_2nd, xx_2nd, radius_2nd, s_cyl_2nd,            &
     &    a_radius_2nd, a_s_cyl_2nd, ncomp_trans, i_trns, i_field,      &
     &    ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_tsr_from_trans_wpole
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_2nd_scl_from_sph_trans(nscalar_trans, i_trns,     &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_scalar_from_sph_trans(nnod_2nd, nscalar_trans, i_trns,  &
     &    i_field, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_scl_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_2nd_vec_from_sph_trans(nvector_trans, i_trns,     &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: nvector_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_vec_from_sph_trans(nnod_2nd, inod_smp_stack_2nd,    &
     &    theta_2nd, phi_2nd, nvector_trans, i_trns, i_field,           &
     &    ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_vec_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_2nd_tsr_from_sph_trans(ncomp_trans, i_trns,       &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_tsr_from_sph_trans(nnod_2nd, inod_smp_stack_2nd,    &
     &    xx_2nd, radius_2nd, s_cyl_2nd, a_radius_2nd, a_s_cyl_2nd,     &
     &    ncomp_trans, i_trns, i_field, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_tsr_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_2nd_scl_to_sph_trans(nscalar_trans, i_trns,       &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_scalar_to_sph_trans(nnod_2nd, nscalar_trans, i_trns,    &
     &    i_field, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_scl_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_2nd_vec_to_sph_trans(ncomp_trans, i_trns,         &
     &          i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_vec_to_sph_trans(nnod_2nd, inod_smp_stack_2nd,      &
     &    xx_2nd, radius_2nd, s_cyl_2nd, a_radius_2nd, a_s_cyl_2nd,     &
     &    ncomp_trans, i_trns, i_field, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_vec_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_2nd_tsr_to_sph_trans(ncomp_trans, i_trns,         &
     &           i_field)
!
      use copy_xyz_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      call copy_xyz_tsr_to_sph_trans(nnod_2nd, inod_smp_stack_2nd,      &
     &    xx_2nd, radius_2nd, s_cyl_2nd, a_radius_2nd, a_s_cyl_2nd,     &
     &    ncomp_trans, i_trns, i_field, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_2nd_tsr_to_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_2nd_nodal_4_sph_trans
