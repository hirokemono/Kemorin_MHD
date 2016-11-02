!>@file   copy_nodal_fld_4_sph_trans.f90
!!@brief  module copy_nodal_fld_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical transform data to 1st FEM data
!!
!!@verbatim
!!      subroutine copy_nodal_vector_from_trans                         &
!!     &         (sph_rtp, m_folding, ncomp_trans, i_trns, v_rtp,       &
!!     &          numnod, ncomp_nod, i_field, d_nod)
!!      subroutine copy_nodal_scalar_from_trans                         &
!!     &         (sph_rtp, m_folding, ncomp_trans, i_trns, v_rtp,       &
!!     &          numnod, ncomp_nod, i_field, d_nod)
!!
!!      subroutine copy_nod_scl_from_trans_wpole(sph_rtp, m_folding,    &
!!     &          ncomp_trans, i_trns, d_rtp, v_pole, i_field,          &
!!     &          node, nod_fld)
!!      subroutine copy_nod_vec_from_trans_wpole(sph_rtp, m_folding,    &
!!     &          ncomp_trans, i_trns, d_rtp, v_pole, i_field,          &
!!     &          node, nod_fld)
!!      subroutine copy_nod_tsr_from_trans_wpole(sph_rtp, m_folding,    &
!!     &          ncomp_trans, i_trns, d_rtp, v_pole, i_field,          &
!!     &          node, nod_fld)
!!
!!      subroutine copy_nod_scl_from_sph_trans                          &
!!     &         (sph_rtp, m_folding, d_rtp, i_field, node, nod_fld)
!!      subroutine copy_nod_vec_from_sph_trans                          &
!!     &         (sph_rtp, m_folding, d_rtp, i_field, node, nod_fld)
!!      subroutine copy_nod_tsr_from_sph_trans                          &
!!     &         (sph_rtp, m_folding, d_rtp, i_field, node, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_data),intent(inout) :: nod_fld
!!
!!      subroutine copy_nod_scl_to_sph_trans                            &
!!     &         (node, sph_rtp, nod_fld, i_field, d_rtp)
!!      subroutine copy_nod_vec_to_sph_trans                            &
!!     &         (node, sph_rtp, nod_fld, i_field, d_rtp)
!!      subroutine copy_nod_tsr_to_sph_trans                            &
!!     &         (node, sph_rtp, nod_fld, i_field, d_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_data),intent(in) :: nod_fld
!!@endverbatim
!
      module copy_nodal_fld_4_sph_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_data
!
      implicit  none
!
      private :: copy_nod_scl_from_sph_trans
      private :: copy_nod_vec_from_sph_trans
      private :: copy_nod_tsr_from_sph_trans
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_nodal_vector_from_trans                           &
     &         (sph_rtp, m_folding, ncomp_trans, i_trns, v_rtp,         &
     &          numnod, ncomp_nod, i_field, d_nod)
!
      use copy_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: ncomp_trans, i_trns
      real(kind = kreal), intent(in)                                    &
     &         :: v_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      integer(kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
!$omp parallel
      call copy_vector_from_trans_smp                                   &
     &   (sph_rtp%nnod_rtp, m_folding, sph_rtp%istack_inod_rtp_smp,     &
     &    numnod, v_rtp(1,i_trns), d_nod(1,i_field) )
!$omp end parallel
!
      end subroutine copy_nodal_vector_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_nodal_scalar_from_trans                           &
     &         (sph_rtp, m_folding, ncomp_trans, i_trns, v_rtp,         &
     &          numnod, ncomp_nod, i_field, d_nod)
!
      use copy_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: ncomp_trans, i_trns
      real(kind = kreal), intent(in)                                    &
     &           :: v_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      integer(kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
!$omp parallel
      call copy_scalar_from_trans_smp                                   &
     &   (sph_rtp%nnod_rtp, m_folding, sph_rtp%istack_inod_rtp_smp,     &
     &    numnod, v_rtp(1,i_trns), d_nod(1,i_field) )
!$omp end parallel
!
      end subroutine copy_nodal_scalar_from_trans
!
!-----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_scl_from_trans_wpole(sph_rtp, m_folding,      &
     &          ncomp_trans, i_trns, d_rtp, v_pole, i_field,            &
     &          node, nod_fld)
!
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_nod_scl_from_sph_trans                                  &
     &   (sph_rtp, m_folding, d_rtp(1,i_trns), i_field, node, nod_fld)
      call copy_pole_scl_fld_from_trans                                 &
     &   (m_folding, sph_rtp, node, v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_nod_scl_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_vec_from_trans_wpole(sph_rtp, m_folding,      &
     &          ncomp_trans, i_trns, d_rtp, v_pole, i_field,            &
     &          node, nod_fld)
!
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &           :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &           :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_nod_vec_from_sph_trans                                  &
     &   (sph_rtp, m_folding, d_rtp(1,i_trns), i_field, node, nod_fld)
      call copy_pole_vec_fld_from_trans(m_folding, sph_rtp, node,       &
     &    v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_nod_vec_from_trans_wpole
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_tsr_from_trans_wpole(sph_rtp, m_folding,      &
     &          ncomp_trans, i_trns, d_rtp, v_pole, i_field,            &
     &          node, nod_fld)
!
      use copy_pole_field_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: ncomp_trans
      real(kind = kreal), intent(in)                                    &
     &          :: d_rtp(sph_rtp%nnod_rtp,ncomp_trans)
      real(kind = kreal), intent(in)                                    &
     &          :: v_pole(sph_rtp%nnod_pole,ncomp_trans)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_nod_tsr_from_sph_trans                                  &
     &   (sph_rtp, m_folding, d_rtp(1,i_trns), i_field, node, nod_fld)
      call copy_pole_tsr_fld_from_trans(m_folding, sph_rtp, node,       &
     &    v_pole(1,i_trns), i_field, nod_fld)
!
      end subroutine copy_nod_tsr_from_trans_wpole
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_scl_from_sph_trans                            &
     &         (sph_rtp, m_folding, d_rtp, i_field, node, nod_fld)
!
      use copy_xyz_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(sph_rtp%nnod_rtp)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_scalar_from_sph_trans(sph_rtp%nnod_rtp, m_folding,      &
     &    sph_rtp%istack_inod_rtp_smp, node%numnod,                     &
     &    d_rtp, i_field, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_nod_scl_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_vec_from_sph_trans                            &
     &         (sph_rtp, m_folding, d_rtp, i_field, node, nod_fld)
!
      use copy_xyz_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(sph_rtp%nnod_rtp,3)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_xyz_vec_from_sph_trans                                  &
     &   (sph_rtp%nnod_rtp, m_folding, sph_rtp%istack_inod_rtp_smp,     &
     &    node%numnod, node%theta, node%phi, d_rtp,                     &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_nod_vec_from_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_tsr_from_sph_trans                            &
     &         (sph_rtp, m_folding, d_rtp, i_field, node, nod_fld)
!
      use copy_xyz_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: d_rtp(sph_rtp%nnod_rtp,6)
!
      type(node_data), intent(in) :: node
      type(phys_data),intent(inout) :: nod_fld
!
!
      call copy_xyz_tsr_from_sph_trans                                  &
     &   (sph_rtp%nnod_rtp, m_folding, sph_rtp%istack_inod_rtp_smp,     &
     &    node%numnod, node%xx, node%rr, node%ss, node%a_r, node%a_s,   &
     &    d_rtp, i_field, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_nod_tsr_from_sph_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_scl_to_sph_trans                              &
     &         (node, sph_rtp, nod_fld, i_field, d_rtp)
!
      use copy_xyz_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_data),intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(sph_rtp%nnod_rtp)
!
!
      call copy_scalar_to_sph_trans(sph_rtp%nnod_rtp, node%numnod,      &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_rtp)
!
      end subroutine copy_nod_scl_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_vec_to_sph_trans                              &
     &         (node, sph_rtp, nod_fld, i_field, d_rtp)
!
      use copy_xyz_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_data),intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(sph_rtp%nnod_rtp,3)
!
!
      call copy_xyz_vec_to_sph_trans                                    &
     &   (sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp, node%numnod,   &
     &    node%xx, node%rr, node%ss, node%a_r, node%a_s,                &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_rtp)
!
      end subroutine copy_nod_vec_to_sph_trans
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_tsr_to_sph_trans                              &
     &         (node, sph_rtp, nod_fld, i_field, d_rtp)
!
      use copy_xyz_field_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_data),intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(inout) :: d_rtp(sph_rtp%nnod_rtp,6)
!
!
      call copy_xyz_tsr_to_sph_trans                                    &
     &   (sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp, node%numnod,   &
     &    node%xx, node%rr, node%ss, node%a_r, node%a_s,                &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_rtp)
!
      end subroutine copy_nod_tsr_to_sph_trans
!
! -------------------------------------------------------------------
!
      end module copy_nodal_fld_4_sph_trans
