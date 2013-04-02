!cvt_nod_type_and_sph_fld.f90
!     module cvt_nod_type_and_sph_fld
!
!      Written by H. Matsui on Feb., 2008
!
!
!      subroutine cvt_vec_type_to_sph_vec(i_rtp, i_field, node, nod_fld)
!      subroutine cvt_sph_vec_to_vec_type(i_rtp, i_field, node, nod_fld)
!
!      subroutine cvt_tsr_type_to_sph_tsr(i_rtp, i_field, node, nod_fld)
!      subroutine cvt_sph_tsr_to_tsr_type(i_rtp, i_field, node, nod_fld)
!
!      subroutine copy_sph_scalar_2_scl_type(i_rtp, i_field,            &
!     &          node, nod_fld)
!      subroutine copy_sph_vector_2_vec_type(i_rtp, i_field,            &
!     &          node, nod_fld)
!      subroutine copy_sph_tensor_2_tsr_type(i_rtp, i_field,            &
!     &          node, nod_fld)
!
!      subroutine copy_scl_type_2_sph_scalar(i_rtp, i_field,            &
!     &          node, nod_fld)
!      subroutine copy_vct_type_2_sph_vector(i_rtp, i_field,            &
!     &          node, nod_fld)
!      subroutine copy_tsr_type_2_sph_tensor(i_rtp, i_field,            &
!     &          node, nod_fld)
!
      module cvt_nod_type_and_sph_fld
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine cvt_vec_type_to_sph_vec(i_rtp, i_field, node, nod_fld)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
!
      call cvt_nod_vec_to_sph_vec                                       &
     &   (node%numnod, np_smp, node%istack_nod_smp, node%xx,            &
     &    node%rr, node%ss, node%a_r, node%a_s,                         &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld,                    &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp, d_nod_rtp)
!
      end subroutine cvt_vec_type_to_sph_vec
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_vec_to_vec_type(i_rtp, i_field, node, nod_fld)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cvt_sph_vec_to_nod_vec(node%numnod,                          &
     &    np_smp, node%istack_nod_smp, node%theta, node%phi,            &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_nod_rtp)
!
      end subroutine cvt_sph_vec_to_vec_type
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cvt_tsr_type_to_sph_tsr(i_rtp, i_field, node, nod_fld)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
!
      call cvt_nod_tsr_to_sph_tsr                                       &
     &   (node%numnod, np_smp, node%istack_nod_smp, node%xx,            &
     &    node%rr, node%ss, node%a_r, node%a_s,                         &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld,                    &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp, d_nod_rtp)
!
      end subroutine cvt_tsr_type_to_sph_tsr
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_tsr_to_tsr_type(i_rtp, i_field, node, nod_fld)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cvt_sph_tsr_to_nod_tsr                                       &
     &   (node%numnod, np_smp, node%istack_nod_smp, node%xx,            &
     &    node%rr, node%ss, node%a_r, node%a_s,                         &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, nod_fld%ntot_phys, nod_fld%d_fld, d_nod_rtp)
!
      end subroutine cvt_sph_tsr_to_tsr_type
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_sph_scalar_2_scl_type(i_rtp, i_field,             &
     &          node, nod_fld)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_scalar_2_scalar_fld                                     &
     &   (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, node%numnod, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_sph_scalar_2_scl_type
!
! -------------------------------------------------------------------
!
      subroutine copy_sph_vector_2_vec_type(i_rtp, i_field,             &
     &          node, nod_fld)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_2_vector_fld                                     &
     &   (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, node%numnod, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_sph_vector_2_vec_type
!
! -------------------------------------------------------------------
!
      subroutine copy_sph_tensor_2_tsr_type(i_rtp, i_field,             &
     &          node, nod_fld)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_tensor_2_tensor_fld                                     &
     &   (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, node%numnod, nod_fld%ntot_phys, nod_fld%d_fld)
!
      end subroutine copy_sph_tensor_2_tsr_type
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_scl_type_2_sph_scalar(i_rtp, i_field,             &
     &          node, nod_fld)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
!
      call copy_scalar_2_scalar_fld                                     &
     &   (i_field, node%numnod, nod_fld%ntot_phys, nod_fld%d_fld,       &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_scl_type_2_sph_scalar
!
! -------------------------------------------------------------------
!
      subroutine copy_vct_type_2_sph_vector(i_rtp, i_field,             &
     &          node, nod_fld)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
!
      call copy_vector_2_vector_fld                                     &
     &   (i_field, node%numnod, nod_fld%ntot_phys, nod_fld%d_fld,       &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_vct_type_2_sph_vector
!
! -------------------------------------------------------------------
!
      subroutine copy_tsr_type_2_sph_tensor(i_rtp, i_field,             &
     &          node, nod_fld)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
!
      call copy_tensor_2_tensor_fld                                     &
     &   (i_field, node%numnod, nod_fld%ntot_phys, nod_fld%d_fld,       &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_tsr_type_2_sph_tensor
!
! -------------------------------------------------------------------
!
      end module cvt_nod_type_and_sph_fld
