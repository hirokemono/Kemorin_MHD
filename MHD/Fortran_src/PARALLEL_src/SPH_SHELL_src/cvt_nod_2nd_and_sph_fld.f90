!cvt_nod_2nd_and_sph_fld.f90
!     module cvt_nod_2nd_and_sph_fld
!
!      Written by H. Matsui on Feb., 2008
!
!      subroutine cvt_vec_2nd_to_sph_vec(i_rtp, i_field, node, nod_fld)
!      subroutine cvt_sph_vec_to_vec_2nd(i_rtp, i_field, node, nod_fld)
!
!      subroutine cvt_tsr_2nd_to_sph_tsr(i_rtp, i_field, node, nod_fld)
!      subroutine cvt_sph_tsr_to_tsr_2nd(i_rtp, i_field, node, nod_fld)
!
!      subroutine copy_sph_scalar_2_scl_2nd(i_rtp, i_field)
!      subroutine copy_sph_vector_2_vec_2nd(i_rtp, i_field)
!      subroutine copy_sph_tensor_2_tsr_2nd(i_rtp, i_field)
!
!      subroutine copy_scl_2nd_2_sph_scalar(i_rtp, i_field)
!      subroutine copy_vct_2nd_2_sph_vector(i_rtp, i_field)
!      subroutine copy_tsr_2nd_2_sph_tensor(i_rtp, i_field)
!
      module cvt_nod_2nd_and_sph_fld
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
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
      subroutine cvt_vec_2nd_to_sph_vec(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_nod_vec_to_sph_vec(nnod_2nd, np_smp, inod_smp_stack_2nd, &
     &    xx_2nd, radius_2nd, s_cyl_2nd, a_radius_2nd, a_s_cyl_2nd,     &
     &    i_field, ntot_nod_phys_2nd, d_nod_2nd,                        &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp, d_nod_rtp)
!
      end subroutine cvt_vec_2nd_to_sph_vec
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_vec_to_vec_2nd(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_sph_vec_to_nod_vec                                       &
     &   (nnod_2nd, np_smp, inod_smp_stack_2nd, theta_2nd, phi_2nd,     &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, ntot_nod_phys_2nd, d_nod_2nd, d_nod_rtp)
!
      end subroutine cvt_sph_vec_to_vec_2nd
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cvt_tsr_2nd_to_sph_tsr(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_nod_tsr_to_sph_tsr(nnod_2nd, np_smp, inod_smp_stack_2nd, &
     &    xx_2nd, radius_2nd, s_cyl_2nd, a_radius_2nd, a_s_cyl_2nd,     &
     &    i_field, ntot_nod_phys_2nd, d_nod_2nd,                        &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp, d_nod_rtp)
!
      end subroutine cvt_tsr_2nd_to_sph_tsr
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_tsr_to_tsr_2nd(i_rtp, i_field)
!
      use cvt_nodal_and_sph_field
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call cvt_sph_tsr_to_nod_tsr(nnod_2nd, np_smp, inod_smp_stack_2nd, &
     &    xx_2nd, radius_2nd, s_cyl_2nd, a_radius_2nd, a_s_cyl_2nd,     &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, ntot_nod_phys_2nd, d_nod_2nd, d_nod_rtp)
!
      end subroutine cvt_sph_tsr_to_tsr_2nd
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_sph_scalar_2_scl_2nd(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_scalar_2_scalar_fld                                     &
     &   (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, nnod_2nd, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_sph_scalar_2_scl_2nd
!
! -------------------------------------------------------------------
!
      subroutine copy_sph_vector_2_vec_2nd(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_vector_2_vector_fld                                     &
     &   (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, nnod_2nd, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_sph_vector_2_vec_2nd
!
! -------------------------------------------------------------------
!
      subroutine copy_sph_tensor_2_tsr_2nd(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_tensor_2_tensor_fld                                     &
     &   (i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp,                        &
     &    i_field, nnod_2nd, ntot_nod_phys_2nd, d_nod_2nd)
!
      end subroutine copy_sph_tensor_2_tsr_2nd
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_scl_2nd_2_sph_vector(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_scalar_2_scalar_fld                                     &
     &   (i_field, nnod_2nd, ntot_nod_phys_2nd, d_nod_2nd,              &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_scl_2nd_2_sph_vector
!
! -------------------------------------------------------------------
!
      subroutine copy_vct_2nd_2_sph_vector(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_vector_2_vector_fld                                     &
     &   (i_field, nnod_2nd, ntot_nod_phys_2nd, d_nod_2nd,              &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_vct_2nd_2_sph_vector
!
! -------------------------------------------------------------------
!
      subroutine copy_tsr_2nd_2_sph_tensor(i_rtp, i_field)
!
      use copy_between_two_fields
!
      integer(kind = kint), intent(in) :: i_rtp, i_field
!
!
      call copy_tensor_2_tensor_fld                                     &
     &   (i_field, nnod_2nd, ntot_nod_phys_2nd, d_nod_2nd,              &
     &    i_rtp, nnod_rtp, ntot_phys_rtp, d_rtp)
!
      end subroutine copy_tsr_2nd_2_sph_tensor
!
! -------------------------------------------------------------------
!
      end module cvt_nod_2nd_and_sph_fld
