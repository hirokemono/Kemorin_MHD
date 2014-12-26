!sph_transfer_all_field.f90
!      module sph_transfer_all_field
!
!     Written by H. Matsui on June, 2012
!
!      subroutine sph_f_trans_all_field
!      subroutine sph_b_trans_all_field
!
      module sph_transfer_all_field
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_all_field
!
      use m_solver_SR
      use sph_transforms
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use spherical_SRs_N
!
      integer(kind = kint) :: nscalar_trans
!
!
      if (ncomp_sph_trans .le. 0) return
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_sph_vect_to_sph_trans'
      call set_sph_vect_to_sph_trans
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'set_sph_scalar_to_sph_trans'
      call set_sph_scalar_to_sph_trans
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'set_sph_tensor_to_sph_trans'
      call set_sph_tensor_to_sph_trans
!
      nscalar_trans = num_scalar_rtp + 6*num_tensor_rtp
      call check_calypso_rtp_2_rtm_buf_N(ncomp_sph_trans)
      call check_calypso_rlm_2_rj_buf_N(ncomp_sph_trans)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_forward_transforms',        &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_forward_transforms(ncomp_sph_trans, num_vector_rtp,      &
     &    nscalar_trans, vr_rtp(1,1), n_WS, n_WR, WS(1), WR(1))
!
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_from_sph_t'
      call set_all_scalar_spec_from_sph_t(ncomp_sph_trans, n_WR, WR)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_vec_spec_from_sph_t'
      call set_all_vec_spec_from_sph_t(ncomp_sph_trans, n_WR, WR)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_from_sph_t'
      call set_all_tensor_spec_from_sph_t(ncomp_sph_trans, n_WR, WR)
!
      end subroutine sph_f_trans_all_field
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_all_field
!
      use m_solver_SR
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use sph_transforms
      use pole_sph_transform
      use spherical_SRs_N
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(ncomp_sph_trans .le. 0) return
!
      nscalar_trans = num_scalar_rtp + 6*num_tensor_rtp
      call check_calypso_rj_2_rlm_buf_N(ncomp_sph_trans)
      call check_calypso_rtm_2_rtp_buf_N(ncomp_sph_trans)
!
      if (iflag_debug.gt.0)                                             &
     &        write(*,*) 'set_all_vec_spec_to_sph_t'
      call set_all_vec_spec_to_sph_t(ncomp_sph_trans, n_WS, WS)
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_to_sph_t'
      call set_all_scalar_spec_to_sph_t(ncomp_sph_trans, n_WS, WS)
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_to_sph_t'
      call set_all_tensor_spec_to_sph_t(ncomp_sph_trans, n_WS, WS)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'pole_backward_transforms'
      call pole_backward_transforms(ncomp_sph_trans, num_vector_rtp,    &
     &    nscalar_trans, n_WR, WR)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_backward_transforms',       &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_backward_transforms(ncomp_sph_trans, num_vector_rtp,     &
     &    nscalar_trans, n_WS, n_WR, WS(1), WR(1), vr_rtp(1,1))
!
!
      if (iflag_debug.gt.0)                                             &
     &        write(*,*) 'set_xyz_vect_from_sph_trans'
      call set_xyz_vect_from_sph_trans
!
      if (iflag_debug.gt.0) write(*,*) 'set_sph_scalar_from_sph_trans'
      call set_sph_scalar_from_sph_trans
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_sph_tensor_from_sph_trans'
      call set_sph_tensor_from_sph_trans
!
      end subroutine sph_b_trans_all_field
!
! -----------------------------------------------------------------------
!
      end module sph_transfer_all_field

