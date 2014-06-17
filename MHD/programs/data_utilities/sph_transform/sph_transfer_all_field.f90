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
      use sph_transforms
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
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
!
      if (iflag_debug.gt.0) write(*,*) 'sph_forward_transforms',        &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_forward_transforms(ncomp_sph_trans, num_vector_rtp,      &
     &      num_scalar_rtp, num_tensor_rtp)
!
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_from_sph_t'
      call set_all_scalar_spec_from_sph_t
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_vec_spec_from_sph_t'
      call set_all_vec_spec_from_sph_t
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_from_sph_t'
      call set_all_tensor_spec_from_sph_t
!
      end subroutine sph_f_trans_all_field
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_all_field
!
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use sph_transforms
      use pole_sph_transform
!
!
      if(ncomp_sph_trans .le. 0) return
!
      if (iflag_debug.gt.0)                                             &
     &        write(*,*) 'set_all_vec_spec_to_sph_t'
      call set_all_vec_spec_to_sph_t
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_to_sph_t'
      call set_all_scalar_spec_to_sph_t
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_to_sph_t'
      call set_all_tensor_spec_to_sph_t
!
!
      if (iflag_debug.gt.0) write(*,*) 'sph_forward_transforms',        &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_backward_transforms(ncomp_sph_trans, num_vector_rtp,     &
     &    num_scalar_rtp, num_tensor_rtp)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'pole_backward_transforms'
      call pole_backward_transforms(ncomp_sph_trans, num_vector_rtp,    &
     &    num_scalar_rtp, num_tensor_rtp)
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

