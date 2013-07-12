!sph_transfer_all_field.f90
!      module sph_transfer_all_field
!
!     Written by H. Matsui on June, 2012
!
!      subroutine sph_f_trans_all_scalar
!      subroutine sph_f_trans_all_vector
!      subroutine sph_f_trans_all_tensor
!
!      subroutine sph_b_trans_all_scalar
!      subroutine sph_b_trans_all_vector
!      subroutine sph_b_trans_all_tensor
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
      subroutine sph_f_trans_all_scalar
!
      use sph_trans_scalar
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
!
!
      if (num_scalar_rtp .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &    write(*,*) 'set_sph_scalar_to_sph_trans'
        call set_sph_scalar_to_sph_trans
!
        if (iflag_debug.gt.0) write(*,*) 'sph_f_trans_scalar',          &
     &      num_scalar_rtp
        call sph_f_trans_scalar(num_scalar_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_all_scalar_spec_from_sph_t'
        call set_all_scalar_spec_from_sph_t
      end if
!
      end subroutine sph_f_trans_all_scalar
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_all_vector
!
      use sph_trans_vector
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
!
!
      if (num_vector_rtp .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_sph_vect_to_sph_trans'
        call set_sph_vect_to_sph_trans
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'sph_f_trans_vector', num_vector_rtp
        call sph_f_trans_vector(num_vector_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_all_vec_spec_from_sph_t'
        call set_all_vec_spec_from_sph_t
      end if
!
      end subroutine sph_f_trans_all_vector
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_all_tensor
!
      use sph_trans_scalar
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
!
!
      if (num_tensor_rtp .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_sph_tensor_to_sph_trans'
        call set_sph_tensor_to_sph_trans
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'sph_f_trans_tensor', num_tensor_rtp
        call sph_f_trans_tensor(num_tensor_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_all_tensor_spec_from_sph_t'
        call set_all_tensor_spec_from_sph_t
      end if
!
      end subroutine sph_f_trans_all_tensor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_all_scalar
!
      use sph_trans_scalar
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use pole_sph_transform
!
!
      if (num_scalar_rtp .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_all_scalar_spec_to_sph_t'
        call set_all_scalar_spec_to_sph_t
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'sph_b_trans_scalar', num_scalar_rtp
        call sph_b_trans_scalar(num_scalar_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'pole_b_trans_scalar'
        call pole_b_trans_scalar(num_scalar_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_sph_scalar_from_sph_trans'
        call set_sph_scalar_from_sph_trans
      end if
!
      end subroutine sph_b_trans_all_scalar
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_all_vector
!
      use sph_trans_vector
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use pole_sph_transform
!
!
      if (num_vector_rtp .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'set_all_vec_spec_to_sph_t'
        call set_all_vec_spec_to_sph_t
!
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'sph_b_trans_vector', num_vector_rtp
        call sph_b_trans_vector(num_vector_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'pole_b_trans_vector'
        call pole_b_trans_vector(num_vector_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'set_xyz_vect_from_sph_trans'
        call set_xyz_vect_from_sph_trans
      end if
!
      end subroutine sph_b_trans_all_vector
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_all_tensor
!
      use sph_trans_scalar
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use pole_sph_transform
!
!
      if (num_tensor_rtp .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_all_tensor_spec_to_sph_t'
        call set_all_tensor_spec_to_sph_t
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'sph_b_trans_tensor', num_tensor_rtp
        call sph_b_trans_tensor(num_tensor_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'pole_b_trans_tensor'
        call pole_b_trans_tensor(num_tensor_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'set_sph_tensor_from_sph_trans'
        call set_sph_tensor_from_sph_trans
      end if
!
      end subroutine sph_b_trans_all_tensor
!
! -----------------------------------------------------------------------
!
      end module sph_transfer_all_field

