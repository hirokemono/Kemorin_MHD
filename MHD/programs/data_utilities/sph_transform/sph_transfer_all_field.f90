!sph_transfer_all_field.f90
!      module sph_transfer_all_field
!
!     Written by H. Matsui on June, 2012
!
!!      subroutine allocate_d_rtp_4_all_trans
!!      subroutine allocate_d_pole_4_all_trans
!!      subroutine deallocate_d_rtp_4_all_trans
!!      subroutine deallocate_d_pole_4_all_trans
!!      subroutine sph_f_trans_all_field(mesh, nod_fld, rj_fld)
!!      subroutine sph_b_trans_all_field(mesh, rj_fld, nod_fld)
!
      module sph_transfer_all_field
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: dall_rtp(:,:)
!
      real(kind = kreal), allocatable :: dall_pole(:,:)
      real(kind = kreal), allocatable :: dlcl_pole(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_d_rtp_4_all_trans
!
      use m_spheric_parameter
      use m_work_4_sph_trans
!
!
      allocate(dall_rtp(nnod_rtp,ncomp_sph_trans))
      if(ncomp_sph_trans .gt. 0) dall_rtp = 0.0d0
!
      end subroutine allocate_d_rtp_4_all_trans
!
! -----------------------------------------------------------------------
!
      subroutine allocate_d_pole_4_all_trans
!
      use m_work_4_sph_trans
      use m_work_pole_sph_trans
!
!
      allocate(dall_pole(nnod_pole,ncomp_sph_trans))
      allocate(dlcl_pole(nnod_pole,ncomp_sph_trans))
      if(ncomp_sph_trans .gt. 0) dall_pole = 0.0d0
      if(ncomp_sph_trans .gt. 0) dlcl_pole = 0.0d0
!
      end subroutine allocate_d_pole_4_all_trans
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_d_rtp_4_all_trans
!
!
      deallocate(dall_rtp, dall_pole, dlcl_pole)
!
      end subroutine deallocate_d_rtp_4_all_trans
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_d_pole_4_all_trans
!
!
      deallocate(dall_rtp, dall_pole, dlcl_pole)
!
      end subroutine deallocate_d_pole_4_all_trans
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_all_field(mesh, nod_fld, rj_fld)
!
      use t_mesh_data
      use t_phys_data
      use m_solver_SR
      use m_spheric_parameter
      use m_work_4_sph_trans
      use sph_transforms
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use spherical_SRs_N
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: nod_fld
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: nscalar_trans
!
!
      if (ncomp_sph_trans .le. 0) return
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_sph_vect_to_sph_trans'
      call set_sph_vect_to_sph_trans(mesh%node, nod_fld,                &
     &    nnod_rtp, ncomp_sph_trans, dall_rtp(1,1))
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'set_sph_scalar_to_sph_trans'
      call set_sph_scalar_to_sph_trans(mesh%node, nod_fld,              &
     &    nnod_rtp, ncomp_sph_trans, dall_rtp(1,1))
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'set_sph_tensor_to_sph_trans'
      call set_sph_tensor_to_sph_trans(mesh%node, nod_fld,              &
     &    nnod_rtp, ncomp_sph_trans, dall_rtp(1,1))
!
      nscalar_trans = num_scalar_rtp + 6*num_tensor_rtp
      call check_calypso_rtp_2_rtm_buf_N(ncomp_sph_trans)
      call check_calypso_rlm_2_rj_buf_N(ncomp_sph_trans)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_forward_transforms',        &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_forward_transforms(ncomp_sph_trans, num_vector_rtp,      &
     &    nscalar_trans, dall_rtp(1,1), n_WS, n_WR, WS(1), WR(1))
!
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_from_sph_t'
      call set_all_scalar_spec_from_sph_t                               &
     &   (ncomp_sph_trans, n_WR, WR, rj_fld)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_vec_spec_from_sph_t'
      call set_all_vec_spec_from_sph_t                                  &
     &   (ncomp_sph_trans, n_WR, WR, rj_fld)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_from_sph_t'
      call set_all_tensor_spec_from_sph_t                               &
     &   (ncomp_sph_trans, n_WR, WR, rj_fld)
!
      end subroutine sph_f_trans_all_field
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_all_field(mesh, rj_fld, nod_fld)
!
      use t_mesh_data
      use t_phys_data
!
      use m_solver_SR
      use m_spheric_parameter
      use m_work_4_sph_trans
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use sph_transforms
      use spherical_SRs_N
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data), intent(in) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
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
      call set_all_vec_spec_to_sph_t                                    &
     &    (ncomp_sph_trans, rj_fld, n_WS, WS)
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_to_sph_t'
      call set_all_scalar_spec_to_sph_t                                 &
     &   (ncomp_sph_trans, rj_fld, n_WS, WS, dlcl_pole(1,1))
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_to_sph_t'
      call set_all_tensor_spec_to_sph_t                                 &
     &   (ncomp_sph_trans, rj_fld, n_WS, WS)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_backward_transforms',       &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_backward_transforms(ncomp_sph_trans, num_vector_rtp,     &
     &    nscalar_trans, n_WS, n_WR, WS(1), WR(1), dall_rtp,            &
     &    dlcl_pole, dall_pole)
!
!
      if (iflag_debug.gt.0)                                             &
     &        write(*,*) 'set_xyz_vect_from_sph_trans'
      call set_xyz_vect_from_sph_trans                                  &
     &   (mesh%node, nnod_rtp, ncomp_sph_trans,                         &
     &    dall_rtp(1,1), dall_pole(1,1), nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'set_sph_scalar_from_sph_trans'
      call set_sph_scalar_from_sph_trans                                &
     &   (mesh%node, nnod_rtp, ncomp_sph_trans,                         &
     &    dall_rtp(1,1), dall_pole(1,1), nod_fld)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_sph_tensor_from_sph_trans'
      call set_sph_tensor_from_sph_trans                                &
     &   (mesh%node, nnod_rtp, ncomp_sph_trans,                         &
     &    dall_rtp(1,1), dall_pole(1,1), nod_fld)
!
      end subroutine sph_b_trans_all_field
!
! -----------------------------------------------------------------------
!
      end module sph_transfer_all_field

