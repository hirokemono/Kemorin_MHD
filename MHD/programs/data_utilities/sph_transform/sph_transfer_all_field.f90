!sph_transfer_all_field.f90
!      module sph_transfer_all_field
!
!     Written by H. Matsui on June, 2012
!
!!      subroutine allocate_d_rtp_4_all_trans(ncomp_sph_trans, sph_rtp)
!!      subroutine allocate_d_pole_4_all_trans(ncomp_sph_trans, sph_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!
!!      subroutine deallocate_d_rtp_4_all_trans
!!      subroutine deallocate_d_pole_4_all_trans
!!      subroutine sph_f_trans_all_field(ncomp_sph_trans,               &
!!     &          sph, comms_sph, mesh, trans_p, nod_fld, rj_fld)
!!      subroutine sph_b_trans_all_field(ncomp_sph_trans,               &
!!     &          sph, comms_sph, mesh, trans_p, rj_fld, nod_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!
      module sph_transfer_all_field
!
      use m_precision
!
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_spheric_rtp_data
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
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
      subroutine allocate_d_rtp_4_all_trans(ncomp_sph_trans, sph_rtp)
!
      integer(kind = kint), intent(in) :: ncomp_sph_trans
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
!
      allocate(dall_rtp(sph_rtp%nnod_rtp,ncomp_sph_trans))
      if(ncomp_sph_trans .gt. 0) dall_rtp = 0.0d0
!
      end subroutine allocate_d_rtp_4_all_trans
!
! -----------------------------------------------------------------------
!
      subroutine allocate_d_pole_4_all_trans(ncomp_sph_trans, sph_rtp)
!
      integer(kind = kint), intent(in) :: ncomp_sph_trans
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
!
      allocate(dall_pole(sph_rtp%nnod_pole,ncomp_sph_trans))
      allocate(dlcl_pole(sph_rtp%nnod_pole,ncomp_sph_trans))
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
      subroutine sph_f_trans_all_field(ncomp_sph_trans,                 &
     &          sph, comms_sph, mesh, trans_p, nod_fld, rj_fld)
!
      use m_solver_SR
      use sph_transforms
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp_sph_trans
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
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
     &    sph%sph_rtp, ncomp_sph_trans, dall_rtp(1,1))
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'set_sph_scalar_to_sph_trans'
      call set_sph_scalar_to_sph_trans(mesh%node, nod_fld,              &
     &    sph%sph_rtp, ncomp_sph_trans, dall_rtp(1,1))
      if (iflag_debug.gt.0)                                             &
     &    write(*,*) 'set_sph_tensor_to_sph_trans'
      call set_sph_tensor_to_sph_trans(mesh%node, nod_fld,              &
     &    sph%sph_rtp, ncomp_sph_trans, dall_rtp(1,1))
!
      nscalar_trans = num_scalar_rtp + 6*num_tensor_rtp
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_sph_trans, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_sph_trans, comms_sph%comm_rlm, comms_sph%comm_rj)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_forward_transforms',        &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_forward_transforms                                       &
     &   (ncomp_sph_trans, num_vector_rtp, nscalar_trans,               &
     &    sph, comms_sph, trans_p, dall_rtp(1,1),                       &
     &    n_WS, n_WR, WS(1), WR(1))
!
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_from_sph_t'
      call set_all_scalar_spec_from_sph_t                               &
     &   (ncomp_sph_trans, comms_sph%comm_rj, n_WR, WR, rj_fld)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_vec_spec_from_sph_t'
      call set_all_vec_spec_from_sph_t                                  &
     &   (ncomp_sph_trans, comms_sph%comm_rj, n_WR, WR, rj_fld)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_from_sph_t'
      call set_all_tensor_spec_from_sph_t                               &
     &   (ncomp_sph_trans, comms_sph%comm_rj, n_WR, WR, rj_fld)
!
      end subroutine sph_f_trans_all_field
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_all_field(ncomp_sph_trans,                 &
     &          sph, comms_sph, mesh, trans_p, rj_fld, nod_fld)
!
      use m_solver_SR
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use sph_transforms
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp_sph_trans
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
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
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_sph_trans, comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_sph_trans, comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      if (iflag_debug.gt.0)                                             &
     &        write(*,*) 'set_all_vec_spec_to_sph_t'
      call set_all_vec_spec_to_sph_t                                    &
     &    (ncomp_sph_trans, comms_sph%comm_rj, rj_fld, n_WS, WS)
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_scalar_spec_to_sph_t'
      call set_all_scalar_spec_to_sph_t(sph%sph_rtp%nnod_pole,          &
     &    ncomp_sph_trans, sph%sph_rj, comms_sph%comm_rj, rj_fld,       &
     &    n_WS, WS, dlcl_pole(1,1))
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_all_tensor_spec_to_sph_t'
      call set_all_tensor_spec_to_sph_t                                 &
     &   (ncomp_sph_trans, comms_sph%comm_rj, rj_fld, n_WS, WS)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_b_trans_w_poles',           &
     &  ncomp_sph_trans, num_vector_rtp, num_scalar_rtp, num_tensor_rtp
      call sph_b_trans_w_poles                                          &
     &   (ncomp_sph_trans, num_vector_rtp, nscalar_trans,               &
     &    sph, comms_sph, trans_p, n_WS, n_WR, WS(1), WR(1),            &
     &    dall_rtp, dlcl_pole, dall_pole)
!
!
      if (iflag_debug.gt.0)                                             &
     &        write(*,*) 'set_xyz_vect_from_sph_trans'
      call set_xyz_vect_from_sph_trans                                  &
     &   (sph%sph_rtp, mesh%node, sph%sph_params%m_folding,             &
     &    ncomp_sph_trans, dall_rtp(1,1), dall_pole(1,1), nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'set_sph_scalar_from_sph_trans'
      call set_sph_scalar_from_sph_trans                                &
     &   (sph%sph_rtp, mesh%node, sph%sph_params%m_folding,             &
     &    ncomp_sph_trans, dall_rtp(1,1), dall_pole(1,1), nod_fld)
!
      if (iflag_debug.gt.0)                                             &
     &      write(*,*) 'set_sph_tensor_from_sph_trans'
      call set_sph_tensor_from_sph_trans                                &
     &   (sph%sph_rtp, mesh%node, sph%sph_params%m_folding,             &
     &    ncomp_sph_trans, dall_rtp(1,1), dall_pole(1,1), nod_fld)
!
      end subroutine sph_b_trans_all_field
!
! -----------------------------------------------------------------------
!
      end module sph_transfer_all_field

