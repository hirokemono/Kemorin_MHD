!>@file   copy_temporal_4_sph_trans.f90
!!@brief  module copy_temporal_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer
!!        for temporally step
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine copy_tmp_vec_spec_to_trans(ncomp_send, n_WS, WS)
!!
!!      subroutine copy_tmp_vec_fld_from_trans
!!
!!  routines for forward transform
!!      subroutine copy_tmp_scl_fld_to_trans
!!      subroutine copy_tmp_scl_spec_from_trans(ncomp_recv, n_WR, WR)
!!@endverbatim
!
      module copy_temporal_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use m_sph_phys_address
      use m_addresses_trans_sph_tmp
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_vec_spec_to_trans(ncomp_send, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &      ipol%i_grad_vx, btmp_trns%i_grad_vx, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &      ipol%i_grad_vy, btmp_trns%i_grad_vy, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &      ipol%i_grad_vz, btmp_trns%i_grad_vz, n_WS, WS)
!
      end subroutine copy_tmp_vec_spec_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_vec_fld_from_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_vec_fld_from_trans(irtp%i_grad_vx, btmp_trns%i_grad_vx)
      call copy_vec_fld_from_trans(irtp%i_grad_vy, btmp_trns%i_grad_vy)
      call copy_vec_fld_from_trans(irtp%i_grad_vz, btmp_trns%i_grad_vz)
!$omp end parallel
!
      end subroutine copy_tmp_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_fld_to_trans
!
      use copy_sph_field_4_sph_trans
!
!
!$omp parallel
      call copy_scalar_fld_to_trans                                     &
     &    (irtp%i_grad_vx, ftmp_trns%i_grad_vx)
      call copy_scalar_fld_to_trans                                     &
     &    (irtp%i_grad_vy, ftmp_trns%i_grad_vy)
      call copy_scalar_fld_to_trans                                     &
     &    (irtp%i_grad_vz, ftmp_trns%i_grad_vz)
!$omp end parallel
!
      end  subroutine copy_tmp_scl_fld_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_spec_from_trans(ncomp_recv, n_WR, WR)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_grad_vx, ftmp_trns%i_grad_vx, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_grad_vy, ftmp_trns%i_grad_vy, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_grad_vz, ftmp_trns%i_grad_vz, n_WR, WR)
!
      end  subroutine copy_tmp_scl_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_temporal_4_sph_trans
