!>@file   copy_sph_MHD_4_send_recv.f90
!!@brief  module copy_sph_MHD_4_send_recv
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!      subroutine copy_tmp_scl_spec_from_trans                         &
!!     &         (ncomp_recv, ft_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: ft_trns
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module copy_sph_MHD_4_send_recv
!
      use m_precision
      use m_machine_parameter
      use copy_spectr_4_sph_trans
!
      use t_sph_trans_comm_tbl
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_spec_from_trans                           &
     &         (ncomp_recv, ft_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: ft_trns
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_grad_vx, ft_trns%i_grad_vx,                            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_grad_vy, ft_trns%i_grad_vy,                            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_grad_vz, ft_trns%i_grad_vz,                            &
     &    comm_rj, n_WR, WR, rj_fld)
!
      end  subroutine copy_tmp_scl_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_sph_MHD_4_send_recv
