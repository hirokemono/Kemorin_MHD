!>@file   copy_fields_from_sph_trans.f90
!!@brief  module copy_fields_from_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_scalar_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine copy_vector_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine copy_tensor_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine copy_scalar_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine copy_vector_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine copy_tensor_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!!
!!      subroutine zmean_scalar_from_snap_trans(i_trns, i_field,        &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zmean_vector_from_snap_trans(i_trns, i_field,        &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zmean_tensor_from_snap_trans(i_trns, i_field,        &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zmean_scalar_from_snap_force(i_trns, i_field,        &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zmean_vector_from_snap_force(i_trns, i_field,        &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zmean_tensor_from_snap_force(i_trns, i_field,        &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!
!!      subroutine zrms_scalar_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zrms_vector_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zrms_tensor_from_snap_trans(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zrms_scalar_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zrms_vector_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!      subroutine zrms_tensor_from_snap_force(i_trns, i_field,         &
!!     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_fields_from_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_spheric_rtp_data
      use t_addresses_sph_transform
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_trans(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_scl_from_trans_wpole                                &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld)
!
      end subroutine copy_scalar_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_trans(i_trns, i_field,           &
     &         m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld)
!
      end subroutine copy_vector_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_snap_trans(i_trns, i_field,           &
     &         m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_tsr_from_trans_wpole                                &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld)
!
      end subroutine copy_tensor_from_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_scl_from_trans_wpole                                &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld)
!
      end subroutine copy_scalar_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_vec_from_trans_wpole                                &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld)
!
      end subroutine copy_vector_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use copy_nodal_fld_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nod_tsr_from_trans_wpole                                &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld)
!
      end subroutine copy_tensor_from_snap_force
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine zmean_scalar_from_snap_trans(i_trns, i_field,          &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zmean_scl_from_trans                                    &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zmean_scalar_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine zmean_vector_from_snap_trans(i_trns, i_field,          &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zmean_vec_from_trans                                    &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zmean_vector_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine zmean_tensor_from_snap_trans(i_trns, i_field,          &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zmean_tsr_from_trans                                    &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zmean_tensor_from_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine zmean_scalar_from_snap_force(i_trns, i_field,          &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zmean_scl_from_trans                                    &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zmean_scalar_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine zmean_vector_from_snap_force(i_trns, i_field,          &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zmean_vec_from_trans                                    &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zmean_vector_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine zmean_tensor_from_snap_force(i_trns, i_field,          &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zmean_tsr_from_trans                                    &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zmean_tensor_from_snap_force
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine zrms_scalar_from_snap_trans(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zrms_scl_from_trans                                     &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zrms_scalar_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine zrms_vector_from_snap_trans(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zrms_vec_from_trans                                     &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zrms_vector_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine zrms_tensor_from_snap_trans(i_trns, i_field,          &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zrms_tsr_from_trans                                     &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rj_2_rtp,                 &
     &    i_trns, trns_snap%fld_rtp, trns_snap%fld_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zrms_tensor_from_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine zrms_scalar_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zrms_scl_from_trans                                     &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zrms_scalar_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine zrms_vector_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zrms_vec_from_trans                                     &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zrms_vector_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine zrms_tensor_from_snap_force(i_trns, i_field,           &
     &          m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      use cal_zonal_mean_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_zrms_tsr_from_trans                                     &
     &   (sph_rtp, m_folding, trns_snap%ncomp_rtp_2_rj,                 &
     &    i_trns, trns_snap%frc_rtp, trns_snap%frc_pole,                &
     &    i_field, node, nod_fld, trns_snap%fld_zm(1,1))
!
      end subroutine zrms_tensor_from_snap_force
!
!-----------------------------------------------------------------------
!
      end module copy_fields_from_sph_trans
