!set_export_sph_center_mesh.f90
!      module set_export_sph_center_mesh
!
!     Written by H. Matsui on March, 2013
!
!      subroutine count_export_4_center_mesh(ip_r, ip_t, jp_r, jp_t,    &
!     &          num_export)
!      subroutine count_export_4_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          num_export)
!      subroutine count_export_4_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          num_export)
!      subroutine set_export_rtp_center_mesh(ip_r, ip_t, jp_r, jp_t,    &
!     &          icou, nod_comm)
!      subroutine set_export_rtp_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          icou, nod_comm)
!      subroutine set_export_rtp_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t, &
!     &          icou, nod_comm)
!
      module set_export_sph_center_mesh
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_export_4_center_mesh(ip_r, ip_t, jp_r, jp_t,     &
     &          num_export)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_export
!
!
      if      (stbl%iflag_center_r(ip_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_center_r(jp_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_Spole_t(ip_t) .eq.  ip_t                      &
     &  .and.  stbl%iflag_Spole_t(jp_t) .eq. 0) then
        num_export = num_export + 1
      end if
!
      end subroutine count_export_4_center_mesh
!
! -----------------------------------------------------------------------
!
      subroutine count_export_4_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          num_export)
!
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_export
!
      integer(kind = kint) :: l, lnum
!
!
      if      (stbl%iflag_center_r(ip_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_center_r(jp_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_Spole_t(jp_t) .eq.  jp_t) then
        do lnum = 1, stbl%nnod_sph_ct
          l = stbl%inod_sph_ct(lnum)
          if(stbl%iflag_internal_t(l,ip_t) .eq. ip_t) then
            num_export = num_export + stbl%nidx_global_fem(3)
          end if
        end do
      end if
!
      end subroutine count_export_4_ctr_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine count_export_4_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          num_export)
!
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: num_export
!
!
      if      (stbl%iflag_center_r(ip_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_center_r(jp_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_Spole_t(jp_t) .eq.  jp_t                      &
     &  .and.  stbl%iflag_Npole_t(jp_t) .ne.  jp_t                      &
     &  .and.  stbl%iflag_Npole_t(ip_t) .eq.  ip_t) then
        num_export = num_export + 1
      end if
!
      end subroutine count_export_4_ctr_Npole_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_center_mesh(ip_r, ip_t, jp_r, jp_t,     &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      if      (stbl%iflag_center_r(ip_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_center_r(jp_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_Spole_t(ip_t) .eq.  ip_t                      &
     &  .and.  stbl%iflag_Spole_t(jp_t) .eq. 0) then
        icou = icou + 1
        stbl%item_export_1d_rtp(1,icou) = 0
        stbl%item_export_1d_rtp(2,icou) = 0
        stbl%item_export_1d_rtp(3,icou) = 1
!
        nod_comm%item_export(icou)  = sph_center_node_id()
      end if
!
      end subroutine set_export_rtp_center_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_ctr_shell_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: l, m, lnum
      integer(kind = kint) :: ist, num_rl
!
!
      if      (stbl%iflag_center_r(ip_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_center_r(jp_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_Spole_t(jp_t) .eq.  jp_t) then
        ist = icou
        do lnum = 1, stbl%nnod_sph_ct
          l = stbl%inod_sph_ct(lnum)
          if(stbl%iflag_internal_t(l,ip_t) .eq. ip_t) then
!
            icou = icou + 1
            stbl%item_export_1d_rtp(1,icou)                             &
     &                           = stbl%irev_sph_r(ione,ip_r)
            stbl%item_export_1d_rtp(2,icou)                             &
     &                           = stbl%irev_sph_t(l,   ip_t)
            stbl%item_export_1d_rtp(3,icou) = 1
!
            nod_comm%item_export(icou)                                  &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &                             stbl%item_export_1d_rtp(1,icou),     &
     &                             stbl%item_export_1d_rtp(2,icou),     &
     &                             stbl%item_export_1d_rtp(3,icou))
          end if
        end do
        num_rl = icou - ist
!
        do m = 2, stbl%nidx_global_fem(3)
          do l = 1, num_rl
            icou = icou + 1
            stbl%item_export_1d_rtp(1,icou)                             &
     &                            = stbl%item_export_1d_rtp(1,l+ist)
            stbl%item_export_1d_rtp(2,icou)                             &
     &                            = stbl%item_export_1d_rtp(2,l+ist)
            stbl%item_export_1d_rtp(3,icou) = m
!
          nod_comm%item_export(icou)                                    &
     &         = sph_shell_node_id(ip_r, ip_t,                          &
     &                             stbl%item_export_1d_rtp(1,icou),     &
     &                             stbl%item_export_1d_rtp(2,icou),     &
     &                             stbl%item_export_1d_rtp(3,icou))
          end do
        end do
      end if
!
      end subroutine set_export_rtp_ctr_shell_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_export_rtp_ctr_Npole_mesh(ip_r, ip_t, jp_r, jp_t,  &
     &          icou, nod_comm)
!
      use t_comm_table
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, jp_r, jp_t
      integer(kind = kint), intent(inout) :: icou
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      if      (stbl%iflag_center_r(ip_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_center_r(jp_r) .eq. ip_r                      &
     &  .and.  stbl%iflag_Spole_t(jp_t) .eq.  jp_t                      &
     &  .and.  stbl%iflag_Npole_t(jp_t) .ne.  jp_t                      &
     &  .and.  stbl%iflag_Npole_t(ip_t) .eq.  ip_t) then
!
        icou = icou + 1
        stbl%item_export_1d_rtp(1,icou) = stbl%irev_sph_r(ione,ip_r)
        stbl%item_export_1d_rtp(2,icou) = stbl%nnod_sph_t(ip_t) + 1
        stbl%item_export_1d_rtp(3,icou) = 1
!
        nod_comm%item_export(icou)                                      &
     &            = sph_n_pole_node_id(stbl%item_export_1d_rtp(1,icou))
      end if
!
      end subroutine set_export_rtp_ctr_Npole_mesh
!
! -----------------------------------------------------------------------
!
      end module set_export_sph_center_mesh
