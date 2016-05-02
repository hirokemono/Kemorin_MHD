!>@file   load_data_for_sph_IO.f90
!!@brief  module load_data_for_sph_IO
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief load spherical harmonics indexing data
!!
!!@verbatim
!!      subroutine input_geom_rtp_sph_trans                             &
!!     &         (my_rank, l_truncation, sph_rtp)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!      subroutine input_modes_rj_sph_trans                             &
!!     &         (my_rank, l_truncation, sph_rj)
!!        type(sph_rj_grid),  intent(inout) :: sph_rj
!!      subroutine input_geom_rtm_sph_trans                             &
!!     &         (my_rank, l_truncation, sph_rtm)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!      subroutine input_modes_rlm_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rlm)
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!
!!      subroutine output_geom_rtp_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine output_modes_rj_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rj)
!!        type(sph_rj_grid),  intent(in) :: sph_rj
!!      subroutine output_geom_rtm_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rtm)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!      subroutine output_modes_rlm_sph_trans                           &
!!     &         (my_rank, l_truncation, sph_rlm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!@endverbatim
!
      module load_data_for_sph_IO
!f
      use m_precision
!
      use m_node_id_spherical_IO
      use m_group_data_sph_specr_IO
      use sph_file_IO_select
!
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtp_sph_trans                               &
     &         (my_rank, l_truncation, sph_rtp)
!
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use copy_sph_groups_from_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
!
      call sel_read_geom_rtp_file(my_rank)
!
      call copy_sph_node_4_rtp_from_IO(l_truncation, sph_rtp)
      call copy_comm_rtp_from_IO(sph_rtp%nnod_rtp, comm_rtp1)
!
      call copy_rj_group_data(bc_rtp_grp_IO, bc_rtp_grp1)
      call copy_rj_group_data(radial_rtp_grp_IO, radial_rtp_grp1)
      call copy_rj_group_data(theta_rtp_grp_IO, theta_rtp_grp1)
      call copy_rj_group_data(zonal_rtp_grp_IO, zonal_rtp_grp)
!
      end subroutine input_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rj_sph_trans                               &
     &         (my_rank, l_truncation, sph_rj)
!
      use m_group_data_sph_specr
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use copy_sph_groups_from_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rj_grid),  intent(inout) :: sph_rj
!
!
      call sel_read_spectr_modes_rj_file(my_rank)
!
      call copy_sph_node_4_rj_from_IO(l_truncation, sph_rj)
      call copy_comm_rj_from_IO(sph_rj%nnod_rj)
!
      call copy_rj_group_data(radial_rj_grp_IO, radial_rj_grp1)
      call copy_rj_group_data(sphere_rj_grp_IO, sphere_rj_grp1)
!
      end subroutine input_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtm_sph_trans                               &
     &         (my_rank, l_truncation, sph_rtm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      call sel_read_geom_rtm_file(my_rank)
!
      call copy_sph_node_4_rtm_from_IO(l_truncation, sph_rtm)
      call copy_comm_rtm_from_IO(sph_rtm%nnod_rtm)
!
      end subroutine input_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rlm_sph_trans                              &
     &         (my_rank, l_truncation, sph_rlm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      call sel_read_modes_rlm_file(my_rank)
!
      call copy_sph_node_4_rlm_from_IO(l_truncation, sph_rlm )
      call copy_comm_rlm_from_IO(sph_rlm%nnod_rlm)
!
      end subroutine input_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtp_sph_trans                              &
     &         (my_rank, l_truncation, sph_rtp)
!
      use m_group_data_sph_specr
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use copy_sph_groups_from_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
!
      call copy_sph_node_4_rtp_to_IO(l_truncation, sph_rtp)
      call copy_comm_rtp_to_IO(my_rank)
!
      call copy_rj_group_data(bc_rtp_grp1, bc_rtp_grp_IO)
      call copy_rj_group_data(radial_rtp_grp1, radial_rtp_grp_IO)
      call copy_rj_group_data(theta_rtp_grp1, theta_rtp_grp_IO)
      call copy_rj_group_data(zonal_rtp_grp, zonal_rtp_grp_IO)
!
      call sel_write_geom_rtp_file(my_rank)
!
      end subroutine output_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rj_sph_trans                              &
     &         (my_rank, l_truncation, sph_rj)
!
      use m_group_data_sph_specr
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use copy_sph_groups_from_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid),  intent(inout) :: sph_rj
!
!
      call copy_sph_node_4_rj_to_IO(l_truncation, sph_rj)
      call copy_comm_rj_to_IO(my_rank)
!
      call copy_rj_group_data(radial_rj_grp1, radial_rj_grp_IO)
      call copy_rj_group_data(sphere_rj_grp1, sphere_rj_grp_IO)
!
      call sel_write_spectr_modes_rj_file(my_rank)
!
      end subroutine output_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtm_sph_trans                              &
     &         (my_rank, l_truncation, sph_rtm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      call copy_sph_node_4_rtm_to_IO(l_truncation, sph_rtm)
      call copy_comm_rtm_to_IO(my_rank)
!
      call sel_write_geom_rtm_file(my_rank)
!
      end subroutine output_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rlm_sph_trans                             &
     &         (my_rank, l_truncation, sph_rlm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      call copy_sph_node_4_rlm_to_IO(l_truncation, sph_rlm)
      call copy_comm_rlm_to_IO(my_rank)
!
      call sel_write_modes_rlm_file(my_rank)
!
      end subroutine output_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
!
      end module load_data_for_sph_IO
