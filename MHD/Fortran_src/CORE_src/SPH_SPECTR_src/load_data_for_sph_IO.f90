!>@file   load_data_for_sph_IO.f90
!!@brief  module load_data_for_sph_IO
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief load spherical harmonics indexing data
!!
!!@verbatim
!!      subroutine input_geom_rtp_sph_trans                             &
!!     &         (my_rank, l_truncation, sph_rtp, comm_rtp, bc_rtp_grp, &
!!     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(group_data), intent(inout) :: bc_rtp_grp
!!        type(group_data), intent(inout) :: radial_rtp_grp
!!        type(group_data), intent(inout) :: theta_rtp_grp
!!        type(group_data), intent(inout) :: zonal_rtp_grp
!!      subroutine input_modes_rj_sph_trans(my_rank, l_truncation,      &
!!     &          sph_rj, comm_rj, radial_rj_grp, sphere_rj_grp)
!!        type(sph_rj_grid),  intent(inout) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!        type(group_data), intent(inout) :: radial_rj_grp
!!        type(group_data), intent(inout) :: sphere_rj_grp
!!      subroutine input_geom_rtm_sph_trans                             &
!!     &         (my_rank, l_truncation, sph_rtm, comm_rtm)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!      subroutine input_modes_rlm_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rlm, comm_rlm)
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!
!!      subroutine output_geom_rtp_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rtp, comm_rtp, bc_rtp_grp, &
!!     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(group_data), intent(inout) :: bc_rtp_grp
!!        type(group_data), intent(inout) :: radial_rtp_grp
!!        type(group_data), intent(inout) :: theta_rtp_grp
!!        type(group_data), intent(inout) :: zonal_rtp_grp
!!      subroutine output_modes_rj_sph_trans(my_rank, l_truncation,     &
!!     &          sph_rj, comm_rj, radial_rj_grp, sphere_rj_grp)
!!        type(sph_rj_grid),  intent(in) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!        type(group_data), intent(inout) :: radial_rj_grp
!!        type(group_data), intent(inout) :: sphere_rj_grp
!!      subroutine output_geom_rtm_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rtm, comm_rtm)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!      subroutine output_modes_rlm_sph_trans                           &
!!     &         (my_rank, l_truncation, sph_rlm, comm_rlm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
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
      use t_sph_trans_comm_tbl
      use t_group_data
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
     &         (my_rank, l_truncation, sph_rtp, comm_rtp, bc_rtp_grp,   &
     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp
!
      type(group_data), intent(inout) :: bc_rtp_grp
      type(group_data), intent(inout) :: radial_rtp_grp
      type(group_data), intent(inout) :: theta_rtp_grp
      type(group_data), intent(inout) :: zonal_rtp_grp
!
!
      call sel_read_geom_rtp_file(my_rank)
!
      call copy_sph_node_4_rtp_from_IO(l_truncation, sph_rtp)
      call copy_comm_sph_from_IO(sph_rtp%nnod_rtp, comm_rtp)
!
      call set_gruop_stracture(bc_rtp_grp_IO, bc_rtp_grp)
      call set_gruop_stracture(radial_rtp_grp_IO, radial_rtp_grp)
      call set_gruop_stracture(theta_rtp_grp_IO, theta_rtp_grp)
      call set_gruop_stracture(zonal_rtp_grp_IO, zonal_rtp_grp)
!
      call deallocate_grp_type(bc_rtp_grp_IO)
      call deallocate_grp_type(radial_rtp_grp_IO)
      call deallocate_grp_type(theta_rtp_grp_IO)
      call deallocate_grp_type(zonal_rtp_grp_IO)
!
      end subroutine input_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rj_sph_trans(my_rank, l_truncation,        &
     &          sph_rj, comm_rj, radial_rj_grp, sphere_rj_grp)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rj_grid),  intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
!
      call sel_read_spectr_modes_rj_file(my_rank)
!
      call copy_sph_node_4_rj_from_IO(l_truncation, sph_rj)
      call copy_comm_sph_from_IO(sph_rj%nnod_rj, comm_rj)
!
      call set_gruop_stracture(radial_rj_grp_IO, radial_rj_grp)
      call set_gruop_stracture(sphere_rj_grp_IO, sphere_rj_grp)
!
      call deallocate_grp_type(radial_rj_grp_IO)
      call deallocate_grp_type(sphere_rj_grp_IO)
!
      end subroutine input_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtm_sph_trans                               &
     &         (my_rank, l_truncation, sph_rtm, comm_rtm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
!
      call sel_read_geom_rtm_file(my_rank)
!
      call copy_sph_node_4_rtm_from_IO(l_truncation, sph_rtm)
      call copy_comm_sph_from_IO(sph_rtm%nnod_rtm, comm_rtm)
!
      end subroutine input_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rlm_sph_trans                              &
     &         (my_rank, l_truncation, sph_rlm, comm_rlm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
!
      call sel_read_modes_rlm_file(my_rank)
!
      call copy_sph_node_4_rlm_from_IO(l_truncation, sph_rlm)
      call copy_comm_sph_from_IO(sph_rlm%nnod_rlm, comm_rlm)
!
      end subroutine input_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtp_sph_trans                              &
     &         (my_rank, l_truncation, sph_rtp, comm_rtp, bc_rtp_grp,   &
     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp
!
      type(group_data), intent(inout) :: bc_rtp_grp
      type(group_data), intent(inout) :: radial_rtp_grp
      type(group_data), intent(inout) :: theta_rtp_grp
      type(group_data), intent(inout) :: zonal_rtp_grp
!
!
      call copy_sph_node_4_rtp_to_IO(l_truncation, sph_rtp)
      call copy_comm_sph_to_IO(my_rank, comm_rtp)
!
      call set_gruop_stracture(bc_rtp_grp, bc_rtp_grp_IO)
      call set_gruop_stracture(radial_rtp_grp, radial_rtp_grp_IO)
      call set_gruop_stracture(theta_rtp_grp, theta_rtp_grp_IO)
      call set_gruop_stracture(zonal_rtp_grp, zonal_rtp_grp_IO)
!
      call deallocate_grp_type(bc_rtp_grp)
      call deallocate_grp_type(radial_rtp_grp)
      call deallocate_grp_type(theta_rtp_grp)
      call deallocate_grp_type(zonal_rtp_grp)
!
      call sel_write_geom_rtp_file(my_rank)
!
      end subroutine output_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rj_sph_trans(my_rank, l_truncation,       &
     &          sph_rj, comm_rj, radial_rj_grp, sphere_rj_grp)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid),  intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
!
      call copy_sph_node_4_rj_to_IO(l_truncation, sph_rj)
      call copy_comm_sph_to_IO(my_rank, comm_rj)
!
      call set_gruop_stracture(radial_rj_grp, radial_rj_grp_IO)
      call set_gruop_stracture(sphere_rj_grp, sphere_rj_grp_IO)
!
      call deallocate_grp_type(radial_rj_grp)
      call deallocate_grp_type(sphere_rj_grp)
!
      call sel_write_spectr_modes_rj_file(my_rank)
!
      end subroutine output_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtm_sph_trans                              &
     &         (my_rank, l_truncation, sph_rtm, comm_rtm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
!
      call copy_sph_node_4_rtm_to_IO(l_truncation, sph_rtm)
      call copy_comm_sph_to_IO(my_rank, comm_rtm)
!
      call sel_write_geom_rtm_file(my_rank)
!
      end subroutine output_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rlm_sph_trans                             &
     &         (my_rank, l_truncation, sph_rlm, comm_rlm)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
!
      call copy_sph_node_4_rlm_to_IO(l_truncation, sph_rlm)
      call copy_comm_sph_to_IO(my_rank, comm_rlm)
!
      call sel_write_modes_rlm_file(my_rank)
!
      end subroutine output_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
!
      end module load_data_for_sph_IO
