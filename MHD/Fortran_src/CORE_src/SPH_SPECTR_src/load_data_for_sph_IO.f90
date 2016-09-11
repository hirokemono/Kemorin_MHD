!>@file   load_data_for_sph_IO.f90
!!@brief  module load_data_for_sph_IO
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief load spherical harmonics indexing data
!!
!!@verbatim
!!      subroutine input_geom_rtp_sph_trans(sph_grps_IO,                &
!!     &          sph_rtp, comm_rtp, sph_grps, l_truncation, ierr)
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(sph_group_data), intent(inout) :: sph_grps
!!      subroutine input_modes_rj_sph_trans(sph_grps_IO,                &
!!     &          sph_rj, comm_rj, sph_grps, l_truncation, ierr)
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
!!        type(sph_rj_grid),  intent(inout) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!        type(sph_group_data), intent(inout) :: sph_grps
!!      subroutine input_geom_rtm_sph_trans                             &
!!     &         (l_truncation, sph_rtm, comm_rtm)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!      subroutine input_modes_rlm_sph_trans                            &
!!     &         (l_truncation, sph_rlm, comm_rlm)
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!
!!      subroutine output_geom_rtp_sph_trans                            &
!!     &         (my_rank, l_truncation, sph_rtp, comm_rtp, sph_grps,   &
!!     &          sph_grps_IO)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(sph_group_data), intent(inout) :: sph_grps
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
!!      subroutine output_modes_rj_sph_trans(my_rank, l_truncation,     &
!!     &          sph_rj, comm_rj, sph_grps, sph_grps_IO)
!!        type(sph_rj_grid),  intent(in) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!        type(sph_group_data), intent(inout) :: sph_grps
!!        type(sph_group_data), intent(inout) :: sph_grps_IO
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
!
      use m_precision
!
      use m_comm_data_IO
      use m_node_id_spherical_IO
!
      use t_spheric_mesh
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
      subroutine input_geom_rtp_sph_trans(sph_grps_IO,                  &
     &          sph_rtp, comm_rtp, sph_grps, l_truncation, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
      use count_num_sph_smp
!
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
      integer(kind = kint), intent(inout) :: l_truncation
      integer(kind = kint), intent(inout) :: ierr
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_group_data), intent(inout) :: sph_grps
!
!
      call copy_sph_node_4_rtp_from_IO(sph_IO1, sph_rtp, l_truncation)
      call copy_comm_sph_from_IO(sph_rtp%nnod_rtp, comm_IO, comm_rtp)
!
      call set_gruop_stracture                                          &
     &   (sph_grps_IO%bc_rtp_grp, sph_grps%bc_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps_IO%radial_rtp_grp, sph_grps%radial_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps_IO%theta_rtp_grp, sph_grps%theta_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps_IO%zonal_rtp_grp, sph_grps%zonal_rtp_grp)
!
      call deallocate_grp_type(sph_grps_IO%bc_rtp_grp)
      call deallocate_grp_type(sph_grps_IO%radial_rtp_grp)
      call deallocate_grp_type(sph_grps_IO%theta_rtp_grp)
      call deallocate_grp_type(sph_grps_IO%zonal_rtp_grp)
!
      call count_num_rtp_smp(sph_rtp, ierr)
!
      end subroutine input_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rj_sph_trans(sph_grps_IO,                  &
     &          sph_rj, comm_rj, sph_grps, l_truncation, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
      use count_num_sph_smp
!
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
      integer(kind = kint), intent(inout) :: l_truncation
      integer(kind = kint), intent(inout) :: ierr
      type(sph_rj_grid),  intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(sph_group_data), intent(inout) :: sph_grps
!
!
      call copy_sph_node_4_rj_from_IO(sph_IO1, sph_rj, l_truncation)
      call copy_comm_sph_from_IO(sph_rj%nnod_rj, comm_IO, comm_rj)
!
      call set_gruop_stracture                                          &
     &   (sph_grps_IO%radial_rj_grp, sph_grps%radial_rj_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps_IO%sphere_rj_grp, sph_grps%sphere_rj_grp)
!
      call deallocate_grp_type(sph_grps_IO%radial_rj_grp)
      call deallocate_grp_type(sph_grps_IO%sphere_rj_grp)
!
      call count_num_rj_smp(sph_rj, ierr)
!
      end subroutine input_modes_rj_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_geom_rtm_sph_trans                               &
     &         (l_truncation, sph_rtm, comm_rtm, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use count_num_sph_smp
!
      integer(kind = kint), intent(inout) :: l_truncation
      integer(kind = kint), intent(inout) :: ierr
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
!
      call copy_sph_node_4_rtm_from_IO(sph_IO1, sph_rtm, l_truncation)
      call copy_comm_sph_from_IO(sph_rtm%nnod_rtm, comm_IO, comm_rtm)
!
      call count_num_rtm_smp(sph_rtm, ierr)
!
      end subroutine input_geom_rtm_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine input_modes_rlm_sph_trans                              &
     &         (l_truncation, sph_rlm, comm_rlm, ierr)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use count_num_sph_smp
!
      integer(kind = kint), intent(inout) :: l_truncation
      integer(kind = kint), intent(inout) :: ierr
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
!
      call copy_sph_node_4_rlm_from_IO(sph_IO1, sph_rlm, l_truncation)
      call copy_comm_sph_from_IO(sph_rlm%nnod_rlm, comm_IO, comm_rlm)
!
      call count_num_rlm_smp(sph_rlm, ierr)
!
      end subroutine input_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_geom_rtp_sph_trans                              &
     &         (my_rank, l_truncation, sph_rtp, comm_rtp, sph_grps,     &
     &          sph_grps_IO)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_group_data), intent(inout) :: sph_grps
!
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
!
      my_rank_IO = my_rank
      call copy_sph_node_4_rtp_to_IO(l_truncation, sph_rtp, sph_IO1)
      call copy_comm_sph_to_comm_tbl(comm_rtp, comm_IO)
!
      call set_gruop_stracture                                          &
     &   (sph_grps%bc_rtp_grp, sph_grps_IO%bc_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%radial_rtp_grp, sph_grps_IO%radial_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%theta_rtp_grp, sph_grps_IO%theta_rtp_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%zonal_rtp_grp, sph_grps_IO%zonal_rtp_grp)
!
      call deallocate_grp_type(sph_grps%bc_rtp_grp)
      call deallocate_grp_type(sph_grps%radial_rtp_grp)
      call deallocate_grp_type(sph_grps%theta_rtp_grp)
      call deallocate_grp_type(sph_grps%zonal_rtp_grp)
!
      end subroutine output_geom_rtp_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine output_modes_rj_sph_trans(my_rank, l_truncation,       &
     &          sph_rj, comm_rj, sph_grps, sph_grps_IO)
!
      use copy_sph_comm_table_4_IO
      use copy_sph_node_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid),  intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(sph_group_data), intent(inout) :: sph_grps
!
      type(sph_group_data), intent(inout) :: sph_grps_IO
!
!
      my_rank_IO = my_rank
      call copy_sph_node_4_rj_to_IO(l_truncation, sph_rj, sph_IO1)
      call copy_comm_sph_to_comm_tbl(comm_rj, comm_IO)
!
      call set_gruop_stracture                                          &
     &   (sph_grps%radial_rj_grp, sph_grps_IO%radial_rj_grp)
      call set_gruop_stracture                                          &
     &   (sph_grps%sphere_rj_grp, sph_grps_IO%sphere_rj_grp)
!
      call deallocate_grp_type(sph_grps%radial_rj_grp)
      call deallocate_grp_type(sph_grps%sphere_rj_grp)
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
      my_rank_IO = my_rank
      call copy_sph_node_4_rtm_to_IO(l_truncation, sph_rtm, sph_IO1)
      call copy_comm_sph_to_comm_tbl(comm_rtm, comm_IO)
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
      my_rank_IO = my_rank
      call copy_sph_node_4_rlm_to_IO(l_truncation, sph_rlm, sph_IO1)
      call copy_comm_sph_to_comm_tbl(comm_rlm, comm_IO)
!
      end subroutine output_modes_rlm_sph_trans
!
! -----------------------------------------------------------------------
!
      end module load_data_for_sph_IO
