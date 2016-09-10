!>@file   MPI_sph_modes_data_IO_b.f90
!!@brief  module MPI_sph_modes_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine mpi_read_geom_rtp_data_b                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_spectr_rj_data_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_geom_rtm_data_b                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_read_modes_rlm_data_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!
!!      subroutine mpi_write_geom_rtp_data_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_write_spectr_rj_data_b                           &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_write_geom_rtm_data_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine mpi_write_modes_rlm_data_b                           &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!@endverbatim
!!
!!@param nprocs_in  Number of subdomain
!!@param id_rank    Domain ID
!
      module MPI_sph_modes_data_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_comm_data_IO
      use m_node_id_spherical_IO
      use m_group_data_sph_specr_IO
      use MPI_domain_data_IO_b
      use MPI_spherical_model_IO_b
      use MPI_sph_gl_1d_idx_IO_b
      use MPI_binary_head_IO
      use MPI_groups_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geom_rtp_data_b                               &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      sph_IO1%numdir_sph =  3
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_read_gl_reso_sph_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rank_4_sph_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rtp_gl_1d_table_b                                   &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_read_gl_nodes_sph_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_import_data_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, bc_rtp_grp_IO)
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, radial_rtp_grp_IO)
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, theta_rtp_grp_IO)
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, zonal_rtp_grp_IO)
!
      end subroutine mpi_read_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_spectr_rj_data_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      sph_IO1%numdir_sph =  2
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_read_gl_reso_sph_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rank_4_sph_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rj_gl_1d_table_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_read_gl_nodes_sph_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_import_data_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, radial_rj_grp_IO)
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, sphere_rj_grp_IO)
!
      end subroutine mpi_read_spectr_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geom_rtm_data_b                               &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      sph_IO1%numdir_sph =  3
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_read_gl_reso_sph_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rank_4_sph_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rtp_gl_1d_table_b                                   &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_gl_nodes_sph_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_read_import_data_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      end subroutine mpi_read_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_modes_rlm_data_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      sph_IO1%numdir_sph =  2
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_read_gl_reso_sph_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rank_4_sph_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_rj_gl_1d_table_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_read_gl_nodes_sph_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_import_data_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      end subroutine mpi_read_modes_rlm_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_geom_rtp_data_b                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call mpi_write_domain_info_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_write_rtp_gl_1d_table_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_write_gl_nodes_sph_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_write_import_data_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO) 
!
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, bc_rtp_grp_IO)
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, radial_rtp_grp_IO)
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, theta_rtp_grp_IO)
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, zonal_rtp_grp_IO)
!
      end subroutine mpi_write_geom_rtp_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_spectr_rj_data_b                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call mpi_write_domain_info_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_write_rj_gl_1d_table_b                                   &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_write_gl_nodes_sph_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_write_import_data_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, radial_rj_grp_IO)
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, sphere_rj_grp_IO)
!
      end subroutine mpi_write_spectr_rj_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geom_rtm_data_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call mpi_write_domain_info_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_write_rtp_gl_1d_table_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_write_gl_nodes_sph_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_write_import_data_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      end subroutine mpi_write_geom_rtm_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_modes_rlm_data_b                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call mpi_write_domain_info_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_write_rj_gl_1d_table_b                                   &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_write_gl_nodes_sph_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_write_import_data_b                                      &
     &   (id_file, nprocs_in, id_rank, ioff_gl, comm_IO)
!
      end subroutine mpi_write_modes_rlm_data_b
!
!------------------------------------------------------------------
!
      end module MPI_sph_modes_data_IO_b
