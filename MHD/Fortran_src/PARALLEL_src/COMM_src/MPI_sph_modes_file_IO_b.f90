!>@file   MPI_sph_modes_file_IO_b.f90
!!@brief  module MPI_sph_modes_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine mpi_read_geom_rtp_file_b                             &
!!     &         (id_rank, nprocs_in, file_name)
!!      subroutine mpi_read_spectr_rj_file_b                            &
!!     &         (id_rank, nprocs_in, file_name)
!!      subroutine mpi_read_geom_rtm_file_b                             &
!!     &         (id_rank, nprocs_in, file_name)
!!      subroutine mpi_read_modes_rlm_file_b                            &
!!     &         (id_rank, nprocs_in, file_name)
!!
!!      subroutine mpi_write_geom_rtp_file_b                            &
!!     &         (id_rank, nprocs_in, file_name)
!!      subroutine mpi_write_spectr_rj_file_b                           &
!!     &         (id_rank, nprocs_in, file_name)
!!      subroutine mpi_write_geom_rtm_file_b                            &
!!     &         (id_rank, nprocs_in, file_name)
!!      subroutine mpi_write_modes_rlm_file_b                           &
!!     &         (id_rank, nprocs_in, file_name)
!!@endverbatim
!!
!!@param nprocs_in  Number of subdomain
!!@param id_rank    Domain ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module MPI_sph_modes_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_node_id_spherical_IO
      use MPI_domain_data_IO_b
      use MPI_spherical_model_IO_b
      use MPI_sph_gl_1d_idx_IO_b
      use MPI_binary_head_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geom_rtp_file_b                               &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped merged binary grid file: ', trim(file_name)
      call open_read_mpi_file_b(file_name, id_file, ioff_gl)
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nprocs_read)
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
     &   (id_file, nprocs_in, id_rank, ioff_gl)
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
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_spectr_rj_file_b                              &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped merged binary spectr modes file: ',           &
     &       trim(file_name)
      call open_read_mpi_file_b(file_name, id_file, ioff_gl)
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nprocs_read)
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
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, radial_rj_grp_IO)
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, sphere_rj_grp_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geom_rtm_file_b                               &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped merged binary grid file: ', trim(file_name)
      call open_read_mpi_file_b(file_name, id_file, ioff_gl)
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nprocs_read)
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
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_modes_rlm_file_b                              &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read merged gzipped binary spectr modes file: ',           &
     &       trim(file_name)
      call open_read_mpi_file_b(file_name, id_file, ioff_gl)
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nprocs_read)
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
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_geom_rtp_file_b                             &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write merged gzipped binary grid file: ', trim(file_name)
      call open_write_mpi_file_b                                        &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call mpi_write_domain_info_b(id_file, ioff_gl, nprocs_in)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, ioff_gl)
      call mpi_write_rtp_gl_1d_table_b(id_file, ioff_gl)
!
      call mpi_write_gl_nodes_sph_b(id_file, ioff_gl)
      call mpi_write_import_data_b(id_file, ioff_gl)
!
      call mpi_write_grp_data_b(id_file, ioff_gl, bc_rtp_grp_IO)
      call mpi_write_grp_data_b(id_file, ioff_gl, radial_rtp_grp_IO)
      call mpi_write_grp_data_b(id_file, ioff_gl, theta_rtp_grp_IO)
      call mpi_write_grp_data_b(id_file, ioff_gl, zonal_rtp_grp_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_spectr_rj_file_b                             &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'gzipped merged binary spectr modes file: ',                &
     &       trim(file_name)
      call open_write_mpi_file_b                                        &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call mpi_write_domain_info_b(id_file, ioff_gl, nprocs_in)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, ioff_gl)
!
      call mpi_write_rj_gl_1d_table_b(id_file, ioff_gl)
      call mpi_write_gl_nodes_sph_b(id_file, ioff_gl)
!
      call mpi_write_import_data_b(id_file, ioff_gl)
!
      call mpi_write_grp_data_b(id_file, ioff_gl, radial_rj_grp_IO)
      call mpi_write_grp_data_b(id_file, ioff_gl, sphere_rj_grp_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_write_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geom_rtm_file_b                              &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped merged binary grid file: ', trim(file_name)
      call open_write_mpi_file_b                                        &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call mpi_write_domain_info_b(id_file, ioff_gl, nprocs_in)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, ioff_gl)
      call mpi_write_rtp_gl_1d_table_b(id_file, ioff_gl)
      call mpi_write_gl_nodes_sph_b(id_file, ioff_gl)
!
      call mpi_write_import_data_b(id_file, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_modes_rlm_file_b                             &
     &         (id_rank, nprocs_in, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write gzipped merged binary spectr modes file: ',           &
     &      trim(file_name)
      call open_write_mpi_file_b                                        &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call mpi_write_domain_info_b(id_file, ioff_gl, nprocs_in)
      call mpi_write_gl_reso_sph_b(id_file, ioff_gl)
      call mpi_write_rank_4_sph_b(id_file, ioff_gl)
      call mpi_write_rj_gl_1d_table_b(id_file, ioff_gl)
      call mpi_write_gl_nodes_sph_b(id_file, ioff_gl)
!
      call mpi_write_import_data_b(id_file, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module MPI_sph_modes_file_IO_b
