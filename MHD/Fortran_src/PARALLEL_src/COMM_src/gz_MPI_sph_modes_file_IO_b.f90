!>@file   gz_MPI_sph_modes_file_IO_b.f90
!!@brief  module gz_MPI_sph_modes_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_mpi_read_geom_rtp_file_b                          &
!!     &         (file_name, nprocs_in, id_rank)
!!      subroutine gz_mpi_read_spectr_rj_file_b                         &
!!     &         (file_name, nprocs_in, id_rank)
!!      subroutine gz_mpi_read_geom_rtm_file_b                          &
!!     &         (file_name, nprocs_in, id_rank)
!!      subroutine gz_mpi_read_modes_rlm_file_b                         &
!!     &         (file_name, nprocs_in, id_rank)
!!
!!      subroutine gz_mpi_write_geom_rtp_file_b                         &
!!     &         (file_name, nprocs_in, id_rank)
!!      subroutine gz_mpi_write_spectr_rj_file_b                        &
!!     &         (file_name, nprocs_in, id_rank)
!!      subroutine gz_mpi_write_geom_rtm_file_b                         &
!!     &         (file_name, nprocs_in, id_rank)
!!      subroutine gz_mpi_write_modes_rlm_file_b                        &
!!     &         (file_name, nprocs_in, id_rank)
!!@endverbatim
!!
!!@param nprocs_in  Number of subdomain
!!@param id_rank    Domain ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_MPI_sph_modes_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_node_id_spherical_IO
      use m_group_data_sph_specr_IO
      use gz_MPI_sph_modes_data_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtp_file_b                            &
     &         (file_name, nprocs_in, id_rank)
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
      call open_read_gz_mpi_file_b(file_name, id_file, ioff_gl)
!
      call gz_mpi_read_geom_rtp_data_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_spectr_rj_file_b                           &
     &         (file_name, nprocs_in, id_rank)
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
      call open_read_gz_mpi_file_b(file_name, id_file, ioff_gl)
!
      call gz_mpi_read_spectr_rj_data_b                                 &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtm_file_b                            &
     &         (file_name, nprocs_in, id_rank)
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
      call open_read_gz_mpi_file_b(file_name, id_file, ioff_gl)
!
      call gz_mpi_read_geom_rtm_data_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_modes_rlm_file_b                           &
     &         (file_name, nprocs_in, id_rank)
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
      call open_read_gz_mpi_file_b(file_name, id_file, ioff_gl)
!
      call gz_mpi_read_modes_rlm_data_b                                 &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtp_file_b                          &
     &         (file_name, nprocs_in, id_rank)
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
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call gz_mpi_write_geom_rtp_data_b(id_file, nprocs_in, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_spectr_rj_file_b                          &
     &         (file_name, nprocs_in, id_rank)
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
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call gz_mpi_write_spectr_rj_data_b(id_file, nprocs_in, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_write_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtm_file_b                           &
     &         (file_name, nprocs_in, id_rank)
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
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call gz_mpi_write_geom_rtm_data_b(id_file, nprocs_in, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_modes_rlm_file_b                          &
     &         (file_name, nprocs_in, id_rank)
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
      call open_write_gz_mpi_file_b                                     &
     &   (file_name, nprocs_in, id_file, ioff_gl)
!
      call gz_mpi_write_modes_rlm_data_b(id_file, nprocs_in, ioff_gl)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_sph_modes_file_IO_b
