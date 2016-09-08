!>@file   sph_file_MPI_IO_select.f90
!!@brief  module sph_file_MPI_IO_select
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Spectr data IO selector
!!
!!@verbatim
!!      subroutine sel_mpi_read_geom_rtp_file
!!      subroutine sel_mpi_read_spectr_modes_rj_file
!!      subroutine sel_mpi_read_geom_rtm_file
!!      subroutine sel_mpi_read_modes_rlm_file
!!
!!      subroutine sel_mpi_write_geom_rtp_file
!!      subroutine sel_mpi_write_spectr_modes_rj_file
!!      subroutine sel_mpi_write_geom_rtm_file
!!      subroutine sel_mpi_write_modes_rlm_file
!!@endverbatim
!!
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_file_MPI_IO_select
!
      use m_precision
!
      use calypso_mpi
      use m_file_format_switch
      use m_node_id_spherical_IO
      use set_parallel_file_name
      use set_mesh_file_names
!
      use sph_file_IO_select
!
      use MPI_sph_modes_file_IO
      use MPI_sph_modes_file_IO_b
      use gz_MPI_sph_modes_file_IO
      use gz_MPI_sph_modes_file_IO_b
!
      implicit none
!
      character(len=kchara), private :: sph_file_name
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_geom_rtp_file
!
!
      call set_sph_rtp_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_geom_rtp_file_b                                   &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call read_geom_rtp_file_mpi(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call read_geom_rtp_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_geom_rtp_file_b                                &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_geom_rtp_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_read_geom_rtp_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_read_geom_rtp_file(my_rank)
      end if
!
      end subroutine sel_mpi_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_spectr_modes_rj_file
!
!
      call set_sph_rj_file_name(sph_file_head, iflag_sph_file_fmt,      &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_spectr_rj_file_b                                  &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call read_spectr_rj_file_mpi(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call read_spectr_modes_rj_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_spectr_rj_file_b                               &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_spectr_rj_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_read_spectr_modes_rj_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_read_spectr_modes_rj_file(my_rank)
      end if
!
      end subroutine sel_mpi_read_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_geom_rtm_file
!
!
      call set_sph_rtm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_geom_rtm_file_b                                   &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call read_geom_rtm_file_mpi(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call read_geom_rtm_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_geom_rtm_file_b                                &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_geom_rtm_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_read_geom_rtm_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_read_geom_rtm_file(my_rank)
      end if
!
      end subroutine sel_mpi_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_modes_rlm_file
!
!
      call set_sph_rlm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_modes_rlm_file_b                                  &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call read_modes_rlm_file_mpi(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call read_modes_rlm_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_modes_rlm_file_b                               &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_modes_rlm_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_read_modes_rlm_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_read_modes_rlm_file(my_rank)
      end if
!
      end subroutine sel_mpi_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_geom_rtp_file
!
!
      call set_sph_rtp_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_geom_rtp_file_b                                  &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call write_geom_rtp_file_mpi(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call write_geom_rtp_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_geom_rtp_file_b                               &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_geom_rtp_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_write_geom_rtp_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_write_geom_rtp_file(my_rank)
      end if
!
      end subroutine sel_mpi_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_spectr_modes_rj_file
!
!
      call set_sph_rj_file_name(sph_file_head, iflag_sph_file_fmt,      &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_spectr_rj_file_b                                 &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call write_spectr_rj_file_mpi(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call write_spectr_modes_rj_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_spectr_rj_file_b                              &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_spectr_rj_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_write_spectr_modes_rj_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_write_spectr_modes_rj_file(my_rank)
      end if
!
      end subroutine sel_mpi_write_spectr_modes_rj_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_geom_rtm_file
!
!
      call set_sph_rtm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_geom_rtm_file_b(my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call write_geom_rtm_file_mpi(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call write_geom_rtm_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_geom_rtm_file_b                               &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_geom_rtm_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_write_geom_rtm_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_write_geom_rtm_file(my_rank)
      end if
!
      end subroutine sel_mpi_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_modes_rlm_file
!
!
      call set_sph_rlm_file_name(sph_file_head, iflag_sph_file_fmt,     &
     &    my_rank, sph_file_name)
!
      if(iflag_sph_file_fmt .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_modes_rlm_file_b                                 &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt .eq. iflag_single) then
        call mpi_write_modes_rlm_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_ascii_file_fmt,       &
     &      my_rank, sph_file_name)
        call write_modes_rlm_file(my_rank, sph_file_name)
!
#ifdef ZLIB_IO
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_modes_rlm_file_b                              &
     &     (my_rank, nprocs, sph_file_name)
      else if(iflag_sph_file_fmt                                        &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_modes_rlm_file(my_rank, sph_file_name)
        call set_mesh_file_name(sph_file_head, id_gzip_txt_file_fmt,    &
     &      my_rank, sph_file_name)
        call gz_write_modes_rlm_file(my_rank, sph_file_name)
#endif
!
      else
        call sel_write_modes_rlm_file(my_rank)
      end if
!
      end subroutine sel_mpi_write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module sph_file_MPI_IO_select
