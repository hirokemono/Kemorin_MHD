!>@file   MPI_sph_modes_file_IO.f90
!!@brief  module MPI_sph_modes_file_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief merged ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine mpi_read_geom_rtp_file(my_rank, file_name)
!!      subroutine mpi_read_spectr_rj_file(my_rank, file_name)
!!      subroutine mpi_read_geom_rtm_file(my_rank, file_name)
!!      subroutine mpi_read_modes_rlm_file(my_rank, file_name)
!!
!!      subroutine mpi_write_geom_rtp_file(my_rank, file_name)
!!      subroutine mpi_write_spectr_rj_file(my_rank, file_name)
!!      subroutine mpi_write_geom_rtm_file(my_rank, file_name)
!!      subroutine mpi_write_modes_rlm_file(my_rank, file_name)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module MPI_sph_modes_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_comm_data_IO
      use m_node_id_spherical_IO
      use m_group_data_sph_specr_IO
      use sph_modes_grids_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geom_rtp_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read merged ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_geom_rtp_data                                           &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1, sph_grp_IO)
      close(mesh_file_id)
!
      end subroutine mpi_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine mpi_read_spectr_rj_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read merged ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_spectr_modes_rj_data                                    &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1, sph_grp_IO)
      close(mesh_file_id)
!
      end subroutine mpi_read_spectr_rj_file
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geom_rtm_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read merged ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_geom_rtm_data                                           &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1)
      close(mesh_file_id)
!
      end subroutine mpi_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine mpi_read_modes_rlm_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read merged ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_spectr_modes_rlm_data                                   &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1)
!
      close(mesh_file_id)
!
      end subroutine mpi_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_geom_rtp_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write merged ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_geom_rtp_data                                          &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1, sph_grp_IO)
      close(mesh_file_id)
!
      end subroutine mpi_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine mpi_write_spectr_rj_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write merged ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_spectr_modes_rj_data                                   &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1, sph_grp_IO)
      close(mesh_file_id)
!
      end subroutine mpi_write_spectr_rj_file
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geom_rtm_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write merged ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_geom_rtm_data                                          &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1)
      close(mesh_file_id)
!
      end subroutine mpi_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine mpi_write_modes_rlm_file(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write merged ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_modes_rlm_data                                         &
     &   (mesh_file_id, my_rank_IO, comm_IO, sph_IO1)
      close(mesh_file_id)
!
      end subroutine mpi_write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module MPI_sph_modes_file_IO
