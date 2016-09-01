!>@file   gz_MPI_sph_modes_file_IO_b.f90
!!@brief  module gz_MPI_sph_modes_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_mpi_read_geom_rtp_file_b(my_rank, file_name)
!!      subroutine gz_mpi_read_spectr_rj_file_b(my_rank, file_name)
!!      subroutine gz_mpi_read_geom_rtm_file_b(my_rank, file_name)
!!      subroutine gz_mpi_read_modes_rlm_file_b(my_rank, file_name)
!!
!!      subroutine gz_mpi_write_geom_rtp_file_b(my_rank, file_name)
!!      subroutine gz_mpi_write_spectr_rj_file_b(my_rank, file_name)
!!      subroutine gz_mpi_write_geom_rtm_file_b(my_rank, file_name)
!!      subroutine gz_mpi_write_modes_rlm_file_b(my_rank, file_name)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_MPI_sph_modes_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_node_id_spherical_IO
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
      subroutine gz_mpi_read_geom_rtp_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_geom_rtp_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine gz_mpi_read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_spectr_rj_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_spectr_modes_rj_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine gz_mpi_read_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_geom_rtm_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine gz_mpi_read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_spectr_modes_rlm_data(mesh_file_id)
!
      close(mesh_file_id)
!
      end subroutine gz_mpi_read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtp_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_geom_rtp_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine gz_mpi_write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_spectr_rj_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_spectr_modes_rj_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine gz_mpi_write_spectr_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_geom_rtm_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine gz_mpi_write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call write_modes_rlm_data(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine gz_mpi_write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_sph_modes_file_IO_b
