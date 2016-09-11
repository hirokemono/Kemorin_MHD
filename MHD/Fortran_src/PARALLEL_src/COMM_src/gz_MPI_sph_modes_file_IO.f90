!>@file   gz_MPI_sph_modes_file_IO.f90
!!@brief  module gz_MPI_sph_modes_file_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief gzipped merged ASCII spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_mpi_read_geom_rtp_file                            &
!!     &         (file_name, my_rank, sph_file)
!!      subroutine gz_mpi_read_spectr_rj_file                           &
!!     &         (file_name, my_rank, sph_file)
!!      subroutine gz_mpi_read_geom_rtm_file                            &
!!     &         (file_name, my_rank, sph_file)
!!      subroutine gz_mpi_read_modes_rlm_file                           &
!!     &         (file_name, my_rank, sph_file)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine gz_mpi_write_geom_rtp_file                           &
!!     &         (file_name, my_rank, sph_file)
!!      subroutine gz_mpi_write_spectr_rj_file                          &
!!     &         (file_name, my_rank, sph_file)
!!      subroutine gz_mpi_write_geom_rtm_file                           &
!!     &         (file_name, my_rank, sph_file)
!!      subroutine gz_mpi_write_modes_rlm_file                          &
!!     &         (file_name, my_rank, sph_file)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_MPI_sph_modes_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_mesh
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
      subroutine gz_mpi_read_geom_rtp_file                              &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped merged ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_geom_rtp_data                                           &
     &   (id_file, sph_file%my_rank_IO,                                 &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      close(id_file)
!
      end subroutine gz_mpi_read_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_spectr_rj_file                             &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped merged ascii spectr modes file: ',             &
     &      trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_spectr_modes_rj_data                                    &
     &    (id_file, sph_file%my_rank_IO,                                &
     &     sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      close(id_file)
!
      end subroutine gz_mpi_read_spectr_rj_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geom_rtm_file                              &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped merged ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_geom_rtm_data(id_file,                                  &
     &   sph_file%my_rank_IO, sph_file%comm_IO, sph_file%sph_IO)
      close(id_file)
!
      end subroutine gz_mpi_read_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_modes_rlm_file                             &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped merged ascii spectr modes file: ',             &
     &      trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call read_spectr_modes_rlm_data(id_file,                          &
     &    sph_file%my_rank_IO, sph_file%comm_IO, sph_file%sph_IO)
!
      close(id_file)
!
      end subroutine gz_mpi_read_modes_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtp_file                             &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write gzipped merged ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_geom_rtp_data(id_file, sph_file%my_rank_IO,            &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      close(id_file)
!
      end subroutine gz_mpi_write_geom_rtp_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_spectr_rj_file                            &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write gzipped merged ascii spectr modes file: ',            &
     &      trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_spectr_modes_rj_data                                   &
     &   (id_file, sph_file%my_rank_IO,                                 &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      close(id_file)
!
      end subroutine gz_mpi_write_spectr_rj_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geom_rtm_file                             &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write gzipped merged ascii grid file: ', trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_geom_rtm_data(id_file,                                 &
     &    sph_file%my_rank_IO, sph_file%comm_IO, sph_file%sph_IO)
      close(id_file)
!
      end subroutine gz_mpi_write_geom_rtm_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_modes_rlm_file                            &
     &         (file_name, my_rank, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      type(sph_file_data_type), intent(inout) :: sph_file
!
      integer :: id_file = 14
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write gzipped merged ascii spectr modes file: ',            &
     &      trim(file_name)
      open (id_file,file = file_name, form = 'formatted')
      call write_modes_rlm_data(id_file,                                &
     &   sph_file%my_rank_IO, sph_file%comm_IO, sph_file%sph_IO)
      close(id_file)
!
      end subroutine gz_mpi_write_modes_rlm_file
!
!------------------------------------------------------------------
!
      end module gz_MPI_sph_modes_file_IO
