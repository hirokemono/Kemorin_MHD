!>@file   sph_modes_grids_file_IO_b.f90
!!@brief  module sph_modes_grids_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Binary spectr data IO routines
!!
!!@verbatim
!!      subroutine read_geom_rtp_file_b(my_rank, file_name)
!!      subroutine read_spectr_modes_rj_file_b(my_rank, file_name)
!!      subroutine read_geom_rtm_file_b(my_rank, file_name)
!!      subroutine read_modes_rlm_file_b(my_rank, file_name)
!!
!!      subroutine write_geom_rtp_file_b(my_rank, file_name)
!!      subroutine write_spectr_modes_rj_file_b(my_rank, file_name)
!!      subroutine write_geom_rtm_file_b(my_rank, file_name)
!!      subroutine write_modes_rlm_file_b(my_rank, file_name)
!!@endverbatim
!!
!!@param my_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module sph_modes_grids_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_node_id_spherical_IO
      use sph_modes_grids_data_IO_b
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtp_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'unformatted')
      call read_geom_rtp_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine read_spectr_modes_rj_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'unformatted')
      call read_spectr_modes_rj_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine read_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine read_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  3
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'unformatted')
      call read_geom_rtm_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine read_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      ndir_sph_IO =  2
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read binary spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'formatted')
      call read_spectr_modes_rlm_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_geom_rtp_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'unformatted')
      call write_geom_rtp_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine write_spectr_modes_rj_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'binary spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'unformatted')
      call write_spectr_modes_rj_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine write_geom_rtm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary grid file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'unformatted')
      call write_geom_rtm_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine write_modes_rlm_file_b(my_rank, file_name)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write binary spectr modes file: ', trim(file_name)
      open (mesh_file_id,file = file_name, form = 'unformatted')
      call write_modes_rlm_data_b(mesh_file_id)
      close(mesh_file_id)
!
      end subroutine write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module sph_modes_grids_file_IO_b
