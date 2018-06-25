!>@file   gz_sph_modes_file_IO_b.f90
!!@brief  module gz_sph_modes_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief gzipped binary spectr data IO routines
!!
!!@verbatim
!!      subroutine gz_read_geom_rtp_file_b                              &
!!     &         (file_name, my_rank_IO, sph_file, ierr)
!!      subroutine gz_read_spectr_modes_rj_file_b                       &
!!     &         (file_name, my_rank_IO, sph_file, ierr)
!!      subroutine gz_read_geom_rtm_file_b                              &
!!     &         (file_name, my_rank_IO, sph_file, ierr)
!!      subroutine gz_read_modes_rlm_file_b                             &
!!     &         (file_name, my_rank_IO, sph_file, ierr)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!
!!      subroutine gz_write_geom_rtp_file_b                             &
!!     &         (file_name, my_rank_IO, sph_file)
!!      subroutine gz_write_spectr_modes_rj_file_b                      &
!!     &         (file_name, my_rank_IO, sph_file)
!!      subroutine gz_write_geom_rtm_file_b                             &
!!     &         (file_name, my_rank_IO, sph_file)
!!      subroutine gz_write_modes_rlm_file_b                            &
!!     &         (file_name, my_rank_IO, sph_file)
!!        type(sph_file_data_type), intent(inout) :: sph_file
!!@endverbatim
!!
!!@param my_rank_IO    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module gz_sph_modes_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_data_IO
      use gz_sph_modes_data_IO_b
      use skip_gz_comment
!
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtp_file_b                                &
     &         (file_name, my_rank_IO, sph_file, ierr)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'Read gzipped binary grid file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank_IO)
      call gz_read_geom_rtp_data_b(my_rank_IO,                          &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO, ierr)
      call close_gzfile_f
!
      end subroutine gz_read_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_spectr_modes_rj_file_b                         &
     &         (file_name, my_rank_IO, sph_file, ierr)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'Read gzipped binary spectr modes file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank_IO)
      call gz_read_spectr_modes_rj_data_b(my_rank_IO,                   &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO, ierr)
      call close_gzfile_f
!
      end subroutine gz_read_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_geom_rtm_file_b                                &
     &         (file_name, my_rank_IO, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'Read gzipped binary grid file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank_IO)
      call gz_read_geom_rtm_data_b                                      &
     &   (my_rank_IO, sph_file%comm_IO, sph_file%sph_IO, ierr)
      call close_gzfile_f
!
      end subroutine gz_read_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_read_modes_rlm_file_b                               &
     &         (file_name, my_rank_IO, sph_file, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
!
      type(sph_file_data_type), intent(inout) :: sph_file
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'Read gzipped binary spectr modes file: ', trim(file_name)
      call open_rd_gzfile_b(file_name, my_rank_IO)
      call gz_read_modes_rlm_data_b                                     &
     &   (my_rank_IO, sph_file%comm_IO, sph_file%sph_IO, ierr)
      call close_gzfile_f
!
      end subroutine gz_read_modes_rlm_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtp_file_b                               &
     &         (file_name, my_rank_IO, sph_file)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'Write gzipped binary grid file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_geom_rtp_data_b(my_rank_IO,                         &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      call close_gzfile_f
!
      call deallocate_type_neib_id(sph_file%comm_IO)
      call deallocate_type_import(sph_file%comm_IO)
      call dealloc_sph_grid_idx_IO(sph_file%sph_IO)
      call deallocate_grp_type(sph_file%sph_grp_IO%bc_rtp_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%radial_rtp_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%theta_rtp_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%zonal_rtp_grp)
!
      end subroutine gz_write_geom_rtp_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_spectr_modes_rj_file_b                        &
     &         (file_name, my_rank_IO, sph_file)
!
      use gz_groups_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'gzipped binary spectr modes file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_spectr_modes_rj_data_b(my_rank_IO,                  &
     &    sph_file%comm_IO, sph_file%sph_IO, sph_file%sph_grp_IO)
      call close_gzfile_f
!
      call deallocate_type_neib_id(sph_file%comm_IO)
      call deallocate_type_import(sph_file%comm_IO)
      call dealloc_sph_mode_idx_IO(sph_file%sph_IO)
      call deallocate_grp_type(sph_file%sph_grp_IO%radial_rj_grp)
      call deallocate_grp_type(sph_file%sph_grp_IO%sphere_rj_grp)
!
      end subroutine gz_write_spectr_modes_rj_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_geom_rtm_file_b                               &
     &         (file_name, my_rank_IO, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'Write gzipped binary grid file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_geom_rtm_data_b                                     &
     &   (my_rank_IO, sph_file%comm_IO, sph_file%sph_IO)
      call close_gzfile_f
!
      call deallocate_type_neib_id(sph_file%comm_IO)
      call deallocate_type_import(sph_file%comm_IO)
      call dealloc_sph_grid_idx_IO(sph_file%sph_IO)
!
      end subroutine gz_write_geom_rtm_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_modes_rlm_file_b                              &
     &         (file_name, my_rank_IO, sph_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank_IO
      type(sph_file_data_type), intent(inout) :: sph_file
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &      'Write gzipped binary spectr modes file: ', trim(file_name)
      call open_wt_gzfile_b(file_name)
      call gz_write_modes_rlm_data_b                                    &
     &   (my_rank_IO, sph_file%comm_IO, sph_file%sph_IO)
      call close_gzfile_f
!
      call deallocate_type_neib_id(sph_file%comm_IO)
      call deallocate_type_import(sph_file%comm_IO)
      call dealloc_sph_mode_idx_IO(sph_file%sph_IO)
!
      end subroutine gz_write_modes_rlm_file_b
!
!------------------------------------------------------------------
!
      end module gz_sph_modes_file_IO_b
