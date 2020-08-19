!>@file   output_gen_sph_grid_modes.f90
!!@brief  module output_gen_sph_grid_modes
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine mpi_output_gen_sph_grids                             &
!!     &         (sph_file_param, sph, comms_sph, sph_grp)
!!      subroutine mpi_output_gen_sph_rj_mode                           &
!!     &         (sph_file_param, sph, comms_sph, sph_grp)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) :: sph_grp
!!
!!      subroutine para_output_sph_mode_grids(sph_file_param, num_pe,   &
!!     &          sph_params, sph_rj, sph_rlm, sph_rtm, sph_rtp,        &
!!     &          comm_rj, comm_rlm, comm_rtm, comm_rtp, sph_grp)
!!      subroutine para_output_sph_rj_modes                             &
!!     &         (sph_file_param, num_pe, sph_params,                   &
!!     &          sph_rj, sph_rlm, comm_rj, comm_rlm, sph_grp)
!!        integer(kind = kint), intent(in) :: num_pe
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_shell_parameters), intent(in) :: sph_params(num_pe)
!!        type(sph_rj_grid), intent(inout) :: sph_rj(num_pe)
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm(num_pe)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm(num_pe)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp(num_pe)
!!        type(sph_comm_tbl), intent(inout) :: comm_rj(num_pe)
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm(num_pe)
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm(num_pe)
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp(num_pe)
!!        type(sph_group_data), intent(inout) :: sph_grp(num_pe)
!!@endverbatim
!
      module output_gen_sph_grid_modes
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_file_IO_parameter
      use t_spheric_data_IO
!
      implicit none
!
      type(sph_file_data_type), private :: sph_file_m
!
      private :: dealloc_sph_grids, dealloc_sph_modes
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine mpi_output_gen_sph_grids                               &
     &         (sph_file_param, sph, comms_sph, sph_grp)
!
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grp
!
!
      call copy_sph_trans_rlm_to_IO(sph%sph_params, sph%sph_rlm,        &
     &                              comms_sph%comm_rlm, sph_file_m)
      call sel_mpi_write_modes_rlm_file                                 &
     &   (nprocs, my_rank, sph_file_param, sph_file_m)
      call dealloc_rlm_mode_IO(sph_file_m)
      write(*,'(a,i6,a)') 'Spherical transform table for domain',       &
     &          my_rank, ' is done.'
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'copy_sph_trans_rj_to_IO', my_rank
      call copy_sph_trans_rj_to_IO(sph%sph_params,                      &
     &    sph%sph_rj, comms_sph%comm_rj, sph_grp, sph_file_m)
      call sel_mpi_write_spectr_rj_file                                 &
     &   (nprocs, my_rank, sph_file_param, sph_file_m)
      call dealloc_rj_mode_IO(sph_file_m)
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          my_rank, ' is done.'
!
!
      call copy_sph_trans_rtm_to_IO(sph%sph_params, sph%sph_rtm,        &
     &    comms_sph%comm_rtm, sph_file_m)
      call sel_mpi_write_geom_rtm_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file_m)
      call dealloc_rtm_grid_IO(sph_file_m)
      write(*,'(a,i6,a)') 'Legendre transform table rtm',               &
     &          my_rank, ' is done.'
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'copy_sph_trans_rtp_to_IO', my_rank
      call copy_sph_trans_rtp_to_IO(sph%sph_params,                     &
     &    sph%sph_rtp, comms_sph%comm_rtp, sph_grp, sph_file_m)
      call sel_mpi_write_geom_rtp_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file_m)
      call dealloc_rtp_grid_IO(sph_file_m)
      write(*,'(a,i6,a)') 'Spherical grids for domain',                 &
     &          my_rank, ' is done.'
!
      call dealloc_sph_modes(sph%sph_rj, sph%sph_rlm,                   &
     &    comms_sph%comm_rj, comms_sph%comm_rlm, sph_grp)
      call dealloc_sph_grids(sph%sph_rtm, sph%sph_rtp,                  &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp, sph_grp)
!
      end subroutine mpi_output_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine mpi_output_gen_sph_rj_mode                             &
     &         (sph_file_param, sph, comms_sph, sph_grp)
!
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grp
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'copy_sph_trans_rj_to_IO', my_rank
      call copy_sph_trans_rj_to_IO(sph%sph_params,                      &
     &    sph%sph_rj, comms_sph%comm_rj, sph_grp, sph_file_m)
      call sel_mpi_write_spectr_rj_file                                 &
     &   (nprocs, my_rank, sph_file_param, sph_file_m)
      call dealloc_rj_mode_IO(sph_file_m)
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          my_rank, ' is done.'
!
      call dealloc_sph_modes(sph%sph_rj, sph%sph_rlm,                   &
     &    comms_sph%comm_rj, comms_sph%comm_rlm, sph_grp)
!
      end subroutine mpi_output_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine para_output_sph_mode_grids(sph_file_param, num_pe,     &
     &          sph_params, sph_rj, sph_rlm, sph_rtm, sph_rtp,          &
     &          comm_rj, comm_rlm, comm_rtm, comm_rtp, sph_grp)
!
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_shell_parameters), intent(in) :: sph_params(num_pe)
!
      type(sph_rj_grid), intent(inout) :: sph_rj(num_pe)
      type(sph_rlm_grid), intent(inout) :: sph_rlm(num_pe)
      type(sph_rtm_grid), intent(inout) :: sph_rtm(num_pe)
      type(sph_rtp_grid), intent(inout) :: sph_rtp(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rj(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rlm(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtm(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtp(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp(num_pe)
!
      integer :: ip, id_rank
!
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rj_to_IO(sph_params(ip),                    &
     &      sph_rj(ip), comm_rj(ip), sph_grp(ip), sph_file_m)
        call sel_mpi_write_spectr_rj_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rj_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical modes for domain',               &
     &          id_rank, ' is done.'
!
        call copy_sph_trans_rlm_to_IO                                   &
     &     (sph_params(ip), sph_rlm(ip), comm_rlm(ip), sph_file_m)
        call sel_mpi_write_modes_rlm_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rlm_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &                      id_rank, ' is done.'
!
        call copy_sph_trans_rtm_to_IO                                   &
     &     (sph_params(ip), sph_rtm(ip), comm_rtm(ip), sph_file_m)
        call sel_mpi_write_geom_rtm_file                                &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rtm_grid_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &                      id_rank, ' is done.'
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                 'copy_sph_trans_rtp_to_IO', id_rank
        call copy_sph_trans_rtp_to_IO(sph_params(ip),                   &
     &      sph_rtp(ip), comm_rtp(ip), sph_grp(ip), sph_file_m)
        call sel_mpi_write_geom_rtp_file                                &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rtp_grid_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical grids for domain',               &
     &          id_rank, ' is done.'
!
        call dealloc_sph_modes(sph_rj(ip), sph_rlm(ip),                 &
     &      comm_rj(ip), comm_rlm(ip), sph_grp(ip))
        call dealloc_sph_grids(sph_rtm(ip), sph_rtp(ip),                &
     &      comm_rtm(ip), comm_rtp(ip), sph_grp(ip))
      end do
!
      end subroutine para_output_sph_mode_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_output_sph_rj_modes                               &
     &         (sph_file_param, num_pe, sph_params,                     &
     &          sph_rj, sph_rlm, comm_rj, comm_rlm, sph_grp)
!
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_shell_parameters), intent(in) :: sph_params(num_pe)
!
      type(sph_rj_grid), intent(inout) :: sph_rj(num_pe)
      type(sph_rlm_grid), intent(inout) :: sph_rlm(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rj(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rlm(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp(num_pe)
!
      integer :: ip, id_rank
!
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rj_to_IO(sph_params(ip),                    &
     &      sph_rj(ip), comm_rj(ip), sph_grp(ip), sph_file_m)
        call sel_mpi_write_spectr_rj_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rj_mode_IO(sph_file_m)
        write(*,'(a,i6,a)') 'Spherical modes for domain',               &
     &          id_rank, ' is done.'
!
        call dealloc_sph_modes(sph_rj(ip), sph_rlm(ip),                 &
     &      comm_rj(ip), comm_rlm(ip), sph_grp(ip))
      end do
!
      end subroutine para_output_sph_rj_modes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_grids                                      &
     &         (sph_rtm, sph_rtp,  comm_rtm, comm_rtp, sph_grp)
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm, comm_rtp
      type(sph_group_data), intent(inout) :: sph_grp
!
!
      call dealloc_sph_grid_group(sph_grp)
!
      call dealloc_type_sph_comm_item(comm_rtm)
      call dealloc_type_sph_comm_item(comm_rtp)
!
      call dealloc_type_sph_1d_index_rtm(sph_rtm)
      call dealloc_type_sph_1d_index_rtp(sph_rtp)
!
      call dealloc_type_spheric_param_rtm(sph_rtm)
      call dealloc_type_spheric_param_rtp(sph_rtp)
!
      end subroutine dealloc_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_modes                                      &
     &         (sph_rj, sph_rlm, comm_rj, comm_rlm, sph_grp)
!
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj, comm_rlm
      type(sph_group_data), intent(inout) :: sph_grp
!
!
      call dealloc_sph_mode_group(sph_grp)
!
      call dealloc_type_sph_comm_item(comm_rj)
      call dealloc_type_sph_comm_item(comm_rlm)
!
      call dealloc_type_sph_1d_index_rj(sph_rj)
      call dealloc_type_sph_1d_index_rlm(sph_rlm)
!
      call dealloc_spheric_param_rj(sph_rj)
      call dealloc_type_spheric_param_rlm(sph_rlm)
!
      end subroutine dealloc_sph_modes
!
! ----------------------------------------------------------------------
!
      end module output_gen_sph_grid_modes
