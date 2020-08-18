!>@file   para_gen_sph_grids_modes.f90
!!@brief  module para_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine para_gen_sph_rlm_grids(sph_file_param,               &
!!     &         gen_sph, sph_params, sph_rlm, comm_rlm_mul)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(nprocs)
!!      subroutine para_gen_sph_rtm_grids(sph_file_param,               &
!!     &         (gen_sph, sph_params, sph_rtm, comm_rtm_mul)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(nprocs)
!!
!!      subroutine para_gen_sph_rj_modes                                &
!!     &         (sph_file_param, comm_rlm_mul, sph_params, sph_rtp,    &
!!     &          gen_sph, sph_rj)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!      subroutine para_gen_sph_rtp_grids                               &
!!     &         (sph_file_param, comm_rtm_mul, sph_params,             &
!!     &          gen_sph, sph_rtp)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(nprocs)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!@endverbatim
!
      module para_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      use m_work_time
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_data_IO
      use t_file_IO_parameter
      use t_const_spherical_grid
      use t_sph_local_parameter
      use t_sph_local_index
!
      use set_local_sphere_by_global
!
      implicit none
!
      type(sph_file_data_type), save :: sph_file_m
      type(sph_local_1d_index), save :: sph_lcx_m
      private :: sph_file_m, sph_lcx_m
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_grids(sph_file_param, gen_sph,        &
     &          num_pe, para_sph, comm_rlm_lc, comm_rlm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(construct_spherical_grid), intent(in) :: gen_sph
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rlm_lc(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(num_pe)
!
      integer :: ip, id_rank
!
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rlm table generation for', id_rank, ' on ', my_rank
        call const_sph_rlm_modes                                        &
     &   (id_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    para_sph(ip)%sph_rlm, comm_rlm_lc(ip))
        call copy_sph_comm_neib(comm_rlm_lc(ip), comm_rlm_mul(ip))
      end do
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rlm_to_IO                                   &
     &     (para_sph(ip)%sph_params, para_sph(ip)%sph_rlm,              &
     &      comm_rlm_lc(ip), sph_file_m)
!
        call dealloc_type_sph_comm_item(comm_rlm_lc(ip))
        call dealloc_type_sph_1d_index_rlm(para_sph(ip)%sph_rlm)
        call dealloc_type_spheric_param_rlm(para_sph(ip)%sph_rlm)
!
        call sel_mpi_write_modes_rlm_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rlm_mode_IO(sph_file_m)
!
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &                      id_rank, ' is done.'
      end do
!
      end subroutine para_gen_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_grids(sph_file_param,                 &
     &          gen_sph, num_pe, para_sph, comm_rtm_lc, comm_rtm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtm_lc(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(num_pe)
!
      integer :: ip, id_rank
!
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'start rtm table generation for', id_rank, ' on ', my_rank
        call const_sph_rtm_grids                                        &
     &     (id_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,             &
     &      gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,        &
     &      para_sph(ip)%sph_rtm, comm_rtm_lc(ip))
        call copy_sph_comm_neib(comm_rtm_lc(ip), comm_rtm_mul(ip))
      end do
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        call copy_sph_trans_rtm_to_IO                                   &
     &     (para_sph(ip)%sph_params, para_sph(ip)%sph_rtm,              &
     &      comm_rtm_lc(ip), sph_file_m)
!
        call dealloc_type_sph_comm_item(comm_rtm_lc(ip))
        call dealloc_type_sph_1d_index_rtm(para_sph(ip)%sph_rtm)
        call dealloc_type_spheric_param_rtm(para_sph(ip)%sph_rtm)
!
        call sel_mpi_write_geom_rtm_file                                &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rtm_grid_IO(sph_file_m)
!
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &                      id_rank, ' is done.'
      end do
!
      end subroutine para_gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rj_modes                                  &
     &         (sph_file_param, num_pe, comm_rlm_mul, gen_sph,          &
     &          para_sph, comm_rj_lc, sph_grp_lc)
!
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(num_pe)
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rj_lc(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp_lc(num_pe)
!
      integer :: ip, id_rank
!
!
      call alloc_rj_1d_local_idx(para_sph(1)%sph_rj, sph_lcx_m)
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &     'Construct spherical modes for domain ', id_rank,            &
     &     ' on ', my_rank
        call const_sph_rj_modes(id_rank, num_pe, comm_rlm_mul,          &
     &      gen_sph%added_radial_grp, gen_sph%s3d_ranks,                &
     &      gen_sph%s3d_radius, gen_sph%sph_lcp,                        &
     &      gen_sph%stk_lc1d, gen_sph%sph_gl1d,                         &
     &      para_sph(ip)%sph_params, para_sph(ip)%sph_rtp,              &
     &      para_sph(ip)%sph_rj, comm_rj_lc(ip), sph_grp_lc(ip),        &
     &      sph_lcx_m)
      end do
      call dealloc_rj_1d_local_idx(sph_lcx_m)
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                 'copy_sph_trans_rj_to_IO', id_rank
        call copy_sph_trans_rj_to_IO(para_sph(ip)%sph_params,           &
     &      para_sph(ip)%sph_rj, comm_rj_lc(ip), sph_grp_lc(ip),        &
     &      sph_file_m)
!
        call dealloc_type_sph_1d_index_rj(para_sph(ip)%sph_rj)
        call dealloc_spheric_param_rj(para_sph(ip)%sph_rj)
        call dealloc_type_sph_comm_item(comm_rj_lc(ip))
        call dealloc_sph_mode_group(sph_grp_lc(ip))
!
        call sel_mpi_write_spectr_rj_file                               &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
        call dealloc_rj_mode_IO(sph_file_m)
!
        write(*,'(a,i6,a)') 'Spherical modes for domain',               &
     &          id_rank, ' is done.'
      end do
!
      end subroutine para_gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtp_grids                                 &
     &         (sph_file_param, num_pe, comm_rtm_mul, gen_sph,          &
     &          para_sph, comm_rtp_lc, sph_grp_lc)
!
      use set_comm_table_rtp_rj
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(num_pe)
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtp_lc(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp_lc(num_pe)
!
      integer :: ip, id_rank
!
!
      call alloc_rtp_1d_local_idx(para_sph(1)%sph_rtp, sph_lcx_m)
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical grids for domain ',  id_rank,   &
     &              ' on ',  my_rank
        call const_sph_rtp_grids(id_rank, num_pe, comm_rtm_mul,         &
     &      gen_sph%added_radial_grp, gen_sph%r_layer_grp,              &
     &      gen_sph%med_layer_grp, gen_sph%s3d_ranks,                   &
     &      gen_sph%s3d_radius, gen_sph%sph_lcp,                        &
     &      gen_sph%stk_lc1d, gen_sph%sph_gl1d,                         &
     &      para_sph(ip)%sph_params, para_sph(ip)%sph_rtp,              &
     &      comm_rtp_lc(ip), sph_grp_lc(ip), sph_lcx_m)
      end do
      call dealloc_rtp_1d_local_idx(sph_lcx_m)
!
      do ip = 1, num_pe
        id_rank = ip - 1
        if(mod(id_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                 'copy_sph_trans_rtp_to_IO', id_rank
        call copy_sph_trans_rtp_to_IO(para_sph(ip)%sph_params,          &
     &      para_sph(ip)%sph_rtp, comm_rtp_lc(ip), sph_grp_lc(ip),      &
     &      sph_file_m)
!
        call dealloc_type_sph_1d_index_rtp(para_sph(ip)%sph_rtp)
        call dealloc_type_spheric_param_rtp(para_sph(ip)%sph_rtp)
        call dealloc_type_sph_comm_item(comm_rtp_lc(ip))
        call dealloc_sph_grid_group(sph_grp_lc(ip))
!
        call sel_mpi_write_geom_rtp_file                                &
     &     (num_pe, id_rank, sph_file_param, sph_file_m)
!
        call dealloc_rtp_grid_IO(sph_file_m)
!
        write(*,'(a,i6,a)') 'Spherical grids for domain',               &
     &          id_rank, ' is done.'
      end do
!
      end subroutine para_gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      end module para_gen_sph_grids_modes
