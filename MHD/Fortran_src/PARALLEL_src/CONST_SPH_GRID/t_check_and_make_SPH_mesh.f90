!>@file   t_check_and_make_SPH_mesh.f90
!!@brief  module t_check_and_make_SPH_mesh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine check_and_make_SPH_mesh                              &
!!     &         (iflag_make_SPH, sph_file_param, sph_maker)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!      subroutine mpi_gen_sph_grids(sph_file_param, sph, gen_sph)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_grids), intent(inout) :: sph
!!@endverbatim
!
      module t_check_and_make_SPH_mesh
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
      use t_const_spherical_grid
      use t_file_IO_parameter
!
      implicit none
!
!>      Structure to check and construct spherical shell mesh
      type sph_grid_maker_in_sim
!>        Structure to construct grid
        type(construct_spherical_grid) :: gen_sph
!>        Structure for temporal spherical grid
        type(sph_grids) :: sph_tmp
      end type sph_grid_maker_in_sim
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_SPH_mesh                                &
     &         (iflag_make_SPH, sph_file_param, sph_maker)
!
      use m_error_IDs
      use calypso_mpi_logical
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: iflag_make_SPH
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc =    check_exsist_rtp_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rtm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rlm_file(my_rank, sph_file_param)    &
     &          .and. check_exsist_rj_file(my_rank, sph_file_param)
      end if
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
      else if(iflag_make_SPH .eq. 0) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        call mpi_gen_sph_grids                                          &
     &     (sph_file_param, sph_maker%sph_tmp, sph_maker%gen_sph)
        call dealloc_gen_mesh_params(sph_maker%gen_sph)
      end if
      call calypso_mpi_barrier
!
      end subroutine check_and_make_SPH_mesh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_grids(sph_file_param, sph, gen_sph)
!
      use t_sph_local_index
!
      use m_elapsed_labels_gen_SPH
      use set_global_spherical_param
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use bcast_comm_stacks_sph
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rtm_mul(:)
!
      type(sph_comm_tables) :: comms_sph
      type(sph_group_data) :: sph_grp
      type(sph_local_1d_index) :: sph_lcx_m
!
      integer :: ip
!
!  =========  Set global resolutions ===================================
!
      write(*,*) 'Tako!'
      call set_global_sph_resolution                                    &
     &   (sph%sph_params%l_truncation, sph%sph_params%m_folding,        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter                             &
     &     (sph%sph_params, sph%sph_rtp)
        call output_set_radial_grid                                     &
     &     (sph%sph_params, sph%sph_rtp, gen_sph%s3d_radius)
      end if
!
!  ========= Generate spherical harmonics table ========================
!
      call s_const_global_sph_grids_modes                               &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rtm, sph%sph_rj,         &
     &    gen_sph%s3d_ranks, gen_sph%sph_lcp,                           &
     &    gen_sph%stk_lc1d, gen_sph%sph_gl1d)
!
!  ========= Generate each spherical harmonics table ===================
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rlm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rlm table generation for', my_rank
      call const_sph_rlm_modes                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_rlm, comms_sph%comm_rlm)
      ip = my_rank + 1
      call copy_sph_comm_neib(comms_sph%comm_rlm, comm_rlm_mul(ip))
!
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical modes for domain ', my_rank
      call alloc_rj_1d_local_idx(sph%sph_rj, sph_lcx_m)
      call const_sph_rj_modes                                           &
     &   (my_rank, nprocs, comm_rlm_mul, gen_sph%added_radial_grp,      &
     &    gen_sph%s3d_ranks, gen_sph%s3d_radius,                        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    comms_sph%comm_rj, sph_grp, sph_lcx_m)
      call dealloc_rj_1d_local_idx(sph_lcx_m)
!
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rtm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rtm table generation for',  my_rank
      call const_sph_rtm_grids                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_rtm, comms_sph%comm_rtm)
      ip = my_rank + 1
      call copy_sph_comm_neib(comms_sph%comm_rtm, comm_rtm_mul(ip))
!
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical grids for domain ',  my_rank
      call alloc_rtp_1d_local_idx(sph%sph_rtp, sph_lcx_m)
      call const_sph_rtp_grids(my_rank, nprocs, comm_rtm_mul,           &
     &    gen_sph%added_radial_grp, gen_sph%r_layer_grp,                &
     &    gen_sph%med_layer_grp, gen_sph%s3d_ranks, gen_sph%s3d_radius, &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_params, sph%sph_rtp, comms_sph%comm_rtp,              &
     &    sph_grp, sph_lcx_m)
      call dealloc_rtp_1d_local_idx(sph_lcx_m)
!
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rtm_mul)
!
      deallocate(comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      call mpi_output_gen_sph_grids                                     &
     &   (sph_file_param, sph, comms_sph, sph_grp)
!
      end subroutine mpi_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine mpi_output_gen_sph_grids                               &
     &         (sph_file_param, sph, comms_sph, sph_grp)
!
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_spheric_data_IO
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grp
!
      type(sph_file_data_type) :: sph_file_m
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
!
      call dealloc_sph_mode_group(sph_grp)
      call dealloc_sph_grid_group(sph_grp)
!
      call dealloc_type_sph_comm_item(comms_sph%comm_rtp)
      call dealloc_type_sph_comm_item(comms_sph%comm_rtm)
      call dealloc_type_sph_comm_item(comms_sph%comm_rlm)
      call dealloc_type_sph_comm_item(comms_sph%comm_rj)
!
      call dealloc_type_sph_1d_index_rtp(sph%sph_rtp)
      call dealloc_type_sph_1d_index_rtm(sph%sph_rtm)
      call dealloc_type_sph_1d_index_rlm(sph%sph_rlm)
      call dealloc_type_sph_1d_index_rj(sph%sph_rj)
!
      call dealloc_type_spheric_param_rtp(sph%sph_rtp)
      call dealloc_type_spheric_param_rtm(sph%sph_rtm)
      call dealloc_type_spheric_param_rlm(sph%sph_rlm)
      call dealloc_spheric_param_rj(sph%sph_rj)
!
      end subroutine mpi_output_gen_sph_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mpi_gen_sph_rj_mode(sph_file_param, sph, gen_sph)
!
      use t_sph_local_index
!
      use m_elapsed_labels_gen_SPH
      use set_global_spherical_param
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use bcast_comm_stacks_sph
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!
      type(sph_comm_tables) :: comms_sph
      type(sph_group_data) :: sph_grp
      type(sph_local_1d_index) :: sph_lcx_m
!
      integer :: ip
!
!  =========  Set global resolutions ===================================
!
      write(*,*) 'Tako!'
      call set_global_sph_resolution                                    &
     &   (sph%sph_params%l_truncation, sph%sph_params%m_folding,        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter                             &
     &     (sph%sph_params, sph%sph_rtp)
        call output_set_radial_grid                                     &
     &     (sph%sph_params, sph%sph_rtp, gen_sph%s3d_radius)
      end if
!
!  ========= Generate spherical harmonics table ========================
!
      call s_const_global_sph_grids_modes                               &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rtm, sph%sph_rj,         &
     &    gen_sph%s3d_ranks, gen_sph%sph_lcp,                           &
     &    gen_sph%stk_lc1d, gen_sph%sph_gl1d)
!
!  ========= Generate each spherical harmonics table ===================
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rlm_mul(gen_sph%s3d_ranks%ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'start rlm table generation for', my_rank
      call const_sph_rlm_modes                                          &
     &   (my_rank, gen_sph%s3d_ranks, gen_sph%s3d_radius,               &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_rlm, comms_sph%comm_rlm)
      ip = my_rank + 1
      call copy_sph_comm_neib(comms_sph%comm_rlm, comm_rlm_mul(ip))
!
      call s_bcast_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &             'Construct spherical modes for domain ', my_rank
      call alloc_rj_1d_local_idx(sph%sph_rj, sph_lcx_m)
      call const_sph_rj_modes                                           &
     &   (my_rank, nprocs, comm_rlm_mul, gen_sph%added_radial_grp,      &
     &    gen_sph%s3d_ranks, gen_sph%s3d_radius,                        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, gen_sph%sph_gl1d,          &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    comms_sph%comm_rj, sph_grp, sph_lcx_m)
      call dealloc_rj_1d_local_idx(sph_lcx_m)
!
      call dealloc_comm_stacks_sph                                      &
     &   (gen_sph%s3d_ranks%ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
!
      call mpi_output_gen_sph_rj_mode                                   &
     &   (sph_file_param, sph, comms_sph, sph_grp)
!
      end subroutine mpi_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
!
      subroutine mpi_output_gen_sph_rj_mode                             &
     &         (sph_file_param, sph, comms_sph, sph_grp)
!
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_spheric_data_IO
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) :: sph_grp
!
      type(sph_file_data_type) :: sph_file_m
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
!
      call dealloc_sph_mode_group(sph_grp)
!
      call dealloc_type_sph_comm_item(comms_sph%comm_rlm)
      call dealloc_type_sph_comm_item(comms_sph%comm_rj)
!
      call dealloc_type_sph_1d_index_rlm(sph%sph_rlm)
      call dealloc_type_sph_1d_index_rj(sph%sph_rj)
!
      call dealloc_type_spheric_param_rlm(sph%sph_rlm)
      call dealloc_spheric_param_rj(sph%sph_rj)
!
      end subroutine mpi_output_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
!
      end module t_check_and_make_SPH_mesh
