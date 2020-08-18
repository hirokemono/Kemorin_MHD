!>@file   parallel_gen_sph_grids.f90
!!@brief  module parallel_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine s_para_gen_sph_grids(sph_file_param, sph, gen_sph)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!        type(sph_grids), intent(inout) :: sph
!!@endverbatim
!
      module parallel_gen_sph_grids
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
      use t_sph_local_parameter
      use t_sph_mesh_1d_connect
      use t_file_IO_parameter
      use t_spheric_data_IO
      use t_sph_local_index
!
      implicit none
!
      type(sph_file_data_type), save :: sph_file_m
      type(sph_local_1d_index), save :: sph_lcx_m
      private :: sph_file_m, sph_lcx_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_para_gen_sph_grids(sph_file_param, sph, gen_sph)
!
      use m_elapsed_labels_gen_SPH
      use set_global_spherical_param
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
!
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      integer(kind = kint) :: num_pe
!
      type(sph_grids), allocatable :: para_sph(:)
      type(sph_comm_tbl), allocatable :: comm_rlm_lc(:)
      type(sph_comm_tbl), allocatable :: comm_rj_lc(:)
      type(sph_comm_tbl), allocatable :: comm_rtm_lc(:)
      type(sph_comm_tbl), allocatable :: comm_rtp_lc(:)
      type(sph_group_data), allocatable :: sph_grp_lc(:)
!
!  =========  Set global resolutions ===================================
!
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
      num_pe = gen_sph%s3d_ranks%ndomain_sph
      allocate(sph_grp_lc(num_pe))
      allocate(para_sph(num_pe))
      allocate(comm_rlm_lc(num_pe))
      allocate(comm_rj_lc(num_pe))
      allocate(comm_rtm_lc(num_pe))
      allocate(comm_rtp_lc(num_pe))
      call copy_para_sph_param_from_ctl(sph, num_pe, para_sph)
      call copy_para_global_sph_resolution(sph, num_pe, para_sph)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_rj_modes'
      call para_gen_sph_rlm_rj_modes(num_pe, gen_sph, para_sph,         &
     &    comm_rlm_lc, comm_rj_lc, sph_grp_lc)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_rtp_grids'
      call para_gen_sph_rtm_rtp_grids(num_pe, gen_sph, para_sph,        &
     &    comm_rtm_lc, comm_rtp_lc, sph_grp_lc)
!
      call para_output_sph_rlm_grids                                    &
     &   (sph_file_param, num_pe, para_sph, comm_rlm_lc)
      call para_output_sph_rj_modes                                     &
     &   (sph_file_param, num_pe, para_sph, comm_rj_lc, sph_grp_lc)
!
      call para_output_sph_rtm_grids                                    &
     &   (sph_file_param, num_pe, para_sph, comm_rtm_lc)
      call para_output_sph_rtp_grids                                    &
     &   (sph_file_param, num_pe, para_sph, comm_rtp_lc, sph_grp_lc)
!
      deallocate(comm_rtp_lc, comm_rj_lc)
      deallocate(comm_rtm_lc, comm_rlm_lc)
      deallocate(para_sph, sph_grp_lc)
!
      end subroutine s_para_gen_sph_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rlm_rj_modes(num_pe, gen_sph,            &
     &          para_sph, comm_rlm_lc, comm_rj_lc, sph_grp_lc)
!
      use m_elapsed_labels_gen_SPH
      use set_comm_table_rtp_rj
      use gen_sph_grids_modes
      use bcast_comm_stacks_sph
!
      integer(kind = kint), intent(in) :: num_pe
      type(sph_comm_tbl), intent(inout) :: comm_rlm_lc(num_pe)
!
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rj_lc(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp_lc(num_pe)
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
      integer :: ip, id_rank
!
!
      allocate(comm_rlm_mul(num_pe))
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
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
      call s_bcast_comm_stacks_sph(num_pe, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
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
      call dealloc_comm_stacks_sph(num_pe, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      end subroutine para_gen_sph_rlm_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rtm_rtp_grids(num_pe, gen_sph, para_sph,  &
     &          comm_rtm_lc, comm_rtp_lc, sph_grp_lc)
!
      use m_elapsed_labels_gen_SPH
      use gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use bcast_comm_stacks_sph
!
      integer(kind = kint), intent(in) :: num_pe
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtm_lc(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtp_lc(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp_lc(num_pe)
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable:: comm_rtm_mul(:)
      integer :: ip, id_rank
!
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rtm_mul(num_pe))
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
      call s_bcast_comm_stacks_sph(num_pe, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
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
      call dealloc_comm_stacks_sph(num_pe, comm_rtm_mul)
      deallocate(comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      end subroutine para_gen_sph_rtm_rtp_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_comm_stacks_sph(ndomain_sph, comm_rtm)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip, iflag, i, irank_tgt
!
!
      do ip = 1, ndomain_sph
        iflag = 0
        do i = 1, comm_rtm(ip)%nneib_domain
          irank_tgt = comm_rtm(ip)%id_domain(i)
          if(mod(irank_tgt,nprocs) .eq. my_rank) then
            iflag = 1
            exit
          end if
        end do
!
        if(iflag .gt. 0) then
!          write(*,*) 'deallocate rtm:', my_rank, ip
          call dealloc_type_sph_comm_stack(comm_rtm(ip))
          comm_rtm(ip)%nneib_domain = 0
        end if
      end do
!
      end subroutine dealloc_comm_stacks_sph
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine para_output_sph_rlm_grids                              &
     &         (sph_file_param, num_pe, para_sph, comm_rlm_lc)
!
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rlm_lc(num_pe)
!
      integer :: ip, id_rank
!
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
      end subroutine para_output_sph_rlm_grids
!
! -----------------------------------------------------------------------
!
      subroutine para_output_sph_rtm_grids                              &
     &         (sph_file_param, num_pe, para_sph, comm_rtm_lc)
!
      use load_data_for_sph_IO
      use sph_file_MPI_IO_select
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtm_lc(num_pe)
!
      integer :: ip, id_rank
!
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
      end subroutine para_output_sph_rtm_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_output_sph_rj_modes(sph_file_param, num_pe,       &
     &          para_sph, comm_rj_lc, sph_grp_lc)
!
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rj_lc(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp_lc(num_pe)
!
      integer :: ip, id_rank
!
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
      end subroutine para_output_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine para_output_sph_rtp_grids(sph_file_param, num_pe,      &
     &          para_sph, comm_rtp_lc, sph_grp_lc)
!
      use sph_file_MPI_IO_select
      use load_data_for_sph_IO
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
!
      type(sph_grids), intent(inout) :: para_sph(num_pe)
      type(sph_comm_tbl), intent(inout) :: comm_rtp_lc(num_pe)
      type(sph_group_data), intent(inout) :: sph_grp_lc(num_pe)
!
      integer :: ip, id_rank
!
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
      end subroutine para_output_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      end module parallel_gen_sph_grids
