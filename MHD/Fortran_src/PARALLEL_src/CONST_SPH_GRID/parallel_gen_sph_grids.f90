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
!
      implicit none
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rtm_mul(:)
!
      integer(kind = kint), allocatable :: nneib_rtm_lc(:)
      integer(kind = kint), allocatable :: nneib_rtm_gl(:)
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
      use para_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
      use bcast_comm_stacks_sph
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
      allocate(comm_rlm_mul(num_pe))
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_grids'
      call para_gen_sph_rlm_grids                                       &
     &   (gen_sph, num_pe, para_sph, comm_rlm_lc, comm_rlm_mul)
      call s_bcast_comm_stacks_sph(num_pe, comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call para_gen_sph_rj_modes(num_pe, comm_rlm_mul, gen_sph,         &
     &                           para_sph, comm_rj_lc, sph_grp_lc)
      call dealloc_comm_stacks_sph(num_pe, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+1)
      allocate(comm_rtm_mul(num_pe))
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_grids'
      call para_gen_sph_rtm_grids                                       &
     &   (gen_sph, num_pe, para_sph, comm_rtm_lc, comm_rtm_mul)
      call s_bcast_comm_stacks_sph(num_pe, comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+1)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call para_gen_sph_rtp_grids(num_pe, comm_rtm_mul, gen_sph,        &
     &                            para_sph, comm_rtp_lc, sph_grp_lc)
      call dealloc_comm_stacks_sph(num_pe, comm_rtm_mul)
      deallocate(comm_rtm_mul)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
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
!
      end module parallel_gen_sph_grids
