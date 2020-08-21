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
      use m_error_IDs
      use calypso_mpi
!
      use m_work_time
!
      use t_SPH_mesh_field_data
      use t_parai_gen_sph_grids_modes
      use t_const_spherical_grid
      use t_check_and_make_SPH_mesh
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_and_make_para_SPH_mesh                           &
     &         (sph_file_param, num_pe, sph_mesh, sph_maker)
!
      use m_elapsed_labels_gen_SPH
      use output_gen_sph_grid_modes
!      use sph_file_IO_select
!      use calypso_mpi_logical
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      logical :: iflag_lc
!
!
!      if(my_rank .eq. izero) then
!        iflag_lc =    check_exsist_rtp_file(my_rank, sph_file_param)   &
!     &          .and. check_exsist_rtm_file(my_rank, sph_file_param)   &
!     &          .and. check_exsist_rlm_file(my_rank, sph_file_param)   &
!     &          .and. check_exsist_rj_file(my_rank, sph_file_param)
!      end if
!      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
      iflag_lc = .FALSE.
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_grids'
        call para_gen_sph_grids(num_pe, sph_mesh, sph_maker)
!
        if(iflag_debug .gt. 0) write(*,*) 'para_output_sph_mode_grids'
        call para_output_sph_mode_grids                                 &
     &     (sph_file_param, num_pe, sph_mesh)
      end if
!
      end subroutine check_and_make_para_SPH_mesh
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_grids(num_pe, sph_mesh, sph_maker)
!
      use m_elapsed_labels_gen_SPH
      use mpi_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
!
      integer(kind = kint), intent(in) :: num_pe
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
!
!  =========  Set global resolutions ===================================
      call const_sph_global_parameters                                  &
     &   (sph_maker%gen_sph, sph_maker%sph_tmp)
!
!      num_pe = sph_maker%gen_sph%s3d_ranks%ndomain_sph
!      allocate(sph_mesh(num_pe))
      call copy_para_sph_param_from_ctl                                 &
     &   (sph_maker%sph_tmp, num_pe, sph_mesh)
      call copy_para_global_sph_resolution                              &
     &   (sph_maker%sph_tmp, num_pe, sph_mesh)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_rj_modes'
      call para_gen_sph_rlm_rj_modes                                    &
     &   (num_pe, sph_maker%gen_sph, sph_mesh)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_rtp_grids'
      call para_gen_sph_rtm_rtp_grids                                   &
     &   (num_pe, sph_maker%gen_sph, sph_mesh)
      call dealloc_gen_mesh_params(sph_maker%gen_sph)
!
      end subroutine para_gen_sph_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_and_make_para_rj_mode                            &
     &         (sph_file_param, num_pe, sph_mesh, sph_maker)
!
      use m_elapsed_labels_gen_SPH
      use mpi_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
      use output_gen_sph_grid_modes
      use sph_file_IO_select
      use calypso_mpi_logical
!
      integer(kind = kint), intent(in) :: num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      logical :: iflag_lc
!
!
      if(my_rank .eq. izero) then
        iflag_lc = check_exsist_rj_file(my_rank, sph_file_param)
        write(*,*) 'iflag_lc: ',         iflag_lc
        write(*,*) 'make_SPH_flag: ',    sph_maker%make_SPH_flag
        write(*,*) 'mesh_output_flag: ', sph_maker%mesh_output_flag
      end if
      call calypso_mpi_bcast_one_logical(iflag_lc, 0)
!
      if(iflag_lc) then
        if(my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
        call load_local_rj_mesh_4_merge                                 &
     &     (sph_file_param, num_pe, sph_mesh)
      else if(sph_maker%make_SPH_flag .eqv. .FALSE.) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rj_mode'
        call para_gen_sph_rj_mode(num_pe, sph_mesh, sph_maker)
!
        if(sph_maker%mesh_output_flag) then
          if(iflag_debug .gt. 0) write(*,*) 'para_output_sph_rj_modes'
          call para_output_sph_rj_modes                                 &
     &       (sph_file_param, num_pe, sph_mesh)
        end if
      end if
      call calypso_mpi_barrier
!
      end subroutine check_and_make_para_rj_mode
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_rj_mode(num_pe, sph_mesh, sph_maker)
!
      use m_elapsed_labels_gen_SPH
      use mpi_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
      use copy_para_sph_global_params
!
      integer(kind = kint), intent(in) :: num_pe
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
!  =========  Set global resolutions ===================================
      call const_sph_global_parameters                                  &
     &   (sph_maker%gen_sph, sph_maker%sph_tmp)
!
!      num_pe = sph_maker%gen_sph%s3d_ranks%ndomain_sph
      call copy_para_sph_param_from_ctl                                 &
     &   (sph_maker%sph_tmp, num_pe, sph_mesh)
      call copy_para_global_sph_resolution                              &
     &   (sph_maker%sph_tmp, num_pe, sph_mesh)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_rj_modes'
      call para_gen_sph_rlm_rj_modes                                    &
     &   (num_pe, sph_maker%gen_sph, sph_mesh)
      call dealloc_gen_mesh_params(sph_maker%gen_sph)
!
      end subroutine para_gen_sph_rj_mode
!
! ----------------------------------------------------------------------
!
      subroutine load_local_rj_mesh_4_merge                             &
     &         (sph_file_param, num_pe, sph_mesh)
!
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use load_data_for_sph_IO
!
      integer, intent(in) ::  num_pe
      type(field_IO_params), intent(in) :: sph_file_param
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      type(field_IO_params) :: file_param
      type(sph_file_data_type) :: sph_file
      integer :: id_rank, ip
      integer(kind = kint) :: iloop, ierr
!
!  Read index data
      call set_sph_mesh_file_fmt_prefix                                 &
     &   (sph_file_param%iflag_format, sph_file_param%file_prefix,      &
     &    file_param)
      do iloop = 0, (num_pe-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_spectr_rj_file                                &
     &     (num_pe, id_rank, file_param, sph_file)
!
        if(id_rank .lt. num_pe) then
          write(*,*) 'load original data for  ', id_rank,               &
     &             ' on ', my_rank, 'at loop ', iloop
          call copy_sph_trans_rj_from_IO(sph_file,                      &
     &       sph_mesh(ip)%sph%sph_rj, sph_mesh(ip)%sph_comms%comm_rj,   &
     &       sph_mesh(ip)%sph_grps, sph_mesh(ip)%sph%sph_params, ierr)
          call dealloc_rj_mode_IO(sph_file)
        end if
      end do
!
      end subroutine load_local_rj_mesh_4_merge
!
! -----------------------------------------------------------------------
!
      end module parallel_gen_sph_grids
