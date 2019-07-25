!>@file   analyzer_gen_FEM_4_rayleigh.f90
!!@brief  module analyzer_gen_FEM_4_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_FEM_rayleigh
!!      subroutine analyze_FEM_rayleigh
!!@endverbatim
!
      module analyzer_gen_FEM_4_rayleigh
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_gen_SPH
!
      use t_mesh_data
      use t_spheric_parameter
      use t_spheric_group
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
      use t_ctl_data_const_sph_mesh
      use t_const_spherical_grid
      use t_ctl_params_gen_sph_shell
      use const_fem_nodes_4_rayleigh
!
      implicit none
!
      character (len = kchara)                                          &
     &         :: control_file_name = 'control_sph_shell'
!
!
!>      Structure for file settings
      type(sph_mesh_generation_ctl), save :: SPH_MAKE_ctl
!
!>       Structure of grid and spectr data for spherical spectr method
      type(sph_grids), save :: sph_const
!>      Structure of mesh file name and formats
      type(gen_sph_file_IO_params), save ::  sph_files1
!
!>      Structure to construct grid
      type(construct_spherical_grid), save :: gen_sph_G
!
      type(sph_comm_tables), save, private :: comms_sph
      type(sph_group_data), save, private ::  sph_grps
      type(mesh_data), save, private :: geofem
!
      type(Rayleigh_grid_param), save, private :: r_reso0
!
      private :: control_file_name
      private :: sph_const, SPH_MAKE_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_FEM_rayleigh
!
      use m_error_IDs
      use m_file_format_switch
!
      integer(kind = kint) :: ierr = 0
      type(field_IO_params) ::  rayleigh_mesh_file
!
! 
      call init_elapse_time_by_TOTAL
      call elpsed_label_gen_sph_grid
!
!
!
      call start_elapsed_time(ied_total_elapsed)
      call read_control_4_const_shell(control_file_name, SPH_MAKE_ctl)
      call set_control_4_gen_shell_grids                                &
     &   (my_rank, SPH_MAKE_ctl%plt, SPH_MAKE_ctl%psph_ctl,             &
     &    sph_const, sph_files1, gen_sph_G, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
      rayleigh_mesh_file%file_prefix = 'Rayleigh_in'
      rayleigh_mesh_file%iflag_format = id_ascii_file_fmt
      call output_fem_nodes_4_rayleigh(rayleigh_mesh_file, r_reso0)
!
!      if(gen_sph_G%s3d_ranks%ndomain_sph .ne. nprocs) then
!        if(my_rank .eq. 0) write(*,*) 'The number of MPI processes ',   &
!     &      'must be equal to the number of subdomains.', char(10),     &
!     &      'Current subdomains: ', gen_sph_G%s3d_ranks%ndomain_sph
!        write(e_message,'(a)') 'Parallellization error'
!        call calypso_mpi_abort(ierr_P_MPI, e_message)
!      end if
!
      end subroutine init_gen_FEM_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_FEM_rayleigh
!
      use m_array_for_send_recv
      use parallel_gen_sph_grids
      use mpi_gen_sph_grids_modes
      use parallel_load_data_4_sph
      use parallel_FEM_mesh_init
!
      use const_FEM_mesh_sph_mhd
!
!  ========= Generate spherical harmonics table ========================
!
!      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_grids'
!      call para_gen_sph_grids(sph_const, gen_sph_G)
!      call dealloc_gen_mesh_params(gen_sph_G)
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+3)
      if(iflag_debug .gt. 0) write(*,*) 'load_para_SPH_and_FEM_mesh2'
      call load_para_SPH_and_FEM_mesh2                                  &
     &   (sph_files1%FEM_mesh_flags, sph_const,    &
     &    geofem, sph_files1%mesh_file_IO, gen_sph_G)
      call calypso_MPI_barrier
!
      call dealloc_gen_sph_fem_mesh_param(gen_sph_G)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+3)
!
      call output_elapsed_times
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_FEM_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh2                            &
     &         (FEM_mesh_flags, sph,               &
     &          fem, mesh_file, gen_sph)
!
      use calypso_mpi
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
      use const_FEM_mesh_sph_mhd
      use cal_minmax_and_stacks
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(sph_grids), intent(inout) :: sph
!
      type(mesh_data), intent(inout) :: fem
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      integer(kind = kint) :: i, irev
!
!
      sph%sph_rtp%irank_sph_rtp(1) = r_reso0%irank_r
      sph%sph_rtp%irank_sph_rtp(2) = r_reso0%irank_h
      sph%sph_rtp%irank_sph_rtp(3) = 0

      sph%sph_rj%nidx_global_rj(1) = r_reso0%nri
      sph%sph_rj%nidx_global_rj(2) = (r_reso0%ltr + 1)**2
      sph%sph_rj%nidx_rj(1) = sph%sph_rj%nidx_global_rj(1)
      sph%sph_rj%nidx_rj(2) = sph%sph_rj%nidx_global_rj(2) / nprocs
!
      sph%sph_rtp%nidx_global_rtp(1) = r_reso0%nri
      sph%sph_rtp%nidx_global_rtp(2) = r_reso0%nth
      sph%sph_rtp%nidx_global_rtp(3) = r_reso0%nphi
      sph%sph_rtp%ist_rtp(1) = r_reso0%kst
      sph%sph_rtp%ist_rtp(2) = r_reso0%lst
      sph%sph_rtp%ist_rtp(3) = 1
      sph%sph_rtp%ied_rtp(1) = r_reso0%ked
      sph%sph_rtp%ied_rtp(2) = r_reso0%led
      sph%sph_rtp%ied_rtp(3) = r_reso0%nphi
      sph%sph_rtp%nidx_rtp(1) = r_reso0%ked - r_reso0%kst + 1
      sph%sph_rtp%nidx_rtp(2) = r_reso0%led - r_reso0%lst + 1
      sph%sph_rtp%nidx_rtp(3) = r_reso0%nphi
!
      call alloc_type_sph_1d_index_rj(sph%sph_rj)
      do i = 1, r_reso0%nri
        irev = r_reso0%nri - i + 1
        sph%sph_rj%radius_1d_rj_r(i) = r_reso0%radius(irev)
      end do
!
      call alloc_type_sph_1d_index_rtp(sph%sph_rtp)
!
      do i = 1, sph%sph_rtp%nidx_rtp(1)
        sph%sph_rtp%idx_gl_1d_rtp_r(i) = sph%sph_rtp%ist_rtp(1) + i- 1
      end do
!
      gen_sph%theta_rtp_grp_lc%num_grp = 0
      call alloc_group_num(gen_sph%theta_rtp_grp_lc)
      call alloc_group_item(gen_sph%theta_rtp_grp_lc)
!
      gen_sph%radial_rtp_grp_lc%num_grp = 5
      call alloc_group_num(gen_sph%radial_rtp_grp_lc)
!
      gen_sph%radial_rj_grp_lc%num_grp =  5
      call alloc_group_num(gen_sph%radial_rj_grp_lc)
      gen_sph%radial_rj_grp_lc%grp_name(1) = 'ICB'
      gen_sph%radial_rj_grp_lc%grp_name(2) = 'CMB'
      gen_sph%radial_rj_grp_lc%grp_name(3) = 'to_Center'
      gen_sph%radial_rj_grp_lc%grp_name(4) = 'inner_core'
      gen_sph%radial_rj_grp_lc%grp_name(5) = 'outer_core'
!
      gen_sph%radial_rj_grp_lc%nitem_grp(1) = 1
      gen_sph%radial_rj_grp_lc%nitem_grp(2) = 1
      gen_sph%radial_rj_grp_lc%nitem_grp(3) = 1
      gen_sph%radial_rj_grp_lc%nitem_grp(4) = 0
      gen_sph%radial_rj_grp_lc%nitem_grp(5) = sph%sph_rj%nidx_rj(1)
!
      call s_cal_total_and_stacks(gen_sph%radial_rj_grp_lc%num_grp,     &
     &    gen_sph%radial_rj_grp_lc%nitem_grp, izero,                    &
     &    gen_sph%radial_rj_grp_lc%istack_grp,                          &
     &    gen_sph%radial_rj_grp_lc%num_item)
      call alloc_group_item(gen_sph%radial_rj_grp_lc)
!
      gen_sph%radial_rj_grp_lc%item_grp(1) = 1
      gen_sph%radial_rj_grp_lc%item_grp(2) = sph%sph_rj%nidx_rj(1)
      gen_sph%radial_rj_grp_lc%item_grp(3) = 1
      do i = 1, sph%sph_rj%nidx_rj(1)
        gen_sph%radial_rj_grp_lc%item_grp(i+3) = i
      end do
!
      gen_sph%radial_rtp_grp_lc%grp_name(1) = 'ICB'
      gen_sph%radial_rtp_grp_lc%grp_name(2) = 'CMB'
      gen_sph%radial_rtp_grp_lc%grp_name(3) = 'to_Center'
      gen_sph%radial_rtp_grp_lc%grp_name(4) = 'inner_core'
      gen_sph%radial_rtp_grp_lc%grp_name(5) = 'outer_core'
!
      gen_sph%radial_rtp_grp_lc%nitem_grp(1:5) = 0
      if(sph%sph_rtp%ist_rtp(1) .eq. 1) then
        gen_sph%radial_rtp_grp_lc%nitem_grp(1) = 1
        gen_sph%radial_rtp_grp_lc%nitem_grp(3) = 1
      end if
      if(sph%sph_rtp%ied_rtp(1) .eq. sph%sph_rj%nidx_rj(1)) then
        gen_sph%radial_rtp_grp_lc%nitem_grp(2) = 1
      end if
      gen_sph%radial_rtp_grp_lc%nitem_grp(4) = 0
      gen_sph%radial_rtp_grp_lc%nitem_grp(5)                            &
     &          = sph%sph_rtp%ied_rtp(1) - sph%sph_rtp%ist_rtp(1) + 1
!
      call s_cal_total_and_stacks(gen_sph%radial_rtp_grp_lc%num_grp,    &
     &    gen_sph%radial_rtp_grp_lc%nitem_grp, izero,                   &
     &    gen_sph%radial_rtp_grp_lc%istack_grp,                         &
     &    gen_sph%radial_rtp_grp_lc%num_item)
      call alloc_group_item(gen_sph%radial_rtp_grp_lc)
!
      if(sph%sph_rtp%ist_rtp(1) .eq. 1) then
        i = gen_sph%radial_rtp_grp_lc%istack_grp(0) + 1
        gen_sph%radial_rtp_grp_lc%nitem_grp(i) = 1
      end if
      if(sph%sph_rtp%ied_rtp(1) .eq. sph%sph_rj%nidx_rj(1)) then
        i = gen_sph%radial_rtp_grp_lc%istack_grp(1) + 1
        gen_sph%radial_rtp_grp_lc%nitem_grp(i) = sph%sph_rtp%ied_rtp(1)
      end if
      if(sph%sph_rtp%ist_rtp(1) .eq. 1) then
        i = gen_sph%radial_rtp_grp_lc%istack_grp(2) + 1
        gen_sph%radial_rtp_grp_lc%nitem_grp(i) = 1
      end if
!
      do i = 1, sph%sph_rtp%nidx_rtp(1)
        gen_sph%radial_rj_grp_lc%item_grp(i+3) = i
      end do
!
!  --  Construct FEM mesh
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      sph%sph_params%iflag_shell_mode = iflag_MESH_same
      call const_FEM_mesh_4_sph_mhd                                     &
     &   (FEM_mesh_flags, sph%sph_params, sph%sph_rtp, sph%sph_rj,      &
     &    fem%mesh, fem%group, mesh_file, gen_sph)
!
      end subroutine load_para_SPH_and_FEM_mesh2
!
! -----------------------------------------------------------------------
!
      end module analyzer_gen_FEM_4_rayleigh
