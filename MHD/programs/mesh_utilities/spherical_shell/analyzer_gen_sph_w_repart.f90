!>@file   analyzer_gen_sph_w_repart.f90
!!@brief  module analyzer_gen_sph_w_repart
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_sph_grids_w_repart
!!      subroutine analyze_gen_sph_grids_w_repart
!!@endverbatim
!
      module analyzer_gen_sph_w_repart
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_gen_SPH
      use m_elapsed_labels_4_REPART
      use m_work_time_4_sleeve_extend
!
      use t_mesh_data
      use t_SPH_mesh_field_data
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
      use t_ctl_data_gen_sph_w_repart
      use t_ctl_params_gen_sph_shell
      use t_control_param_vol_grping
!
      use para_const_kemoview_mesh
!
      implicit none
!
      character (len = kchara), private, parameter                      &
     &         :: control_file_name = 'control_sph_shell'
!
!
!>      Structure for file settings
      type(ctl_data_gen_sph_w_repart), save, private :: gen_SPH_wP_c1
!
!>      Structure of spherical transform mesh information
      type(SPH_mesh_field_data), save :: SPH_GEN_S
!>      Structure of mesh file name and formats
      type(gen_sph_file_IO_params), save ::  sph_files_S
!
      type(volume_partioning_param), save :: repart_p_C
!
!>      Structure of FEM mesh
      type(mesh_data), save, private :: geofem_S
!
      type(parallel_make_vierwer_mesh), save, private :: para_v1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_sph_grids_w_repart
!
      use m_error_IDs
!
      integer(kind = kint) :: ierr = 0
!
! 
      call init_elapse_time_by_TOTAL
      call elpsed_label_gen_sph_grid
      call elpsed_label_4_repartition
      call elpsed_label_4_sleeve_ext
!
      call start_elapsed_time(ied_total_elapsed)
      call read_ctl_file_gen_sph_w_repart(control_file_name,            &
     &                                    gen_SPH_wP_c1)
!
      call set_control_4_gen_shell_grids                                &
     &   (my_rank, gen_SPH_wP_c1%plt, gen_SPH_wP_c1%psph_ctl,           &
     &    sph_files_S, SPH_GEN_S%sph_maker, ierr)
      call set_ctl_param_vol_repart(gen_SPH_wP_c1%repart_ctl,           &
     &                              repart_p_C)
!
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
!
!
      if(SPH_GEN_S%sph_maker%gen_sph%s3d_ranks%ndomain_sph              &
     &     .ne. nprocs) then
        if(my_rank .eq. 0) write(*,*) 'The number of MPI processes ',   &
     &      'must be equal to the number of subdomains.', char(10),     &
     &      'Current subdomains: ',                                     &
     &      SPH_GEN_S%sph_maker%gen_sph%s3d_ranks%ndomain_sph
        write(e_message,'(a)') 'Parallellization error'
        call calypso_mpi_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine init_gen_sph_grids_w_repart
!
! ----------------------------------------------------------------------
!
      subroutine analyze_gen_sph_grids_w_repart
!
      use t_comm_table
      use t_calypso_comm_table
      use t_mesh_SR
!
      use t_next_node_ele_4_node
      use t_jacobians
      use t_shape_functions
      use t_ctl_param_sleeve_extend
      use t_ctl_param_masking
!
      use calypso_mpi_int
      use parallel_FEM_mesh_init
      use mpi_gen_sph_grids_modes
      use output_gen_sph_grid_modes
      use parallel_load_data_4_sph
      use const_FEM_mesh_sph_mhd
      use nod_phys_send_recv
      use const_element_comm_tables
      use set_element_id_4_node
      use int_volume_of_single_domain
      use repartiton_by_volume
      use compare_mesh_structures
!
      type(mesh_data), save :: new_fem_S
      type(communication_table), save :: ele_comm_S
      type(calypso_comm_table), save :: repart_nod_tbl_S
      type(calypso_comm_table), save :: repart_ele_tbl_S
!
      type(mesh_data), save :: new_fem_2
      type(communication_table), save :: new_ele_comm_2
      type(calypso_comm_table), save :: part_nod_tbl_2
      type(calypso_comm_table), save :: part_ele_tbl_2
!
      type(mesh_SR), save :: m_SR_S
!
      type(jacobians_type), save :: jacobians_S
      type(shape_finctions_at_points), save :: spfs_S
      type(next_nod_ele_table), save :: next_tbl_S
      type(sleeve_extension_work), save :: sleeve_exp_WKS
!
      type(masking_parameter), allocatable, target :: masking_S(:)
      real(kind = kreal), allocatable :: d_mask_org_S(:,:)
      real(kind = kreal), allocatable :: vect_ref_S(:,:)
      integer(kind = kint) :: icount_error, icou_error_gl
!
!
!  ========= Generate spherical harmonics table ========================
!
      if(iflag_debug .gt. 0) write(*,*) 'mpi_gen_sph_grids'
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call mpi_gen_sph_grids(SPH_GEN_S%sph_maker,                       &
     &    SPH_GEN_S%sph, SPH_GEN_S%comms, SPH_GEN_S%groups)
!
      if(SPH_GEN_S%sph_maker%mesh_output_flag) then
        call output_sph_mesh(sph_files_S%sph_file_param,                &
     &    SPH_GEN_S%sph, SPH_GEN_S%comms, SPH_GEN_S%groups)
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params                                   &
     &   (SPH_GEN_S%groups, SPH_GEN_S%sph, SPH_GEN_S%comms)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
!
!  ========= Generate FEM mesh ===========================
!
      if(sph_files_S%FEM_mesh_flags%flag_access_FEM                     &
     &               .or. repart_p_C%flag_repartition) then
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+3)
        if(iflag_debug .gt. 0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
        call const_FEM_mesh_4_sph_mhd                                   &
     &     (sph_files_S%FEM_mesh_flags, sph_files_S%sph_file_param,     &
     &     SPH_GEN_S%groups, SPH_GEN_S%sph, geofem_S%mesh,              &
     &     geofem_S%group, SPH_GEN_S%sph_maker%gen_sph)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+3)
        call calypso_MPI_barrier
      end if
!
!  ========= Generate repartitioned mesh ===========================
!
      if(repart_p_C%flag_repartition) then
        if(iflag_debug.gt.0) write(*,*) 'FEM_mesh_initialization'
        call FEM_mesh_initialization(geofem_S%mesh, geofem_S%group,     &
     &                             m_SR_S%SR_sig, m_SR_S%SR_i)
        if (iflag_debug.gt.0 ) write(*,*) 'FEM_comm_initialization'
        call FEM_comm_initialization(geofem_S%mesh, m_SR_S)
!
!  -----  Const volume of each element
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_single_vol'
      call const_jacobian_and_single_vol                                &
     &   (geofem_S%mesh, geofem_S%group, spfs_S, jacobians_S)
      call finalize_jac_and_single_vol                                  &
     &   (geofem_S%mesh, spfs_S, jacobians_S)
!
        if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
        call const_global_numele_list(geofem_S%mesh%ele)
        call const_ele_comm_table(geofem_S%mesh%node,                   &
     &      geofem_S%mesh%nod_comm, geofem_S%mesh%ele,                  &
     &      ele_comm_S, m_SR_S)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'set_belonged_ele_and_next_nod'
        call set_belonged_ele_and_next_nod                              &
     &     (geofem_S%mesh, next_tbl_S%neib_ele, next_tbl_S%neib_nod)
        if(iflag_debug.gt.0) write(*,*) 's_repartiton_by_volume'
        allocate(masking_S(0))
        allocate(d_mask_org_S(geofem_S%mesh%node%numnod,1))
        allocate(vect_ref_S(geofem_S%mesh%node%numnod,3))
        call s_repartiton_by_volume((.TRUE.), repart_p_C,               &
     &     geofem_S%mesh, geofem_S%group, ele_comm_S, next_tbl_S,       &
     &     izero, masking_S, vect_ref_S(1,1), d_mask_org_S, vect_ref_S, &
     &     new_fem_S%mesh, new_fem_S%group,                             &
     &     repart_nod_tbl_S, repart_ele_tbl_S, sleeve_exp_WKS, m_SR_S)
        deallocate(masking_S, d_mask_org_S, vect_ref_S)
        call dealloc_calypso_comm_table(repart_nod_tbl_S)
        call dealloc_calypso_comm_table(repart_ele_tbl_S)
        call dealloc_next_nod_ele_table(next_tbl_S)
        call calypso_MPI_barrier
!
!  ========= Check table ===========================
!
        if(iflag_debug.gt.0) write(*,*) ' load_repartitoned_table_mesh'
        call load_repartitoned_table_mesh((.FALSE.), repart_p_C,        &
     &    geofem_S, ele_comm_S, new_fem_2, new_ele_comm_2,              &
     &    part_nod_tbl_2, part_ele_tbl_2, m_SR_S)
        call dealloc_calypso_comm_table(part_nod_tbl_2)
        call dealloc_calypso_comm_table(part_ele_tbl_2)
        call dealloc_comm_table(new_ele_comm_2)
        call dealloc_comm_table(ele_comm_S)
!
      end if
!
      call compare_node_comm_types(my_rank, new_fem_S%mesh%nod_comm,    &
     &                           new_fem_2%mesh%nod_comm, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare node comm table: ',        &
      &                 icou_error_gl
!      write(*,*) my_rank, 'Compare node comm table: ', icount_error
!
      call compare_node_position(my_rank, new_fem_S%mesh%node,          &
     &                           new_fem_2%mesh%node, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare node: ', icou_error_gl
!      write(*,*) my_rank, 'Compare node: ', icount_error
!
      call compare_ele_connect(my_rank, new_fem_S%mesh%ele,             &
     &    new_fem_2%mesh%ele, icount_error)
      call calypso_mpi_reduce_one_int                                   &
     &   (icount_error, icou_error_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Compare element: ', icou_error_gl
!      write(*,*) my_rank, 'Compare element: ', icount_error
!
      call compare_mesh_groups(my_rank, new_fem_S%group,                &
     &                         new_fem_2%group)
!
!  ========= Generate viewer mesh ===========================
!
      if(sph_files_S%FEM_mesh_flags%flag_access_FEM                     &
              .and. sph_files_S%FEM_mesh_flags%flag_output_VMESH) then
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+5)
        if(iflag_debug .gt. 0) write(*,*) 'pickup_surface_mesh'
        call pickup_surface_mesh(sph_files_S%sph_file_param, para_v1)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+5)
      end if
!
!
      call dealloc_sph_modes(SPH_GEN_S%sph, SPH_GEN_S%comms,            &
     &                       SPH_GEN_S%groups)
      call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_gen_sph_grids_w_repart
!
!  ---------------------------------------------------------------------
!
      end module analyzer_gen_sph_w_repart
