!>@file   sel_make_SPH_mesh_w_LIC.f90
!!@brief  module sel_make_SPH_mesh_w_LIC
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_w_LIC(FEM_mesh_flags,          &
!!     &          sph_file_param, sph, comms_sph, sph_grps, geofem,     &
!!     &          mesh_file, sph_maker)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(mesh_data), intent(inout) ::   geofem
!!        type(MHD_file_IO_params), intent(inout) ::  MHD_files
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!@endverbatim
!
!
      module sel_make_SPH_mesh_w_LIC
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_spheric_group
      use t_mesh_data
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_trans_arrays_MHD
      use t_check_and_make_SPH_mesh
!
      implicit none
!
      private :: load_FEM_mesh_4_SPH_w_LIC
      private :: const_FEM_mesh_4_sph_MHD_w_LIC
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_w_LIC(FEM_mesh_flags,            &
     &          sph_file_param, sph, comms_sph, sph_grps, geofem,       &
     &          mesh_file, sph_maker)
!
      use calypso_mpi
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
      use set_loaded_data_4_sph
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  sph_file_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(mesh_data), intent(inout) :: geofem
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      sph_file_param%file_prefix = 'tako'
!
!  Check and construct spherical shell table
      call check_and_make_SPH_mesh(sph_file_param, sph_maker,           &
     &                             sph, comms_sph, sph_grps)
!
!  --  load geofem mesh data
      if(check_exist_mesh(mesh_file, my_rank)) then
        if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
        call mpi_input_mesh(mesh_file, nprocs, geofem)
        call set_fem_center_mode_4_SPH(geofem%mesh%node%internal_node,  &
     &      sph%sph_rtp, sph%sph_params)
      else
        call copy_sph_radial_groups(sph_grps, sph_maker%gen_sph)
!  --  Construct FEM mesh
        mesh_file%file_prefix = sph_file_param%file_prefix
        call load_FEM_mesh_4_SPH_w_LIC(FEM_mesh_flags, mesh_file,       &
     &      sph%sph_params, sph%sph_rtp, sph%sph_rj, geofem,            &
     &      sph_maker%gen_sph)
        call dealloc_gen_sph_radial_groups(sph_maker%gen_sph)
      end if
!
      end subroutine load_para_SPH_and_FEM_w_LIC
!
! -----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH_w_LIC                              &
     &         (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj, &
     &          geofem, gen_sph)
!
      use calypso_mpi
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use m_spheric_constants
      use copy_mesh_structures
      use const_FEM_mesh_sph_mhd
      use gen_sph_grids_modes
      use mesh_IO_select
      use set_nnod_4_ele_by_type
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  mesh_file
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(mesh_data), intent(inout) :: geofem
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      type(mesh_data) :: femmesh_s
!
!
!  --  Construct FEM mesh
      if(sph_params%iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(sph_rj%iflag_rj_center .gt. 0) then
          sph_params%iflag_shell_mode =  iflag_MESH_w_center
        else
          sph_params%iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_MHD_w_LIC'
      call const_FEM_mesh_4_sph_MHD_w_LIC                               &
     &   (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj,       &
     &    femmesh_s%mesh, femmesh_s%group, gen_sph)
!      call compare_mesh_type                                           &
!     &   (my_rank, geofem%mesh%nod_comm, mesh%node, mesh%ele,          &
!     &    femmesh_s%mesh)
!      call compare_mesh_groups(geofem%group%nod_grp, femmesh_s%group)
!
      if (iflag_debug.gt.0) write(*,*) 'set_mesh_data_from_type'
      femmesh_s%mesh%ele%first_ele_type                                 &
     &   = set_cube_eletype_from_num(femmesh_s%mesh%ele%nnod_4_ele)
      call set_mesh_data_from_type(femmesh_s%mesh, femmesh_s%group,     &
     &    geofem%mesh, geofem%group)
      if (iflag_debug.gt.0) write(*,*) 'set_mesh_data_from_type end'
!
      end subroutine load_FEM_mesh_4_SPH_w_LIC
!
! -----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_sph_MHD_w_LIC                         &
     &         (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj, &
     &          mesh, group, gen_sph)
!
      use sph_file_IO_select
      use mpi_load_mesh_data
      use para_const_kemoview_mesh
      use parallel_sleeve_extension
      use const_FEM_mesh_sph_mhd
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  mesh_file
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      type(parallel_make_vierwer_mesh) :: par_view
      integer(kind = kint) :: i_level
!
!
      call base_FEM_mesh_sph_mhd(sph_params, sph_rtp, sph_rj,           &
     &    mesh, group, gen_sph)
!
! Increase sleeve size
      do i_level = 2, gen_sph%num_FEM_sleeve
        if(my_rank .eq. 0) write(*,*) 'extend sleeve:', i_level
        call para_sleeve_extension(mesh, group)
      end do
!
! Output mesh data
      if(FEM_mesh_flags%iflag_access_FEM .gt. 0) then
        call mpi_output_mesh(mesh_file, mesh, group)
        write(*,'(a,i6,a)')                                             &
     &          'FEM mesh for domain', my_rank, ' is done.'
!
        if(FEM_mesh_flags%iflag_output_VMESH .gt. 0) then
          call pickup_surface_mesh(mesh_file, par_view)
        end if
      end if
!
      end subroutine const_FEM_mesh_4_sph_MHD_w_LIC
!
!-----------------------------------------------------------------------
!
      end module sel_make_SPH_mesh_w_LIC
