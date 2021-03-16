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
!!     &          sph_file_param, SPH_MHD, geofem, mesh_file)
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(mesh_data), intent(inout) ::   geofem
!!        type(MHD_file_IO_params), intent(inout) ::  MHD_files
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
      use t_SPH_mesh_field_data
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
     &          sph_file_param, SPH_MHD, geofem, mesh_file)
!
      use calypso_mpi
      use m_elapsed_labels_gen_SPH
      use m_work_time
      use t_mesh_data
      use copy_mesh_structures
      use mesh_file_name_by_param
      use mpi_load_mesh_data
      use parallel_load_data_4_sph
      use set_loaded_data_4_sph
!
      type(FEM_file_IO_flags), intent(in) :: FEM_mesh_flags
      type(field_IO_params), intent(in) ::  sph_file_param
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(mesh_data), intent(inout) :: geofem
      type(field_IO_params), intent(inout) ::  mesh_file
!
!  Check and construct spherical shell table
      call check_and_make_SPH_mesh(sph_file_param, SPH_MHD)
!
!  --  load geofem mesh data
      if(check_exist_mesh(my_rank, mesh_file)) then
        if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+6)
        call mpi_input_mesh(mesh_file, nprocs, geofem)
        call set_fem_center_mode_4_SPH(geofem%mesh%node%internal_node,  &
     &      SPH_MHD%sph%sph_rtp, SPH_MHD%sph%sph_params)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+6)
      else
!  --  Construct FEM mesh
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+3)
        mesh_file%file_prefix = sph_file_param%file_prefix
        call load_FEM_mesh_4_SPH_w_LIC(FEM_mesh_flags, mesh_file,       &
     &      SPH_MHD%groups, SPH_MHD%sph, geofem, SPH_MHD%sph_maker)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+3)
      end if
!
      end subroutine load_para_SPH_and_FEM_w_LIC
!
! -----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH_w_LIC(FEM_mesh_flags, mesh_file,   &
     &          sph_grps, sph, geofem, sph_maker)
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
      type(sph_group_data), intent(in) :: sph_grps
!
      type(sph_grids), intent(inout) :: sph
      type(mesh_data), intent(inout) :: geofem
!
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      type(mesh_data) :: femmesh_s
!
!
!  --  Construct FEM mesh
      if(sph%sph_params%iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(sph%sph_rj%iflag_rj_center .gt. 0) then
          sph%sph_params%iflag_shell_mode =  iflag_MESH_w_center
        else
          sph%sph_params%iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      call copy_sph_radial_groups(sph_grps, sph_maker%gen_sph)
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_MHD_w_LIC'
      call const_FEM_mesh_4_sph_MHD_w_LIC(FEM_mesh_flags, mesh_file,    &
     &    sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    femmesh_s%mesh, femmesh_s%group, sph_maker%gen_sph)
!      call compare_mesh_type                                           &
!     &   (my_rank, geofem%mesh%nod_comm, mesh%node, mesh%ele,          &
!     &    femmesh_s%mesh)
!      call compare_mesh_groups(geofem%group%nod_grp, femmesh_s%group)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_mesh_and_group'
      femmesh_s%mesh%ele%first_ele_type                                 &
     &   = set_cube_eletype_from_num(femmesh_s%mesh%ele%nnod_4_ele)
      call copy_mesh_and_group(femmesh_s%mesh, femmesh_s%group,         &
     &    geofem%mesh, geofem%group)
      call dealloc_groups_data(femmesh_s%group)
      call dealloc_mesh_geometry_base(femmesh_s%mesh)
      call dealloc_gen_sph_radial_groups(sph_maker%gen_sph)
!
      end subroutine load_FEM_mesh_4_SPH_w_LIC
!
! -----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_sph_MHD_w_LIC                         &
     &         (FEM_mesh_flags, mesh_file, sph_params, sph_rtp, sph_rj, &
     &          mesh, group, gen_sph)
!
      use m_elapsed_labels_gen_SPH
      use m_work_time
      use sph_file_IO_select
      use mpi_load_mesh_data
      use para_const_kemoview_mesh
      use sleeve_extend
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
      type(communication_table) :: ele_comm
      type(parallel_make_vierwer_mesh) :: par_view
      integer(kind = kint) :: i_level
!
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+9)
      call base_FEM_mesh_sph_mhd(sph_params, sph_rtp, sph_rj,           &
     &    mesh, group, gen_sph)
      if(iflag_GSP_time) call end_elapsed_time(ied_elapsed_GSP+9)
!
! Increase sleeve size
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+10)
      sleeve_exp_p1%iflag_expand = iflag_ele_count
      sleeve_exp_p1%dist_max = real(gen_sph%num_FEM_sleeve) * 0.9
      call sleeve_extension_loop(sleeve_exp_p1, mesh, group, ele_comm)
      call dealloc_comm_table(ele_comm)
      if(iflag_GSP_time) call end_elapsed_time(ied_elapsed_GSP+10)
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
