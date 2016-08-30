!>@file   parallel_load_data_4_sph.f90
!!@brief  module parallel_load_data_4_sph
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Load spherical harmonics indexing data on multiple processes
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh(sph, comms_sph, sph_grps, &
!!     &         mesh, group, ele_mesh)
!!      subroutine load_para_SPH_rj_mesh(sph, comms_sph, sph_grps)
!!      subroutine load_para_sph_mesh(sph, bc_rtp_grp, sph_grps)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!
!!      subroutine load_para_rj_mesh(sph_param, sph_rj, comm_rj,        &
!!     &          radial_rj_grp, sphere_rj_grp)
!!         type(sph_shell_parameters), intent(inout) :: sph_param
!!         type(sph_rtp_grid), intent(inout) :: sph_rtp
!!         type(sph_rtm_grid), intent(inout) :: sph_rtm
!!         type(sph_rlm_grid), intent(inout) :: sph_rlm
!!         type(sph_rj_grid), intent(inout) :: sph_rj
!!         type(sph_comm_tbl), intent(inout) :: comm_rj
!!         type(group_data), intent(inout) :: radial_rj_grp
!!         type(group_data), intent(inout) :: sphere_rj_grp
!!@endverbatim
!
      module parallel_load_data_4_sph
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_mesh
!
      implicit none
!
      private :: set_index_flags_4_SPH
      private :: count_interval_4_each_dir, self_comm_flag
      private :: set_fem_center_mode_4_SPH, load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh(sph, comms_sph, sph_grps,   &
     &         mesh, group, ele_mesh)
!
      use t_mesh_data
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      call load_FEM_mesh_4_SPH                                          &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rj,                      &
     &    sph_grps%radial_rtp_grp, sph_grps%radial_rj_grp,              &
     &    mesh, group, ele_mesh)
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_rj_mesh(sph, comms_sph, sph_grps)
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
!
      call load_para_rj_mesh                                            &
     &   (sph%sph_params, sph%sph_rj, comms_sph%comm_rj,                &
     &    sph_grps%radial_rj_grp, sph_grps%sphere_rj_grp)
!
      end subroutine load_para_SPH_rj_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH(sph_param, sph_rtp, sph_rj,        &
     &          radial_rtp_grp, radial_rj_grp, mesh, group, ele_mesh)
!
      use calypso_mpi
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use m_spheric_constants
      use load_mesh_data
      use copy_mesh_structures
      use const_FEM_mesh_sph_mhd
      use gen_sph_grids_modes
      use mesh_IO_select
!
      type(sph_shell_parameters), intent(inout) :: sph_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rtp_grp
      type(group_data), intent(in) :: radial_rj_grp
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      type(mesh_geometry) :: mesh_s
      type(mesh_groups) ::  group_s
!
!
!  --  load FEM mesh data
      if(check_exist_mesh(my_rank) .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'input_mesh'
        call input_mesh(my_rank, mesh, group,                           &
     &      ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
        call allocate_ele_geometry_type(mesh%ele)
        call set_fem_center_mode_4_SPH                                  &
     &     (mesh%node%internal_node, sph_rtp, sph_param)
        return
      end if
!
!  --  Construct FEM mesh
      if(sph_param%iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(sph_rj%iflag_rj_center .gt. 0) then
          sph_param%iflag_shell_mode =  iflag_MESH_w_center
        else
          sph_param%iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      if (iflag_debug.gt.0) write(*,*) 'const_FEM_mesh_4_sph_mhd'
      call const_FEM_mesh_4_sph_mhd                                     &
     &   (sph_param, sph_rtp, sph_rj, radial_rtp_grp,                   &
     &    radial_rj_grp, mesh_s, group_s)
!      call compare_mesh_type                                           &
!     &   (my_rank, mesh%nod_comm, mesh%node, mesh%ele, mesh_s)
!      call compare_group_types(my_rank, group%nod_grp, group_s%nod_grp)
!      call compare_group_types(my_rank, group%ele_grp, group_s%ele_grp)
!      call compare_surface_grp_types                                   &
!     &   (my_rank, group%surf_grp, group_s%surf_grp)
!
      call set_mesh_data_from_type(mesh_s, group_s,                     &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      ele_mesh%surf, ele_mesh%edge,                               &
     &      group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      use calypso_mpi
      use m_machine_parameter
!
      use load_data_for_sph_IO
      use set_from_recv_buf_rev
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtp_sph_trans'
      call input_geom_rtp_sph_trans(my_rank,                            &
     &    sph%sph_params%l_truncation, sph%sph_rtp, comms_sph%comm_rtp, &
     &    sph_grps%bc_rtp_grp, sph_grps%radial_rtp_grp,                 &
     &    sph_grps%theta_rtp_grp, sph_grps%zonal_rtp_grp, ierr)
      call set_reverse_import_table(sph%sph_rtp%nnod_rtp,               &
     &    comms_sph%comm_rtp%ntot_item_sr, comms_sph%comm_rtp%item_sr,  &
     &    comms_sph%comm_rtp%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank,                            &
     &    sph%sph_params%l_truncation, sph%sph_rj, comms_sph%comm_rj,   &
     &    sph_grps%radial_rj_grp, sph_grps%sphere_rj_grp, ierr)
      call set_reverse_import_table(sph%sph_rj%nnod_rj,                 &
     &    comms_sph%comm_rj%ntot_item_sr, comms_sph%comm_rj%item_sr,    &
     &    comms_sph%comm_rj%irev_sr)
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtm_sph_trans'
      call input_geom_rtm_sph_trans                                     &
     &   (my_rank, sph%sph_params%l_truncation,                         &
     &    sph%sph_rtm, comms_sph%comm_rtm, ierr)
      call set_reverse_import_table(sph%sph_rtm%nnod_rtm,               &
     &    comms_sph%comm_rtm%ntot_item_sr, comms_sph%comm_rtm%item_sr,  &
     &    comms_sph%comm_rtm%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rlm_sph_trans'
      call input_modes_rlm_sph_trans                                    &
     &   (my_rank, sph%sph_params%l_truncation,                         &
     &    sph%sph_rlm, comms_sph%comm_rlm, ierr)
      call set_reverse_import_table(sph%sph_rlm%nnod_rlm,               &
     &    comms_sph%comm_rlm%ntot_item_sr, comms_sph%comm_rlm%item_sr,  &
     &    comms_sph%comm_rlm%irev_sr)
!
      if (iflag_debug.gt.0) write(*,*) 'set_index_flags_4_SPH'
      call set_index_flags_4_SPH(sph%sph_params,                        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj,            &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm,                       &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
      end subroutine load_para_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_index_flags_4_SPH                                  &
     &         (sph_param, sph_rtp, sph_rtm, sph_rlm, sph_rj,           &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj)
!
      use calypso_mpi
      use m_machine_parameter
!
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      type(sph_shell_parameters), intent(inout) :: sph_param
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
!
      comm_rtp%iflag_self                                               &
     &    = self_comm_flag(comm_rtp%nneib_domain, comm_rtp%id_domain)
      comm_rtm%iflag_self                                               &
     &    = self_comm_flag(comm_rtm%nneib_domain, comm_rtm%id_domain)
      comm_rlm%iflag_self                                               &
     &    = self_comm_flag(comm_rlm%nneib_domain, comm_rlm%id_domain)
      comm_rj%iflag_self                                                &
     &    = self_comm_flag(comm_rj%nneib_domain,  comm_rj%id_domain)
!
      call count_interval_4_each_dir(ithree, sph_rtp%nnod_rtp,          &
     &    sph_rtp%idx_global_rtp, sph_rtp%istep_rtp)
      call count_interval_4_each_dir(ithree, sph_rtm%nnod_rtm,          &
     &    sph_rtm%idx_global_rtm, sph_rtm%istep_rtm)
      call count_interval_4_each_dir(itwo,   sph_rlm%nnod_rlm,          &
     &    sph_rlm%idx_global_rlm, sph_rlm%istep_rlm)
      call count_interval_4_each_dir(itwo,   sph_rj%nnod_rj,            &
     &    sph_rj%idx_global_rj, sph_rj%istep_rj)
!
      sph_param%m_folding = 2 * sph_rtp%idx_gl_1d_rtp_p(2,2)            &
     &                      / sph_rtp%nidx_rtp(3)
!
      call set_special_degree_order_flags                               &
     &   (sph_rj%nidx_rj(2), sph_rlm%nidx_rlm(2),                       &
     &    sph_rj%idx_gl_1d_rj_j, sph_rlm%idx_gl_1d_rlm_j,               &
     &    sph_rj%idx_rj_degree_zero, sph_rj%idx_rj_degree_one,          &
     &    sph_rtm%ist_rtm_order_zero, sph_rtm%ist_rtm_order_1s,         &
     &    sph_rtm%ist_rtm_order_1c)
!
!
      call set_sph_rj_center_flag(sph_rj%nnod_rj, sph_rj%nidx_rj,       &
     &    sph_rj%inod_rj_center)
!
      sph_rj%iflag_rj_center = 0
      call MPI_allREDUCE                                                &
     &   (sph_rj%inod_rj_center, sph_rj%iflag_rj_center, ione,          &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      if(sph_rj%iflag_rj_center .gt. 0) sph_rj%iflag_rj_center = 1
!
      end subroutine set_index_flags_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine set_fem_center_mode_4_SPH                              &
     &         (internal_node, sph_rtp, sph_param)
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_constants
!
      integer(kind = kint), intent(in) :: internal_node
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_shell_parameters), intent(inout) :: sph_param
!
      integer(kind = kint) :: iflag_shell_local, nsample
      integer(kind = kint) :: nnod_full_shell
      integer(kind = kint) :: nnod_npole, nnod_npole_ctr
      integer(kind = kint) :: nnod_spole, nnod_spole_ctr
!
!
      nnod_full_shell = sph_rtp%nnod_rtp * sph_param%m_folding
      nnod_npole = nnod_full_shell + sph_rtp%nidx_rtp(1)
      nnod_spole = nnod_full_shell + 2 * sph_rtp%nidx_rtp(1)
      nnod_npole_ctr = nnod_npole + 1
      nnod_spole_ctr = nnod_spole + 1
!
      nsample = internal_node
      sph_param%iflag_shell_mode = 0
      if(nsample .le. nnod_full_shell) then
        iflag_shell_local = iflag_MESH_same
      else if(nsample .eq. nnod_npole) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(nsample .eq. nnod_spole) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(nsample .eq. nnod_npole_ctr) then
        iflag_shell_local = iflag_MESH_w_center
      else if(nsample .eq. nnod_spole_ctr) then
        iflag_shell_local = iflag_MESH_w_center
      end if
!
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_local',   &
     &     my_rank, iflag_shell_local, internal_node, nnod_full_shell
      call MPI_allreduce(iflag_shell_local, sph_param%iflag_shell_mode, &
     &    ione, CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_mode',    &
     &     my_rank, sph_param%iflag_shell_mode
!
      end subroutine set_fem_center_mode_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_rj_mesh(sph_param, sph_rj, comm_rj,          &
     &          radial_rj_grp, sphere_rj_grp)
!
      use calypso_mpi
      use m_machine_parameter
!
      use load_data_for_sph_IO
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      type(sph_shell_parameters), intent(inout) :: sph_param
      type(sph_rj_grid), intent(inout) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: comm_rj
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank, sph_param%l_truncation,    &
     &    sph_rj, comm_rj, radial_rj_grp, sphere_rj_grp, ierr)
      call set_reverse_import_table(sph_rj%nnod_rj,                     &
     &    comm_rj%ntot_item_sr, comm_rj%item_sr, comm_rj%irev_sr)
!
!      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message_Rsmp)
      comm_rj%iflag_self                                                &
     &    =  self_comm_flag(comm_rj%nneib_domain,  comm_rj%id_domain)
!
      call count_interval_4_each_dir(itwo, sph_rj%nnod_rj,              &
     &    sph_rj%idx_global_rj, sph_rj%istep_rj)
!
      call set_sph_rj_center_flag                                       &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj, sph_rj%inod_rj_center)
!
      sph_rj%iflag_rj_center = 0
      call MPI_allREDUCE                                                &
     &   (sph_rj%inod_rj_center, sph_rj%iflag_rj_center, ione,          &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      if(sph_rj%iflag_rj_center .gt. 0) sph_rj%iflag_rj_center = 1
!
      end subroutine load_para_rj_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_interval_4_each_dir(numdir, nnod, idx_global,    &
     &    istep)
!
      integer(kind = kint), intent(in) :: numdir, nnod
      integer(kind = kint), intent(in) :: idx_global(nnod,numdir)
!
      integer(kind = kint), intent(inout) :: istep(numdir)
!
      integer(kind = kint) :: nd, inod, iref
!
!
      do nd = 1, numdir
        iref = idx_global(1,nd)
        do inod = 2, nnod
          if(idx_global(inod,nd) .ne. iref) then
            istep(nd) = inod - 1
            exit
          end if
        end do
      end do
!
      end subroutine count_interval_4_each_dir
!
! -----------------------------------------------------------------------
!
      integer function self_comm_flag(num_neib, id_neib)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      self_comm_flag = 0
      if(id_neib(num_neib) .eq. my_rank) self_comm_flag = 1
!
      end function self_comm_flag
!
! -----------------------------------------------------------------------
!
      end module parallel_load_data_4_sph
