!>@file   parallel_load_data_4_sph.f90
!!@brief  module parallel_load_data_4_sph
!!
!!@date  Programmed by H.Matsui on July., 2007
!
!>@brief Load spherical harmonics indexing data on multiple processes
!!
!!@verbatim
!!      subroutine load_para_SPH_and_FEM_mesh(mesh, group, ele_mesh)
!!      subroutine load_para_sph_mesh
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!@endverbatim
!
      module parallel_load_data_4_sph
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: count_interval_4_each_dir, self_comm_flag
      private :: set_fem_center_mode_4_SPH, load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_para_SPH_and_FEM_mesh(mesh, group, ele_mesh)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      call load_para_sph_mesh
      call load_FEM_mesh_4_SPH(mesh, group, ele_mesh)
!
      end subroutine load_para_SPH_and_FEM_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_FEM_mesh_4_SPH(mesh, group, ele_mesh)
!
      use calypso_mpi
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
!
      use m_spheric_constants
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
      use load_mesh_data
      use copy_mesh_structures
      use const_FEM_mesh_sph_mhd
      use mesh_IO_select
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
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
        call set_fem_center_mode_4_SPH(mesh%node%internal_node)
        return
      end if
!
!  --  Construct FEM mesh
      if(sph_param1%iflag_shell_mode .eq. iflag_no_FEMMESH) then
        if(sph_rj1%iflag_rj_center .gt. 0) then
          sph_param1%iflag_shell_mode =  iflag_MESH_w_center
        else
          sph_param1%iflag_shell_mode = iflag_MESH_same
        end if
      end if
!
      call const_FEM_mesh_4_sph_mhd(mesh_s, group_s)
!      call compare_mesh_type                                           &
!     &   (my_rank, mesh%nod_comm, mesh%node, mesh%ele, mesh_s)
!      call compare_group_types(my_rank, group%nod_grp, group_s%nod_grp)
!      call compare_group_types(my_rank, group%ele_grp, group_s%ele_grp)
!      call compare_surface_grp_types                                   &
!     &   (my_rank, group%surf_grp, group_s%surf_grp)
!
      call set_mesh_data_from_type(mesh_s, group_s,                     &
     &    mesh%nod_comm, mesh%node, mesh%ele,                           &
     &    ele_mesh%surf, ele_mesh%edge,                                 &
     &    group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine load_FEM_mesh_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_sph_mesh
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
!
      use load_data_for_sph_IO
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtp_sph_trans'
      call input_geom_rtp_sph_trans(my_rank, sph_param1%l_truncation,   &
     &    sph_rtp1, comm_rtp1, bc_rtp_grp1, &
     &          radial_rtp_grp1, theta_rtp_grp1, zonal_rtp_grp)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank, sph_param1%l_truncation,   &
     &    sph_rj1, comm_rj1, radial_rj_grp1, sphere_rj_grp1)
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_geom_rtm_sph_trans'
      call input_geom_rtm_sph_trans                                     &
     &   (my_rank, sph_param1%l_truncation, sph_rtm1, comm_rtm1)
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rlm_sph_trans'
      call input_modes_rlm_sph_trans                                    &
     &   (my_rank, sph_param1%l_truncation, sph_rlm1, comm_rlm1)
!
      nnod_rtp =      sph_rtp1%nnod_rtp
      nidx_rtp(1:3) = sph_rtp1%nidx_rtp(1:3)
      nnod_rj =       sph_rj1%nnod_rj
      nidx_rj(1:2) =  sph_rj1%nidx_rj(1:2)
      nnod_rtm =      sph_rtm1%nnod_rtm
      nidx_rtm(1:3) = sph_rtm1%nidx_rtm(1:3)
      nnod_rlm =      sph_rlm1%nnod_rlm
      nidx_rlm(1:2) = sph_rlm1%nidx_rlm(1:2)
!
      if (iflag_debug.gt.0) write(*,*) 'set_reverse_tables_4_SPH'
      call set_reverse_tables_4_SPH
!
      end subroutine load_para_sph_mesh
!
! -----------------------------------------------------------------------
!
      subroutine set_reverse_tables_4_SPH
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_sph_trans_comm_table
!
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 's_count_num_sph_smp'
      call s_count_num_sph_smp                                          &
     &   (sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1, ierr)
!      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message_Rsmp)
!
      call set_reverse_import_table(sph_rtp1%nnod_rtp,                  &
     &    comm_rtp1%ntot_item_sr, comm_rtp1%item_sr, comm_rtp1%irev_sr)
      call set_reverse_import_table(sph_rtm1%nnod_rtm,                  &
     &    comm_rtm1%ntot_item_sr, comm_rtm1%item_sr, comm_rtm1%irev_sr)
      call set_reverse_import_table(sph_rlm1%nnod_rlm,                  &
     &    comm_rlm1%ntot_item_sr, comm_rlm1%item_sr, comm_rlm1%irev_sr)
      call set_reverse_import_table(sph_rj1%nnod_rj,                    &
     &    comm_rj1%ntot_item_sr,  comm_rj1%item_sr,  comm_rj1%irev_sr)
!
      comm_rtp1%iflag_self                                              &
     &    = self_comm_flag(comm_rtp1%nneib_domain, comm_rtp1%id_domain)
      comm_rtm1%iflag_self                                              &
     &    = self_comm_flag(comm_rtm1%nneib_domain, comm_rtm1%id_domain)
      comm_rlm1%iflag_self                                              &
     &    = self_comm_flag(comm_rlm1%nneib_domain, comm_rlm1%id_domain)
      comm_rj1%iflag_self                                               &
     &    = self_comm_flag(comm_rj1%nneib_domain,  comm_rj1%id_domain)
!
      call count_interval_4_each_dir(ithree, sph_rtp1%nnod_rtp,         &
     &    sph_rtp1%idx_global_rtp, sph_rtp1%istep_rtp)
      call count_interval_4_each_dir(ithree, sph_rtm1%nnod_rtm,         &
     &    sph_rtm1%idx_global_rtm, sph_rtm1%istep_rtm)
      call count_interval_4_each_dir(itwo,   sph_rlm1%nnod_rlm,         &
     &    sph_rlm1%idx_global_rlm, sph_rlm1%istep_rlm)
      call count_interval_4_each_dir(itwo,   sph_rj1%nnod_rj,           &
     &    sph_rj1%idx_global_rj, sph_rj1%istep_rj)
!
      sph_param1%m_folding = 2 * sph_rtp1%idx_gl_1d_rtp_p(2,2)          &
     &                      / sph_rtp1%nidx_rtp(3)
!
      call set_special_degree_order_flags                               &
     &   (sph_rj1%nidx_rj(2), sph_rlm1%nidx_rlm(2),                     &
     &    sph_rj1%idx_gl_1d_rj_j, sph_rlm1%idx_gl_1d_rlm_j,             &
     &    sph_rj1%idx_rj_degree_zero, sph_rj1%idx_rj_degree_one,        &
     &    sph_rtm1%ist_rtm_order_zero, sph_rtm1%ist_rtm_order_1s,       &
     &    sph_rtm1%ist_rtm_order_1c)
!
!
      call set_sph_rj_center_flag(sph_rj1%nnod_rj, sph_rj1%nidx_rj,     &
     &    sph_rj1%inod_rj_center)
!
      sph_rj1%iflag_rj_center = 0
      call MPI_allREDUCE                                                &
     &   (sph_rj1%inod_rj_center, sph_rj1%iflag_rj_center, ione,        &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      if(sph_rj1%iflag_rj_center .gt. 0) sph_rj1%iflag_rj_center = 1
!
      end subroutine set_reverse_tables_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine set_fem_center_mode_4_SPH(internal_node)
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_constants
!
      integer(kind = kint), intent(in) :: internal_node
!
      integer(kind = kint) :: iflag_shell_local, nsample
      integer(kind = kint) :: nnod_full_shell
!
!
      nnod_full_shell = nnod_rtp * sph_param1%m_folding
      nsample = internal_node
      sph_param1%iflag_shell_mode = 0
      if(nsample .le. nnod_full_shell) then
        iflag_shell_local = iflag_MESH_same
      else if(nsample .eq. nnod_full_shell+nidx_rtp(1)) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(nsample .eq. nnod_full_shell+2*nidx_rtp(1)) then
        iflag_shell_local = iflag_MESH_w_pole
      else if(nsample .eq. nnod_full_shell+nidx_rtp(1)+1) then
        iflag_shell_local = iflag_MESH_w_center
      else if(nsample .eq. nnod_full_shell+2*nidx_rtp(1)+1) then
        iflag_shell_local = iflag_MESH_w_center
      end if
!
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_local',   &
     &     my_rank, iflag_shell_local, internal_node, nnod_full_shell
      call MPI_allreduce(iflag_shell_local, sph_param1%iflag_shell_mode, &
     &    ione, CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if(i_debug .eq. iflag_full_msg) write(*,*) 'iflag_shell_mode',    &
     &     my_rank, sph_param1%iflag_shell_mode
!
      end subroutine set_fem_center_mode_4_SPH
!
! -----------------------------------------------------------------------
!
      subroutine load_para_rj_mesh
!
      use calypso_mpi
      use m_machine_parameter
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
!
      use load_data_for_sph_IO
      use count_num_sph_smp
      use set_special_sph_lm_flags
!
      use set_from_recv_buf_rev
!
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'input_modes_rj_sph_trans'
      call input_modes_rj_sph_trans(my_rank, sph_param1%l_truncation,   &
     &    sph_rj1, comm_rj1, radial_rj_grp1, sphere_rj_grp1)
      nnod_rj =      sph_rj1%nnod_rj
      nidx_rj(1:2) = sph_rj1%nidx_rj(1:2)
!
      if (iflag_debug.gt.0) write(*,*) 's_count_num_sph_smp'
      call s_count_num_sph_smp                                          &
     &   (sph_rtp1, sph_rtm1, sph_rlm1, sph_rj1, ierr)
!      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message_Rsmp)
!
      call set_reverse_import_table                                     &
     &   (sph_rj1%nnod_rj, comm_rj1%ntot_item_sr,                       &
     &    comm_rj1%item_sr, comm_rj1%irev_sr)
      comm_rj1%iflag_self                                               &
     &    =  self_comm_flag(comm_rj1%nneib_domain,  comm_rj1%id_domain)
!
      call count_interval_4_each_dir(itwo, sph_rj1%nnod_rj,             &
     &    sph_rj1%idx_global_rj, sph_rj1%istep_rj)
!
      call set_sph_rj_center_flag                                       &
     &   (sph_rj1%nnod_rj, sph_rj1%nidx_rj, sph_rj1%inod_rj_center)
!
      sph_rj1%iflag_rj_center = 0
      call MPI_allREDUCE                                                &
     &   (sph_rj1%inod_rj_center, sph_rj1%iflag_rj_center, ione,        &
     &    CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      if(sph_rj1%iflag_rj_center .gt. 0) sph_rj1%iflag_rj_center = 1
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
