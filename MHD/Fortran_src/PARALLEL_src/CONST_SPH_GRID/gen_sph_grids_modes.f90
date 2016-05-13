!>@file   gen_sph_grids_modes.f90
!!@brief  module gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Serial version)
!!
!!@verbatim
!!      subroutine const_sph_rlm_modes(ip_rank, sph_rlm, comm_rlm)
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!      subroutine const_sph_rtm_grids(ip_rank, sph_rtm, comm_rtm)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!
!!      subroutine const_fem_mesh_for_sph                               &
!!     &         (ip_rank, sph_params, radial_rj_grp, sph_rtp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!@endverbatim
!
      module gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>      Integer flag to excluding FEM mesh
      integer(kind = kint) :: iflag_excluding_FEM_mesh = 0
!
      private :: const_comm_table_4_rlm, const_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rlm_modes(ip_rank, sph_rlm, comm_rlm)
!
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
!
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer(kind = kint), intent(in) :: ip_rank
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
!
      call copy_gl_2_local_rlm_param(ip_rank, sph_rlm)
!
!      nnod_rlm = sph_rlm%nnod_rlm
!      nidx_rlm(1:2) = sph_rlm%nidx_rlm(1:2)
      call alloc_type_spheric_param_rlm(sph_rlm)
      call alloc_type_sph_1d_index_rlm(sph_rlm)
!
      call copy_sph_1d_gl_idx_rlm(sph_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_global_sph_4_rlm', ip_rank
      call set_global_sph_4_rlm(sph_rlm)
!
      if(iflag_debug .gt. 0) then
        call check_type_spheric_param_rlm(ip_rank, sph_rlm)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rlm', ip_rank
      call const_comm_table_4_rlm(ip_rank, sph_rlm, comm_rlm)
!
      end subroutine const_sph_rlm_modes
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_rtm_grids(ip_rank, sph_rtm, comm_rtm)
!
      use t_spheric_rtm_data
      use t_sph_trans_comm_tbl
!
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer(kind = kint), intent(in) :: ip_rank
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
!
      call copy_gl_2_local_rtm_param(ip_rank, sph_rtm)
!
      call alloc_type_spheric_param_rtm(sph_rtm)
      call alloc_type_sph_1d_index_rtm(sph_rtm)
!
      call copy_sph_1d_gl_idx_rtm(sph_rtm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_global_sph_4_rtm', ip_rank
      call set_global_sph_4_rtm(sph_rtm)
!
      if(iflag_debug .gt. 0) then
        call check_type_spheric_param_rtm(ip_rank, sph_rtm)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rtm', ip_rank
      call const_comm_table_4_rtm(ip_rank, sph_rtm, comm_rtm)
!
      end subroutine const_sph_rtm_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_fem_mesh_for_sph                                 &
     &         (ip_rank, sph_params, radial_rj_grp, sph_rtp)
!
      use t_spheric_parameter
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
!
      use m_node_id_spherical_IO
      use m_read_mesh_data
      use m_read_boundary_data
      use m_gauss_points
      use m_sph_mesh_1d_connect
!
      use set_local_sphere_by_global
      use set_FEM_mesh_4_sph
      use set_comm_table_4_IO
      use set_node_data_4_IO
      use set_element_data_4_IO
      use mesh_IO_select
!
      integer(kind = kint), intent(in) :: ip_rank
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      type(mesh_geometry) :: mesh
      type(mesh_groups) ::  group
!
!
      call copy_gl_2_local_rtp_param(ip_rank, sph_rtp)
      nidx_local_fem(1:3) = sph_rtp%nidx_rtp(1:3)
      nidx_local_fem(3) =   sph_params%m_folding * nidx_local_fem(3)
!
      call s_const_FEM_mesh_for_sph                                     &
     &   (ip_rank, sph_rtp%nidx_rtp, radius_1d_gl,                      &
     &    sph_params, radial_rj_grp, mesh, group)
!
      call copy_comm_tbl_type_to_IO(ip_rank, mesh%nod_comm)
      call copy_node_geometry_to_IO(mesh%node)
      call copy_ele_connect_to_IO(mesh%ele)
      call set_grp_data_to_IO                                           &
     &   (group%nod_grp, group%ele_grp, group%surf_grp)
!
      call dealloc_groups_data(group)
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
      call deallocate_type_comm_tbl(mesh%nod_comm)
!
      mesh_file_head = sph_file_head
      call sel_write_mesh_file(ip_rank)
!
      write(*,'(a,i6,a)')                                               &
     &          'FEM mesh for domain', ip_rank, ' is done.'
!
      end subroutine const_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rlm(ip_rank, sph_rlm, comm_rlm)
!
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use set_comm_table_rtm_rlm
!
      integer(kind = kint), intent(in) :: ip_rank
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
!
      call allocate_ncomm
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'count_comm_table_4_rlm', ip_rank
      call count_comm_table_4_rlm                                       &
     &   (sph_rlm%nnod_rlm, sph_rlm%idx_global_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'count_num_domain_rtm_rlm', ip_rank
      call count_num_domain_rtm_rlm(comm_rlm%nneib_domain)
!
      call alloc_type_sph_comm_stack(comm_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_comm_stack_rtm_rlm', ip_rank
      call set_comm_stack_rtm_rlm                                       &
     &   (ip_rank, comm_rlm%nneib_domain, comm_rlm%id_domain,           &
     &    comm_rlm%istack_sr, comm_rlm%ntot_item_sr)
!
      call alloc_type_sph_comm_item(sph_rlm%nnod_rlm, comm_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_comm_table_4_rlm', ip_rank
      call set_comm_table_4_rlm                                         &
     &   (sph_rlm%nnod_rlm, sph_rlm%idx_global_rlm,                     &
     &    comm_rlm%nneib_domain, comm_rlm%ntot_item_sr,                 &
     &    comm_rlm%istack_sr, comm_rlm%item_sr)
      call deallocate_ncomm
!
!      call allocate_idx_gl_rlm_out
!      call set_global_id_4_comm_rlm
!
      end subroutine const_comm_table_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtm(ip_rank, sph_rtm, comm_rtm)
!
      use t_spheric_rtm_data
      use t_sph_trans_comm_tbl
      use set_comm_table_rtm_rlm
!
      integer(kind = kint), intent(in) :: ip_rank
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
!
!      write(*,*) 'allocate_ncomm'
      call allocate_ncomm
!
!      write(*,*) 'count_comm_table_4_rtm'
      call count_comm_table_4_rtm(sph_rtm%nnod_rtm,                     &
     &    sph_rtm%nidx_global_rtm, sph_rtm%idx_global_rtm)
!
!      write(*,*) 'count_num_domain_rtm_rlm'
      call count_num_domain_rtm_rlm(comm_rtm%nneib_domain)
!
!      write(*,*) 'alloc_type_sph_comm_stack'
      call alloc_type_sph_comm_stack(comm_rtm)
!
!      write(*,*) 'set_comm_stack_rtm_rlm'
      call set_comm_stack_rtm_rlm                                       &
     &   (ip_rank, comm_rtm%nneib_domain, comm_rtm%id_domain,           &
     &    comm_rtm%istack_sr, comm_rtm%ntot_item_sr)
!
      call alloc_type_sph_comm_item(sph_rtm%nnod_rtm, comm_rtm)
!
!      write(*,*) 'set_comm_table_4_rtm'
      call set_comm_table_4_rtm(sph_rtm%nnod_rtm,                       &
     &    sph_rtm%nidx_global_rtm, sph_rtm%idx_global_rtm,              &
     &    comm_rtm%nneib_domain, comm_rtm%ntot_item_sr,                 &
     &    comm_rtm%istack_sr, comm_rtm%item_sr)
      call deallocate_ncomm
!
!      call allocate_idx_gl_rtm_out
!      write(*,*) 'set_global_id_4_comm_rtm'
!      call set_global_id_4_comm_rtm
!
      end subroutine const_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      end module gen_sph_grids_modes
